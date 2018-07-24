{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

-- Module      :  Network.Ethereum.Web3.Test.SimpleStorage
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- SimpleStorage is a Solidity contract which stores a uint256.
-- The point of this test is to test function calls to update and
-- read the value, as well as an event monitor.

module Network.Ethereum.Web3.Test.LinearizationSpec where

import           Control.Concurrent               (forkIO)
import           Control.Concurrent.Async         (Async, async, wait)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TQueue    (TQueue, flushTQueue,
                                                   newTQueueIO, writeTQueue)
import           Control.Concurrent.STM.TSem      (TSem, newTSem, signalTSem,
                                                   waitTSem)
import           Control.Monad                    (forM, void)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Reader       (ReaderT, ask)
import           Data.Default
import           Data.Either
import           Data.List                        (sort)
import           Data.Maybe                       (fromJust)
import           Network.Ethereum.Contract.Event
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3            hiding (convert)
import qualified Network.Ethereum.Web3.Eth        as Eth
import           Network.Ethereum.Web3.Test.Utils
import           Network.Ethereum.Web3.Types
import           System.Environment               (getEnv)
import           System.Random                    (randomRIO)
import           Test.Hspec

[abiFrom|test-support/build/contracts/abis/Linearization.json|]

spec :: Spec
spec = do
  makeEnv `before` linearizationSpec
  makeEnv `before` floodSpec

floodCount :: Int
floodCount = 200

-- waitTSem will block until the counter is positive (i.e., > 0)
-- so if there's -(floodCount - 1), that means when the floodCount `signalTSem`s are done
-- there will be 1 unit left in the TSem for the waitTSem at the end of a test
floodSemCount :: Int
floodSemCount = -(floodCount - 1)

linearizationSpec :: SpecWith (ContractsEnv, Address)
linearizationSpec = describe "can bundle and linearize events" $ do
    it "can call e12" $ \(ContractsEnv{linearization}, primaryAccount) -> do
      let theCall = callFromTo primaryAccount linearization
      var <- monitorE1OrE2 linearization
      _ <- runWeb3Configured' (e12 theCall)
      res <- takeMVar var
      res `shouldSatisfy` isLeft
    it "can call e21" $ \(ContractsEnv{linearization}, primaryAccount) -> do
      -- wait on the next block
      runWeb3Configured Eth.blockNumber >>= \bn -> awaitBlock (bn + 1)
      let theCall = callFromTo primaryAccount linearization
      var <- monitorE1OrE2 linearization
      _ <- runWeb3Configured' (e21 theCall)
      res <- takeMVar var
      res `shouldSatisfy` isRight

singleFlood :: forall m. (MonadIO m) => Address -> Address -> m Hash
singleFlood from to = liftIO $ do
  rando :: Int <- randomRIO (1, 4)
  let theCall = callFromTo from to
      fnToCall = case rando of
                   1 -> e1
                   2 -> e2
                   3 -> e3
                   4 -> e4
                   _ -> error "got a number outside of (1,4) after randomR (1,4)"

  retryWeb3Configured (fnToCall theCall)

floodSpec :: SpecWith (ContractsEnv, Address)
floodSpec = describe "can correctly demonstrate the difference between `multiEvent` and multiple `event'`s" $ do
      it "properly linearizes with `multiEvent` when flooded" $ \(ContractsEnv{linearization}, primaryAccount) -> do
        recvSem <- liftIO . atomically $ newTSem floodSemCount
        q <- monitorAllFourMulti linearization (Just recvSem)
        sleepBlocks 5 -- to let the filter settle so we dont block indefinitely on missing events?

        -- flood em and wait for all to finish
        hashes <- forM [1..floodCount] . const $ singleFlood primaryAccount linearization
        void $ forM hashes awaitTxMined

        -- wait for all multiEvents to be received and flush em out
        receivedEvents <- liftIO . atomically $ flushTQueue q

        -- the events pushed into the TQueue should already be sorted if they happened in the right order
        length receivedEvents `shouldSatisfy` (>= (floodSemCount `div` 4)) -- did we get at least 1/4 of the events?
        sort receivedEvents `shouldBe` receivedEvents

      it "doesn't properly linearize with multiple `event`s when flooded" $ \(ContractsEnv{linearization}, primaryAccount) -> do
        recvSem <- liftIO . atomically $ newTSem floodSemCount
        q <- monitorAllFourPar linearization (Just recvSem)
        sleepBlocks 5 -- to let the filter settle so we dont block indefinitely on missing events?

        -- flood em and wait for all to finish
        hashes <- forM [1..floodCount] . const $ singleFlood primaryAccount linearization
        void $ forM hashes awaitTxMined

        -- wait for all multiEvents to be received and flush em out
        receivedEvents <- liftIO . atomically $ flushTQueue q

        -- the events pushed into the TQueue should not be sorted if they didnt come in in the right order
        length receivedEvents `shouldSatisfy` (>= (floodSemCount `div` 4)) -- did we get at least 1/4 of the events?
        sort receivedEvents `shouldNotBe` receivedEvents

monitorE1OrE2
  :: Address
  -> IO (MVar (Either E1 E2))
monitorE1OrE2 addr = do
  var <- newEmptyMVar
  let fltr1 = (def :: Filter E1) { filterAddress = Just [addr] }
      fltr2 = (def :: Filter E2) { filterAddress = Just [addr] }
      filters = fltr1 :? fltr2 :? NilFilters
      handler1 e1 = do
        liftIO $ putMVar var (Left e1)
        pure TerminateEvent
      handler2 e2 = do
        liftIO $ putMVar var (Right e2)
        pure TerminateEvent
      handlers = H handler1 :& H handler2 :& RNil
  _ <- runWeb3Configured' $ multiEvent filters handlers
  pure var

data EventTag = ETE1 | ETE2 | ETE3 | ETE4
                deriving (Eq, Read, Show)

instance {-# OVERLAPPING #-} Ord (EventTag, Integer, Integer) where
  (_, b1, t1) `compare` (_, b2, t2) =
    let bCmp = b1 `compare` b2
     in if bCmp == EQ
          then t1 `compare` t2
          else bCmp

monitorAllFourMulti
  :: Address
  -> Maybe TSem
  -> IO (TQueue (EventTag, Integer, Integer)) -- (EventTag, BlockNumber, LogIndex)
monitorAllFourMulti addr sem = do
  q <- newTQueueIO
  let f :: forall a. Default (Filter a) => Filter a
      f = defFilter addr
      h = enqueueingHandler sem q
      filters = f @E1 :? f @E2 :? f @E3 :? f @E4 :? NilFilters
      handlers = h ETE1 :& h ETE2 :& h ETE3 :& h ETE4 :& RNil
  void . runWeb3Configured' $ multiEvent filters handlers
  return q

monitorAllFourPar
  :: Address
  -> Maybe TSem
  -> IO (TQueue (EventTag, Integer, Integer)) -- (EventTag, BlockNumber, LogIndex)
monitorAllFourPar addr sem = do
  q <- newTQueueIO
  let f :: forall a. Default (Filter a) => Filter a
      f = defFilter addr
      h = enqueueingHandler sem q
      unH (H h) = h

  void . runWeb3Configured' $ event' (f @E1) (unH $ h ETE1)
  void . runWeb3Configured' $ event' (f @E2) (unH $ h ETE2)
  void . runWeb3Configured' $ event' (f @E3) (unH $ h ETE3)
  void . runWeb3Configured' $ event' (f @E4) (unH $ h ETE4)
  return q

defFilter :: forall a. Default (Filter a) => Address -> Filter a
defFilter addr = (def :: Filter a) { filterAddress = Just [addr] }

enqueueingHandler :: forall a. Maybe TSem -> TQueue (EventTag, Integer, Integer) -> EventTag -> Handler (ReaderT Change Web3 EventAction) a
enqueueingHandler sem q tag = H . const $ do
  Change{..} <- ask
  let bn = unQuantity $ fromJust changeBlockNumber
      li = unQuantity $ fromJust changeLogIndex
  liftIO . atomically $ do
    writeTQueue q (tag, bn, li)
    case sem of
      Nothing      -> pure ()
      Just recvSem -> signalTSem recvSem
  pure ContinueEvent
