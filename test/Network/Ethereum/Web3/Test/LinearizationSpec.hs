{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

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

import           Control.Concurrent.Async         (waitAny, wait)
import           Control.Concurrent.MVar
import           Control.Monad                    (void)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Default
import           Network.Ethereum.Contract.TH
import           Network.Ethereum.Web3            hiding (convert)
import           Network.Ethereum.Web3.Types
import           System.Environment               (getEnv)
import           Test.Hspec

import           Network.Ethereum.Web3.Test.Utils

[abiFrom|test-support/build/contracts/abis/Linearization.json|]

spec :: Spec
spec = makeEnv `before` linearizationSpec

linearizationSpec :: SpecWith (ContractsEnv, Address)
linearizationSpec = describe "can bundle and linearize events" $ do
    it "can call e12" $ \(ContractsEnv contractAddress _, primaryAccount) -> do
        True `shouldBe` True -- we need to get this far

monitorE1OrE2
  :: Address
  -> IO (Either E1 E2)
monitorE1OrE2 addr = do
  var <- newEmptyMVar
  let fltr1 = (def :: Filter E1) { filterAddress = Just [addr] }
      fltr2 = (def :: Filter E2) { filterAddress = Just [addr] }
  fiber1 <- runWeb3Configured' $ event fltr1 $ \e1 -> do
    liftIO $ putMVar var (Right e1)
    pure TerminateEvent
  fiber2 <- runWeb3Configured' $ event fltr2 $ \e2 -> do
    liftIO $ putMVar var (Right e2)
    pure TerminateEvent
  _ <- waitAny [fiber1, fiber2]
  takeMVar var
