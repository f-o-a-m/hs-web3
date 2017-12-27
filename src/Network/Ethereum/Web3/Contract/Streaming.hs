{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Ethereum.Web3.Contract.Streaming where

import           Network.Ethereum.Unit
import           Network.Ethereum.Web3.Address
import           Network.Ethereum.Web3.Contract
import           Network.Ethereum.Web3.Encoding
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forM, void)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT (..))
import           Control.Monad.Trans.Reader     (ReaderT (..), runReaderT)
import           Data.Machine                   (Is (Refl), ProcessT, Step (Await, Stop, Yield), autoT, runMachineT)
import           Data.Machine.MealyT            (MealyT (..), arrM)
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Text.Lazy                 (toStrict)

data FilterChange a = FilterChange { filterChangeRawChange :: Change
                                   , filterChangeEvent     :: a
                                   }

-- the halting case is provided by Nothing
type HaltingMealyT m = MealyT (MaybeT m)

reduceEventStream :: forall m s a . Monad m
                  => HaltingMealyT m s [FilterChange a]
                  -> (a -> ReaderT Change m EventAction)
                  -> s
                  -> m (Maybe s)
reduceEventStream machine handler initialState =
  let process = autoT machine
  in go process

  where go :: ProcessT (MaybeT m) s [FilterChange a] -> m (Maybe s)
        go process = do
          res <- runMaybeT $ runMachineT process
          case res of
            Nothing   -> return $ Just initialState
            Just Stop -> return $ Just initialState -- Pretty sure this will never get called,
                                                    -- due to autoT for MealyT
            Just (Yield changes next) -> do
              acts <- processChanges handler changes
              if TerminateEvent `notElem` acts
              then go next
              else return Nothing
            Just (Await f Refl next) -> go (f initialState) >> go next -- This is a guess


        processChanges :: (a -> ReaderT Change m EventAction) -> [FilterChange a] -> m [EventAction]
        processChanges handler changes = forM changes $ \FilterChange{..} ->
                                           flip runReaderT filterChangeRawChange $
                                                handler filterChangeEvent

data FilterStreamState = FilterStreamState { fssCurrentBlock  :: BlockNumber
                                           , fssInitialFilter :: Filter
                                           , fssWindowSize    :: Integer
                                           }


-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these
-- | 'Filter' objects.
playLogs :: forall p a. (Provider p, ABIEncoding a)
         => HaltingMealyT (Web3 p) FilterStreamState [FilterChange a]
playLogs  = do
  filter <- filterStream
  changes <- arrM . const . lift $ Eth.getLogs filter
  return $ mkFilterChanges changes

pollFilter :: forall p a s . (Provider p, ABIEncoding a)
           => FilterId
           -> DefaultBlock
           -> HaltingMealyT (Web3 p) s [FilterChange a]
pollFilter fid stop = MealyT $ \s -> do
  bn <- lift Eth.blockNumber
  if BlockWithNumber bn > stop
  then do
    lift $ Eth.uninstallFilter fid
    MaybeT $ return Nothing
  else do
    lift . Web3 $ threadDelay 1000
    changes <- lift $ Eth.getFilterChanges fid
    return (mkFilterChanges changes, pollFilter fid stop)

mkFilterChanges :: ABIEncoding a => [Change] -> [FilterChange a]
mkFilterChanges cs = flip mapMaybe cs $ \c@Change{..} -> do
                       x <- fromData changeData
                       return $ FilterChange c x

filterStream :: forall p . Provider p
             => HaltingMealyT (Web3 p) FilterStreamState Filter
filterStream = MealyT $ \FilterStreamState{..} ->
    case filterToBlock fssInitialFilter of
        Nothing -> MaybeT (return Nothing)
        Just end ->
            if BlockWithNumber fssCurrentBlock > end
            then MaybeT $ return Nothing -- halt
            else do
                  to' <- newTo end (BlockWithNumber fssCurrentBlock) fssWindowSize
                  succTo' <- succ to'
                  let filter' = fssInitialFilter { filterFromBlock = Just (BlockWithNumber fssCurrentBlock)
                                                 , filterToBlock = Just to'
                                                 }
                      MealyT next = filterStream
                  return (filter', MealyT $ \s -> next s { fssCurrentBlock = succTo' })

    where succ :: DefaultBlock -> MaybeT (Web3 p) BlockNumber
          succ (BlockWithNumber (BlockNumber bn)) = return . BlockNumber $ bn + 1
          succ x = do
              bn' <- lift $ resolveDefaultBlock (Just x)
              succ (BlockWithNumber bn')

          newTo :: DefaultBlock -> DefaultBlock -> Integer -> MaybeT (Web3 p) DefaultBlock
          newTo upper current window = do
              current' <- lift $ resolveDefaultBlock (Just current)
              return $ min upper (BlockWithNumber (current' + BlockNumber window))


event' :: (ABIEncoding a, Provider p)
       => Filter
       -> Integer
       -> (a -> ReaderT Change (Web3 p) EventAction)
       -> Web3 p ()
event' fltr window handler = do
  start <- resolveDefaultBlock (filterFromBlock fltr)
  let initState = FilterStreamState { fssCurrentBlock = start
                                    , fssInitialFilter = fltr
                                    , fssWindowSize = window
                                    }
  mLastProcessedFilterState <- reduceEventStream playLogs handler initState
  case mLastProcessedFilterState of
    Nothing -> return ()
    Just lastProcessedFilterState -> do
      let BlockNumber lastBlock = fssCurrentBlock lastProcessedFilterState
          pollingFromBlock = BlockNumber $ lastBlock + 1
          pollTo = fromMaybe Latest (filterToBlock fltr)
      filterId <- Eth.newFilter fltr { filterFromBlock = Just (BlockWithNumber pollingFromBlock) }
      void $ reduceEventStream (pollFilter filterId pollTo) handler ()

event :: forall p a . (Provider p, ABIEncoding a)
      => Filter
      -> (a -> ReaderT Change (Web3 p) EventAction)
      -> Web3 p ()
event fltr = event' fltr 0

resolveDefaultBlock :: (Provider p) => Maybe DefaultBlock -> Web3 p BlockNumber
resolveDefaultBlock = \case
    Nothing -> Eth.blockNumber
    Just (BlockWithNumber n) -> return n
    Just x -> do
        block <- Eth.getBlockByNumber . toStrict $ renderDefaultBlock x
        return (blockBlockNumber block)
