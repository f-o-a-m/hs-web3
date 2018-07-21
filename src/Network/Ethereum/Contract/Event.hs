{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


-- |
-- Module      :  Network.Ethereum.Contract.Event
-- Copyright   :  Alexander Krupenkin 2016-2018
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  unportable
--
-- Ethereum contract event support.
--

module Network.Ethereum.Contract.Event where

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.Async       (Async)
import           Control.Exception              (Exception, throwIO)
import           Control.Monad                  (forM, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     (ReaderT (..))
import           Data.Either                    (lefts, rights)
import           Data.List                      (sortOn)
import           Data.Machine                   (MachineT, asParts, autoM,
                                                 await, construct, final,
                                                 repeatedly, runT, unfoldPlan,
                                                 (~>))
import           Data.Machine.Plan              (PlanT, stop, yield)
import           Data.Maybe                     (catMaybes, fromJust,
                                                 listToMaybe)
import           Data.Monoid                    ((<>))
import           Data.Proxy                     (Proxy (..))
import           Data.Tagged
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           Network.Ethereum.ABI.Event     (DecodeEvent (..))
import qualified Network.Ethereum.Web3.Eth      as Eth
import           Network.Ethereum.Web3.Provider (Web3, forkWeb3)
import           Network.Ethereum.Web3.Types    (Change (..), DefaultBlock (..),
                                                 Filter (..), Quantity)

-- | Event callback control response
data EventAction = ContinueEvent
                 -- ^ Continue to listen events
                 | TerminateEvent
                 -- ^ Terminate event listener
  deriving (Show, Eq)

-- | Run 'event\'' one block at a time.
event :: DecodeEvent i ni e
      => Filter e
      -> (e -> ReaderT Change Web3 EventAction)
      -> Web3 (Async ())
event fltr = forkWeb3 . event' fltr

-- | Same as 'event', but does not immediately spawn a new thread.
event' :: DecodeEvent i ni e
       => Filter e
       -> (e -> ReaderT Change Web3 EventAction)
       -> Web3 ()
event' fltr = eventMany' fltr 0

-- | 'eventMany\'' take s a filter, a window size, and a handler.
--
-- It runs the handler over the results of 'eventLogs' results using
-- 'reduceEventStream'. If no 'TerminateEvent' action is thrown and
-- the toBlock is not yet reached, it then transitions to polling.
--
eventMany' :: DecodeEvent i ni e
           => Filter e
           -> Integer
           -> (e -> ReaderT Change Web3 EventAction)
           -> Web3 ()
eventMany' fltr window handler = do
    start <- mkBlockNumber $ filterFromBlock fltr
    let initState = FilterStreamState { fssCurrentBlock = start
                                      , fssInitialFilter = fltr
                                      , fssWindowSize = window
                                      }
    mLastProcessedFilterState <- reduceEventStream (playLogs initState) handler
    case mLastProcessedFilterState of
      Nothing -> startPolling fltr {filterFromBlock = BlockWithNumber start}
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . filterToBlock $ fltr
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFromBlock = lastBlock + 1
          in startPolling fltr {filterFromBlock = BlockWithNumber pollingFromBlock}
  where
    startPolling fltr' = do
      filterId <- Eth.newFilter fltr'
      let pollTo = filterToBlock fltr'
      void $ reduceEventStream (pollFilter filterId pollTo) handler

-- | Effectively a mapM_ over the machine using the given handler.
reduceEventStream :: Monad m
                  => MachineT m k [FilterChange a]
                  -> (a -> ReaderT Change m EventAction)
                  -> m (Maybe (EventAction, Quantity))
reduceEventStream filterChanges handler = fmap listToMaybe . runT $
       filterChanges
    ~> autoM (processChanges handler)
    ~> asParts
    ~> runWhile (\(act, _) -> act /= TerminateEvent)
    ~> final
  where
    runWhile p = repeatedly $ do
      v <- await
      if p v
        then yield v
        else yield v >> stop
    processChanges :: Monad m
                   => (a -> ReaderT Change m EventAction)
                   -> [FilterChange a]
                   -> m [(EventAction, Quantity)]
    processChanges handler' changes = fmap catMaybes $
        forM changes $ \FilterChange{..} -> do
            act <- flip runReaderT filterChangeRawChange $
                handler' filterChangeEvent
            return ((,) act <$> changeBlockNumber filterChangeRawChange)

data FilterChange a = FilterChange { filterChangeRawChange :: Change
                                   , filterChangeEvent     :: a
                                   } deriving Functor

deriving instance Show a => Show (FilterChange a)

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playLogs :: DecodeEvent i ni e
         => FilterStreamState e
         -> MachineT Web3 k [FilterChange e]
playLogs s = filterStream s
          ~> autoM Eth.getLogs
          ~> autoM (liftIO . mkFilterChanges)

-- | Polls a filter from the given filterId until the target toBlock is reached.
pollFilter :: forall i ni e k . DecodeEvent i ni e
           => Quantity
           -> DefaultBlock
           -> MachineT Web3 k [FilterChange e]
pollFilter i = construct . pollPlan i
  where
    pollPlan :: Quantity -> DefaultBlock -> PlanT k [FilterChange e] Web3 ()
    pollPlan fid end = do
      bn <- lift $ Eth.blockNumber
      if BlockWithNumber bn > end
        then do
          _ <- lift $ Eth.uninstallFilter fid
          stop
        else do
          liftIO $ threadDelay 1000000
          changes <- lift $ Eth.getFilterChanges fid >>= liftIO . mkFilterChanges
          yield $ changes
          pollPlan fid end

data EventParseFailure = EventParseFailure String deriving (Show)

instance Exception EventParseFailure

mkFilterChanges :: DecodeEvent i ni e
                => [Change]
                -> IO [FilterChange e]
mkFilterChanges changes =
  let eChanges = map (\c@Change{..} -> FilterChange c <$> decodeEvent c) changes
      ls = lefts eChanges
      rs = rights eChanges
  in if ls /= [] then throwIO (EventParseFailure $ (show ls)) else pure rs

data FilterStreamState e =
  FilterStreamState { fssCurrentBlock  :: Quantity
                    , fssInitialFilter :: Filter e
                    , fssWindowSize    :: Integer
                    }


-- | 'filterStream' is a machine which represents taking an initial filter
-- over a range of blocks b1, ... bn (where bn is possibly `Latest` or `Pending`,
-- but b1 is an actual block number), and making a stream of filter objects
-- which cover this filter in intervals of size `windowSize`. The machine
-- halts whenever the `fromBlock` of a spanning filter either (1) excedes then
-- initial filter's `toBlock` or (2) is greater than the chain head's block number.
filterStream :: FilterStreamState e
             -> MachineT Web3 k (Filter e)
filterStream initialPlan = unfoldPlan initialPlan filterPlan
  where
    filterPlan :: FilterStreamState e -> PlanT k (Filter e) Web3 (FilterStreamState e)
    filterPlan initialState@FilterStreamState{..} = do
      end <- lift . mkBlockNumber $ filterToBlock fssInitialFilter
      if fssCurrentBlock > end
        then stop
        else do
          let to' = min end $ fssCurrentBlock + fromInteger fssWindowSize
              filter' = fssInitialFilter { filterFromBlock = BlockWithNumber fssCurrentBlock
                                         , filterToBlock = BlockWithNumber to'
                                         }
          yield filter'
          filterPlan $ initialState { fssCurrentBlock = to' + 1 }

-- | Coerce a 'DefaultBlock' into a numerical block number.
mkBlockNumber :: DefaultBlock -> Web3 Quantity
mkBlockNumber bm = case bm of
  BlockWithNumber bn -> return bn
  Earliest           -> return 0
  _                  -> Eth.blockNumber

--------------------------------------------------------------------------------

data Filters (es :: [*]) where
  NilFilters :: Filters '[]
  (:?) :: Filter e -> Filters es -> Filters (e ': es)

infixr 5 :?

data FiltersStreamState es =
  FiltersStreamState { fsssCurrentBlock   :: Quantity
                     , fsssInitialFilters :: Filters es
                     , fsssWindowSize     :: Integer
                     }

filtersStream :: FiltersStreamState es
              -> MachineT Web3 k (Filters es)
filtersStream initialPlan = do
  unfoldPlan initialPlan $ \s -> do
    end <- lift . mkBlockNumber . minBlock . fsssInitialFilters $ initialPlan
    filterPlan end s
  where
    filterPlan :: Quantity -> FiltersStreamState es -> PlanT k (Filters es) Web3 (FiltersStreamState es)
    filterPlan end initialState@FiltersStreamState{..} = do
      if fsssCurrentBlock > end
        then stop
        else do
          let to' = min end $ fsssCurrentBlock + fromInteger fsssWindowSize
              h :: forall e . Filter e -> Filter e
              h f = f { filterFromBlock = BlockWithNumber fsssCurrentBlock
                      , filterToBlock = BlockWithNumber to'
                      }
          yield (modifyFilters h fsssInitialFilters)
          filterPlan end initialState { fsssCurrentBlock = to' + 1 }

minBlock :: Filters es -> DefaultBlock
minBlock NilFilters             = Pending
minBlock (Filter _ _ e _ :? fs) = e `min` minBlock fs

modifyFilters :: (forall e. Filter e -> Filter e) -> Filters es -> Filters es
modifyFilters _ NilFilters = NilFilters
modifyFilters h (f :? fs)  = (h f :? modifyFilters h fs)


weakenCoRec
  :: ( RecApplicative ts
     , FoldRec (t ': ts) (t ': ts)
     )
  => Field ts
  -> Field (t ': ts)
weakenCoRec = fromJust . firstField . (Compose Nothing :&) . coRecToRec

class QueryAllLogs (es :: [*]) where
  type WithChange es :: [*]
  queryAllLogs :: Filters es -> Web3 [Field (WithChange es)]

instance QueryAllLogs '[] where
  type WithChange '[] = '[]
  queryAllLogs NilFilters = pure []

instance forall e i ni es.
  ( DecodeEvent i ni e
  , QueryAllLogs es
  , RecApplicative (WithChange es)
  , FoldRec (FilterChange e : WithChange es) (WithChange es)
  ) => QueryAllLogs (e:es) where

  type WithChange (e:es) = FilterChange e : WithChange es

  queryAllLogs (f  :? fs) = do
    changes <- Eth.getLogs f
    filterChanges <- liftIO . mkFilterChanges @_ @_ @e $ changes
    filterChanges' <- queryAllLogs fs
    pure $ map (CoRec . Identity) filterChanges <> map weakenCoRec filterChanges'

class HasLogIndex a where
  getLogIndex :: a -> Maybe (Quantity, Quantity)

instance HasLogIndex (FilterChange e) where
  getLogIndex FilterChange{..} =
    (,) <$> changeBlockNumber filterChangeRawChange <*> changeLogIndex filterChangeRawChange

sortChanges
  :: ( AllAllSat '[HasLogIndex] es
     , RecApplicative es
     )
  => [Field es]
  -> [Field es]
sortChanges changes =
  let sorterProj change = onField (Proxy @'[HasLogIndex]) getLogIndex change
  in sortOn sorterProj changes

class MapHandlers m es es' where
  mapHandlers
    :: Handlers es (ReaderT Change m EventAction)
    -> Handlers es' (m (Maybe (EventAction, Quantity)))

instance Monad m => MapHandlers m '[] '[] where
  mapHandlers RNil = RNil

instance ( Monad m
         , MapHandlers m es es'
         ) => MapHandlers m (e : es) (FilterChange e : es') where
  mapHandlers ((H f) :& fs) =
    let f' = \FilterChange{..} -> do
          act <- runReaderT (f filterChangeEvent) filterChangeRawChange
          return ((,) act <$> changeBlockNumber filterChangeRawChange)
    in (H f') :& mapHandlers fs


reduceEventStream'
  :: ( Monad m
     , MapHandlers m es (WithChange es)
     )
  => MachineT m k [Field (WithChange es)]
  -> Handlers es (ReaderT Change m EventAction)
  -> m (Maybe (EventAction, Quantity))
reduceEventStream' filterChanges handlers = fmap listToMaybe . runT $
       filterChanges
    ~> autoM (processChanges handlers)
    ~> asParts
    ~> runWhile (\(act, _) -> act /= TerminateEvent)
    ~> final
  where
    runWhile p = repeatedly $ do
      v <- await
      if p v
        then yield v
        else yield v >> stop
    processChanges handlers' changes = fmap catMaybes $
        forM changes $ \fc -> match fc (mapHandlers handlers')

-- | 'playLogs' streams the 'filterStream' and calls eth_getLogs on these 'Filter' objects.
playLogs'
  :: forall es k.
     ( QueryAllLogs es
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => FiltersStreamState es
  -> MachineT Web3 k [Field (WithChange es)]
playLogs' s = filtersStream s
          ~> autoM queryAllLogs
          ~> autoM (return . sortChanges)

data TaggedFilterIds (es :: [*]) where
  TaggedFilterNil :: TaggedFilterIds '[]
  TaggedFilterCons :: Tagged e Quantity -> TaggedFilterIds es -> TaggedFilterIds (e : es)

class QueryAllLogs es => PollFilters (es :: [*]) where
  openFilters :: Filters es -> Web3 (TaggedFilterIds es)
  checkFilters :: TaggedFilterIds es -> Web3 [Field (WithChange es)]
  closeFilters :: TaggedFilterIds es -> Web3 ()

instance PollFilters '[] where
  openFilters _ = pure TaggedFilterNil
  checkFilters _ = pure []
  closeFilters _ = pure ()

instance forall e i ni es.
  ( DecodeEvent i ni e
  , PollFilters es
  , RecApplicative (WithChange es)
  , FoldRec (FilterChange e : WithChange es) (WithChange es)
  ) => PollFilters (e:es) where
  openFilters (f :? fs) = do
    fId <- Eth.newFilter f
    fsIds <- openFilters fs
    pure $ TaggedFilterCons (Tagged fId) fsIds

  checkFilters (TaggedFilterCons (Tagged fId) fsIds) = do
    changes <- Eth.getFilterChanges fId
    filterChanges <- liftIO . mkFilterChanges @_ @_ @e $ changes
    filterChanges' :: [Field (WithChange es)] <- checkFilters fsIds
    pure  $ map (CoRec . Identity) filterChanges <>  map weakenCoRec filterChanges'

  closeFilters (TaggedFilterCons (Tagged fId) fsIds) = do
    _ <- Eth.uninstallFilter fId
    closeFilters fsIds


-- | Polls a filter from the given filterId until the target toBlock is reached.
pollFilters
  :: ( PollFilters es
     , RecApplicative (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     )
  => TaggedFilterIds es
  -> DefaultBlock
  -> MachineT Web3 k [Field (WithChange es)]
pollFilters is = construct . pollPlan is
  where
    -- pollPlan :: TaggedFilterIds es -> DefaultBlock -> PlanT k [Field (Map (TyCon1 FilterChange) es)] Web3 ()
    pollPlan (fIds :: TaggedFilterIds es) end = do
      bn <- lift $ Eth.blockNumber
      if BlockWithNumber bn > end
        then do
          _ <- lift $ closeFilters fIds
          stop
        else do
          liftIO $ threadDelay 1000000
          changes <- lift $ sortChanges <$> checkFilters fIds
          yield changes
          pollPlan fIds end

eventManys'
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => Filters es
  -> Integer
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
eventManys' fltrs window handlers = do
    start <- mkBlockNumber $ minBlock fltrs
    let initState = FiltersStreamState { fsssCurrentBlock = start
                                       , fsssInitialFilters = fltrs
                                       , fsssWindowSize = window
                                       }
    mLastProcessedFilterState <- reduceEventStream' (playLogs' initState) handlers
    case mLastProcessedFilterState of
      Nothing -> startPolling (modifyFilters (\f -> f {filterFromBlock = BlockWithNumber start}) fltrs)
      Just (act, lastBlock) -> do
        end <- mkBlockNumber . minBlock $ fltrs
        when (act /= TerminateEvent && lastBlock < end) $
          let pollingFromBlock = lastBlock + 1
          in startPolling (modifyFilters (\f -> f {filterFromBlock = BlockWithNumber pollingFromBlock}) fltrs)
  where
    startPolling fltrs' = do
      fIds <- openFilters fltrs'
      let pollTo = minBlock fltrs'
      void $ reduceEventStream' (pollFilters fIds pollTo) handlers

-- | Run 'event\'' one block at a time.
events
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => Filters es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 (Async ())
events fltrs = forkWeb3 . events' fltrs

-- | Same as 'event', but does not immediately spawn a new thread.
events'
  :: ( PollFilters es
     , QueryAllLogs es
     , MapHandlers Web3 es (WithChange es)
     , AllAllSat '[HasLogIndex] (WithChange es)
     , RecApplicative (WithChange es)
     )
  => Filters es
  -> Handlers es (ReaderT Change Web3 EventAction)
  -> Web3 ()
events' fltrs = eventManys' fltrs 0
