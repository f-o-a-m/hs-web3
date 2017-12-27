{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Ethereum.Web3.Test.SimpleStorageSpec where

import           Control.Concurrent                       (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad                            (void)
import           Control.Monad.IO.Class                   (liftIO)
import           Control.Monad.Trans.Reader               (ask)
import           Data.ByteString                          (ByteString)
import           Data.Default
import           Data.Either                              (isRight)
import           Data.List                                (sort)
import           Data.Proxy
import           Data.String                              (fromString)
import qualified Data.Text                                as T
import           Data.Traversable                         (for)
import           GHC.TypeLits
import           Network.Ethereum.Web3                    hiding (convert)
import           Network.Ethereum.Web3.Contract           (Event (..))
import           Network.Ethereum.Web3.Contract.Streaming (event')
import           Network.Ethereum.Web3.Encoding           (ABIEncoding (..))
import qualified Network.Ethereum.Web3.Eth                as Eth
import           Network.Ethereum.Web3.Test.Utils
import           Network.Ethereum.Web3.TH
import           Network.Ethereum.Web3.Types              (BlockNumber (..), Call (..), Change (..), DefaultBlock (..),
                                                           Filter (..), parseDefaultBlock)
import           Numeric                                  (showHex)
import           System.Environment                       (getEnv)
import           System.IO.Unsafe                         (unsafePerformIO)
import           Test.Hspec

[abiFrom|build/contracts/abis/SimpleStorage.json|]

contractAddress :: Address
contractAddress = fromString . unsafePerformIO $ getEnv "SIMPLESTORAGE_CONTRACT_ADDRESS"

newtype BoundedEvent ev (from :: Symbol) (to :: Symbol) = BoundedEvent ev

instance ABIEncoding ev => ABIEncoding (BoundedEvent ev from to) where
    fromDataParser = BoundedEvent <$> fromDataParser
    toDataBuilder (BoundedEvent ev) = toDataBuilder ev

instance (Event ev, KnownSymbol from, KnownSymbol to) => Event (BoundedEvent ev from to) where
    eventFilter (BoundedEvent ev) a =
        let baseFilter = eventFilter ev a
         in baseFilter { filterFromBlock = parseDefaultBlock . T.pack $ symbolVal (Proxy :: Proxy from)
                       , filterToBlock = parseDefaultBlock . T.pack $ symbolVal (Proxy :: Proxy to)
                       }

instance Show ev => Show (BoundedEvent ev from to) where
    show (BoundedEvent ev) = show ev

spec :: Spec
spec = describe "Simple Storage" $ do
    it "should inject contract addresses" injectExportedEnvironmentVariables
    withPrimaryEthereumAccount `before` interactions
    --withPrimaryEthereumAccount `before` events
    withPrimaryEthereumAccount `before` streaming

interactions :: SpecWith Address
interactions = describe "can interact with a SimpleStorage contract" $ do
    -- todo: this should ideally be arbitrary!
    let theValue = 12345
    it "can set the value of a SimpleStorage contract" $ \primaryAccount -> do
        let theCall = callFromTo primaryAccount contractAddress
        ret <- runWeb3Configured $ setCount theCall theValue
        True `shouldBe` True -- we need to get this far

    it "can read the value back" $ \primaryAccount -> do
        let theCall = callFromTo primaryAccount contractAddress
        sleepSeconds 7 -- should open up resolveDefaultBlock and wait till Pending is committed
        v <- runWeb3Configured (count theCall)
        v `shouldBe` theValue

events :: SpecWith Address
events = describe "can interact with a SimpleStorage contract across block intervals using the classic API" $ do
    it "can stream events starting and ending in the past" $ \primaryAccount -> do
        var <- newMVar []
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [1, 2, 3]
        start <- runWeb3Configured Eth.blockNumber
        blockNumberV <- newEmptyMVar
        SomeSymbol (Proxy :: Proxy start) <- return (symbolizeBlockNumber start)
        void . runWeb3Configured $ event contractAddress $ \ev -> do
            let (CountSet cs) = ev
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            if cs == 3
                then do
                    bn <- changeBlockNumber <$> ask
                    liftIO $ putMVar blockNumberV bn
                    return TerminateEvent
                else return ContinueEvent
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        end' <- takeMVar blockNumberV
        SomeSymbol (Proxy :: Proxy end) <- return (symbolizeBlockNumber end')
        termination <- newEmptyMVar
        void . runWeb3Configured $ event contractAddress $ \(ev :: BoundedEvent CountSet start end) -> do
            let BoundedEvent (CountSet cs) = ev
            liftIO . putStrLn $ "2: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 3
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else return ContinueEvent
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for second filter!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

    it "can stream events starting in the past and ending in the future" $ \primaryAccount -> do
        var <- newMVar []
        term1 <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            firstSets = [1, 2, 3]
            secondSets = [4, 5, 6]
        start <- runWeb3Configured Eth.blockNumber
        SomeSymbol (Proxy :: Proxy start) <- return (symbolizeBlockNumber start)
        termination <- newEmptyMVar
        void . runWeb3Configured $ event contractAddress $ \(CountSet cs) -> do
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            if cs == 3
                then do
                    liftIO $ putMVar term1 True
                    return TerminateEvent
                else return ContinueEvent
        void . for firstSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 term1 >>= \case
            Nothing -> error "timed out waiting for first filter!"
            Just term -> return ()

        term2 <- newEmptyMVar
        -- nb: termination is reused since it was taken just prior!
        void . runWeb3Configured $ event contractAddress $ \(ev :: BoundedEvent CountSet start "latest") -> do
            let BoundedEvent (CountSet cs) = ev
            liftIO . putStrLn $ "2: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 6
                then do
                    liftIO $ putMVar term2 True
                    return TerminateEvent
                else return ContinueEvent
        void . for secondSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 term2 >>= \case
            Nothing -> error "timed out waiting for second filter!"
            Just term -> return ()

        vals <- takeMVar var
        sort vals `shouldBe` sort (firstSets ++ secondSets)

    it "can stream events starting and ending in the future, unbounded" $ \primaryAccount -> do
        var <- newMVar []
        termination <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [8, 9, 10]
        (BlockNumber now) <- runWeb3Configured Eth.blockNumber
        let later = now + 3
            later' = "0x" ++ showHex later ""
        liftIO . putStrLn $ "now is " ++ show now
        SomeSymbol (Proxy :: Proxy later) <- return (someSymbolVal later')
        void . runWeb3Configured $ event contractAddress $ \(ev :: BoundedEvent CountSet later "latest") -> do
            let BoundedEvent (CountSet cs) = ev
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 10
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else return ContinueEvent
        awaitBlock later
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for filter!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

    it "can stream events starting and ending in the future, bounded" $ \primaryAccount -> do
        var <- newMVar []
        termination <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [10, 11, 12]
        (BlockNumber now) <- runWeb3Configured Eth.blockNumber
        let later = now + 3
            latest = now + 8
            later' = "0x" ++ showHex later ""
            latest' = "0x" ++ showHex latest ""
        liftIO . putStrLn $ "now is " ++ show now
        SomeSymbol (Proxy :: Proxy later) <- return (someSymbolVal later')
        SomeSymbol (Proxy :: Proxy latest) <- return (someSymbolVal latest')
        void . runWeb3Configured $ event contractAddress $ \(ev :: BoundedEvent CountSet later latest) -> do
            let BoundedEvent (CountSet cs) = ev
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 12
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else return ContinueEvent
        awaitBlock later
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for filter!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

streaming :: SpecWith Address
streaming = describe "can interact with a SimpleStorage contract across block intervals using the streaming API" $ do
    let countSetFilter from to address = (eventFilter (undefined :: CountSet) address) { filterFromBlock = from, filterToBlock = to }
        windowSize = 1
    it "can stream events starting and ending in the past" $ \primaryAccount -> do
        var <- newMVar []
        let theCall = callFromTo primaryAccount contractAddress
            theSets = [1, 2, 3]
        start <- BlockWithNumber <$> runWeb3Configured Eth.blockNumber
        blockNumberV <- newEmptyMVar
        void . runWeb3Configured $ event contractAddress $ \ev -> do
            let (CountSet cs) = ev
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            if cs == 3
                then do
                    bn <- changeBlockNumber <$> ask
                    liftIO $ putMVar blockNumberV bn
                    return TerminateEvent
                else return ContinueEvent
        void . for theSets $ \v -> runWeb3Configured (setCount theCall v)
        end <- BlockWithNumber <$> takeMVar blockNumberV
        termination <- newEmptyMVar
        runWeb3Configured $ event' (countSetFilter (Just start) (Just end) contractAddress) windowSize $ \(CountSet cs) -> do
            liftIO . putStrLn $ "2: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 3
                then do
                    liftIO $ putMVar termination True
                    return TerminateEvent
                else return ContinueEvent
        takeMVarWithTimeout 20000000 termination >>= \case
            Nothing -> error "timed out waiting for second filter!"
            Just term -> return ()
        vals <- takeMVar var
        sort vals `shouldBe` sort theSets

    it "can stream events starting in the past and ending in the future" $ \primaryAccount -> do
        var <- newMVar []
        term1 <- newEmptyMVar
        let theCall = callFromTo primaryAccount contractAddress
            firstSets = [1, 2, 3]
            secondSets = [4, 5, 6]
        start <- BlockWithNumber <$> runWeb3Configured Eth.blockNumber
        termination <- newEmptyMVar
        runWeb3Configured $ event' (countSetFilter Nothing Nothing contractAddress) windowSize $ \(CountSet cs) -> do
            liftIO . putStrLn $ "1: Got a CountSet! " ++ show cs
            if cs == 3
                then do
                    liftIO $ putMVar term1 True
                    return TerminateEvent
                else return ContinueEvent
        void . for firstSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 term1 >>= \case
            Nothing -> error "timed out waiting for first filter!"
            Just term -> return ()

        term2 <- newEmptyMVar
        -- nb: termination is reused since it was taken just prior!
        void . runWeb3Configured $ event' (countSetFilter (Just start) Nothing contractAddress) windowSize $ \(CountSet cs) -> do
            liftIO . putStrLn $ "2: Got a CountSet! " ++ show cs
            liftIO $ modifyMVar_ var (return . (cs:))
            if cs == 6
                then do
                    liftIO $ putMVar term2 True
                    return TerminateEvent
                else return ContinueEvent
        void . for secondSets $ \v -> runWeb3Configured (setCount theCall v)
        takeMVarWithTimeout 20000000 term2 >>= \case
            Nothing -> error "timed out waiting for second filter!"
            Just term -> return ()

        vals <- takeMVar var
        sort vals `shouldBe` sort (firstSets ++ secondSets)
