-- |
-- Module      :  Network.Ethereum.Web3.Provider
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Web3 service provider.
--
module Network.Ethereum.Web3.Provider where

import           Control.Concurrent.Async    (Async, async)
import           Control.Exception           (try)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Data.Proxy
import           Network.Ethereum.Web3.Types
import           Network.HTTP.Client         (Manager, newManager)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)



data RPCNode =
  RPCNode { rpcUri :: String
          , rpcManager :: Manager
          }

makeRPCNode :: String -> IO RPCNode
makeRPCNode uri = do
  manager <- newManager tlsManagerSettings
  return $ RPCNode { rpcUri = uri
                   , rpcManager = manager
                   }

-- | Ethereum node service provider
class Provider a where
    -- | JSON-RPC provider URI, default: localhost:8545
    rpcNode :: Web3 a RPCNode

-- | 'Web3' monad runner
runWeb3 :: MonadIO m => Web3 a b -> m (Either Web3Error b)
{-# INLINE runWeb3 #-}
runWeb3 = liftIO . try . unWeb3

-- | Fork 'Web3' with the same 'Provider'
forkWeb3 :: Web3 a b -> Web3 a (Async b)
{-# INLINE forkWeb3 #-}
forkWeb3 = Web3 . async . unWeb3
