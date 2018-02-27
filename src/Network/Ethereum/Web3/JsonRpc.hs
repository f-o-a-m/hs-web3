{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      :  Network.JsonRpc
-- Copyright   :  Alexander Krupenkin 2016
-- License     :  BSD3
--
-- Maintainer  :  mail@akru.me
-- Stability   :  experimental
-- Portability :  portable
--
-- Little JSON-RPC 2.0 client.
-- Functions for implementing the client side of JSON-RPC 2.0.
-- See <http://www.jsonrpc.org/specification>.
--
module Network.Ethereum.Web3.JsonRpc (
    remote
  , MethodName
  , ServerUri
  ) where

import           Network.Ethereum.Web3.Provider
import           Network.Ethereum.Web3.Types

import           Control.Applicative            ((<|>))
import           Control.Exception              (throwIO)
import           Control.Monad                  ((>=>))
import           Data.Aeson
import           Data.ByteString.Lazy           (ByteString)
import           Data.Proxy
import           Data.Text                      (Text)
import           Data.Vector                    (fromList)
import           Network.HTTP.Client            (RequestBody (RequestBodyLBS),
                                                 httpLbs, method,
                                                 parseRequest, requestBody,
                                                 requestHeaders, responseBody)


-- | Name of called method.
type MethodName = Text

-- | JSON-RPC server URI
type ServerUri  = String

-- | Remote call of JSON-RPC method.
-- Arguments of function are stored into @params@ request array.
remote :: Remote a => MethodName -> a
remote n = remote_ (\node -> call node . Array . fromList)
  where
    call node = connection node . encode . Request n 1
    connection node body = do
        request <- parseRequest (rpcUri node)
        let request' = request
                     { requestBody = RequestBodyLBS body
                     , requestHeaders = [("Content-Type", "application/json")]
                     , method = "POST" }
        responseBody <$> httpLbs request' (rpcManager node)

decodeResponse :: FromJSON a => ByteString -> IO a
decodeResponse = tryParse . eitherDecode
             >=> tryJsonRpc . rsResult
             >=> tryParse . eitherDecode . encode
  where tryJsonRpc :: Either RpcError a -> IO a
        tryJsonRpc = either (throwIO . JsonRpcFail) return
        tryParse :: Either String a -> IO a
        tryParse = either (throwIO . ParserFail) return

class Remote a where
    remote_ :: (RPCNode -> [Value] -> IO ByteString) -> a

instance (ToJSON a, Remote b) => Remote (a -> b) where
    remote_ f x = remote_ (\u xs -> f u (toJSON x : xs))

instance (Provider p, FromJSON a) => Remote (Web3 p a) where
    remote_ f = (\u -> Web3 (decodeResponse =<< f u [])) =<< rpcNode

-- | JSON-RPC request.
data Request = Request { rqMethod :: !Text
                       , rqId     :: !Int
                       , rqParams :: !Value }

instance ToJSON Request where
    toJSON rq = object [ "jsonrpc" .= String "2.0"
                       , "method"  .= rqMethod rq
                       , "params"  .= rqParams rq
                       , "id"      .= rqId rq ]

-- | JSON-RPC response.
data Response = Response
  { rsResult :: !(Either RpcError Value)
  } deriving (Show)

instance FromJSON Response where
    parseJSON = withObject "JSON-RPC response object" $
                \v -> Response <$>
                      (Right <$> v .: "result" <|> Left <$> v .: "error")
