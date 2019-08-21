{-# LANGUAGE OverloadedStrings #-}
module Network.JsonRpc.Exceptions where

import           Control.Exception (Exception)
import           Data.Aeson        (FromJSON (..), Value, withObject, (.:),
                                    (.:?))
import           Data.Text         (Text, unpack)

-- | JSON-RPC error message
data RpcError = RpcError
  { errCode    :: !Int
  , errMessage :: !Text
  , errData    :: !(Maybe Value)
  } deriving Eq

data JsonRpcException
    = ParsingException String
    | CallException RpcError
    deriving (Eq, Show)

instance Show RpcError where
    show (RpcError code msg dat) =
        "JSON-RPC error " ++ show code ++ ": " ++ unpack msg
         ++ ". Data: " ++ show dat

instance FromJSON RpcError where
    parseJSON = withObject "JSON-RPC error object" $
        \v -> RpcError <$> v .: "code"
                       <*> v .: "message"
                       <*> v .:? "data"


instance Exception JsonRpcException
