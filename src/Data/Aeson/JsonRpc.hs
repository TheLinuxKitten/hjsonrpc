{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Data.Aeson.JsonRpc
  ( JsonRpcMethod
  , JsonRpcId(..)
  , JsonRpcVersion(..)
  -- * Request
  , JsonRpcRequest(..)
  , jsonRpcDecodeRequest
  , jsonRpcEncodeRequest
  -- * Response
  , JsonRpcResp
  , JsonRpcError(..)
  , JsonRpcResponse(..)
  , jsonRpcDecodeResponse
  , jsonRpcDecodeResponseResult
  , jsonRpcDecodeResponseVersion
  , jsonRpcDecodeResponseId
  , jsonRpcEncodeResponse
  , toArrayValue
  , fromArrayValue
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Nombre del método RPC
type JsonRpcMethod = Text

-- | ID de la petición
data JsonRpcId = JsonRpcIdInt { idInt :: Int }
               | JsonRpcIdTxt { idTxt :: Text }
               deriving (Show)

instance Enum JsonRpcId where
  toEnum = JsonRpcIdInt
  fromEnum (JsonRpcIdInt i) = i
  fromEnum _ = error "No se pueden enumerar ids no numéricos"

instance ToJSON JsonRpcId where
  toJSON (JsonRpcIdInt i) = toJSON i
  toJSON (JsonRpcIdTxt t) = toJSON t

instance FromJSON JsonRpcId where
  parseJSON i@(Number _) = JsonRpcIdInt <$> parseJSON i
  parseJSON s@(String _) = JsonRpcIdTxt <$> parseJSON s
  parseJSON _ = mzero

-- | Versión
data JsonRpcVersion = JsonRpcV1 | JsonRpcV2 deriving (Show)

parseVersion :: Object -> Parser JsonRpcVersion
parseVersion o = do
  j <- o .:? "jsonrpc"
  return $ if j == Just ("2.0" :: Text) then JsonRpcV2 else JsonRpcV1

-- | Request JSON-RPC
data JsonRpcRequest = JsonRpcRequest
                        { _requestVersion :: JsonRpcVersion
                        , _method :: JsonRpcMethod
                        , _params :: Value
                        , _reqId :: JsonRpcId
                        }
                    | JsonRpcNotification
                        { _requestVersion :: JsonRpcVersion
                        , _method :: JsonRpcMethod
                        , _params :: Value
                        }
                    deriving (Show)

jr2 :: Pair
jr2 = "jsonrpc" .= ("2.0" :: Text)

instance ToJSON JsonRpcRequest where
  toJSON (JsonRpcRequest JsonRpcV2 m p i) = object $ case p of
    Null -> [ jr2, "method" .= m, "id" .= i]
    _    -> [ jr2, "method" .= m, "id" .= i, "params" .= p]
  toJSON (JsonRpcRequest JsonRpcV1 m p i) = object $ case p of
    Null -> [ "method" .= m, "params" .= emptyArray, "id" .= i]
    _    -> [ "method" .= m, "params" .= p, "id" .= i]
  toJSON (JsonRpcNotification JsonRpcV2 m p) = object $ case p of
    Null -> [ jr2, "method" .= m]
    _    -> [ jr2, "method" .= m, "params" .= p]
  toJSON (JsonRpcNotification JsonRpcV1 m p) = object $ case p of
    Null -> [ "method" .= m, "params" .= emptyArray, "id" .= Null]
    _    -> [ "method" .= m, "params" .= p, "id" .= Null]

instance FromJSON JsonRpcRequest where
  parseJSON = withObject "request" $ \o -> do
    v <- parseVersion o
    mi <- o .:? "id"
    m <- o .: "method"
    p <- o .:? "params" .!= Null
    return $ case mi of
      Nothing -> JsonRpcNotification v m p
      Just i -> JsonRpcRequest v m p i

toArrayValue :: [Value] -> Value
toArrayValue = Array . V.fromList

fromArrayValue :: Value -> [Value]
fromArrayValue (Array a) = V.toList a

-- | Respuesta JSON-RPC
type JsonRpcResp a = Either Text a

jsonRpcDecode :: FromJSON a => LBS.ByteString -> JsonRpcResp a
jsonRpcDecode = either (Left . T.pack) Right . eitherDecode

-- | Decodificar Request
jsonRpcDecodeRequest :: LBS.ByteString -> JsonRpcResp JsonRpcRequest
jsonRpcDecodeRequest = jsonRpcDecode

-- | Codificar Request
jsonRpcEncodeRequest :: JsonRpcRequest -> LBS.ByteString
jsonRpcEncodeRequest = encode

data JsonRpcError = JsonRpcErrorObject
                      { _code :: Int
                      , _message :: Text
                      , _data :: Value
                      }
                  | JsonRpcErrorValue
                      { _data :: Value
                      }
                  deriving (Show)

instance ToJSON JsonRpcError where
  toJSON (JsonRpcErrorObject c m d) = object $
    [ "code" .= c
    , "message" .= m
    ] <> if d == Null then [] else ["data" .= d]
  toJSON (JsonRpcErrorValue d) = d

instance FromJSON JsonRpcError where
  parseJSON Null = mzero
  parseJSON v@(Object o) = p1 <|> p2
    where
      p1 = JsonRpcErrorObject
       <$> o .: "code"
       <*> o .: "message"
       <*> o .:? "data" .!= Null
      p2 = return $ JsonRpcErrorValue v

data JsonRpcResponse = JsonRpcResponseResult
                         { _responseVersion :: JsonRpcVersion
                         , _result :: Value
                         , _resId :: JsonRpcId
                         }
                     | JsonRpcResponseError
                         { _responseVersion :: JsonRpcVersion
                         , _error :: JsonRpcError
                         , _resId :: JsonRpcId
                         }
                     | JsonRpcOrphanError
                         { _responseVersion :: JsonRpcVersion
                         , _error :: JsonRpcError
                         }
                     deriving (Show)

instance ToJSON JsonRpcResponse where
  toJSON (JsonRpcResponseResult JsonRpcV1 r i) = object
    ["id" .= i, "result" .= r, "error" .= Null]
  toJSON (JsonRpcResponseResult JsonRpcV2 r i) = object
    [jr2, "id" .= i, "result" .= r]
  toJSON (JsonRpcResponseError JsonRpcV1 e i) = object
    ["id" .= i, "error" .= e, "result" .= Null]
  toJSON (JsonRpcResponseError JsonRpcV2 e i) = object
    [jr2, "id" .= i, "error" .= e]
  toJSON (JsonRpcOrphanError JsonRpcV1 e) = object
    ["id" .= Null, "error" .= e, "result" .= Null]
  toJSON (JsonRpcOrphanError JsonRpcV2 e) = object
    [jr2, "id" .= Null, "error" .= e]

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "response" $ \o -> do
    v <- parseVersion o
    i <- o .:? "id"
    r <- o .:? "result" .!= Null
    p <- case v of
          JsonRpcV1 -> if r == Null
                        then Left <$> o .: "error"
                        else return (Right r)
          JsonRpcV2 -> maybe (Right r) Left <$> o .:? "error"
    case p of
      Right r' -> do
        guard $ isJust i
        return $ JsonRpcResponseResult v r' (fromJust i)
      Left e -> case i of
        Just i' -> return $ JsonRpcResponseError v e i'
        Nothing -> return $ JsonRpcOrphanError v e

-- | Decodificar Response
jsonRpcDecodeResponse :: LBS.ByteString -> JsonRpcResp JsonRpcResponse
jsonRpcDecodeResponse = jsonRpcDecode

jsonRpcDecodeResponseResult :: (FromJSON a)
                            => JsonRpcResp JsonRpcResponse -> JsonRpcResp a
jsonRpcDecodeResponseResult er = case er of
  Left e1 -> Left e1
  Right r -> case r of
    (JsonRpcResponseResult _ r _) -> case fromJSON r of
      Error e2 -> Left $ T.pack e2
      Success r' -> Right r'
    (JsonRpcResponseError _ e _) -> Left $ _message e
    (JsonRpcOrphanError _ e) -> Left $ _message e

jsonRpcDecodeResponseVersion :: JsonRpcResp JsonRpcResponse -> Maybe JsonRpcVersion
jsonRpcDecodeResponseVersion er = case er of
  Left _ -> Nothing
  Right r -> case r of
    (JsonRpcResponseResult v _ _) -> Just v
    (JsonRpcResponseError v _ _) -> Just v
    (JsonRpcOrphanError v _) -> Just v

jsonRpcDecodeResponseId :: JsonRpcResp JsonRpcResponse -> Maybe JsonRpcId
jsonRpcDecodeResponseId er = case er of
  Left _ -> Nothing
  Right r -> case r of
    (JsonRpcResponseResult _ _ i) -> Just i
    (JsonRpcResponseError _ _ i) -> Just i
    (JsonRpcOrphanError _ _) -> Nothing

-- | Codificar response
jsonRpcEncodeResponse :: JsonRpcResponse -> LBS.ByteString
jsonRpcEncodeResponse = encode
