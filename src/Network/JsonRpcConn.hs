{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.JsonRpcConn
  ( Text
  , FromResponse(..)
  , JsonRpcExceptT
  , JsonRpcConnT
  , JsonRpcConn(..)
  , rpcThrowE
  , fromRpcE
  , ToRequest(..)
  , logDebug'
  , jsonRpcLibVer
  , fromJsonRpcT
  , runJsonRpcConnT
  , LogLevel(..)
  , filterLoggerLogLevel
  , liftJsonRpcConnT
  , liftJsonRpcConnTIO
  , module Control.Monad.Logger
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson.JsonRpc
import Data.Aeson.Types
import Data.Either (either)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.JsonRpc

type JsonRpcExceptT = ExceptT Text

-- | Monad independiente de la conexión
type JsonRpcConnT c m = ReaderT c (JsonRpcExceptT m)

-- | Métodos accesibles desde el monad independiente de la conexión
class JsonRpcConn c where
  -- | Envia un Request y espera la respuesta. Permite tratar los errores.
  sendJsonRpc' :: ( MonadLoggerIO m, MonadBaseControl IO m
                  , ToJSON r, ToRequest r, FromJSON a)
               => JsonRpcVersion -> r -> JsonRpcConnT c m (JsonRpcResp a)

  -- | Envia un Request y espera la respuesta. No trata los errores.
  sendJsonRpc :: ( MonadLoggerIO m, MonadBaseControl IO m
                 , ToJSON r, ToRequest r, FromJSON a)
              => JsonRpcVersion -> r -> JsonRpcConnT c m a
  sendJsonRpc ver rq = do
    er <- sendJsonRpc' ver rq
    case er of
      Left e -> rpcThrowE e
      Right r -> lift $ return r

rpcThrowE :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
          => Text -> JsonRpcConnT c m a
rpcThrowE = lift . throwE

fromRpcE :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
         => Either Text a -> JsonRpcConnT c m a
fromRpcE = lift . either throwE return

fromJsonRpcT :: (FromJSON a) => Maybe (Either ErrorObj Value) -> JsonRpcResp a
fromJsonRpcT mer = case mer of
    Nothing -> Left "fromJsonRpcT: No response, could not parse response or request is notification"
    Just er -> case er of
      Left e -> Left $ T.pack $ getErrMsg e
      Right r -> fromRpcJson r
  where
    fromRpcJson r = case fromJSON r of
      Error e' -> Left $ T.pack e'
      Success r' -> Right r'

logDebug' :: (MonadLoggerIO m, ToJSON r, ToRequest r) => r -> m ()
logDebug' rq = do
  let reqmn = requestMethod rq
  let reqmps = toJSON rq
  logDebugN $ reqmn <> " " <> T.pack (show reqmps)

jsonRpcLibVer :: JsonRpcVersion -> Ver
jsonRpcLibVer JsonRpcV1 = V1
jsonRpcLibVer JsonRpcV2 = V2

runJsonRpcConnT :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => c -> JsonRpcConnT c m a -> m (Either Text a)
runJsonRpcConnT c f = runExceptT $ runReaderT f c

-- | Filtra logs al nivel indicado; los de menor nivel son filtrados.
filterLoggerLogLevel :: LogLevel -> LoggingT m a -> LoggingT m a
filterLoggerLogLevel l = filterLogger (filterLevel l)
    where
        filterLevel lm _ lv = lv >= lm

liftJsonRpcConnT :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                 => m a -> JsonRpcConnT c m a
liftJsonRpcConnT = lift . lift

liftJsonRpcConnTIO :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                   => IO a -> JsonRpcConnT c m a
liftJsonRpcConnTIO = lift . lift . liftIO
