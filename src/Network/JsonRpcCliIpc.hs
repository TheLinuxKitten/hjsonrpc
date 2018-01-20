{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Network.JsonRpcCliIpc
  ( CNU.AppDataUnix
  , runJsonRpcIpcT
  ) where

import Control.Monad.Logger
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Aeson.JsonRpc
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network.Unix as CNU
import Data.Text (Text)
import Network.JsonRpc
import Network.JsonRpcConn

-- | Métodos accesibles desde el monad independiente de la conexión
cr :: Monad m => Conduit ByteString m ByteString
cr = CL.map (`C8.snoc` '\n')

instance JsonRpcConn CNU.AppDataUnix where
  sendJsonRpc' :: ( MonadLoggerIO m, MonadBaseControl IO m
                  , ToJSON r, ToRequest r, FromJSON a )
               => JsonRpcVersion -> r -> JsonRpcConnT CNU.AppDataUnix m (JsonRpcResp a)
  sendJsonRpc' ver rq = do
    logDebug' rq
    ad <- ask
    fromJsonRpcT <$> lift (runJsonRpcT (jsonRpcLibVer ver) True 
                              (cr =$ CNU.appSink ad)
                              (CNU.appSource ad)
                              (sendRequest rq))

-- | Ejecuta sesión JSON-RPC sobre conexión socket UNIX IPC
runJsonRpcIpcT :: (MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
                => FilePath   -- ^ Path del socket UNIX
                -> JsonRpcConnT CNU.AppDataUnix m a -> m (Either Text a)
runJsonRpcIpcT fp f = runGeneralUnixClient
                          (CNU.clientSettings fp)
                          (`runJsonRpcConnT` f)
  where
    runGeneralUnixClient :: (MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
                         => CNU.ClientSettingsUnix -> (CNU.AppDataUnix -> m a)
                         -> m a
    runGeneralUnixClient st f = control $ \run -> CNU.runUnixClient st (run . f)

