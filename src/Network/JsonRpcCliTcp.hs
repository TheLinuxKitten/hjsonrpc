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

module Network.JsonRpcCliTcp
  ( CN.AppData
  , runJsonRpcTcpT
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
import qualified Data.Conduit.Network as CN
import Data.Text (Text)
import Network.JsonRpc
import Network.JsonRpcConn

cr :: Monad m => Conduit ByteString m ByteString
cr = CL.map (`C8.snoc` '\n')

instance JsonRpcConn CN.AppData where
  sendJsonRpc' :: ( MonadLoggerIO m, MonadBaseControl IO m
                  , ToJSON r, ToRequest r, FromJSON a )
               => JsonRpcVersion -> r -> JsonRpcConnT CN.AppData m (JsonRpcResp a)
  sendJsonRpc' ver rq = do
    logDebug' rq
    ad <- ask
    fromJsonRpcT <$> lift (runJsonRpcT (jsonRpcLibVer ver) True
                              (cr =$ CN.appSink ad)
                              (CN.appSource ad)
                              (sendRequest rq))

-- | Ejecuta sesión JSON-RPC sobre conexión socket TCP
runJsonRpcTcpT :: (MonadLoggerIO m, MonadBaseControl IO m, FromJSON a)
                => ByteString   -- ^ Host
                -> Int          -- ^ Puerto
                -> JsonRpcConnT CN.AppData m a -> m (Either Text a)
runJsonRpcTcpT h po f = CN.runGeneralTCPClient
                            (CN.clientSettings po h)
                            (`runJsonRpcConnT` f)

