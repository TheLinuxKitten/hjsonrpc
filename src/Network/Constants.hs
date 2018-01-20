--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------


module Network.Constants
  ( hContentTypeValue
  , hServerValue
  , hUserAgentValue
  ) where

hContentTypeValue :: (String,String)
hContentTypeValue = ("content-type", "application/json; charset=utf-8")

hServerValue :: (String,String)
hServerValue = ("server", "The Linux Kitten JSON-RPC Server")

hUserAgentValue :: (String,String)
hUserAgentValue = ("user-agent", "The Linux Kitten JSON-RPC Client")

