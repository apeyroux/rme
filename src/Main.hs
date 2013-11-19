{-# LANGUAGE OverloadedStrings #-}

module Main where

import Snap.Core
import Data.Aeson
import Snap.Http.Server
import Control.Applicative
import Snap.Util.FileServe
import System.Cmd (rawSystem)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Lifted (handle)
import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Reverseme

nginxConfiguration = NginxConfiguration "/etc/nginx/sites-enabled/" "/etc/init.d/nginx"

data JsonMessage = JsonMessage {
  mAction :: String,
  mMessage :: String,
  mVhostDirectory :: String
} deriving (Eq, Show)

instance ToJSON JsonMessage where
  toJSON (JsonMessage a m p) = object ["message" .= m,
    "action" .= a,
    "direcrory" .= p]

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "STSI2/SDAC/BCM") <|>
    route [ --("foo", writeBS "bar"),
          ("append/:appendparam", appendHandler),
          ("drop/:dropparam", dropHandler),
          ("push", pushHandler)
          ] <|>
    dir "static" (serveDirectory ".")

exceptionCfg :: SomeException -> Snap ()
exceptionCfg ex = do
  writeBS "no non non !"

vHost :: BS.ByteString -> NginxVhost
vHost param = vh 
  where
    (Just vh) = decode (BL.fromStrict param)::Maybe NginxVhost

pushHandler :: Snap ()
pushHandler = do
  liftIO $ ngxReload nginxConfig
  writeBS $ BL.toStrict jsMessage
  where
    nginxConfig = nginxConfiguration
    jsMessage = encode $ JsonMessage "push" "push modif" ""

dropHandler :: Snap ()
dropHandler = do
  param <- getParam "dropparam"
  case param of
    Just param' -> do 
      liftIO $ ngxDropVHost nginxConfig (vHost param')
      liftIO $ ngxReload nginxConfig
      writeBS $ BL.toStrict jsMessage
  where
    nginxConfig = nginxConfiguration
    jsMessage = encode $ JsonMessage "drop" "drop vhost" ""

appendHandler :: Snap ()
appendHandler = do
    param <- getParam "appendparam"
    case param of
      Just param' -> do 
        liftIO $ ngxWriteVHost nginxConfig (vHost param')
        liftIO $ ngxReload nginxConfig
        writeBS $ BL.toStrict jsMessage
  where
    nginxConfig = nginxConfiguration
    jsMessage = encode $ JsonMessage "append" "append vhost" ""