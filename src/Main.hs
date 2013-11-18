{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)
import Reverseme

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "{\"message\":\"404 not found !\"}") <|>
    route [ --("foo", writeBS "bar"),
          ("append/:appendparam", appendHandler)
          , ("remove/:removeparam", removeHandler)
          ] <|>
    dir "static" (serveDirectory ".")

appendHandler :: Snap ()
appendHandler = do
    param <- getParam "appendparam"
    case param of Just param' -> liftIO $ appendNginxVhost $ BS.unpack param'
    	          Nothing -> writeBS "no no no !"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

invalidJson :: SomeException -> Snap ()
invalidJson ex = do
  writeBS "param"
  
removeHandler :: Snap ()
removeHandler = do 
    param <- getParam "removeparam"
    case param of 
      Just param' ->
          liftIO $ removeNginxVhost $ BS.unpack param'
    maybe (writeBS "must specify echo/param in URL")
          writeBS param