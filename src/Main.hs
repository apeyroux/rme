{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import Control.Monad.IO.Class (liftIO)
import 			 Reverseme

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "{\"message\":\"404 not found !\"}") <|>
    route [ ("foo", writeBS "bar")
          , ("append/:appendparam", appendHandler)
          ] <|>
    dir "static" (serveDirectory ".")

appendHandler :: Snap ()
appendHandler = do
    param <- getParam "appendparam"
    liftIO $ writeNginxConf
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
