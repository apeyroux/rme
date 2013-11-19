{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import Prelude hiding (catch)
import qualified Control.Exception as C

-- import           Control.Exception.Lifted (handle)
-- import           Control.Exception      (SomeException)
-- import Control.Monad.CatchIO
import Control.Exception.Base
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS
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

exceptionCfg :: SomeException -> Snap ()
exceptionCfg ex = do
  writeBS "no non non !"

appendHandler :: Snap ()
appendHandler = do
    param <- getParam "appendparam"
    case param of 
      Just param' -> C.catch (createConfig (BS.unpack param')) exceptionCfg
    maybe (writeBS "must specify echo/param in URL") writeBS param

removeHandler :: Snap ()
removeHandler = do
  param <- getParam "removeparam"
  case  param of
    Just param' ->
      liftIO $ removeNginxVhost $ BS.unpack param'

  maybe (writeBS "must specify echo/param in URL")
    writeBS param
