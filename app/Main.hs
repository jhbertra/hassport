{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.Identity
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, runReaderT)
import Data.Text (Text)
import System.Environment

import Hassport
import Hassport.Local

import qualified Data.Text as T

data Provider
    = Local1 LocalAuthRequest
    | Local2 LocalAuthRequest

main :: IO ()
main = do
    [pwd] <- getArgs
    authResult1 <- runAuth (auth1 $ T.pack pwd) authProvider
    authResult2 <- runAuth (auth2 $ T.pack pwd) authProvider
    print authResult1
    print authResult2
  where
    auth1 pwd = authenticateWith Local1 $ LocalAuthRequest "baz" pwd
    auth2 pwd = authenticateWith Local2 $ LocalAuthRequest "baz" pwd

authProvider :: AuthProvider Provider Text
authProvider = AuthProvider verify
  where
    verify (Local1 (LocalAuthRequest username password)) =
        pure $ if password == "foo"
            then Right username
            else Left $ NotAuthorized "Bad"
    verify (Local2 (LocalAuthRequest username password)) =
        pure $ if password == "food"
            then Right username
            else Left $ NotAuthorized "Bad"