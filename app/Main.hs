{-# LANGUAGE OverloadedStrings #-}

module Main where

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
    let password = T.pack pwd
    authResult1 <- runAuth (auth1 password) authProvider
    authResult2 <- runAuth (auth2 password) authProvider
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