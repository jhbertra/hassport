{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Monad.Identity
import Control.Monad.Reader (MonadReader, ReaderT(..), ask, runReaderT)
import Data.Text (Text)
import System.Environment

import Hassport
import Hassport.Local

import qualified Data.Text as T

type AppState = LocalAuthProvider String App
newtype App a = App { unApp :: ReaderT AppState IO a }
    deriving (Functor, Applicative, Monad, MonadReader AppState)

runApp :: App a -> AppState -> IO a
runApp app state = runReaderT (unApp app) state

instance MonadAuth LocalAuthRequest String App where
    liftAuth auth = ask >>= runAuthT auth

main :: IO ()
main = do
    [pwd] <- getArgs
    print =<< runApp (app $ T.pack pwd) appConfig
  where
    app :: Text -> App (AuthResult String)
    app pwd = liftAuth $ authenticate $ LocalAuthRequest "baz" pwd

appConfig :: AppState
appConfig = localProvider $ \(LocalAuthRequest username password) ->
    pure $ if password == "foo"
        then Right "bar"
        else Left $ NotAuthorized "Bad"