{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hassport
    ( AuthError(..)
    , AuthResult
    , AuthProvider(..)
    , Auth(..)
    , MonadAuth(..)
    , authenticate
    , authenticateWith
    , runAuth
    ) where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, MonadReader(..), ask, runReaderT)

data AuthError
    = NotAuthorized Text
    | ServerError Text
    deriving Show

type AuthResult a = Either AuthError a

newtype AuthProvider r u = AuthProvider { runAuthProvider :: r -> IO (AuthResult u) } 
newtype Auth r u a = Auth
    { unAuth :: ReaderT (AuthProvider r u) IO a
    } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (AuthProvider r u)
    )

runAuth :: Auth r u a -> AuthProvider r u -> IO a
runAuth auth provider = runReaderT (unAuth auth) provider

authenticate :: r -> Auth r u (AuthResult u)
authenticate r = fmap runAuthProvider ask >>= liftIO . ($ r)

authenticateWith :: (a -> r) -> a -> Auth r u (AuthResult u)
authenticateWith f = authenticate . f

class MonadIO m => MonadAuth r u m where
    liftAuth :: Auth r u a -> m a

instance MonadAuth r u (Auth r u) where
    liftAuth = id