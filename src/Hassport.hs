{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Hassport
    ( AuthError(..)
    , AuthResult
    , AuthProvider(..)
    , AuthT(..)
    , Auth
    , MonadAuth(..)
    , authenticate
    , runAuth
    ) where

import Data.Text (Text)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO)

data AuthError
    = NotAuthorized Text
    | ServerError Text
    deriving Show

type AuthResult a = Either AuthError a

newtype AuthProvider req res m = AuthProvider { runAuthProvider :: req -> m (AuthResult res) } 
newtype AuthT req res m a = AuthT { runAuthT :: AuthProvider req res m -> m a }
type Auth req res a = AuthT req res Identity a

runAuth :: Auth req res a -> AuthProvider req res Identity -> a
runAuth auth provider = runIdentity $ runAuthT auth provider

instance Functor f => Functor (AuthT req res f) where
    fmap f (AuthT auth) = AuthT $ fmap f . auth

instance Applicative f => Applicative (AuthT req res f) where
    pure = AuthT . const . pure
    (AuthT authF) <*> (AuthT auth) = AuthT $ \config -> authF config <*> auth config

instance Monad m => Monad (AuthT req res m) where
    (AuthT auth) >>= f = AuthT $ \config -> do
        a <- auth config
        let nextAuth = runAuthT $ f a
        nextAuth config

authenticate :: Monad m => req -> AuthT req res m (AuthResult res)
authenticate req = AuthT $ \(AuthProvider auth) -> auth req

class Monad m => MonadAuth req res m | req res -> m where
    liftAuth :: AuthT req res m a -> m a