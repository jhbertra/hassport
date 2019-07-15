{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

module Hassport.Local
    ( LocalAuthRequest(..)
    , LocalAuthProvider
    , localProvider
    ) where

import Data.Text (Text)
import Control.Monad.Identity

import Hassport

data LocalAuthRequest = LocalAuthRequest
    { requestUsername :: !Text
    , requestPassword :: !Text
    }
type LocalAuthProvider user m = AuthProvider LocalAuthRequest user m

localProvider :: Monad m => (LocalAuthRequest -> m (AuthResult user)) -> LocalAuthProvider user m
localProvider = AuthProvider