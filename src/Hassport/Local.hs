module Hassport.Local
    ( LocalAuthRequest(..)
    , LocalAuthProvider
    , makeLocalProvider
    ) where

import Data.Text (Text)

import Hassport

data LocalAuthRequest = LocalAuthRequest
    { requestUsername :: !Text
    , requestPassword :: !Text
    }
type LocalAuthProvider u = AuthProvider LocalAuthRequest u

makeLocalProvider :: (LocalAuthRequest -> IO (AuthResult u)) -> LocalAuthProvider u
makeLocalProvider = AuthProvider