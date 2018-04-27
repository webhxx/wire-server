{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Galley.Types.Bot.Service
    ( ServiceToken (..)
    , ServiceRef
    , newServiceRef
    , serviceRefId
    , serviceRefProvider
    , Service
    , newService
    , serviceRef
    , serviceUrl
    , serviceToken
    , serviceFingerprints
    , serviceEnabled
    ) where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (Fingerprint, Rsa, HttpsUrl)
import Data.Text.Ascii
import Galley.Types.Bot.Service.Internal
