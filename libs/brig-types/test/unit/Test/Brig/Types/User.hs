{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Brig.Types.User where

import Brig.Types.User
import Control.Monad
import Data.Aeson
import Data.Currency
import Data.Misc
import Data.Monoid
import Data.Range
import Data.Typeable
import Galley.Types.Teams
import GHC.TypeLits
import Test.Brig.Types.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as ST

tests :: TestTree
tests = testGroup "User (types vs. aeson)"
    [ run @BindingNewTeamUser Proxy
    , run @CheckHandles Proxy
    , run @CompletePasswordReset Proxy
    , run @DeleteUser Proxy
    , run @DeletionCodeTimeout Proxy
    , run @EmailRemove Proxy
    , run @EmailUpdate Proxy
    , run @HandleUpdate Proxy
    , run @LocaleUpdate Proxy
    , run @NewPasswordReset Proxy
    , run @NewUser Proxy
    , run @PasswordChange Proxy
    , run @PhoneRemove Proxy
    , run @PhoneUpdate Proxy
    , run @SelfProfile Proxy
    , run @UserHandleInfo Proxy
    , run @UserProfile Proxy
    , run @UserUpdate Proxy
    , run @User Proxy
    , run @VerifyDeleteUser Proxy
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => Proxy a -> TestTree
    run Proxy = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = Right v === (eitherDecode . encode) v
