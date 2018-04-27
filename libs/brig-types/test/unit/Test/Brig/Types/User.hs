{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Brig.Types.User where

import Brig.Types.User
import Control.Lens
import Data.Aeson
import Data.Typeable
import Galley.Types.Teams
import Test.Brig.Types.Arbitrary ()
import Test.Tasty
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "User (types vs. aeson)"
    [ run' @BindingNewTeamUser Proxy repair
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
    run proxy = run' proxy id

    run' :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => Proxy a -> (a -> a) -> TestTree
    run' Proxy rep = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = Right (rep v) === (eitherDecode . encode) v


class NeedsRepair a where
    repair :: a -> a

-- the 'ToJSON' instance destroys the new team members, so we have to destroy them here in the
-- input, too.
instance NeedsRepair BindingNewTeamUser where
    repair (BindingNewTeamUser (BindingNewTeam nt) cur) = BindingNewTeamUser (BindingNewTeam (nt & newTeamMembers .~ Nothing)) cur
