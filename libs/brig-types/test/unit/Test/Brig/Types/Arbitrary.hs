{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Brig.Types.Arbitrary where

import Brig.Types.Activation
import Brig.Types.Code
import Brig.Types.TURN
import Brig.Types.User
import Brig.Types.User.Auth
import Control.Monad
import Data.Aeson
import Data.Currency
import Data.Id
import Data.IP
import Data.Misc
import Data.Monoid
import Data.Range
import Data.Text.Ascii
import Data.Time
import Data.Typeable
import Data.Word
import Galley.Types.Bot.Service
import Galley.Types.Teams
import Galley.Types.Teams.Internal
import GHC.TypeLits
import Test.QuickCheck
import Test.Tasty hiding (Timeout)
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Char8 as SBS
import qualified Data.Text as ST


newtype Octet = Octet { octet :: Word16 }
    deriving (Eq, Show)

instance Arbitrary Octet where
    arbitrary = Octet <$> arbitrary `suchThat` (<256)

instance Arbitrary Scheme where
    arbitrary = genEnumBounded

-- TODO: Add an arbitrary instance for IPv6
instance Arbitrary IpAddr where
    arbitrary = ipV4Arbitrary
      where
        ipV4Arbitrary :: Gen IpAddr
        ipV4Arbitrary = do
            a <- ipV4Part
            b <- ipV4Part
            c <- ipV4Part
            d <- ipV4Part
            let adr = show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
            IpAddr . IPv4 <$> return (read adr)

        ipV4Part = octet <$> arbitrary

instance Arbitrary TurnHost where
    arbitrary = TurnHost <$> arbitrary

instance Arbitrary Port where
    arbitrary = Port <$> arbitrary

instance Arbitrary Transport where
    arbitrary = genEnumBounded

instance Arbitrary TurnURI where
    arbitrary = turnURI <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary


instance Arbitrary Handle where
  arbitrary = Handle . ST.pack <$> do
      let manyC n = replicateM n (elements $ ['a'..'z'] <> ['0'..'9'] <> ['_'])
      ((<>) <$> manyC 2 <*> (manyC =<< choose (0, 19)))

instance Arbitrary Name where
  arbitrary = Name . ST.pack <$>
      ((`replicateM` arbitrary) =<< choose (1, 128))

instance Arbitrary ColourId where
  arbitrary = ColourId <$> arbitrary

instance Arbitrary Email where
  arbitrary = do
      local  <- ST.filter (/= '@') <$> genText
      domain <- ST.filter (/= '@') <$> genText
      pure $ Email local domain

instance Arbitrary Phone where
  arbitrary = Phone . ST.pack <$> do
      let mkdigits n = replicateM n (elements ['0'..'9'])
      mini <- mkdigits 8
      maxi <- mkdigits =<< choose (0, 7)
      pure $ '+' : mini <> maxi

instance Arbitrary UserIdentity where
  arbitrary = oneof
    [ FullIdentity  <$> arbitrary <*> arbitrary
    , EmailIdentity <$> arbitrary
    , PhoneIdentity <$> arbitrary
    , SSOIdentity   <$> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary UserSSOId where
  arbitrary = UserSSOId <$> genText

instance Arbitrary AssetSize where
  arbitrary = genEnumBounded

instance Arbitrary Asset where
  arbitrary = ImageAsset <$> genText <*> arbitrary


instance Arbitrary BindingNewTeamUser where
    arbitrary = BindingNewTeamUser <$> (BindingNewTeam <$> arbitrary) <*> genMaybe genEnumBounded

instance Arbitrary (NewTeam ()) where
    arbitrary = NewTeam <$> txt <*> txt <*> genMaybe txt <*> genMaybe (pure ())
      where txt = genRangeText @1 @256 arbitrary

instance Arbitrary CheckHandles where
    arbitrary = CheckHandles <$> genRangeList @1 @50 genText <*> (unsafeRange @Word @1 @10 <$> choose (1, 10))

instance Arbitrary CompletePasswordReset where
    arbitrary = CompletePasswordReset <$> arbitrary <*> (PasswordResetCode <$> arbitrary) <*> arbitrary

instance Arbitrary PasswordResetIdentity where
    arbitrary = oneof
        [ PasswordResetIdentityKey . PasswordResetKey <$> arbitrary
        , PasswordResetEmailIdentity <$> arbitrary
        , PasswordResetPhoneIdentity <$> arbitrary
        ]

instance Arbitrary AsciiBase64Url where
    arbitrary = encodeBase64Url . SBS.pack <$> arbitrary @String

instance Arbitrary PlainTextPassword where
    arbitrary = PlainTextPassword . ST.pack <$> arbitrary @String

instance Arbitrary DeleteUser where
    arbitrary = DeleteUser <$> genMaybe arbitrary

instance Arbitrary DeletionCodeTimeout where
    arbitrary = DeletionCodeTimeout <$> arbitrary

instance Arbitrary Timeout where
    arbitrary = Timeout . fromIntegral <$> arbitrary @Int

instance Arbitrary EmailRemove where
    arbitrary = EmailRemove <$> arbitrary

instance Arbitrary EmailUpdate where
    arbitrary = EmailUpdate <$> arbitrary

instance Arbitrary HandleUpdate where
    arbitrary = HandleUpdate . ST.pack <$> arbitrary

instance Arbitrary LocaleUpdate where
    arbitrary = LocaleUpdate <$> arbitrary

instance Arbitrary NewPasswordReset where
    arbitrary = NewPasswordReset <$> arbitrary

instance Arbitrary NewUser where
    arbitrary = do
        x0  <- arbitrary
        x1  <- arbitrary
        x2  <- pure Nothing
        x3  <- arbitrary
        x4  <- arbitrary
        x5  <- genMaybe $ ActivationCode <$> arbitrary
        x6  <- genMaybe $ ActivationCode <$> arbitrary
        x7  <- genMaybe $ InvitationCode <$> arbitrary
        x8  <- genMaybe $ CookieLabel . ST.pack <$> arbitrary
        x9  <- arbitrary
        x10 <- arbitrary
        x11 <- arbitrary
        x12 <- genMaybe $ unsafeRange @Integer @1 @604800 <$> choose (1, 604800)

        pure NewUser
            { newUserName           = x0  :: Name
            , newUserIdentity       = x1  :: (Maybe UserIdentity)
            , newUserPict           = x2  :: (Maybe Pict) -- ^ DEPRECATED
            , newUserAssets         = x3  :: [Asset]
            , newUserAccentId       = x4  :: (Maybe ColourId)
            , newUserEmailCode      = x5  :: (Maybe ActivationCode)
            , newUserPhoneCode      = x6  :: (Maybe ActivationCode)
            , newUserInvitationCode = x7  :: (Maybe InvitationCode)
            , newUserLabel          = x8  :: (Maybe CookieLabel)
            , newUserLocale         = x9  :: (Maybe Locale)
            , newUserPassword       = x10 :: (Maybe PlainTextPassword)
            , newUserTeam           = x11 :: (Maybe NewTeamUser)
            , newUserExpiresIn      = x12 :: (Maybe (Range 1 604800 Integer)) -- ^ 1 second - 1 week
            }

instance Arbitrary NewTeamUser where
    arbitrary = oneof
        [ NewTeamMember <$> arbitrary
        , NewTeamCreator <$> arbitrary
        ]

instance Arbitrary InvitationCode where
    arbitrary = InvitationCode <$> arbitrary

instance Arbitrary PasswordChange where
    arbitrary = PasswordChange <$> genMaybe arbitrary <*> arbitrary

instance Arbitrary PhoneRemove where
    arbitrary = PhoneRemove <$> arbitrary

instance Arbitrary PhoneUpdate where
    arbitrary = PhoneUpdate <$> arbitrary

instance Arbitrary SelfProfile where
    arbitrary = SelfProfile <$> arbitrary

instance Arbitrary UserHandleInfo where
    arbitrary = UserHandleInfo <$> arbitrary

instance Arbitrary UserProfile where
    arbitrary = do
        x0 <- arbitrary
        x1 <- arbitrary
        x2 <- pure $ Pict []
        x3 <- arbitrary
        x4 <- arbitrary
        x5 <- arbitrary
        x6 <- arbitrary
        x7 <- arbitrary
        x8 <- arbitrary
        x9 <- arbitrary
        x10 <- arbitrary

        pure UserProfile
            { profileId       = x0  :: UserId
            , profileName     = x1  :: Name
            , profilePict     = x2  :: Pict -- ^ DEPRECATED
            , profileAssets   = x3  :: [Asset]
            , profileAccentId = x4  :: ColourId
            , profileDeleted  = x5  :: Bool
            , profileService  = x6  :: (Maybe ServiceRef)
            , profileHandle   = x7  :: (Maybe Handle)
            , profileLocale   = x8  :: (Maybe Locale)
            , profileExpire   = x9  :: (Maybe UTCTime)
            , profileTeam     = x10 :: (Maybe TeamId)
            }

instance Arbitrary ServiceRef where
    arbitrary = _

instance Arbitrary UserUpdate where
    arbitrary = _

instance Arbitrary User where
    arbitrary = _

instance Arbitrary VerifyDeleteUser where
    arbitrary = _

instance Arbitrary Locale where
    arbitrary = _


----------------------------------------------------------------------
-- utilities

genText :: Gen ST.Text
genText = ST.pack <$> arbitrary

genRangeList :: forall (n :: Nat) (m :: Nat) (a :: *). (Show a, KnownNat n, KnownNat m, LTE n m)
         => Gen a -> Gen (Range n m [a])
genRangeList gc = unsafeRange @[a] @n @m <$> grange (val (Proxy @n)) (val (Proxy @m)) gc
  where
    grange mi ma gelem = (`replicateM` gelem) =<< choose (mi, ma + mi)

    val :: forall (k :: Nat). (KnownNat k) => Proxy k -> Int
    val p = fromIntegral $ natVal p

genRangeText :: forall (n :: Nat) (m :: Nat). (KnownNat n, KnownNat m, LTE n m)
         => Gen Char -> Gen (Range n m ST.Text)
genRangeText gc = unsafeRange @ST.Text @n @m . ST.pack <$> grange (val (Proxy @n)) (val (Proxy @m)) gc
  where
    grange mi ma gelem = (`replicateM` gelem) =<< choose (mi, ma + mi)

    val :: forall (k :: Nat). (KnownNat k) => Proxy k -> Int
    val p = fromIntegral $ natVal p

genAlphaNum :: Gen Char
genAlphaNum = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_']

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = oneof [pure Nothing, Just <$> gen]

genEnumBounded :: (Enum a, Bounded a) => Gen a
genEnumBounded = elements [minBound..]
