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

import Brig.Types.TURN
import Brig.Types.User
import Control.Monad
import Data.Aeson
import Data.Currency
import Data.IP
import Data.Misc
import Data.Misc
import Data.Monoid
import Data.Range
import Data.Typeable
import Data.Word
import Galley.Types.Teams
import GHC.TypeLits
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as ST


newtype Octet = Octet { octet :: Word16 }
    deriving (Eq, Show)

instance Arbitrary Octet where
    arbitrary = Octet <$> arbitrary `suchThat` (<256)

instance Arbitrary Scheme where
    arbitrary = elements [ SchemeTurn
                         , SchemeTurns
                         ]

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
    arbitrary = elements [ TransportUDP
                         , TransportTCP
                         ]

instance Arbitrary TurnURI where
    arbitrary = turnURI <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary


instance Arbitrary Handle where
  arbitrary = Handle . ST.pack <$> do
      let manyC n = replicateM n (elements $ ['a'..'z'] <> ['0'..'9'] <> ['_'])
      ((<>) <$> manyC 2 <*> (manyC =<< elements [0..19]))

instance Arbitrary Name where
  arbitrary = Name . ST.pack <$>
      ((`replicateM` arbitrary) =<< elements [1..128])

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
      maxi <- mkdigits =<< elements [0..7]
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
  arbitrary = elements [minBound..]

instance Arbitrary Asset where
  arbitrary = ImageAsset <$> genText <*> arbitrary


instance Arbitrary BindingNewTeamUser where
    arbitrary = BindingNewTeamUser <$> (BindingNewTeam <$> arbitrary) <*> genMaybe genEnumBounded

instance Arbitrary (NewTeam ()) where
    arbitrary = NewTeam <$> txt <*> txt <*> genMaybe txt <*> genMaybe (pure ())
      where txt = genRangeText @1 @256 arbitrary

instance Arbitrary CheckHandles where
    arbitrary = CheckHandles <$> genRangeList @1 @50 genText <*> (unsafeRange @Word @1 @10 <$> elements [1..10])

instance Arbitrary CompletePasswordReset where
    arbitrary = CompletePasswordReset <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PasswordResetIdentity where
    arbitrary = oneof
        [ PasswordResetIdentityKey <$> undefined
        , PasswordResetEmailIdentity <$> arbitrary
        , PasswordResetPhoneIdentity <$> arbitrary
        ]

instance Arbitrary PasswordResetCode where
    arbitrary = undefined

instance Arbitrary PlainTextPassword where
    arbitrary = undefined

instance Arbitrary DeleteUser where
    arbitrary = undefined

instance Arbitrary DeletionCodeTimeout where
    arbitrary = undefined

instance Arbitrary EmailRemove where
    arbitrary = undefined

instance Arbitrary EmailUpdate where
    arbitrary = undefined

instance Arbitrary HandleUpdate where
    arbitrary = undefined

instance Arbitrary LocaleUpdate where
    arbitrary = undefined

instance Arbitrary NewPasswordReset where
    arbitrary = undefined

instance Arbitrary NewUser where
    arbitrary = undefined

instance Arbitrary PasswordChange where
    arbitrary = undefined

instance Arbitrary PhoneRemove where
    arbitrary = undefined

instance Arbitrary PhoneUpdate where
    arbitrary = undefined

instance Arbitrary SelfProfile where
    arbitrary = undefined

instance Arbitrary UserHandleInfo where
    arbitrary = undefined

instance Arbitrary UserProfile where
    arbitrary = undefined

instance Arbitrary UserUpdate where
    arbitrary = undefined

instance Arbitrary User where
    arbitrary = undefined

instance Arbitrary VerifyDeleteUser where
    arbitrary = undefined


----------------------------------------------------------------------
-- utilities

genText :: Gen ST.Text
genText = ST.pack <$> arbitrary

genRangeList :: forall (n :: Nat) (m :: Nat) (a :: *). (Show a, KnownNat n, KnownNat m, LTE n m)
         => Gen a -> Gen (Range n m [a])
genRangeList gc = unsafeRange @[a] @n @m <$> grange (val (Proxy @n)) (val (Proxy @m)) gc
  where
    grange mi ma gelem = (`replicateM` gelem) =<< elements [mi .. (ma + mi)]

    val :: forall (k :: Nat). (KnownNat k) => Proxy k -> Int
    val p = fromIntegral $ natVal p

genRangeText :: forall (n :: Nat) (m :: Nat). (KnownNat n, KnownNat m, LTE n m)
         => Gen Char -> Gen (Range n m ST.Text)
genRangeText gc = unsafeRange @ST.Text @n @m . ST.pack <$> grange (val (Proxy @n)) (val (Proxy @m)) gc
  where
    grange mi ma gelem = (`replicateM` gelem) =<< elements [mi .. (ma + mi)]

    val :: forall (k :: Nat). (KnownNat k) => Proxy k -> Int
    val p = fromIntegral $ natVal p

genAlphaNum :: Gen Char
genAlphaNum = elements $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_']

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = oneof [pure Nothing, Just <$> gen]

genEnumBounded :: (Enum a, Bounded a) => Gen a
genEnumBounded = elements [minBound..]
