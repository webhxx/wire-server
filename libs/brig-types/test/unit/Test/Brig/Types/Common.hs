{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Brig.Types.Common where

import Brig.Types.Common
import Control.Monad
import Data.Aeson
import Data.Monoid
import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.Text as ST

tests :: TestTree
tests = testGroup "Common (types vs. aeson)"
    [ run @Handle Proxy
    , run @Name Proxy
    , run @ColourId Proxy
    , run @Email Proxy
    , run @Phone Proxy
    , run @UserIdentity Proxy
    , run @UserSSOId Proxy
    , run @AssetSize Proxy
    , run @Asset Proxy
    ]
  where
    run :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a, Eq a, Show a)
         => Proxy a -> TestTree
    run Proxy = testProperty msg trip
      where
        msg = show $ typeOf (undefined :: a)
        trip (v :: a) = Right v === (eitherDecode . encode) v


arbst :: Gen ST.Text
arbst = ST.pack <$> arbitrary

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
      local  <- ST.filter (/= '@') <$> arbst
      domain <- ST.filter (/= '@') <$> arbst
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
  arbitrary = UserSSOId <$> arbst

instance Arbitrary AssetSize where
  arbitrary = elements [minBound..]

instance Arbitrary Asset where
  arbitrary = ImageAsset <$> arbst <*> arbitrary
