{-# OPTIONS_GHC -fno-warn-orphans #-}
module TodoSpec where

import Prelude ()
import Prelude.Compat

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Paths_example
import           Servant.OpenApi.Test
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()
import           Todo

spec :: Spec
spec = describe "Swagger" $ do
  context "ToJSON matches ToSchema" $ validateEveryToJSON todoAPI
  it "swagger.json is up-to-date" $ do
    path <- getDataFileName "swagger.json"
    swag <- eitherDecode <$> BL8.readFile path
    swag `shouldBe` Right todoSwagger

instance Arbitrary Todo where
  arbitrary = Todo <$> arbitrary <*> arbitrary

instance Arbitrary TodoId where
  arbitrary = TodoId <$> arbitrary
