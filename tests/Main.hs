{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Cabal.Json.BuildInfo
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Proxy
import System.FilePath
import Test.QuickCheck.Instances.Text ()
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.QuickCheck as QC

main :: IO ()
main =
  defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Cabal Build-Info tests"
    [ buildInfoTests
    , componentInfoTests
    , propertyTests
    ]

buildInfoTests :: TestTree
buildInfoTests =
  testGroup
    "build info"
    [ mkBuildInfoTestCase "hie-bios" "hie-bios-build-info.json"
    ]
 where
  mkBuildInfoTestCase = mkGoldenTestCase (Proxy :: Proxy BuildInfo)

componentInfoTests :: TestTree
componentInfoTests =
  testGroup
    "component info"
    [ mkComponentInfoTestCase "hie-bios lib" "hie-bios-lib.json"
    , mkComponentInfoTestCase "hie-bios exe" "hie-bios-exe.json"
    , mkComponentInfoTestCase "hie-bios parser" "hie-bios-parser.json"
    ]
 where
  mkComponentInfoTestCase = mkGoldenTestCase (Proxy :: Proxy ComponentInfo)

mkGoldenTestCase :: forall a. (ToJSON a, FromJSON a) => Proxy a -> String -> String -> TestTree
mkGoldenTestCase _ testName testInput = do
  let inputFile = "tests/resources/input" </> testInput
      outputFile = "tests/resources/output" </> testInput
      goldenFile = "tests/resources/golden" </> testInput
  goldenVsFile testName goldenFile outputFile $ do
    Right (componentInfo :: a) <- eitherDecodeFileStrict inputFile
    createDirectoriesAndWriteFile outputFile (encodePretty componentInfo)

propertyTests :: TestTree
propertyTests =
  testGroup
    "Json Roundtrip test"
    [ QC.testProperty "CompilerInfo round trips" $ \(compilerInfo :: CompilerInfo) ->
        decode (encode compilerInfo) == Just compilerInfo
    , QC.testProperty "CompilerId round trips" $ \(arbitraryCompilerId :: CompilerId) ->
        decode (encode arbitraryCompilerId) == Just arbitraryCompilerId
    , QC.testProperty "ComponentInfo round trips" $ \(componentInfo :: ComponentInfo) ->
        decode (encode componentInfo) == Just componentInfo
    , QC.testProperty "BuildInfo round trips" $ \(buildInfo :: BuildInfo) ->
        decode (encode buildInfo) == Just buildInfo
    ]

instance Arbitrary ComponentInfo where
  arbitrary =
    ComponentInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CompilerInfo where
  arbitrary =
    CompilerInfo
      <$> oneof [pure "GHC", pure "GHCJS", pure "NHC"]
      <*> arbitrary
      <*> arbitrary

instance Arbitrary CompilerId where
  arbitrary =
    CompilerId
      <$> oneof [pure "ghc", pure "ghcjs"]
      <*> arbitrary

instance Arbitrary BuildInfo where
  arbitrary =
    BuildInfo
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
