{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cabal.Json.BuildInfo (
  decodeBuildInfoFile,
  BuildInfo (..),
  CompilerInfo (..),
  CompilerId (..),
  prettyCompilerId,
  ComponentInfo (..),
)
where

import Data.Aeson
import qualified Data.Text as T
import Data.Version
import GHC.Generics

import Cabal.Json.Common

decodeBuildInfoFile :: FilePath -> IO (Either String BuildInfo)
decodeBuildInfoFile = eitherDecodeFileStrict

decodeComponentInfoFile :: FilePath -> IO (Either String ComponentInfo)
decodeComponentInfoFile = eitherDecodeFileStrict

data BuildInfo = BuildInfo
  { cabalLibVersion :: Version
  , compiler :: CompilerInfo
  , components :: [ComponentInfo]
  }
  deriving (Generic, Show, Eq)

data CompilerInfo = CompilerInfo
  { flavour :: T.Text
  , compilerId :: CompilerId
  , path :: FilePath
  }
  deriving (Generic, Show, Eq)

data ComponentInfo = ComponentInfo
  { componentType ::   T.Text
  , componentName ::   T.Text
  , componentUnitId :: T.Text
  , componentCompilerArgs :: [T.Text]
  , componentModules :: [T.Text]
  , componentSrcFiles :: [T.Text]
  , componentHsSrcDirs :: [T.Text]
  , componentSrcDir :: T.Text
  , componentCabalFile :: Maybe T.Text
  }
  deriving (Generic, Show, Eq)

-- ----------------------------------------------
-- JSON instances
-- ----------------------------------------------

instance ToJSON BuildInfo where
  toJSON = genericToJSON snakeCaseOptions
  toEncoding = genericToEncoding snakeCaseOptions

instance FromJSON BuildInfo where
  parseJSON = genericParseJSON snakeCaseOptions

instance ToJSON CompilerInfo where
  toJSON = genericToJSON snakeCaseOptions
  toEncoding = genericToEncoding snakeCaseOptions

instance FromJSON CompilerInfo where
  parseJSON = genericParseJSON snakeCaseOptions

instance ToJSON ComponentInfo where
  toJSON = genericToJSON componentInfoDefaultOptions
  toEncoding = genericToEncoding componentInfoDefaultOptions

instance FromJSON ComponentInfo where
  parseJSON = genericParseJSON componentInfoDefaultOptions

snakeCaseOptions :: Options
snakeCaseOptions = defaultOptions{fieldLabelModifier = camelTo2 '-'}

componentInfoDefaultOptions :: Options
componentInfoDefaultOptions = snakeCaseOptions{fieldLabelModifier = drop 10 . camelTo2 '-'}
