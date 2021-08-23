{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Cabal.BuildInfo
  ( decodeBuildInfoFile,
    BuildInfo (..),
    CompilerInfo (..),
    CompilerId (..),
    prettyCompilerId,
    ComponentInfo (..),
  )
where

import Data.Aeson
import qualified Data.Aeson.Encoding as A
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Version
import GHC.Generics
import Text.ParserCombinators.ReadP

decodeBuildInfoFile :: FilePath -> IO (Either String BuildInfo)
decodeBuildInfoFile = eitherDecodeFileStrict

decodeComponentInfoFile :: FilePath -> IO (Either String ComponentInfo)
decodeComponentInfoFile = eitherDecodeFileStrict

data CompilerId = CompilerId
  { compilerName :: T.Text,
    compilerVersion :: Version
  }
  deriving (Show, Eq, Ord)

prettyCompilerId :: CompilerId -> T.Text
prettyCompilerId CompilerId {..} = compilerName <> T.pack "-" <> T.pack (showVersion compilerVersion)

data BuildInfo = BuildInfo
  { cabalLibVersion :: Version,
    compiler :: CompilerInfo,
    components :: [ComponentInfo],
    projectRoot :: Maybe FilePath
  }
  deriving (Generic, Show, Eq)

data CompilerInfo = CompilerInfo
  { flavour :: T.Text,
    compilerId :: CompilerId,
    path :: FilePath
  }
  deriving (Generic, Show, Eq)

data ComponentInfo = ComponentInfo
  { componentType :: String,
    componentName :: String,
    componentUnitId :: String,
    componentCompilerArgs :: [String],
    componentModules :: [String],
    componentSrcFiles :: [FilePath],
    componentHsSrcDirs :: [FilePath],
    componentSrcDir :: FilePath,
    componentCabalFile :: Maybe FilePath
  }
  deriving (Generic, Show, Eq)

-- ----------------------------------------------
-- JSON instances
-- ----------------------------------------------

instance ToJSON CompilerId where
  toJSON = String . prettyCompilerId
  toEncoding = A.text . prettyCompilerId

instance FromJSON CompilerId where
  parseJSON (String v) = case parseCompilerId v of
    Just compId -> pure compId
    Nothing ->
      prependFailure
        "parsing CompilerId failed, "
        (typeMismatch "Version" (String v))
    where
      parseInnerVersion :: T.Text -> Maybe Version
      parseInnerVersion s =
        case reverse $ readP_to_S parseVersion (T.unpack s) of
          (version, "") : _ -> Just version
          _ -> Nothing

      parseCompilerId v = do
        let (namePart, versionStr) = T.breakOnEnd (T.pack "-") v
        version <- parseInnerVersion versionStr
        name <- T.stripSuffix (T.pack "-") namePart
        pure
          CompilerId
            { compilerName = name,
              compilerVersion = version
            }

  -- We do not expect a non-String value here.
  -- We could use empty to fail, but typeMismatch
  -- gives a much more informative error message.
  parseJSON invalid =
    prependFailure
      "parsing CompilerId failed, "
      (typeMismatch "String" invalid)

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
snakeCaseOptions = defaultOptions {fieldLabelModifier = camelTo2 '-'}

componentInfoDefaultOptions :: Options
componentInfoDefaultOptions = snakeCaseOptions {fieldLabelModifier = drop 10 . camelTo2 '-'}
