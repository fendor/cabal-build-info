{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Cabal.Json.Plan where

import Cabal.Json.Common
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Text as T
import Data.Version
import GHC.Generics
import Data.Data
import Data.Aeson.Types

type UnitId = T.Text

type PackageName = T.Text

readCabalPlanJson :: FilePath -> IO (Either String (CabalPlan UnitId))
readCabalPlanJson fp = Aeson.eitherDecodeFileStrict' fp

data CabalPlan key = CabalPlan
  { planCabalVersion :: Version
  , planCabalLibVersion :: Version
  , planCompilerId :: CompilerId
  , planOs :: T.Text
  , planArch :: T.Text
  , planInstallPlan :: [InstallPlanNode key]
  }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

data InstallPlanNode key where
  InstalledUnit :: InstalledUnitNode key -> InstallPlanNode key
  InstalledPackage :: InstalledPackageNode key -> InstallPlanNode key
  Existing :: PreExistingNode key -> InstallPlanNode key
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

data PreExistingNode key = PreExistingNode
  { preExistingType :: T.Text
  , preExistingId :: T.Text
  , preExistingPkgName :: T.Text
  , preExistingPkgVersion :: T.Text
  , preExistingDepends :: [key]
  }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

data InstalledUnitNode key = InstalledUnitNode
  { unitType :: T.Text
  , unitId :: key
  , unitPkgName :: T.Text
  , unitPkgVersion :: Version
  , unitFlags :: Map T.Text Bool
  , unitStyle :: T.Text
  , unitPkgSrc :: PackageSource
  , unitPkgCabalSha256 :: Maybe T.Text
  , unitPkgSrcSha256 :: Maybe T.Text
  , unitBuildInfo :: Maybe FilePath
  , unitDistDir :: Maybe FilePath
  , unitDepends :: [key]
  , unitExeDepends :: [key]
  , unitBinFile :: Maybe FilePath
  , unitComponentName :: T.Text
  }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

data InstalledPackageNode key = InstalledPackageNode
  { packageType :: T.Text
  , packageId :: key
  , packagePkgName :: T.Text
  , packagePkgVersion :: Version
  , packageFlags :: Map T.Text Bool
  , packageStyle :: T.Text
  , packagePkgSrc :: PackageSource
  , packagePkgCabalSha256 :: Maybe T.Text
  , packagePkgSrcSha256 :: Maybe T.Text
  , packageBuildInfo :: Maybe FilePath
  , packageDistDir :: Maybe FilePath
  , packageComponents :: Map Component (PackageDepends key)
  }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

type Component = T.Text

type ComponentId = T.Text

data PackageDepends key = PackageDepends
  { packageDependsDepends :: [key]
  , packageDependsExeDepends :: [T.Text]
  , packageDependsBinFile :: Maybe FilePath
  }
  deriving (Show, Ord, Eq, Functor, Foldable, Traversable, Generic, Data, Typeable)

data PackageSource
  = LocalSource LocalPackageSource
  | -- | UriSource RepoSourceUri
    TarSource RepoSourceTar
  | VcsSource SourceRepo
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

data SourceRepo = SourceRepo
  { sourceRepoType :: T.Text
  , sourceRepoLocation :: T.Text
  , sourceRepoTag :: T.Text
  , sourceReposSubdir :: Maybe T.Text
  }
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

data LocalPackageSource = LocalPackageSource
  { localPackageSourcePath :: FilePath
  }
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

data RepoSourceUri = RepoSourceUri
  { repoSourceUri :: T.Text
  }
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

data RepoSourceTar = RepoSourceTar
  { repoSourceTarUri :: T.Text
  , repoSourceTarType :: T.Text
  }
  deriving (Show, Ord, Eq, Generic, Data, Typeable)

instance (ToJSON key) => ToJSON (CabalPlan key) where
  toJSON = genericToJSON cabalPlanOptions
  toEncoding = genericToEncoding cabalPlanOptions

instance (FromJSON key) => FromJSON (CabalPlan key) where
  parseJSON = genericParseJSON cabalPlanOptions

instance (ToJSON key) => ToJSON (InstallPlanNode key) where
  toJSON = genericToJSON installPlanNodeOptions
  toEncoding = genericToEncoding installPlanNodeOptions

instance (FromJSON key) => FromJSON (InstallPlanNode key) where
  parseJSON = genericParseJSON installPlanNodeOptions

instance (ToJSON key) => ToJSON (PreExistingNode key) where
  toJSON = genericToJSON preExistingNodeOptions
  toEncoding = genericToEncoding preExistingNodeOptions

instance (FromJSON key) => FromJSON (PreExistingNode key) where
  parseJSON = genericParseJSON preExistingNodeOptions

instance (ToJSON key) => ToJSON (InstalledUnitNode key) where
  toJSON = genericToJSON installedUnitNodeOptions
  toEncoding = genericToEncoding installedUnitNodeOptions

instance (FromJSON key) => FromJSON (InstalledUnitNode key) where
  parseJSON = genericParseJSON installedUnitNodeOptions

instance (ToJSON key) => ToJSON (InstalledPackageNode key) where
  toJSON = genericToJSON installedPackageNodeOptions
  toEncoding = genericToEncoding installedPackageNodeOptions

instance (FromJSON key) => FromJSON (InstalledPackageNode key) where
  parseJSON = genericParseJSON installedPackageNodeOptions

instance (ToJSON key) => ToJSON (PackageDepends key) where
  toJSON = genericToJSON packageDependsOptions
  toEncoding = genericToEncoding packageDependsOptions

instance (FromJSON key) => FromJSON (PackageDepends key) where
  parseJSON = genericParseJSON packageDependsOptions

instance ToJSON PackageSource where
  toJSON (LocalSource (LocalPackageSource fp)) =
    Aeson.object
      [ "type" .= ("local" :: T.Text)
      , "path" .= T.pack fp
      ]
  toJSON (TarSource repoSourceTar) =
    Aeson.object
      [ "type" .= ("repo-tar" :: T.Text)
      , "repo" .= repoSourceTar
      ]
  toJSON (VcsSource sourceRepo) =
    Aeson.object
      [ "type" .= ("source-repo" :: T.Text)
      , "source-repo" .= sourceRepo
      ]

  toEncoding (LocalSource (LocalPackageSource fp)) =
    Aeson.pairs $ mconcat
      [ "type" .= ("local" :: T.Text)
      , "path" .= T.pack fp
      ]
  toEncoding (TarSource repoSourceTar) =
    Aeson.pairs $ mconcat
      [ "type" .= ("repo-tar" :: T.Text)
      , "repo" .= repoSourceTar
      ]
  toEncoding (VcsSource sourceRepo) =
    Aeson.pairs $ mconcat
      [ "type" .= ("source-repo" :: T.Text)
      , "source-repo" .= sourceRepo
      ]

instance FromJSON PackageSource where
  parseJSON = withObject "PackageSource" $ \o -> do
    ty :: T.Text <- o .: "type"
    case ty of
      "local" -> LocalSource <$> (LocalPackageSource <$> o .: "path")
      "repo-tar" -> TarSource <$> (o .: "repo")
      "source-repo" -> VcsSource <$> (o .: "source-repo")
      _ ->
        prependFailure
          "parsing PackageSource failed, "
          (typeMismatch "type" (String ty))

instance ToJSON SourceRepo where
  toJSON = genericToJSON sourceRepoOptions
  toEncoding = genericToEncoding sourceRepoOptions

instance FromJSON SourceRepo where
  parseJSON = genericParseJSON sourceRepoOptions

instance ToJSON RepoSourceTar where
  toJSON = genericToJSON repoSourceTarOptions
  toEncoding = genericToEncoding repoSourceTarOptions

instance FromJSON RepoSourceTar where
  parseJSON = genericParseJSON repoSourceTarOptions

installPlanNodeOptions :: Options
installPlanNodeOptions = defaultOptions{sumEncoding = UntaggedValue}

cabalPlanOptions :: Options
cabalPlanOptions = defaultOptionsWithPrefix "plan"

sourceRepoOptions :: Options
sourceRepoOptions = defaultOptionsWithPrefix "sourceRepo"

repoSourceTarOptions :: Options
repoSourceTarOptions = defaultOptionsWithPrefix "repoSourceTar"

installedPackageNodeOptions :: Options
installedPackageNodeOptions = defaultOptionsWithPrefix "package"

preExistingNodeOptions :: Options
preExistingNodeOptions = defaultOptionsWithPrefix "preExisting"

installedUnitNodeOptions :: Options
installedUnitNodeOptions = defaultOptionsWithPrefix "unit"

packageDependsOptions :: Options
packageDependsOptions = defaultOptionsWithPrefix "packageDepends"

defaultOptionsWithPrefix :: T.Text -> Options
defaultOptionsWithPrefix t = defaultOptions{fieldLabelModifier = drop (prefix + 1) . camelTo2 '-'}
 where
  prefix = length (camelTo2 '-' $ T.unpack t)
