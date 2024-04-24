{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Cabal.Json.Common where

import qualified Data.Aeson.Encoding as A
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Version
import Text.ParserCombinators.ReadP
import Data.Data

data CompilerId = CompilerId
  { compilerName :: T.Text
  , compilerVersion :: Version
  }
  deriving (Show, Eq, Ord, Data, Typeable)

prettyCompilerId :: CompilerId -> T.Text
prettyCompilerId CompilerId{..} = compilerName <> T.pack "-" <> T.pack (showVersion compilerVersion)

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
          { compilerName = name
          , compilerVersion = version
          }

  -- We do not expect a non-String value here.
  -- We could use empty to fail, but typeMismatch
  -- gives a much more informative error message.
  parseJSON invalid =
    prependFailure
      "parsing CompilerId failed, "
      (typeMismatch "String" invalid)
