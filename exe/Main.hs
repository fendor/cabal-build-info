{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Cabal.Json.BuildInfo
import Cabal.Json.Plan
import Cabal.Plan
import Cabal.Target (listTargets)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.Directory (getCurrentDirectory)
import System.FilePath

main :: IO ()
main = do
  theOpts <- execParser opts

  planJsonLoc <- planJsonLocToFp $ optPlanJson theOpts
  mPlanJson <- readCabalPlanJson planJsonLoc
  case mPlanJson of
    Left err -> error $ "Failed to parse `plan.json` found at " <> planJsonLoc <> " because: " <> err
    Right planJson -> do
      case optCommand theOpts of
        ListTargets -> mapM_ T.putStrLn (listTargets planJson)
        ShowCompilationOptions target -> case findLocalUnitByTarget target planJson of
          Nothing -> error "No such unit was found"
          Just unitOrPkg -> do
            let Just biFp = case unitOrPkg of
                  InstalledUnit node -> unitBuildInfo node
                  InstalledPackage node -> packageBuildInfo node
                  _ -> error "unexpected preexisting"

                pkgName = case unitOrPkg of
                  InstalledUnit node -> unitPkgName node
                  InstalledPackage node -> packagePkgName node
                  _ -> error "unexpected preexisting"

            decodeBuildInfoFile biFp >>= \case
              Left err -> error $ "Failed to parse build-info at " <> biFp <> ": " <> show err
              Right bi ->
                case findComponent pkgName target bi of
                  Nothing -> error "No such component"
                  Just compInfo -> do
                    mapM_ T.putStrLn (componentCompilerArgs compInfo <> componentModules compInfo <> componentSrcFiles compInfo)
 where
  opts =
    info
      (optionParser <**> helper)
      ( fullDesc
          <> progDesc "Parse various build artefacts of cabal"
          <> header "cabal-build-info: parse and query for information found in cabal's build artefacts"
      )

findComponent :: T.Text -> Target -> BuildInfo -> Maybe ComponentInfo
findComponent pkgName target bi =
  Maybe.listToMaybe
    [ compInfo
    | compInfo <- components bi
    , T.intercalate ":" [pkgName, compName (componentName compInfo)] == target
    ]
 where
  compName name
    | name == "lib" = "lib:" <> pkgName
    | otherwise = name

data PlanJsonLoc
  = InDirectory FilePath
  | InBuildDir FilePath
  | InExact FilePath
  deriving (Show, Eq, Ord)

planJsonLocToFp :: Maybe PlanJsonLoc -> IO FilePath
planJsonLocToFp Nothing = do
  cwd <- getCurrentDirectory
  pure $ cwd </> "dist-newstyle" </> "cache" </> "plan.json"
planJsonLocToFp (Just loc) = pure $ case loc of
  InDirectory fp -> fp </> "dist-newstyle" </> "cache" </> "plan.json"
  InBuildDir fp -> fp </> "cache" </> "plan.json"
  InExact fp -> fp

data Options = Options
  { optPlanJson :: Maybe PlanJsonLoc
  , optCommand :: Command
  }

data Command
  = ListTargets
  | ShowCompilationOptions Target

optionParser :: Parser Options
optionParser =
  Options
    <$> optional
      ( InDirectory
          <$> strOption (long "work-dir" <> short 'w' <> metavar "FILE")
            <|> InBuildDir
          <$> strOption (long "builddir" <> metavar "FILE")
            <|> InExact
          <$> strOption (long "plan-json" <> metavar "FILE")
      )
    <*> ( subparser $
            mconcat
              [ command "list-targets" (info (pure ListTargets) (progDesc "List targets of the cabal project"))
              , command
                  "build-info"
                  ( info showCompilationOptions (progDesc "Find compilation options for this target")
                  )
              ]
        )
 where
  showCompilationOptions =
    ShowCompilationOptions
      <$> argument str (metavar "TARGET")
