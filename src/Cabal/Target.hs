{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cabal.Target where

import Cabal.Json.Plan
import qualified Data.Map as Map
import qualified Data.Text as T

listTargets :: CabalPlan UnitId -> [T.Text]
listTargets cplan =
  let
    installPlan = planInstallPlan cplan
   in
    flip concatMap installPlan $ \case
      InstalledUnit unitNode -> do
        "local" <- pure $ unitStyle unitNode
        pure $ prettyTarget (unitPkgName unitNode) (unitComponentName unitNode)
      InstalledPackage pkgNode -> do
        "local" <- pure $ packageStyle pkgNode
        [ prettyTarget (packagePkgName pkgNode) componentName
          | componentName <- Map.keys (packageComponents pkgNode)
          ]
      Existing _ -> mempty

  where
    prettyTarget package component
      | component == "lib" = package <> ":lib:" <> package
      | otherwise = package <> ":" <> component
