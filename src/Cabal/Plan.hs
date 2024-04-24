{-# LANGUAGE OverloadedStrings #-}

module Cabal.Plan where

import Cabal.Json.Plan
import qualified Data.Maybe as Maybe
import qualified Data.Text as T

type Target = T.Text

findUnitByTarget :: T.Text -> CabalPlan UnitId -> Maybe (InstallPlanNode UnitId)
findUnitByTarget target = findUnitBy
  (\unit -> prettyTargetName unit == target)
  (\pkg -> packagePkgName pkg == pkgName)
  where
    (pkgName:_:_) = T.splitOn ":" target
    prettyTargetName node
      | unitComponentName node == "lib" = unitPkgName node <> ":lib:" <> unitPkgName node
      | otherwise = unitPkgName node <> ":" <> unitComponentName node

findUnitBy ::
  (InstalledUnitNode UnitId -> Bool) ->
  (InstalledPackageNode UnitId -> Bool) ->
  CabalPlan UnitId ->
  Maybe (InstallPlanNode UnitId)
findUnitBy pUnit pPkg planJson = do
  let plan = planInstallPlan planJson
  Maybe.listToMaybe $ filter go plan
 where
  go (InstalledUnit node) = pUnit node
  go (InstalledPackage node) = pPkg node
  go _ = False -- We don't support these nodes right now
