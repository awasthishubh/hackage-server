{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             TypeOperators, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Distribution.Server.Features.BuildReports.State where

import Distribution.Server.Features.BuildReports.BuildReports (BuildReportId, BuildLog, BuildCovg, BuildReport, BuildReports, BuildReports_v2)
import qualified Distribution.Server.Features.BuildReports.BuildReports as BuildReports

import Distribution.Package

import Control.Monad.Reader
import qualified Control.Monad.State as State
import Data.Acid     (Query, Update, makeAcidic)

initialBuildReports :: BuildReports_v2
initialBuildReports = BuildReports.emptyReports_v2

-- and defined methods
addReport :: PackageId -> (BuildReport, Maybe BuildLog) -> Update BuildReports_v2 BuildReportId
addReport pkgid report = do
    buildReports <- State.get
    let (reports, reportId) = BuildReports.addReport_v2 pkgid report buildReports
    State.put reports
    return reportId

setBuildLog :: PackageId -> BuildReportId -> Maybe BuildLog -> Update BuildReports_v2 Bool
setBuildLog pkgid reportId buildLog = do
    buildReports <- State.get
    case BuildReports.setBuildLog_v2 pkgid reportId buildLog buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

deleteReport :: PackageId -> BuildReportId -> Update BuildReports_v2 Bool --Maybe BuildReports
deleteReport pkgid reportId = do
    buildReports <- State.get
    case BuildReports.deleteReport_v2 pkgid reportId buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

lookupReport :: PackageId -> BuildReportId -> Query BuildReports_v2 (Maybe (BuildReport, Maybe BuildLog))
lookupReport pkgid reportId = asks (BuildReports.lookupReport_v2 pkgid reportId)

lookupPackageReports :: PackageId -> Query BuildReports_v2 [(BuildReportId, (BuildReport, Maybe BuildLog))]
lookupPackageReports pkgid = asks (BuildReports.lookupPackageReports_v2 pkgid)

getBuildReports :: Query BuildReports_v2 BuildReports_v2
getBuildReports = ask

replaceBuildReports :: BuildReports_v2-> Update BuildReports_v2()
replaceBuildReports = State.put


initialBuildReports_v3 :: BuildReports
initialBuildReports_v3 = BuildReports.emptyReports

-- and defined methods
addReport_v3 :: PackageId -> (BuildReport, Maybe BuildLog, Maybe BuildCovg ) -> Update BuildReports BuildReportId
addReport_v3 pkgid report = do
    buildReports <- State.get
    let (reports, reportId) = BuildReports.addReport pkgid report buildReports
    State.put reports
    return reportId

setBuildLog_v3 :: PackageId -> BuildReportId -> Maybe BuildLog -> Update BuildReports Bool
setBuildLog_v3 pkgid reportId buildLog = do
    buildReports <- State.get
    case BuildReports.setBuildLog pkgid reportId buildLog buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

deleteReport_v3 :: PackageId -> BuildReportId -> Update BuildReports Bool --Maybe BuildReports
deleteReport_v3 pkgid reportId = do
    buildReports <- State.get
    case BuildReports.deleteReport pkgid reportId buildReports of
        Nothing -> return False
        Just reports -> State.put reports >> return True

lookupReport_v3 :: PackageId -> BuildReportId -> Query BuildReports (Maybe (BuildReport, Maybe BuildLog, Maybe BuildCovg))
lookupReport_v3 pkgid reportId = asks (BuildReports.lookupReport pkgid reportId)

lookupPackageReports_v3 :: PackageId -> Query BuildReports [(BuildReportId, (BuildReport, Maybe BuildLog, Maybe BuildCovg))]
lookupPackageReports_v3 pkgid = asks (BuildReports.lookupPackageReports pkgid)

getBuildReports_v3 :: Query BuildReports BuildReports
getBuildReports_v3 = ask

replaceBuildReports_v3 :: BuildReports -> Update BuildReports ()
replaceBuildReports_v3 = State.put






makeAcidic ''BuildReports['addReport_v3
                          ,'setBuildLog_v3
                          ,'deleteReport_v3
                          ,'lookupReport_v3
                          ,'lookupPackageReports_v3
                          ,'getBuildReports_v3
                          ,'replaceBuildReports_v3
                          ]


makeAcidic ''BuildReports_v2 ['addReport
                          ,'setBuildLog
                          ,'deleteReport
                          ,'lookupReport
                          ,'lookupPackageReports
                          ,'getBuildReports
                          ,'replaceBuildReports
                          ]

