###################################################
################one field ############################
###################################################
# Create seasonStarts data frame for 2018
EProcWay32018 <- sEddyProc$new('Way3-Data2018', way3_data_reddyproc[[1]], c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EProcWay32018$sSetLocationInfo(LatDeg = LatDeg, LongDeg = LongDeg, TimeZoneHour = TimeZoneHour)  # Set location and timezone

seasonStartsway2018 <- as.data.frame(do.call(rbind, list(
  c(way32018PD_DOY, way32018HD_DOY)
)))

seasonFactorway32018 <- usCreateSeasonFactorYdayYear(
  way3_data_reddyproc[[1]]$DateTime - 15*60, starts = seasonStartsway2018)
EProcWay32018$sEstimateUstarScenarios(seasonFactor = seasonFactorway32018, nSample = 30L, probs = c(0.1, 0.9))
EProcWay32018$sMDSGapFillUStarScens("NEE", FillAll = TRUE)

EProcWay32018$sPlotFingerprintY('NEE_uStar_orig', Year = 2018)
EProcWay32018$sPlotFingerprint('NEE_U90_f', Dir = "plotsHandsOn")

EProcWay32018$sMDSGapFill('Rg', FillAll = FALSE)
EProcWay32018$sMDSGapFill('Tair', FillAll = FALSE)
EProcWay32018$sMDSGapFill('VPD', FillAll = FALSE)

EProcWay32018$sPlotFingerprintY('Rg_f', Year = 2018)
EProcWay32018$sPlotFingerprintY('Tair_f', Year = 2018)
EProcWay32018$sPlotFingerprintY('VPD_f', Year = 2018)

EProcWay32018$sMRFluxPartitionUStarScens()
dsResultsWay32018 <- EProcWay32018$sExportResults()
View(dsResults)
EProcWay32018$sGLFluxPartitionUStarScens()

# Bonus Task: Repeat with fixed Temperature Sensitivity to 80 $\pm$40 K
EProcWay32018$sGLFluxPartitionUStarScens(
  controlGLPart = partGLControl(
    fixedTempSens = data.frame(E0 = 80, sdE0 = 40, RRef = NA_real_))
  , isWarnReplaceColumns = FALSE
)

EProcWay32018$sPlotFingerprintY("GPP_DT_uStar", Year = 2018)
EProcWay32018$sPlotFingerprintY("Reco_DT_uStar", Year = 2018)

dsResultsWay32018 <- EProcWay32018$sExportResults()
EProcWay32018$sGLFluxPartitionUStarScens()
colnames(dsResultsWay32018)
