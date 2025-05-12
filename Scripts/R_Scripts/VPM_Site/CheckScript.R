way3_data [[1]] = 2018
way3_data[[2]] = 2019
way3_data[[3]] = 2020
way3_data [[4]] = 2021
way3_data [[5]] = 2022
way3_data[[6]] = 2023
way3_data[[7]] = 2024
way4_data [[1]] = 2018
way4_data[[2]] = 2019
way4_data[[3]] = 2020
way4_data [[4]] = 2021
way4_data [[5]] = 2022
way4_data[[6]] = 2023
way4_data[[7]] = 2024


####year 

EProcway42023 <- sEddyProc$new('way4-Data2023', way4_data_reddyproc[[7]], c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EProcway42023$sSetLocationInfo(LatDeg = LatDeg, LongDeg = LongDeg, TimeZoneHour = -6)  # Set location and timezone

seasonStartsway42024 <- as.data.frame(do.call(rbind, list(
  c(way42024PD_DOY, way42024HD_DOY)
)))
nrow(way4_data_reddyproc[[7]])
nrow(seasonStartsway42024)
seasonFactorway42023 <- usCreateSeasonFactorYdayYear(
  way4_data_reddyproc[[7]]$DateTime - 15*60, starts = seasonStartsway42024)
EProcway42023$sEstimateUstarScenarios(seasonFactor = seasonFactorway42023, nSample = 30L, probs = c(0.1, 0.9))
EProcway42023$sMDSGapFillUStarScens("NEE", FillAll = TRUE)

EProcway42023$sPlotFingerprintY('NEE_uStar_orig', Year = 2023)
EProcway42023$sPlotFingerprint('NEE_U90_f', Dir = "plotsHandsOn")

EProcway42023$sMDSGapFill('Rg', FillAll = FALSE)
EProcway42023$sMDSGapFill('Tair', FillAll = FALSE)
EProcway42023$sMDSGapFill('VPD', FillAll = FALSE)

EProcway42023$sPlotFingerprintY('Rg_f', Year = 2023)
EProcway42023$sPlotFingerprintY('Tair_f', Year = 2023)
EProcway42023$sPlotFingerprintY('VPD_f', Year = 2023)

EProcway42023$sMRFluxPartitionUStarScens()
dsResultsway42023 <- EProcway42023$sExportResults()
View(dsResults)
EProcway42023$sGLFluxPartitionUStarScens()

# Bonus Task: Repeat with fixed Temperature Sensitivity to 80 $\pm$40 K
EProcway42023$sGLFluxPartitionUStarScens(
  controlGLPart = partGLControl(
    fixedTempSens = data.frame(E0 = 80, sdE0 = 40, RRef = NA_real_))
  , isWarnReplaceColumns = FALSE
)

EProcway42023$sPlotFingerprintY("GPP_DT_uStar", Year = 2023)
EProcway42023$sPlotFingerprintY("Reco_DT_uStar", Year = 2023)

dsResultsway42023 <- EProcway42023$sExportResults()
EProcway42023$sGLFluxPartitionUStarScens()
colnames(dsResultsway42023)


