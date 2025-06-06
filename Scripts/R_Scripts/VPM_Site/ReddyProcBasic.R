# Title: Gebesee handson
# Author: Thomas Wutzler
# Date: Sys.Date()
# Output: html_notebook

# uStar Threshold estimation

# Preparing the data
# First, the data is loaded. This example uses data that has been downloaded 
# from http://www.europe-fluxdata.eu
# and preprocessed by `fLoadEuroFlux16`, where the DateTime Column has been created, 
# and the variables renamed to the BGC-convention (e.g. Tair instead of Ta).

library(REddyProc)
data(DEGebExample)
summary(DEGebExample)

# VPD was not given with the original dataset and is calculated from Tair and rH.
DEGebExample$VPD <- fCalcVPDfromRHandTair(DEGebExample$rH, DEGebExample$Tair)

# Task 1a: Create the REddyProc class
# named `EProc` with columns c('NEE','Rg','Tair','VPD', 'Ustar')
# at Location LatDeg = 51.1, LongDeg = 10.9, 
# and timezone one hour ahead of GMT (TimeZoneHour = 1)

EProc <- sEddyProc$new('DE-Geb', DEGebExample, c('NEE','Rg','Tair','VPD', 'Ustar'))
EProc$sSetLocationInfo(LatDeg = 51.1, LongDeg = 10.9, TimeZoneHour = 1)  # Location of Gebesee

# Defining Seasons with different surface friction conditions 
# The site is a crop site. The harvesting times are visible as sharp edges in the plots of NEE.
# The micrometeorological conditions differ between the different cropping periods,
# because the friction at the surface differs.
# Also, the cropping periods do not correspond very well to seasons.
# Hence, for the estimation of uStar-Thresholds, we apply a user-defined splitting 
# of uStar-seasons. With function `usCreateSeasonFactorYdayYear` we provide the starting
# points of the seasons.

plot(NEE ~ DateTime, DEGebExample)

# Task 1b: Define season
# Based on the NEE plot, define seasons with
# starting at day 70, 210, 320 in 2004 and 70,180,320 in 2005 and 120,305 in 2006
# Optional: Add horizontal lines to the NEE~DateTime plot.

seasonStarts <- as.data.frame(do.call(rbind, list(
  c(70, 2004),
  c(210, 2004),
  c(320, 2004),
  c(70, 2005),
  c(180, 2005),
  c(320, 2005),
  c(120, 2006),
  c(305, 2006)
)))
seasonFactor <- usCreateSeasonFactorYdayYear(
  DEGebExample$DateTime - 15*60, starts = seasonStarts)

# Optional
seasonStartsDate <- fConvertTimeToPosix(data.frame(Year = seasonStarts[, 2],
                                                   DoY = seasonStarts[, 1], Hour = 0.25), 'YDH',
                                        Year = "Year", Day = "DoY", Hour = "Hour")
plot(NEE ~ DateTime, DEGebExample)
abline(v = seasonStartsDate$DateTime)

# Task 1c: Estimate the distribution of u* thresholds
# Specify the seasons defined before.
# For saving time, here use only 30 bootstrap samples (argument nSample) and 
# estimate only the 10th and 90th percentile (argument probs)
# Print the estimated thresholds.

EProc$sEstimateUstarScenarios(seasonFactor = seasonFactor, nSample = 30L, probs = c(0.1, 0.9))
EProc$sGetEstimatedUstarThresholdDistribution()

# Gapfilling

# Task 2a: Use seasonal u* thresholds
# - display the default annually used thresholds
# - specify using seasonal thresholds
# - display the scenarios again

EProc$sGetUstarScenarios()
EProc$useSeaonsalUStarThresholds()
EProc$sGetUstarScenarios()

# Task 2b: Perform gapfilling for NEE
# Estimate random error also for non-gap records.

EProc$sMDSGapFillUStarScens("NEE", FillAll = TRUE)

# What columns have been created?
colnames(EProc$sExportResults())

# For an explanation of the column names see [output format description](https://www.bgc-jena.mpg.de/bgi/index.php/Services/REddyProcWebOutput)
# - NEE_<scenario>_f: gaps replaced by modeled values (gapfilled)
# - NEE_<scenario>_fall: all NEE replaced by modeled values
# - NEE_<scenario>_fqc: quality flag: 0 observations, 1 good quality of gapfilling 

# Task 2c: Produce a fingerprint plot of gapfilled values
# First a single plot in document
# - for 90% quantile $u_{*Th}$ (suffix U90)
# - for year 2005

EProc$sPlotFingerprintY('NEE_U90_f', Year = 2005)

# Next, produce pdf-files with legend for all years in subdirectory "plotsHandsOn"
EProc$sPlotFingerprint('NEE_U90_f', Dir = "plotsHandsOn")

# Flux partitioning

# Task 3a: Prepare for partitioning
# Specify the Location and time zone (51.1N, 10.0W, 1hour ahead of GMT)
# Gapfill the necessary meteorological input variables (Rg, Tair, VPD). Here
# we do not need computing the uncertainty of the non-filled records.

EProc$sSetLocationInfo(LatDeg = 51.1, LongDeg = 10.9, TimeZoneHour = 1)  # Location of Gebesee
EProc$sMDSGapFill('Rg', FillAll = FALSE)
EProc$sMDSGapFill('Tair', FillAll = FALSE)
EProc$sMDSGapFill('VPD', FillAll = FALSE)

# Task 3b: Nighttime partitioning
# Perform the nighttime partitioning

EProc$sMRFluxPartitionUStarScens()

# Task 3c: Plotting the head GPP
# Query the result data.frame to variable dsResults

dsResults <- EProc$sExportResults()

# Plot the head of GPP for U90 scenario against time (DEGebExample$DateTime)
nRec <- 48 * 4  # 4 days of half-hours
plot(head(dsResults$GPP_U90_f, nRec) ~ head(DEGebExample$DateTime, nRec), type = "l")

# Task 3d: Daytime partitioning
# Perform the daytime partitioning.

EProc$sGLFluxPartitionUStarScens()

# Bonus Task: Repeat with fixed Temperature Sensitivity to 80 $\pm$40 K
EProc$sGLFluxPartitionUStarScens(
  controlGLPart = partGLControl(
    fixedTempSens = data.frame(E0 = 80, sdE0 = 40, RRef = NA_real_))
  , isWarnReplaceColumns = FALSE
)

# Task 3e: Produce fingerprint plots of GPP_DT and Reco_DT 
# - For the original non-bootstrapped data uStar scenario (suffix uStar)
# - In Sub-Directory `plotsHandsOn`

EProc$sPlotFingerprint("GPP_DT_uStar", Dir = "plotsHandsOn")
EProc$sPlotFingerprint("Reco_DT_uStar", Dir = "plotsHandsOn")

# Task 3f: Save the results together with the data
# as tab-delimited text file "plotsHandsOn/DEGeb_Part.txt"
# Store the combined data and results in variable 'results'

dsRes <- EProc$sExportResults()
dsData <- EProc$sExportData()
results <- cbind(dsData, dsRes)
fWriteDataframeToFile(results, "DE-Geb_Part.txt", Dir = "plotsHandsOn")

# Bias with $u_{*Th}$
# Focus on year 2005

results$year <- as.POSIXlt(results$DateTime)$year + 1900
res05 <- subset(results, year == 2005)

# Task 4a: compute the annual mean NEE for each $u_{*Th}$ scenario
# For the mean we can use gap-filled series in column `NEE_<suffix>_f`

scens <- c("uStar", "U10", "U90")
NEE_UStar <- sapply(scens, function(suffix) {
  colName <- paste0("NEE_", suffix, "_f")
  mean(res05[[colName]])
})
NEE_UStar

# Task 4b: Compute statistic across the obtained aggregates
# median, standard deviation and relative error

c(mean(NEE_UStar), sd(NEE_UStar), sd(NEE_UStar) / abs(mean(NEE_UStar)))

# Random uncertainty aggregation

# Task 5a: approximate the error terms in results 
# For each good measurement, i.e. where `NEE_uStar_fqc == 0` we have
# measured NEE that includes random fluctuation `NEE_uStar_orig` 
# and gap-filling estimate `NEE_uStar_fall`
# of the expected flux for the conditions at this time.
# Create a new column `resid` that stores the difference for good conditions
# and NA for other records.

n <- sum(results$NEE_uStar_fqc == 0)  # number of good records
results$resid <- ifelse(
  results$NEE_uStar_fqc == 0, results$NEE_uStar_orig - results$NEE_uStar_fall, NA)

# Task 5b: compute the empirical autocorrelation function

library(lognorm)
rho <- computeEffectiveAutoCorr(results$resid)
plot(rho[-1])

# Task 5c: Compute the effective number of effective observation
# To variable `nEff`
# and compare to the number of good observations `n`.

nEff <- computeEffectiveNumObs(results$resid, na.rm = TRUE)
c(nEff, n)

# Task 5d: Compute its effective number of observations for year 2005
# Use the autocorrelation function that you determined on the 
# entire dataset before (`rho`)

nEff <- computeEffectiveNumObs(res05$resid, na.rm = TRUE, effAcf = rho)

# Task 5e: Compute the mean annual NEE and its standard deviation for 2005
# For scenario $u_{*Th}$ threshold scenario `uStar`
# Report also the relative error, i.e. coefficient of variation.
# Consider gap-filled values in the mean. 
# But make sure to not use gap-filled records in uncertainty estimation.
# Remember that the root-mean-squared averaged standard deviation decreases 
# by factor $\sqrt{nEff -1}$.

sdGood <- res05$NEE_uStar_fsd[res05$NEE_uStar_fqc == 0]
NEE <- mean(res05$NEE_uStar_f)
sdNEE <- sqrt(mean(sdGood^2)) / sqrt(nEff - 1)
c(mean = NEE, sd = sdNEE, cv = sdNEE / abs(NEE))

# Combined uncertainty
# Task 6a: Compute the combined uncertainty of random and $u_{*Th}$
# Remember that standard deviation of independent variables adds in squares.

sdNEEUStar <- sd(NEE_UStar)
sdNEECombined <- sqrt(sdNEEUStar^2 + sdNEE^2)
sdNEECombined