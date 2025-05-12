library(REddyProc)
library(bigleaf)
library(readr)
library(lubridate)
library(dplyr)
# Description of Key Variables in the Dataset for reddyproc
# Year: The year of the measurement.
# DoY: Day of the Year (usually ranging from 1 to 365 or 366, depending on the year).
# Hour: The hour of the measurement (typically in 24-hour format).
# NEE: Net Ecosystem Exchange (the flux measurement of carbon dioxide in micromoles of CO2 per square meter per second).
# Rg: Global radiation or incoming solar radiation (in W/m²).
# Tair: Air temperature (in °C).
# rH: Relative humidity (in %).
# Ustar: Friction velocity (a measure of turbulence in the atmosphere, typically in m/s).

LatDeg = 34.585
LongDeg = -91.751
TimeZoneHour = -6

LatDeg = 34.588
LonDeg = -91.751
##########
###LOAD the Data
# Define the correct directory for Way3 data
way3_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/AmerifluxRawdata/Way3"
way4_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/AmerifluxRawdata/Way4"

# List all files in the directory
way3_files <- list.files(way3_dir, full.names = TRUE)
way4_files <- list.files(way4_dir, full.names = TRUE)
# Read each file and store it in a list
way3_data <- lapply(way3_files, read_csv)
way4_data <- lapply(way4_files, read_csv)

# Replace -9999 with NaN in all data frames within way3_data
way3_data <- lapply(way3_data, function(df) {
  df[df == -9999] <- NaN
  return(df)
})

# Replace -9999 with NaN in all data frames within way4_data
way4_data <- lapply(way4_data, function(df) {
  df[df == -9999] <- NaN
  return(df)
})

# Convert the TIMESTAMP_START column to a Date-Time format in way3_data and way4_data
way3_data <- lapply(way3_data, function(df) {
  # Ensure TIMESTAMP_START is character
  df$TIMESTAMP_START <- as.character(df$TIMESTAMP_START)
  # Convert the TIMESTAMP_START column to a Date-Time format
  df$TIMESTAMP_START <- as.POSIXct(df$TIMESTAMP_START, format = "%Y%m%d%H%M")
  return(df)
})

way4_data <- lapply(way4_data, function(df) {
  # Ensure TIMESTAMP_START is character
  df$TIMESTAMP_START <- as.character(df$TIMESTAMP_START)
  # Convert the TIMESTAMP_START column to a Date-Time format
  df$TIMESTAMP_START <- as.POSIXct(df$TIMESTAMP_START, format = "%Y%m%d%H%M")
  return(df)
})


# Deriving Year, DoY, and Hour for way3_data
way3_data <- lapply(way3_data, function(df) {
  # Derive Year, DoY (Day of Year), and Hour using lubridate functions
  df$Year <- year(df$TIMESTAMP_START)
  df$DoY <- yday(df$TIMESTAMP_START)
  df$Hour <- hour(df$TIMESTAMP_START)
  return(df)
})

# Deriving Year, DoY, and Hour for way4_data
way4_data <- lapply(way4_data, function(df) {
  # Derive Year, DoY (Day of Year), and Hour using lubridate functions
  df$Year <- year(df$TIMESTAMP_START)
  df$DoY <- yday(df$TIMESTAMP_START)
  df$Hour <- hour(df$TIMESTAMP_START)
  return(df)
})

###################################################
#############Planting and harvesting date#########
##################################################
# 2015
way32015PD <- as.Date("2015-04-08") 
way42015PD <- as.Date("2015-04-07")
way32015HD <- as.Date("2015-08-19") 
way42015HD <- as.Date("2015-08-19")
# 2016
way32016PD <- as.Date("2016-04-23") 
way42016PD <- as.Date("2016-04-23")
way32016HD <- as.Date("2016-09-13") 
way42016HD <- as.Date("2016-09-13")
# 2017
way32017PD <- as.Date("2017-04-10")
way42017PD <- as.Date("2017-04-09")
way32017HD <- as.Date("2017-08-27")
way42017HD <- as.Date("2017-08-27")
# 2018
way32018PD <- as.Date("2018-04-30")
way42018PD <- as.Date("2018-04-30")
way32018HD <- as.Date("2018-09-15")
way42018HD <- as.Date("2018-08-31")
# 2019
way32019PD <- as.Date("2019-05-13")
way42019PD <- as.Date("2019-05-13")
way32019HD <- as.Date("2019-09-12")
way42019HD <- as.Date("2019-09-12")
# 2020
way32020PD <- as.Date("2020-04-02")
way42020PD <- as.Date("2020-04-02")
way32020HD <- as.Date("2020-08-19")
way42020HD <- as.Date("2020-08-18")
# 2021
way32021PD <- as.Date("2021-05-01")
way42021PD <- as.Date("2021-05-01") 
way32021HD <- as.Date("2021-09-28")
way42021HD <- as.Date("2021-09-27")

# 2022
way32022PD <- as.Date("2022-04-29")
way42022PD <- as.Date("2022-04-28")
way32022HD <- as.Date("2022-09-05")
way42022HD <- as.Date("2022-09-05")

# 2023
way32023PD <- as.Date("2023-04-14")
way42023PD <- as.Date("2023-04-12")
way32023HD <- as.Date("2023-08-26")
way42023HD <- as.Date("2023-08-23")

# 2024
way32024PD <- as.Date("2024-04-03")
way42024PD <- as.Date("2024-04-03")
way32024HD <- as.Date("2024-08-16") ####edited values ## mail from Bea
way42024HD <- as.Date("2024-08-16") ####edited values ## mail from Bea

# Function to calculate DOY
calculate_doy <- function(date) {
  yday(date)
}

# 2015
way32015PD_DOY <- calculate_doy(as.Date("2015-04-08"))
way42015PD_DOY <- calculate_doy(as.Date("2015-04-07"))
way32015HD_DOY <- calculate_doy(as.Date("2015-08-19"))
way42015HD_DOY <- calculate_doy(as.Date("2015-08-19"))

# 2016
way32016PD_DOY <- calculate_doy(as.Date("2016-04-23"))
way42016PD_DOY <- calculate_doy(as.Date("2016-04-23"))
way32016HD_DOY <- calculate_doy(as.Date("2016-09-13"))
way42016HD_DOY <- calculate_doy(as.Date("2016-09-13"))

# 2017
way32017PD_DOY <- calculate_doy(as.Date("2017-04-10"))
way42017PD_DOY <- calculate_doy(as.Date("2017-04-09"))
way32017HD_DOY <- calculate_doy(as.Date("2017-08-27"))
way42017HD_DOY <- calculate_doy(as.Date("2017-08-27"))

# 2018
way32018PD_DOY <- calculate_doy(as.Date("2018-04-30"))
way42018PD_DOY <- calculate_doy(as.Date("2018-04-30"))
way32018HD_DOY <- calculate_doy(as.Date("2018-09-15"))
way42018HD_DOY <- calculate_doy(as.Date("2018-08-31"))

# 2019
way32019PD_DOY <- calculate_doy(as.Date("2019-05-13"))
way42019PD_DOY <- calculate_doy(as.Date("2019-05-13"))
way32019HD_DOY <- calculate_doy(as.Date("2019-09-12"))
way42019HD_DOY <- calculate_doy(as.Date("2019-09-12"))

# 2020
way32020PD_DOY <- calculate_doy(as.Date("2020-04-02"))
way42020PD_DOY <- calculate_doy(as.Date("2020-04-02"))
way32020HD_DOY <- calculate_doy(as.Date("2020-08-19"))
way42020HD_DOY <- calculate_doy(as.Date("2020-08-18"))

# 2021
way32021PD_DOY <- calculate_doy(as.Date("2021-05-01"))
way42021PD_DOY <- calculate_doy(as.Date("2021-05-01"))
way32021HD_DOY <- calculate_doy(as.Date("2021-09-28"))
way42021HD_DOY <- calculate_doy(as.Date("2021-09-27"))

# 2022
way32022PD_DOY <- calculate_doy(as.Date("2022-04-29"))
way42022PD_DOY <- calculate_doy(as.Date("2022-04-28"))
way32022HD_DOY <- calculate_doy(as.Date("2022-09-05"))
way42022HD_DOY <- calculate_doy(as.Date("2022-09-05"))

# 2023
way32023PD_DOY <- calculate_doy(as.Date("2023-04-14"))
way42023PD_DOY <- calculate_doy(as.Date("2023-04-12"))
way32023HD_DOY <- calculate_doy(as.Date("2023-08-26"))
way42023HD_DOY <- calculate_doy(as.Date("2023-08-23"))

# 2024
way32024PD_DOY <- calculate_doy(as.Date("2024-04-03"))
way42024PD_DOY <- calculate_doy(as.Date("2024-04-03"))
way32024HD_DOY <- calculate_doy(as.Date("2024-08-13"))
way42024HD_DOY <- calculate_doy(as.Date("2024-08-13"))

# Should now print years like "2018", "2019", etc.
# List of start (PD) and end (HD) dates for each year
pd_dates_way3 <- list(way32018PD, way32019PD, way32020PD, way32021PD, way32022PD, way32023PD, way32024PD)
hd_dates_way3 <- list(way32018HD, way32019HD, way32020HD, way32021HD, way32022HD, way32023HD, way32024HD)

pd_dates_way4 <- list(way42018PD, way42019PD, way42020PD, way42021PD, way42022PD, way42023PD, way42024PD)
hd_dates_way4 <- list(way42018HD, way42019HD, way42020HD, way42021HD, way42022HD, way42023HD, way42024HD)

# Function to filter data
filter_by_dates <- function(data_list, pd_dates, hd_dates) {
  lapply(seq_along(data_list), function(i) {
    subset(data_list[[i]], as.Date(TIMESTAMP_START) >= pd_dates[[i]] & as.Date(TIMESTAMP_START) <= hd_dates[[i]])
  })
}

# Apply filtering
way3_data <- filter_by_dates(way3_data, pd_dates_way3, hd_dates_way3)
way4_data <- filter_by_dates(way4_data, pd_dates_way4, hd_dates_way4)


#######################################
#######ReddyProc Based dataframes######
#######################################
# Create new list for way3_data_reddyproc by selecting specific columns
select_and_rename_columns <- function(df) {
  # Select the columns of interest
  df <- df %>%
    select(Year, DoY, FC, SW_IN, TA, RH, USTAR, VPD, TIMESTAMP_START)  # Selecting NEE, Rg, Tair, rH, Ustar
  # Rename the specific columns
  df <- df %>%
    rename(
      NEE = FC,           # Rename FC to NEE
      Rg = SW_IN,         # Rename SW_IN to Rg
      Tair = TA,          # Rename TA to Tair
      rH = RH,            # Rename RH to rH
      Ustar = USTAR,       # Rename USTAR to Ustar
      VPD = VPD,
      DateTime= TIMESTAMP_START 
    )
  
  return(df)
}

# Apply the function to the way3_data list
way3_data_reddyproc <- lapply(way3_data, select_and_rename_columns)

# Apply the function to the way4_data list
way4_data_reddyproc <- lapply(way4_data, select_and_rename_columns)


####################################################
##############Make equidistant Timestamp ###########
####################################################
# Function to process each dataset and fill missing half-hourly timestamps with NaN
process_data <- function(data) {
  # Convert DateTime to POSIXct if not already
  data$DateTime <- as.POSIXct(data$DateTime, format = "%Y-%m-%d %H:%M:%S")
  # Create a sequence of half-hourly timestamps from the min to the max of the DateTime column
  timestamp_seq <- seq(from = min(data$DateTime), to = max(data$DateTime), by = "30 min")
  # Create a new data frame with the full half-hourly sequence
  full_data <- data.frame(DateTime = timestamp_seq)
  # Merge the full timestamp sequence with the existing data, keeping all timestamps
  full_data <- merge(full_data, data, by = "DateTime", all.x = TRUE)
  # Replace NA values with NaN
  full_data[is.na(full_data)] <- NaN
  return(full_data)
}

# Apply the function to all elements in way4_data_reddyproc from [[1]] to [[7]]
way4_data_reddyproc <- lapply(way4_data_reddyproc, process_data)
# Apply the function to all elements in way3_data_reddyproc from [[1]] to [[7]]
way3_data_reddyproc <- lapply(way3_data_reddyproc, process_data)

nonnegative_removal_vpd_rg <- function(df) {
  df$Rg[df$Rg < 0] <- NaN
  df$VPD[df$VPD < 0] <- NaN
  return(df)
}

# Apply the function to all elements in way4_data_reddyproc from [[1]] to [[7]]
way4_data_reddyproc <- lapply(way4_data_reddyproc[1:7], nonnegative_removal_vpd_rg)

# Apply the function to all elements in way3_data_reddyproc from [[1]] to [[7]]
way3_data_reddyproc <- lapply(way3_data_reddyproc[1:7], nonnegative_removal_vpd_rg)


#######################################################
#######################################################
#######################################################
# Define function to plot and save NEE vs. DateTime with year in the filename and figure caption
plot_and_save_NEE <- function(data_list, label, output_dir, year_labels, color) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)  # Create directory if it doesn't exist
  for (i in seq_along(data_list)) {
    if (!is.null(data_list[[i]])) {
      year <- year_labels[i]  # Get the corresponding year label
      p <- ggplot(data_list[[i]], aes(x = DateTime, y = NEE)) +
        geom_point(color = color, alpha = 0.5) +  # Scatter plot with dynamic color
        labs(title = paste(label, "Year:", year), 
             caption = paste("Year:", year),  # Adding year as a caption
             x = "DateTime", y = "NEE") +
        theme_minimal()
      # Define filename with year
      filename <- file.path(output_dir, paste0(label, "_", year, ".png"))
      # Save the plot
      ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
    }
  }
}

# Define output directory
output_directory <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/SeasonFactor"

# Define the year labels for Way3 and Way4
way3_years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024)
way4_years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024)

# Plot and save for Way3 and Way4 data with different colors
plot_and_save_NEE(way3_data_reddyproc, "Way3", output_directory, way3_years, "blue")
plot_and_save_NEE(way4_data_reddyproc, "Way4", output_directory, way4_years, "red")




# Function: create_sEddyProc_objects
# Task 1a: Create the REddyProc class named `EProc`
# with columns c('NEE','Rg','Tair','VPD', 'Ustar')
# at Location LatDeg = 34.585, LongDeg = -91.751,
# and timezone six hours behind GMT (TimeZoneHour = -6)
create_sEddyProc_objects <- function(data_list, site_name) {
  LatDeg <- 34.585
  LongDeg <- -91.751
  TimeZoneHour <- -6
  
  results <- lapply(seq_along(data_list[1:7]), function(i) {
    EProc <- sEddyProc$new(paste0(site_name, "-Data", 2018 + i - 1), data_list[[i]], 
                           c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
    EProc$sSetLocationInfo(LatDeg = LatDeg, LongDeg = LongDeg, TimeZoneHour = TimeZoneHour)
    return(EProc)
  })
  return(results)
}

# Apply function to Way3 and Way4 datasets
EProcWay3 <- create_sEddyProc_objects(way3_data_reddyproc, "Way3")
EProcWay4 <- create_sEddyProc_objects(way4_data_reddyproc, "Way4")

# Function to create season starts for each year
create_seasonStarts <- function(PD_DOY, HD_DOY) {
  return(as.data.frame(do.call(rbind, list(
    c(PD_DOY, HD_DOY)
  ))))
}
# Create seasonStarts for Way3 (2018-2024)
seasonStartsWay3 <- list(
  create_seasonStarts(way32018PD_DOY, way32018HD_DOY),
  create_seasonStarts(way32019PD_DOY, way32019HD_DOY),
  create_seasonStarts(way32020PD_DOY, way32020HD_DOY),
  create_seasonStarts(way32021PD_DOY, way32021HD_DOY),
  create_seasonStarts(way32022PD_DOY, way32022HD_DOY),
  create_seasonStarts(way32023PD_DOY, way32023HD_DOY),
  create_seasonStarts(way32024PD_DOY, way32024HD_DOY)
)

# Create seasonStarts for Way4 (2018-2024)
seasonStartsWay4 <- list(
  create_seasonStarts(way42018PD_DOY, way42018HD_DOY),
  create_seasonStarts(way42019PD_DOY, way42019HD_DOY),
  create_seasonStarts(way42020PD_DOY, way42020HD_DOY),
  create_seasonStarts(way42021PD_DOY, way42021HD_DOY),
  create_seasonStarts(way42022PD_DOY, way42022HD_DOY),
  create_seasonStarts(way42023PD_DOY, way42023HD_DOY),
  create_seasonStarts(way42024PD_DOY, way42024HD_DOY)
)

# Create a list to store seasonFactor variables for Way3
seasonFactorListway3 <- list()
# Create a list to store seasonFactor variables for Way4
seasonFactorListway4 <- list()
# Loop through the years 2018-2024 (7 years)
for (i in 1:7) {
  # For Way3
  seasonFactorListway3[[paste0("seasonFactorWay3_", 2017 + i)]] <- usCreateSeasonFactorYdayYear(
    way3_data_reddyproc[[i]]$DateTime - 15 * 60, 
    starts = seasonStartsWay3[[i]]
  )
  
  # For Way4
  seasonFactorListway4[[paste0("seasonFactorWay4_", 2017 + i)]] <- usCreateSeasonFactorYdayYear(
    way4_data_reddyproc[[i]]$DateTime - 15 * 60, 
    starts = seasonStartsWay4[[i]]
  )
}

# Loop through the 7 elements of EProcWay3 and apply sEstimateUstarScenarios
for (i in 1:7) {
  EProcWay3[[i]]$sEstimateUstarScenarios(
    seasonFactor = seasonFactorListway3[[i]], 
    nSample = 30L, 
    probs = c(0.1, 0.9)
  )
}

# Loop through the 7 elements of EProcWay4 and apply sEstimateUstarScenarios
for (i in 1:7) {
  EProcWay4[[i]]$sEstimateUstarScenarios(
    seasonFactor = seasonFactorListway4[[i]], 
    nSample = 30L, 
    probs = c(0.1, 0.9)
  )
}

EProcWay4[[6]]$sEstimateUstarScenarios(
  seasonFactor = seasonFactorListway4[[6]], 
  nSample = 30L, 
  probs = c(0.1, 0.9)
)
EProcWay4[[6]]
###################################################
################one field ############################
###################################################
# Create seasonStarts data frame for 2024
EProcWay32024 <- sEddyProc$new('Way3-Data2024', way3_data_reddyproc[[7]], c('NEE', 'Rg', 'Tair', 'VPD', 'Ustar'))
EProcWay32024$sSetLocationInfo(LatDeg = 51.1, LongDeg = 10.9, TimeZoneHour = 1)  # Set location and timezone

seasonStartsway32024 <- as.data.frame(do.call(rbind, list(
  c(way32024PD_DOY, way32024HD_DOY)
)))

seasonFactorway32024 <- usCreateSeasonFactorYdayYear(
  way3_data_reddyproc[[7]]$DateTime - 15*60, starts = seasonStartsway32024)
EProcWay32024$sEstimateUstarScenarios(seasonFactor = seasonFactorway32024, nSample = 30L, probs = c(0.1, 0.9))
EProcWay32024$sMDSGapFillUStarScens("NEE", FillAll = TRUE)

EProcWay32024$sPlotFingerprintY('NEE_uStar_orig', Year = 2024)
EProcWay32024$sPlotFingerprint('NEE_U90_f', Dir = "plotsHandsOn")

EProcWay32024$sMDSGapFill('Rg', FillAll = FALSE)
EProcWay32024$sMDSGapFill('Tair', FillAll = FALSE)
EProcWay32024$sMDSGapFill('VPD', FillAll = FALSE)

EProcWay32024$sPlotFingerprintY('Rg_f', Year = 2024)
EProcWay32024$sPlotFingerprintY('Tair_f', Year = 2024)
EProcWay32024$sPlotFingerprintY('VPD_f', Year = 2024)

EProcWay32024$sMRFluxPartitionUStarScens()
dsResultsWay32024 <- EProcWay32024$sExportResults()
View(dsResults)
EProcWay32024$sGLFluxPartitionUStarScens()

# Bonus Task: Repeat with fixed Temperature Sensitivity to 80 $\pm$40 K
EProcWay32024$sGLFluxPartitionUStarScens(
  controlGLPart = partGLControl(
    fixedTempSens = data.frame(E0 = 80, sdE0 = 40, RRef = NA_real_))
  , isWarnReplaceColumns = FALSE
)

EProcWay32024$sPlotFingerprintY("GPP_DT_uStar", Year = 2024)
EProcWay32024$sPlotFingerprintY("Reco_DT_uStar", Year = 2024)

dsResultsWay32024 <- EProcWay32024$sExportResults()
EProcWay32024$sGLFluxPartitionUStarScens()
colnames(dsResultsWay32024)





##########################################
#########################################
#########################################

