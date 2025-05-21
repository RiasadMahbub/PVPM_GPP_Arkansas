# =============================================================================
# LOAD REQUIRED LIBRARIES
# =============================================================================
library(readxl)
library(dplyr)
library(lubridate)
library(formattable)

# =============================================================================
# GLOBAL METADATA DEFINITIONS
# =============================================================================
Variety_values <- c(
  "USBDA2015" = "XL745", "USBDA2016" = "XL745", "USBDC2015" = "XL745", 
  "USBDC2016" = "XL745", "USHRA2015" = "XL745", "USHRA2016" = "XL745", 
  "USHRA2017" = "XL745", "USHRC2015" = "XL745", "USHRC2016" = "XL745", 
  "USHRC2017" = "XL745", "USOF12017" = "XL753", "USOF22017" = "XL753", 
  "USOF32017" = "XL753", "USOF42018" = "XL753", "USOF52018" = "XL753", 
  "USOF62018" = "XL753"
)

DOP_values <- c(
  "USBDA2015" = 92, "USBDA2016" = 82, "USBDC2015" = 92, "USBDC2016" = 82,
  "USHRA2015" = 97, "USHRA2016" = 114, "USHRA2017" = 99, "USHRC2015" = 98,
  "USHRC2016" = 114, "USHRC2017" = 100, "USOF12017" = 91, "USOF22017" = 91,
  "USOF32017" = 91, "USOF42018" = 99, "USOF52018" = 99, "USOF62018" = 99
)

# =============================================================================
# PROCESS SINGLE SHEET FUNCTION
# =============================================================================
process_sheet <- function(file_path, sheet_num, site_label) {
  if (!site_label %in% names(DOP_values)) {
    stop(paste("DOP not defined for site:", site_label))
  }
  DOP_doy <- DOP_values[[site_label]]
  
  # Read column names and then data
  myCols <- as.character(readxl::read_excel(file_path, n_max = 1, col_names = FALSE, sheet = sheet_num))
  df <- readxl::read_excel(file_path, skip = 2, sheet = sheet_num, col_names = myCols)
  
  # Clean and parse datetime
  df$`Date Time` <- gsub("'", "", df$`Date Time`)
  df$datetime <- as.POSIXct(df$`Date Time`, format = '%d-%b-%Y %H:%M:%S', tz = "UTC")
  df$Date <- as.Date(df$datetime)
  df$DOY <- yday(df$datetime)
  
  # Filter data to keep rows where DOY >= DOP (Day of Planting)
  df <- dplyr::filter(df, DOY >= DOP_doy)
  
  # Aggregate daily means of selected variables
  daily_df <- aggregate(cbind(GPP_modeled, PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, df, mean, na.rm = TRUE)
  
  # Unit conversions
  daily_df <- daily_df %>%
    mutate(
      GPP_modeled = GPP_modeled * 1e-6 * 86400 * 12.011,  # umol m-2 s-1 to gC m-2 day-1
      PAR_Regression = PAR_Regression * 1e-6 * 86400      # umol m-2 s-1 to mol m-2 day-1
    )
  
  # Calculate DAP (Days After Planting) for aggregated data
  daily_df <- daily_df %>%
    mutate(
      DAP = yday(Date) - DOP_doy
    )
  
  # Add metadata and format numeric columns for output
  daily_df <- daily_df %>%
    mutate(
      GPP_site = formattable::formattable(GPP_modeled, digits = 2, format = "f"),
      PAR_site = formattable::formattable(PAR_Regression, digits = 2, format = "f"),
      VPD_site = formattable::formattable(VPD_REddyProc, digits = 2, format = "f"),
      Tair_site = formattable::formattable(Ta_REddyProc, digits = 2, format = "f"),
      rH_site = formattable::formattable(rH_REddyProc, digits = 2, format = "f"),
      doy = yday(Date),
      siteyear = site_label,
      site = substr(site_label, 1, 5)
    ) %>%
    select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site, doy, siteyear, site, DAP)
  
  return(daily_df)
}

# =============================================================================
# ADD SITE METADATA TO COMBINED DATA
# =============================================================================
add_site_metadata <- function(df) {
  df %>%
    mutate(
      DOY = yday(Date),
      Variety = Variety_values[siteyear],
      DOP = DOP_values[siteyear],
      DAP = DOY - DOP,
      siteyeardate = paste0(site, "_", Date)
    )
}

# =============================================================================
# PROCESS ALL SITES
# =============================================================================
file_path <- "C:/Users/rbmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx"

site_list <- list(
  USBDA2015 = 14, USBDA2016 = 15, USBDC2015 = 16, USBDC2016 = 17,
  USHRA2015 = 5,  USHRA2016 = 6,  USHRA2017 = 7,
  USHRC2015 = 2,  USHRC2016 = 3,  USHRC2017 = 4,
  USOF12017 = 8,  USOF22017 = 9,  USOF32017 = 10,
  USOF42018 = 11, USOF52018 = 12, USOF62018 = 13
)

# Process all sheets using lapply and combine
site_dfs <- lapply(names(site_list), function(site) {
  process_sheet(file_path, site_list[[site]], site)
})

sitecombineddata <- bind_rows(site_dfs)

# Add extra metadata columns to the combined data
sitecombineddata <- add_site_metadata(sitecombineddata)

# =============================================================================
# FINAL CHECK
# =============================================================================
str(sitecombineddata)
head(sitecombineddata)
View(sitecombineddata)
