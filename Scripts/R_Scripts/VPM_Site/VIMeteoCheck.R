# =============================================================================
# LOAD REQUIRED LIBRARIES
# =============================================================================
library(dplyr)
library(zoo)        # For na.approx (linear interpolation)
library(tidyr)
library(ggplot2)
library(signal)     # For sgolayfilt()
library(tidyverse)
library(lubridate)

# Define DOP values
DOP_values <- c(
  "USBDA_2015" = 92, "USBDA_2016" = 82, "USBDC_2015" = 92, "USBDC_2016" = 82,
  "USHRA_2015" = 97, "USHRA_2016" = 114, "USHRA_2017" = 99, "USHRC_2015" = 98,
  "USHRC_2016" = 114, "USHRC_2017" = 100, "USOF1_2017" = 91, "USOF2_2017" = 91,
  "USOF3_2017" = 91, "USOF4_2018" = 99, "USOF5_2018" = 99, "USOF6_2018" = 99
)

# =============================================================================
# DEFINE CORE FUNCTIONS
# =============================================================================

# Convert Unix timestamp (milliseconds) to Date format
convert_unix_time <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.POSIXct(df$Date / 1000, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}

# Convert Unix timestamp (seconds) to Date format for meteo data
convert_unix_time_meteo <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.POSIXct(df$Date, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}

# Sort dataframe by Date column
sort_by_date <- function(df) {
  if ("Date" %in% colnames(df)) {
    df <- df[order(df$Date), ]
  }
  return(df)
}

# Filter data to include only March-October period
filter_march_to_october <- function(df) {
  if ("Date" %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(format(Date, "%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10"))
  }
  return(df)
}

# Remove unnecessary columns
cols_to_drop <- c("system.index", "Name", "image_id", ".geo")
drop_columns <- function(df) {
  df[ , !(names(df) %in% cols_to_drop)]
}

# Expand dataframe to daily resolution with NA for missing dates
make_daily <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.Date(df$Date)
    date_seq <- seq(from = min(df$Date), to = max(df$Date), by = "day")
    daily_df <- data.frame(Date = date_seq)
    df <- left_join(daily_df, df, by = "Date")
  }
  return(df)
}

# Linear interpolation for missing values in numeric columns
interpolate_missing_values <- function(df) {
  if (is.data.frame(df)) {
    numeric_cols <- sapply(df, is.numeric)
    for (col_name in names(df)[numeric_cols]) {
      df[[col_name]] <- na.approx(df[[col_name]], na.rm = FALSE)
    }
  }
  return(df)
}

# Apply Savitzky-Golay smoothing filter
apply_sg_filter <- function(df, order = 3, window_size = 15) {
  if (!is.data.frame(df)) return(df)
  numeric_cols <- sapply(df, is.numeric)
  if (!any(numeric_cols)) return(df)
  
  df[numeric_cols] <- lapply(df[numeric_cols], function(col) {
    if (sum(!is.na(col)) > window_size) {
      tryCatch({
        sgolayfilt(col, p = order, n = window_size)
      }, error = function(e) {
        warning("SG filter error: ", conditionMessage(e))
        col
      })
    } else {
      col
    }
  })
  return(df)
}

# Add siteyeardate identifier column
add_siteyeardate <- function(df, name) {
  if (is.data.frame(df)) {
    site_code <- substr(name, 1, 5)
    df$siteyeardate <- paste0(site_code, "_", format(df$Date, "%Y-%m-%d"))
  }
  return(df)
}

# Join VI and Meteo data by siteyeardate
left_join_by_siteyeardate <- function(df1, df2) {
  if (is.data.frame(df1) && is.data.frame(df2)) {
    df2 <- df2[, setdiff(names(df2), "Date")]
    merged <- dplyr::left_join(df1, df2, by = "siteyeardate")
    return(merged)
  }
  return(df1)
}


# Function to replace LAI < 2.5 between (DOP + 20) and Sept 1 with NA
fix_LAI_less_than_2_5_grow <- function(meteo_list, DOP_values, harvest_date = "2020-09-01") {
  harvest_doy <- yday(ymd(harvest_date))
  fixed_list <- lapply(names(meteo_list), function(site_name) {
    df <- meteo_list[[site_name]]
    # Extract site key (first 10 characters)
    site_key <- substr(site_name, 1, 10)
    # Get Date of Planting (DOY)
    dop <- DOP_values[site_key]
    if (is.na(dop)) return(df)  # skip if no DOP
    df <- df %>%
      mutate(
        DOY = yday(Date),
        Lai = ifelse(DOY > (dop + 20) & DOY < harvest_doy & Lai < 2.1, NA, Lai)
      )
    return(df)
  })
  names(fixed_list) <- names(meteo_list)
  return(fixed_list)
}

# =============================================================================
# PROCESS VEGETATION INDEX (VI) DATA
# =============================================================================

# Load and process VI data
vi_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SatelliteVIdata/PVPMVI"
vi_files <- list.files(vi_dir, full.names = TRUE)
file_names <- tools::file_path_sans_ext(basename(vi_files))
vi_df_list <- setNames(lapply(vi_files, read.csv), file_names)

# Filter for years 2015-2018 and specific sites
vi_df_2015_2018 <- vi_df_list[grep("2015|2016|2017|2018", names(vi_df_list))]
vi_df_2015_2018 <- vi_df_2015_2018[c(
  "USHRA_2015_VI", "USHRA_2016_VI", "USHRA_2017_VI",
  "USHRC_2015_VI", "USHRC_2016_VI", "USHRC_2017_VI", 
  "USBDA_2015_VI", "USBDA_2016_VI", 
  "USBDC_2015_VI", "USBDC_2016_VI", 
  "USOF1_2017_VI", "USOF2_2017_VI", "USOF3_2017_VI",
  "USOF4_2018_VI", "USOF5_2018_VI", "USOF6_2018_VI"
)]

# Clean and process VI data
vi_df_2015_2018 <- lapply(vi_df_2015_2018, drop_columns)
vi_df_2015_2018 <- lapply(vi_df_2015_2018, convert_unix_time)
vi_df_2015_2018 <- lapply(vi_df_2015_2018, sort_by_date)
vi_df_2015_2018 <- lapply(vi_df_2015_2018, filter_march_to_october)
vi_df_2015_2018 <- lapply(vi_df_2015_2018, make_daily)

# Apply interpolation and smoothing
vi_interpolated_list <- lapply(vi_df_2015_2018, interpolate_missing_values)
sg_interpolated_list <- lapply(vi_interpolated_list, apply_sg_filter)
sg_interpolated_list <- mapply(add_siteyeardate, sg_interpolated_list, names(sg_interpolated_list), SIMPLIFY = FALSE)

# =============================================================================
# PROCESS METEOROLOGICAL DATA
# =============================================================================

# Load and process meteo data
meteo_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SatelliteMeteodata/PVPMMeteo"
meteo_files <- list.files(meteo_dir, full.names = TRUE)
file_names <- tools::file_path_sans_ext(basename(meteo_files))
meteo_df_list <- setNames(lapply(meteo_files, read.csv), file_names)

# Filter for years 2015-2018 and specific sites
meteo_df_2015_2018 <- meteo_df_list[grep("2015|2016|2017|2018", names(meteo_df_list))]
meteo_df_2015_2018 <- meteo_df_2015_2018[c(
  "USHRA_2015_Meteo", "USHRA_2016_Meteo", "USHRA_2017_Meteo",
  "USHRC_2015_Meteo", "USHRC_2016_Meteo", "USHRC_2017_Meteo",
  "USBDA_2015_Meteo", "USBDA_2016_Meteo",
  "USBDC_2015_Meteo", "USBDC_2016_Meteo",
  "USOF1_2017_Meteo", "USOF2_2017_Meteo", "USOF3_2017_Meteo",
  "USOF4_2018_Meteo", "USOF5_2018_Meteo", "USOF6_2018_Meteo"
)]

# Clean and process meteo data
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, drop_columns)
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, convert_unix_time_meteo)
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, sort_by_date)
meteo_df_2015_2018ni <- meteo_df_2015_2018
meteo_df_2015_2018 <- fix_LAI_less_than_2_5_grow(meteo_df_2015_2018, DOP_values)
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, interpolate_missing_values)
meteo_df_2015_2018nisg <- lapply(meteo_df_2015_2018ni, interpolate_missing_values)
meteo_df_2015_2018 <- mapply(add_siteyeardate, meteo_df_2015_2018, names(meteo_df_2015_2018), SIMPLIFY = FALSE)
plot(meteo_df_2015_2018$USHRA_2015_Meteo$Date, meteo_df_2015_2018$USHRA_2015_Meteo$Lai)
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, function(df) {
  df$Lai <- if (sum(!is.na(df$Lai)) > 15) {
    sgolayfilt(df$Lai, p = 3, n = 15)
  } else {
    df$Lai
  }
  return(df)
})

plot(meteo_df_2015_2018$USHRA_2016_Meteo$Date, meteo_df_2015_2018$USHRA_2016_Meteo$Lai)

# =============================================================================
# COMBINE VI AND METEO DATA
# =============================================================================

# Merge VI and Meteo data
VImeteo20152018list <- mapply(left_join_by_siteyeardate, sg_interpolated_list, meteo_df_2015_2018, SIMPLIFY = FALSE)

# Standardize columns across all dataframes
all_cols <- unique(unlist(lapply(VImeteo20152018list, names)))
VImeteo20152018list <- lapply(VImeteo20152018list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  df[missing_cols] <- NA
  df[all_cols]
})

# Combine all sites into single dataframe
VImeteo20152018combine <- do.call(rbind, VImeteo20152018list)

# =============================================================================
# DATA QUALITY CHECKS
# =============================================================================

# Verify column counts
col_counts <- sapply(VImeteo20152018list, ncol)
if (length(unique(col_counts)) > 1) {
  message("Warning: Dataframes have different numbers of columns")
  print(col_counts[col_counts != max(col_counts)])
}

# Plot sample data for visual inspection
plot(VImeteo20152018combine$Date, VImeteo20152018combine$kNDVI, 
     main = "kNDVI Time Series", xlab = "Date", ylab = "kNDVI")

# View final combined data structure
str(VImeteo20152018combine)
head(VImeteo20152018combine)

# =============================================================================
# END OF MAIN PROCESSING CODE
# =============================================================================

# Everything below this line is either:
# - Diagnostic/checking code used during development
# - Exploratory analysis code
# - Temporary code snippets
# =============================================================================

########################################
# DEVELOPMENT/DIAGNOSTIC CODE BELOW - NOT PART OF MAIN PIPELINE
########################################

########################################
# CODE FRAGMENTS USED DURING DEVELOPMENT
########################################

########################################
# END OF FILE
########################################


###############################
######PLOTTING#################
###############################
# Set output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/VIraw"

# Loop through each dataframe in the list
for (df_name in names(vi_df_2015_2018)) {
  df <- vi_df_2015_2018[[df_name]]
  # Check if Date column exists
  if (!("Date" %in% colnames(df))) next  # Skip if no Date column
  # Loop through all columns excluding "Date"
  for (col_name in setdiff(colnames(df), "Date")) {
    # Create the scatter plot for each variable
    p <- ggplot(df, aes(x = Date, y = .data[[col_name]])) +
      geom_point(na.rm = TRUE, size = 2, color = "blue") +  # Scatter plot
      labs(title = paste(df_name, "VI Time Series -", col_name),
           x = "Date", y = col_name) +
      theme_minimal(base_size = 14) +  # Minimal theme with larger text
      theme(
        axis.line = element_line(color = "black", size = 0.5),  # Adds x and y axis lines
        axis.ticks = element_line(color = "black", size = 0.5),  # Adds ticks on axes
        panel.grid.major = element_line(color = "grey90", size = 0.5),  # Major gridlines
        panel.grid.minor = element_line(color = "grey95", size = 0.25),  # Minor gridlines
        panel.grid.major.x = element_blank(),  # Remove vertical gridlines if preferred
        panel.grid.minor.x = element_blank()   # Remove minor vertical gridlines if preferred
      ) +
      theme(
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
      )
    # Save the plot as a .jpeg file
    ggsave(filename = file.path(output_dir, paste0(df_name, "_", col_name, "_scatter_plot.jpeg")),
           plot = p, width = 10, height = 6, dpi = 300)
  }
}

# Set output directory
output_dir <-"C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/VILinearInterpolated"
# Loop through each dataframe in the list
for (df_name in names(vi_interpolated_list)) {
  df <- vi_interpolated_list[[df_name]]
  # Check if Date column exists
  if (!("Date" %in% colnames(df))) next  # Skip if no Date column
  # Loop through all columns excluding "Date"
  for (col_name in setdiff(colnames(df), "Date")) {
    # Create the scatter plot for each variable
    p <- ggplot(df, aes(x = Date, y = .data[[col_name]])) +
      geom_point(na.rm = TRUE, size = 2, color = "blue") +  # Scatter plot
      labs(title = paste(df_name, "VI Time Series -", col_name),
           x = "Date", y = col_name) +
      theme_minimal(base_size = 14) +  # Minimal theme with larger text
      theme(
        axis.line = element_line(color = "black", size = 0.5),  # Adds x and y axis lines
        axis.ticks = element_line(color = "black", size = 0.5),  # Adds ticks on axes
        panel.grid.major = element_line(color = "grey90", size = 0.5),  # Major gridlines
        panel.grid.minor = element_line(color = "grey95", size = 0.25),  # Minor gridlines
        panel.grid.major.x = element_blank(),  # Remove vertical gridlines if preferred
        panel.grid.minor.x = element_blank()   # Remove minor vertical gridlines if preferred
      ) +
      theme(
        axis.ticks.length = unit(0.2, "cm"),  # Set length of ticks
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
      )
    # Save the plot as a .jpeg file
    ggsave(filename = file.path(output_dir, paste0(df_name, "_", col_name, "_scatter_plot.jpeg")),
           plot = p, width = 10, height = 6, dpi = 300)
  }
}


#######################
##Check column numbers##
######################
col_counts <- sapply(vi_files, get_num_columns)# Apply the function to all files
col_info <- data.frame(File = basename(vi_files), Columns = col_counts)# Create a data frame with file names and column counts
files_with_4_cols <- col_info %>% filter(Columns == 4)# Find files with 4 columns
# Find the file with 145 columns
files_with_146_cols <- col_info %>% filter(Columns == 146)
# Print results
print("Files with 4 columns:")
print(files_with_4_cols)
#print("File with 146 columns:")
print(files_with_146_cols)
print(col_info)
# Create a data frame with file names and column counts
col_info <- data.frame(File = basename(vi_files), Columns = col_counts)
# Check if all files have 146 columns
if (all(col_counts == 146)) {
  print("✅ All files have exactly 146 columns.")
} else {
  print("⚠️ Some files do not have 146 columns.")
  
  # Print files that do not have 146 columns
  incorrect_files <- col_info %>% filter(Columns != 146)
  print(incorrect_files)
}



###########################################
# Apply to all dataframes in the list
mapply(classify_columns, names(meteo_df_2015_2018), meteo_df_2015_2018)
# Apply the function to all files in the meteo directory
columns_per_file <- lapply(meteo_files, get_column_names)
# Find files with 14 columns and compare them with the ones having 15 columns
files_with_14_columns <- which(sapply(columns_per_file, length) == 14)
files_with_15_columns <- which(sapply(columns_per_file, length) == 15)

# Output the file names with 14 columns
cat("Files with 14 columns:\n")
print(meteo_files[files_with_14_columns])

# Output the file names with 15 columns and the missing column for those with 14 columns
cat("\nFiles with 15 columns and missing columns in files with 14 columns:\n")
for (i in files_with_14_columns) {
  file_14 <- meteo_files[i]
  file_15 <- meteo_files[files_with_15_columns[i]]
  missing_column <- setdiff(colnames(read.csv(file_15, nrows = 5)), colnames(read.csv(file_14, nrows = 5)))
  cat("\nFile with 14 columns:", file_14, "\nMissing column:", missing_column, "\n")
}
# List all files in the directory
meteo_files <- list.files(meteo_dir, full.names = TRUE)
meteo_files

# Apply the function to all files in the meteo directory
columns_per_file <- lapply(meteo_files, get_column_names)

# Find the common columns present in all files
common_columns <- Reduce(intersect, columns_per_file)

# Output the common columns
cat("Common columns in all files:\n")
print(common_columns)

# Get the years from the files with 14 columns
years_with_14_columns <- sapply(meteo_files[files_with_14_columns], extract_year)

# Output the missing years from the files with 14 columns
cat("Years missing in the files with 14 columns:\n")
print(years_with_14_columns)


# Load required packages
library(ggplot2)
library(patchwork)
library(dplyr)

# Generate a list of plots
plot_list <- lapply(1:16, function(i) {
  df <- meteo_df_2015_2018ni[[i]]
  ggplot(df, aes(x = Date, y = Lai)) +
    geom_line(color = "darkgreen", size = 1) +
    labs(
      title = paste("Site", i),
      x = "Date",
      y = "LAI"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
})

# Arrange in 4 columns × 4 rows
combined_plot <- wrap_plots(plotlist = plot_list, ncol = 4)

# Save the figure
output_path <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LAI_data/LAI_16sites_grid.png"

ggsave(
  filename = output_path,
  plot = combined_plot,
  width = 16,
  height = 12,
  dpi = 300
)


