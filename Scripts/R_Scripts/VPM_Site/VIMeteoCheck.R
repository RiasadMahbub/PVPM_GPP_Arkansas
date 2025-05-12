#The timestamp 1420416000000 is a Unix timestamp in milliseconds. When converted, it corresponds to January 5, 2015, at 00:00:00 UTC. This is useful for tracking or comparing events in a standardized time format across different systems.
### vi_df_list vi_df_2015_2018
### vi_interpolated_list
### sg_interpolated_list
### meteo_df_list
### meteo_interpolated_list
### vi_df_list read them and name them 
### get the vi_df_list from 2015 to 2017
### vi_sg_interpolated_combine
### VImeteo20152018combine

# Load necessary package
library(dplyr)
library(zoo)      # For na.approx (linear interpolation)
library(dplyr)
library(tidyr)
library(ggplot2)
library(signal)  # For sgolayfilt()
library(tidyverse)
library(dplyr)
library(lubridate)

#######################
#####FUNCTIONS#########
#######################
# Function to convert Unix time (milliseconds) to R Date format
convert_unix_time <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.POSIXct(df$Date / 1000, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}

convert_unix_time_meteo <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.POSIXct(df$Date, origin = "1970-01-01", tz = "UTC")
  }
  return(df)
}
# Function to sort dataframe by Date
sort_by_date <- function(df) {
  if ("Date" %in% colnames(df)) {
    df <- df[order(df$Date), ]
  }
  return(df)
}
get_num_columns <- function(file) {# Function to get the number of columns in each file
  data <- read.csv(file, nrows = 5)  # Read only the first few rows to check structure
  return(ncol(data))
}
# Function to read a CSV file
read_csv_file <- function(file) {
  data <- read.csv(file)  # Read the entire file
  return(data)
}
# Function to filter data between March and October
filter_march_to_october <- function(df) {
  if ("Date" %in% colnames(df)) {
    df <- df %>%
      dplyr::filter(format(Date, "%m") %in% c("03", "04", "05", "06", "07", "08", "09", "10"))
  }
  return(df)
}
# Function to classify columns in each dataframe
classify_columns <- function(df_name, df) {
  col_classes <- sapply(df, class)
  character_cols <- names(col_classes)[col_classes %in% c("character", "factor")]
  date_cols      <- names(col_classes)[col_classes %in% c("Date", "POSIXct", "POSIXt")]
  numeric_cols   <- names(col_classes)[col_classes %in% c("numeric", "integer", "double")]
  cat("DataFrame:", df_name, "\n")
  cat("Character columns:", toString(character_cols), "\n")
  cat("Date columns     :", toString(date_cols), "\n")
  cat("Numeric columns  :", toString(numeric_cols), "\n\n")
}
####Dropping character columns####
cols_to_drop <- c("system.index", "Name", "image_id", ".geo")# Columns to drop
drop_columns <- function(df) {
  df[ , !(names(df) %in% cols_to_drop)]
} # Function to remove specified columns
# Function to make the dataset daily by adding missing dates with NA
make_daily <- function(df) {
  if ("Date" %in% colnames(df)) {
    df$Date <- as.Date(df$Date)# Truncate time to just date
    date_seq <- seq(from = min(df$Date), to = max(df$Date), by = "day")# Create a sequence of dates from min to max date
    daily_df <- data.frame(Date = date_seq)    # Create a new dataframe with all dates
    df <- left_join(daily_df, df, by = "Date")    # Merge with original dataframe
  }
  return(df)
}
# Function to interpolate missing values in all numeric columns, modifying in place
interpolate_missing_values <- function(df) {
  if (is.data.frame(df)) {# Check if df is actually a dataframe
    numeric_cols <- sapply(df, is.numeric)  # Identify numeric columns
    # Apply na.approx only to numeric columns, making sure to handle single-column dataframes
    for (col_name in names(df)[numeric_cols]) {
      df[[col_name]] <- na.approx(df[[col_name]], na.rm = FALSE)  # Apply interpolation directly on column
    }
  }
  return(df)
}

#######Functoin for savitzkly golay filter##################
apply_sg_filter <- function(df, order = 3, window_size = 15) {
  if (!is.data.frame(df)) return(df)  # skip if not a dataframe
  numeric_cols <- sapply(df, is.numeric)
  if (!any(numeric_cols)) return(df)  # skip if no numeric columns
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

add_siteyeardate <- function(df, name) {
  if (is.data.frame(df)) {
    site_code <- substr(name, 1, 5)  # Extract first 5 characters
    df$siteyeardate <- paste0(site_code, "_", format(df$Date, "%Y-%m-%d"))
  }
  return(df)
}

left_join_by_siteyeardate <- function(df1, df2) {
  if (is.data.frame(df1) && is.data.frame(df2)) {
    df2 <- df2[, setdiff(names(df2), "Date")]  # Drop 'Date' from meteo
    merged <- dplyr::left_join(df1, df2, by = "siteyeardate")
    return(merged)
  }
  return(df1)
}



# Define the directory path
vi_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SatelliteVIdata/PVPMVI"# List all files in the directory
vi_files <- list.files(vi_dir, full.names = TRUE)
#vi_df_list <- setNames(lapply(vi_files, read_csv_file), basename(vi_files))# Read all files into a named list of dataframes
file_names <- tools::file_path_sans_ext(basename(vi_files))# Extract file names without extension
vi_df_list <- setNames(lapply(vi_files, read_csv_file), file_names)# Read files and assign names from filenames (without extension)
vi_df_2015_2018 <- vi_df_list[grep("2015|2016|2017|2018", names(vi_df_list))] # Filter list to only include names with 2015, 2016, or 2017
vi_df_2015_2018 <- vi_df_2015_2018[c("USHRA_2015_VI", "USHRA_2016_VI", "USHRA_2017_VI", 
                               "USHRC_2015_VI", "USHRC_2016_VI", "USHRC_2017_VI", "USBDA_2015_VI", 
                               "USBDA_2016_VI", "USBDC_2015_VI", "USBDC_2016_VI", "USOF3_2017_VI", 
                               "USOF2_2017_VI", "USOF1_2017_VI", "USOF4_2018_VI", "USOF5_2018_VI",
                               "USOF6_2018_VI")]
ncol(vi_df_2015_2018$USHRC_2015_VI)
# Applyclassify_columns
mapply(classify_columns, names(vi_df_2015_2018), vi_df_2015_2018)
vi_df_2015_2018 <- lapply(vi_df_2015_2018, drop_columns)# Apply to all dataframes in the list
vi_df_2015_2018 <- lapply(vi_df_2015_2018, convert_unix_time)# Apply the conversion to each dataframe in the list
vi_df_2015_2018 <- lapply(vi_df_2015_2018, sort_by_date)
# FILTER BETWEEN MARCH AND OCTOBER
vi_df_2015_2018 <- lapply(vi_df_2015_2018, filter_march_to_october)
###Create Daily scale dataset
# Apply the make_daily function to all dataframes in the list
vi_df_2015_2018 <- lapply(vi_df_2015_2018, make_daily)
#######################
######Linear Interpolation VI######
#######################
# Apply the interpolate_missing_values function to all dataframes in the list
vi_interpolated_list <- lapply(vi_df_2015_2018, interpolate_missing_values)
plot(vi_interpolated_list$USOF1_2017_VI$Date, vi_interpolated_list$USOF1_2017_VI$kNDVI)
##save them here: C:\Users\rbmahbub\Documents\RProjects\GapfillingOtherRiceSites\Figure\VILinearInterpolated
#######################
######Savitzky Golay Filter######
#######################

# Apply filter safely to all items in the list
sg_interpolated_list <- lapply(vi_interpolated_list, apply_sg_filter)
#sg_interpolated_list <- lapply(vi_df_2015_2018, apply_sg_filter)
plot(vi_interpolated_list$USBDA_2015_VI$Date, vi_interpolated_list$USBDA_2015_VI$kNDVI)
plot(vi_interpolated_list$USOF1_2017_VI$Date, vi_interpolated_list$USOF1_2017_VI$kNDVI)
plot(sg_interpolated_list$USOF1_2017_VI$Date, sg_interpolated_list$USOF1_2017_VI$kNDVI)

# Or using mapply (more concise, preserves names)
sg_interpolated_list <- mapply(add_siteyeardate, 
                               sg_interpolated_list, 
                               names(sg_interpolated_list), 
                               SIMPLIFY = FALSE)

########################
###METEO
########################
# Define the directory path for Satellite Meteo data
meteo_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SatelliteMeteodata/PVPMMeteo"
meteo_files <- list.files(meteo_dir, full.names = TRUE)# List all files in the directory
meteo_df_list <- lapply(meteo_files, read.csv)###read the meteo files # Read the meteo files into a list of dataframes
file_names <- tools::file_path_sans_ext(basename(meteo_files))# Extract file names without extension
meteo_df_list <- setNames(lapply(meteo_files, read_csv_file), file_names)# Read files and assign names from filenames (without extension)
meteo_df_2015_2018 <- meteo_df_list[grep("2015|2016|2017|2018", names(meteo_df_list))] # Filter list to only include names with 2015, 2016, or 2017
meteo_df_2015_2018 <- meteo_df_2015_2018[c("USHRA_2015_Meteo", "USHRA_2016_Meteo", "USHRA_2017_Meteo", 
                                     "USHRC_2015_Meteo", "USHRC_2016_Meteo", "USHRC_2017_Meteo", "USBDA_2015_Meteo", 
                                     "USBDA_2016_Meteo", "USBDC_2015_Meteo", "USBDC_2016_Meteo", "USOF3_2017_Meteo", 
                                     "USOF2_2017_Meteo", "USOF1_2017_Meteo" 
                                     ,"USOF4_2018_Meteo", "USOF5_2018_Meteo","USOF6_2018_Meteo"
                                     )]
# Applyclassify_columns
mapply(classify_columns, names(meteo_df_2015_2018), meteo_df_2015_2018)
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, drop_columns)# Apply to all dataframes in the list
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, convert_unix_time_meteo)# Apply the conversion to each dataframe in the list
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, sort_by_date)

sapply(meteo_df_2015_2018, nrow) #### nrows is 364 or 365
# Apply the interpolate_missing_values function to all dataframes in the list
meteo_df_2015_2018 <- lapply(meteo_df_2015_2018, interpolate_missing_values)
plot(meteo_df_2015_2018$USOF6_2018_Meteo$Date, meteo_df_2015_2018$USOF6_2018_Meteo$Ec)
# Apply to meteo_df_2015_2018 using mapply (preserves names)
meteo_df_2015_2018 <- mapply(add_siteyeardate,
                             meteo_df_2015_2018,
                             names(meteo_df_2015_2018),
                             SIMPLIFY = FALSE)

# Verify one element
meteo_df_2015_2018[[1]]$siteyeardate  # Check first dataframe
##########################merge list ###############################
VImeteo20152018list <- mapply(left_join_by_siteyeardate, sg_interpolated_list, meteo_df_2015_2018, SIMPLIFY = FALSE)
lapply(VImeteo20152018list, function(df) setdiff(names(df), names(VImeteo20152018list[[which.min(sapply(VImeteo20152018list, ncol))]])))
VImeteo20152018list <- lapply(VImeteo20152018list, function(df) {missing_cols <- setdiff(all_cols <- unique(unlist(lapply(VImeteo20152018list, names))), names(df)); df[missing_cols] <- NA; df[all_cols] <- df[all_cols]; df})
VImeteo20152018combine <- do.call(rbind, VImeteo20152018list)
plot(VImeteo20152018list$USOF6_2018_VI$Date, VImeteo20152018list$USOF6_2018_VI$Ec)

sapply(VImeteo20152018list, ncol)
# Get column counts
col_counts <- sapply(VImeteo20152018list, ncol)
# Find which ones are not 146
not_146 <- col_counts[col_counts != 146]
# Print them
print(not_146)




########################################
# Add a 'site_year' column to each dataframe
sg_interpolated_list <- mapply(function(df, nm) {
  if (is.data.frame(df)) {
    site_code <- substr(nm, 1, 5)  # first 5 characters from the name
    df$siteyeardate <- paste0(site_code, "_", format(df$Date, "%Y-%m-%d"))
  }
  return(df)
}, sg_interpolated_list, names(sg_interpolated_list), SIMPLIFY = FALSE)

sg_interpolated_list$USBDA_2015_VI$siteyeardate


####Join VI and MeteoFiles#################
################Merge them with the GPP Files ############
ncol(sg_interpolated_list)
plot(vi_sg_interpolated_combin$NDVI, vi_sg_interpolated_combin$kNDVI)


vi_sg_interpolated_combin





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


