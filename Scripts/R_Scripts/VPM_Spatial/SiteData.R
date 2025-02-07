# Specify the folder path
folder_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/SiteData"

# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list
csv_data_list <- lapply(csv_files, read.csv)
csv_data_list
# Combine the data frames into a single data frame
# Convert the column to numeric
csv_data_list <- lapply(csv_data_list, function(df) {
  df$sur_refl_b03 <- as.numeric(df$sur_refl_b03)
  return(df)
})

# Combine the data frames into a single data frame
combined_df <- bind_rows(csv_data_list)
combined_df$gpp, combined_df$
