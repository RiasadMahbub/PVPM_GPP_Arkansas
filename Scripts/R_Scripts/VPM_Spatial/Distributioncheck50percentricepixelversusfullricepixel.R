### VPM Analysis
# Load necessary libraries
library(sf)
library(exactextractr)
library(terra)
library(ggplot2)
library(raster)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(lubridate)
library(maptools)  ## For wrld_simpl
library(sp)
library(rgdal)
library(cowplot)
library(ggsn)
library(ggpubr)
library(sf)
library(gridExtra)
library(grid)
library(rasterVis)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(RColorBrewer)
library(magrittr)
library(conflicted)
library(exactextractr)
library(ggspatial)
library(osmdata)

######################################################################
########################### Filter the images based on 50% coverage###
######################################################################
###### Define paths to the folders containing raster and polygon files ######
raster_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
polygon_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"
output_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter"

###### Function to filter raster based on coverage fraction ######
filter_raster_based_on_coverage <- function(raster_file, polygon_file, threshold = 0.5) {
  # Read the raster file
  raster_obj <- raster(raster_file)
  
  # Read the polygon file
  polygon_obj <- st_read(polygon_file)
  
  # Transform polygons to match raster CRS if needed
  if (!identical(st_crs(polygon_obj), projection(raster_obj))) {
    polygon_obj <- st_transform(polygon_obj, crs = projection(raster_obj))
  }
  
  # Perform exact extraction
  extracted_data <- exact_extract(raster_obj, polygon_obj, include_xy = TRUE)
  
  # Flatten the list of data frames into one data frame
  extracted_df <- do.call(rbind, extracted_data)
  
  # Filter based on coverage fraction
  filtered_df <- extracted_df[extracted_df$coverage_fraction >= threshold, ]
  
  # Convert filtered data frame back to raster
  if (nrow(filtered_df) > 0) {
    filtered_raster <- terra::rast(filtered_df[, c("x", "y", "value")], type = 'xyz')
    # Extend the raster to match the original raster's extent
    filtered_raster <- extend(filtered_raster, raster_obj)
  } else {
    filtered_raster <- raster_obj
    values(filtered_raster) <- NA
  }
  
  return(list(filtered_raster = filtered_raster, coverage_fraction = extracted_df$coverage_fraction))
}

###### Iterate through each year (2008 to 2020) and filter rasters ######
years <- 2008:2020
filtered_rasters <- list()
coverage_fractions <- list()

for (i in seq_along(years)) {
  # Construct file paths for raster and polygon files
  raster_file <- file.path(raster_folder, paste0("arkansasRice", years[i], "VPMcumulative.tif"))
  polygon_file <- file.path(polygon_folder, paste0("ArkansasRice", years[i], "_Shapefile_corrected.shp"))
  
  # Filter raster based on coverage fraction threshold
  result <- filter_raster_based_on_coverage(raster_file, polygon_file, threshold = 0.5)
  
  # Store the filtered raster in the list
  filtered_rasters[[years[i]]] <- result$filtered_raster
  
  # Store the coverage fraction data if it's not empty
  if (length(result$coverage_fraction) > 0) {
    coverage_fractions[[years[i]]] <- result$coverage_fraction
  }
  
  # Save the filtered raster to the output folder
  filtered_raster_path <- file.path(output_folder, paste0("filtered_arkansasRice", years[i], "VPMcumulative.tif"))
  writeRaster(result$filtered_raster, filename = filtered_raster_path, overwrite = TRUE)
}


#### Part 1: All Raster Files
# Define directory containing the raster files
raster_dir_vpm <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
# List all .tif files in the directory
rastlist_vpm <- list.files(path = raster_dir_vpm, pattern='.tif$', all.files=TRUE, full.names=FALSE)

# Set working directory to raster directory
setwd(raster_dir_vpm)
# Import all raster files in folder using lapply
allrasters_vpm <- lapply(rastlist_vpm, raster)
# Stack all rasters
stacked_raster_vpm <- stack(allrasters_vpm)
# Convert to terra rast object
stacked_raster_rast_vpm <- rast(stacked_raster_vpm)

# Read the shapefile
ME2 <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/ME_Shapefile/ME.shp")


# Calculate the mean of the stacked raster layers
stacked_raster_rast_mean_vpm <- terra::app(stacked_raster_rast_vpm, fun = mean, na.rm = TRUE)
names(stacked_raster_rast_mean_vpm) <- "GPP_VPM"

# Create mean raster data frame
cumulative_raster_df_vpm <- as.data.frame(stacked_raster_rast_mean_vpm, xy = TRUE) %>% drop_na()

# Multiply by 8
cumulative_raster_df_vpm$mean_8_vpm <- cumulative_raster_df_vpm$GPP_VPM * 8



###############################################################
#### Part 2: Raster Files with 50% or More Rice Pixel Coverage
###############################################################
# Define the path to the folder containing the filtered raster files
raster_folder_coverage <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter"
# List all the raster files in the folder
raster_files_coverage <- list.files(path = raster_folder_coverage, pattern = "^filtered_arkansasRice\\d{4}VPMcumulative\\.tif$", full.names = TRUE)
# Import all raster files in folder using lapply
all_rasters_coverage <- lapply(raster_files_coverage, raster)
# Stack all rasters
stacked_raster_coverage <- stack(all_rasters_coverage)

# Convert to terra rast object
stacked_raster_rast_coverage <- rast(stacked_raster_coverage)

# Calculate the mean of the stacked raster layers
stacked_raster_rast_mean_coverage <- terra::app(stacked_raster_rast_coverage, fun = mean, na.rm = TRUE)
names(stacked_raster_rast_mean_coverage) <- "GPP_Coverage"

# Create mean raster data frame
cumulative_raster_df_coverage <- as.data.frame(stacked_raster_rast_mean_coverage, xy = TRUE) %>% drop_na()

# Multiply by 8
cumulative_raster_df_coverage$mean_8_coverage <- cumulative_raster_df_coverage$GPP_Coverage * 8

# Print the resulting data frame
print(cumulative_raster_df_coverage)

# Plot histogram of the resulting data frame for coverage
hist(cumulative_raster_df_coverage$mean_8_coverage)



# Plot histogram for mean_8_coverage
# Calculate means
mean_coverage <- mean(cumulative_raster_df_coverage$mean_8_coverage, na.rm = TRUE)
mean_vpm <- mean(cumulative_raster_df_vpm$mean_8_vpm, na.rm = TRUE)

# Plot histogram for mean_8_coverage with mean line
ggplot(cumulative_raster_df_coverage, aes(x = mean_8_coverage)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_coverage), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of All MODIS Pixels that contains 50% or greater rice",
       x = "Mean 8 Coverage",
       y = "Frequency") +
  theme_minimal()

# Plot histogram for mean_8_vpm with mean line
ggplot(cumulative_raster_df_vpm, aes(x = mean_8_vpm)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_vpm), color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of All MODIS Pixels that contain rice",
       x = "GPP",
       y = "Frequency") +
  theme_minimal()


# Add a new column to each data frame to identify the source
cumulative_raster_df_coverage <- cumulative_raster_df_coverage %>%
  mutate(Source = "Coverage")

cumulative_raster_df_vpm <- cumulative_raster_df_vpm %>%
  mutate(Source = "VPM")

# Rename columns to have a common column name for the values
cumulative_raster_df_coverage <- cumulative_raster_df_coverage %>%
  rename(mean_8 = mean_8_coverage)

cumulative_raster_df_vpm <- cumulative_raster_df_vpm %>%
  rename(mean_8 = mean_8_vpm)

# Combine the two data frames
combined_df <- bind_rows(cumulative_raster_df_coverage, cumulative_raster_df_vpm)

View(combined_df)

# Plot histogram for mean_8_coverage and mean_8_vpm
ggplot(combined_df, aes(x = mean_8, fill = Source)) +
  geom_histogram(binwidth = 0.5, color = "black", alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Coverage" = "blue", "VPM" = "green")) +
  labs(title = "Histogram of Mean 8 Coverage and Mean 8 VPM",
       x = "Mean 8",
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())
# Number of rows
num_rows_coverage <- nrow(cumulative_raster_df_coverage)
num_rows_vpm <- nrow(cumulative_raster_df_vpm)

# Calculate the percentage
percentage_coverage_of_vpm <- (num_rows_coverage / num_rows_vpm) * 100

# Print the result
percentage_coverage_of_vpm



###### Combine coverage fractions into a single data frame ######
coverage_fraction_df <- bind_rows(lapply(seq_along(coverage_fractions), function(i) {
  if (length(coverage_fractions[[i]]) > 0) {
    data.frame(year = rep(years[i], length(coverage_fractions[[i]])), coverage_fraction = coverage_fractions[[i]])
  } else {
    data.frame(year = integer(0), coverage_fraction = numeric(0))
  }
}))

###### Create a distribution graph of the coverage fraction ######
ggplot(coverage_fraction_df, aes(x = coverage_fraction)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Distribution of Coverage Fraction from 2008 to 2020",
       x = "Coverage Fraction",
       y = "Frequency") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal()

###### Create a distribution graph of the coverage fraction ######
ggplot(cumulative_raster_df_coverage, aes(x = coverage_fraction)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Distribution of Coverage Fraction from 2008 to 2020",
       x = "Coverage Fraction",
       y = "Frequency") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal()



View(coverage_fraction_df)



# Calculate the proportion of coverage fractions above 0.50
total_count <- nrow(coverage_fraction_df)
above_threshold_count <- nrow(coverage_fraction_df %>% dplyr::filter(coverage_fraction > 0.50))

proportion_above_threshold <- above_threshold_count / total_count

# Print the result
proportion_above_threshold



###### Define paths to the folders containing raster and polygon files ######
raster_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
polygon_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"
output_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter"

###### Function to filter raster based on coverage fraction ######
filter_raster_based_on_coverage <- function(raster_file, polygon_file, threshold = 0.5) {
  # Read the raster file
  raster_obj <- raster(raster_file)
  
  # Read the polygon file
  polygon_obj <- st_read(polygon_file)
  
  # Transform polygons to match raster CRS if needed
  if (!identical(st_crs(polygon_obj), projection(raster_obj))) {
    polygon_obj <- st_transform(polygon_obj, crs = projection(raster_obj))
  }
  
  # Perform exact extraction
  extracted_data <- exact_extract(raster_obj, polygon_obj, include_xy = TRUE)
  
  # Flatten the list of data frames into one data frame
  extracted_df <- do.call(rbind, extracted_data)
  
  # Filter based on coverage fraction
  filtered_df <- extracted_df[extracted_df$coverage_fraction >= threshold, ]
  
  # Convert filtered data frame back to raster
  if (nrow(filtered_df) > 0) {
    filtered_raster <- terra::rast(filtered_df[, c("x", "y", "value")], type = 'xyz')
    # Extend the raster to match the original raster's extent
    filtered_raster <- extend(filtered_raster, raster_obj)
  } else {
    filtered_raster <- raster_obj
    values(filtered_raster) <- NA
  }
  
  return(list(filtered_raster = filtered_raster, coverage_fraction = extracted_df$coverage_fraction))
}

###### Iterate through each year (2008 to 2020) and filter rasters ######
years <- 2008:2020
filtered_rasters <- list()
coverage_fraction_dfs <- list()

for (i in seq_along(years)) {
  # Construct file paths for raster and polygon files
  raster_file <- file.path(raster_folder, paste0("arkansasRice", years[i], "VPMcumulative.tif"))
  polygon_file <- file.path(polygon_folder, paste0("ArkansasRice", years[i], "_Shapefile_corrected.shp"))
  
  # Filter raster based on coverage fraction threshold
  result <- filter_raster_based_on_coverage(raster_file, polygon_file, threshold = 0.5)
  
  # Store the filtered raster in the list
  filtered_rasters[[years[i]]] <- result$filtered_raster
  
  # Store the coverage fraction data if it's not empty
  if (length(result$coverage_fraction) > 0) {
    coverage_fraction_dfs[[paste0("year_", years[i])]] <- data.frame(year = years[i], coverage_fraction = result$coverage_fraction)
  }
  
  # Save the filtered raster to the output folder
  filtered_raster_path <- file.path(output_folder, paste0("filtered_arkansasRice", years[i], "VPMcumulative.tif"))
  writeRaster(result$filtered_raster, filename = filtered_raster_path, overwrite = TRUE)
}

###### Create distribution graphs of the coverage fraction for each year ######
for (year in names(coverage_fraction_dfs)) {
  ggplot(coverage_fraction_dfs[[year]], aes(x = coverage_fraction)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Distribution of Coverage Fraction for Year", year),
         x = "Coverage Fraction",
         y = "Frequency") +
    theme_minimal() +
    ggsave(paste0("coverage_fraction_distribution_", year, ".png"))
}

###### Print the names of the coverage fraction data frames ######
print(names(coverage_fraction_dfs))
hist(coverage_fraction_dfs$year_2008$coverage_fraction)
hist(coverage_fraction_dfs$year_2009$coverage_fraction)
hist(coverage_fraction_dfs$year_2010$coverage_fraction)
hist(coverage_fraction_dfs$year_2020$coverage_fraction)


mean(coverage_fraction_dfs$year_2008$coverage_fraction)
mean(coverage_fraction_dfs$year_2009$coverage_fraction)
mean(coverage_fraction_dfs$year_2010$coverage_fraction)
mean(coverage_fraction_dfs$year_2020$coverage_fraction)



####################################################
######## 05 30
#####################################################
###### Define paths to the folders containing raster and polygon files ######
raster_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM_0_05_30"
polygon_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllShapefile"
output_folder <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter_0_05_30"

###### Function to filter raster based on coverage fraction ######
filter_raster_based_on_coverage <- function(raster_file, polygon_file, threshold = 0.5) {
  # Read the raster file
  raster_obj <- raster(raster_file)
  
  # Read the polygon file
  polygon_obj <- st_read(polygon_file)
  
  # Transform polygons to match raster CRS if needed
  if (!identical(st_crs(polygon_obj), projection(raster_obj))) {
    polygon_obj <- st_transform(polygon_obj, crs = projection(raster_obj))
  }
  
  # Perform exact extraction
  extracted_data <- exact_extract(raster_obj, polygon_obj, include_xy = TRUE)
  
  # Flatten the list of data frames into one data frame
  extracted_df <- do.call(rbind, extracted_data)
  
  # Filter based on coverage fraction
  filtered_df <- extracted_df[extracted_df$coverage_fraction >= threshold, ]
  
  # Convert filtered data frame back to raster
  if (nrow(filtered_df) > 0) {
    filtered_raster <- terra::rast(filtered_df[, c("x", "y", "value")], type = 'xyz')
    # Extend the raster to match the original raster's extent
    filtered_raster <- extend(filtered_raster, raster_obj)
  } else {
    filtered_raster <- raster_obj
    values(filtered_raster) <- NA
  }
  
  return(list(filtered_raster = filtered_raster, coverage_fraction = extracted_df$coverage_fraction))
}

###### Iterate through each year (2008 to 2020) and filter rasters ######
years <- 2008:2020
filtered_rasters <- list()
coverage_fractions <- list()

for (i in seq_along(years)) {
  raster_file <- file.path(raster_folder, paste0("arkansasRice", years[i], "VPMcumulative0_530.tif"))
  polygon_file <- file.path(polygon_folder, paste0("ArkansasRice", years[i], "_Shapefile_corrected.shp"))
  
  print(paste("Processing files:", raster_file, polygon_file))
  
  if (!file.exists(raster_file) || !file.exists(polygon_file)) {
    warning(paste("File missing for year", years[i]))
    next
  }
  
  result <- tryCatch({
    filter_raster_based_on_coverage(raster_file, polygon_file, threshold = 0.5)
  }, error = function(e) {
    message(paste("Error processing year", years[i], ":", e))
    NULL
  })
  
  if (!is.null(result)) {
    filtered_rasters[[years[i]]] <- result$filtered_raster
    if (!is.null(result$coverage_fraction)) {
      coverage_fractions[[years[i]]] <- result$coverage_fraction
    }
    
    filtered_raster_path <- file.path(output_folder, paste0("filtered_arkansasRice", years[i], "VPMcumulative0_530.tif"))
    writeRaster(result$filtered_raster, filename = filtered_raster_path, overwrite = TRUE)
  }
}



# List files in raster_folder
raster_files <- list.files(raster_folder, full.names = TRUE)
print("Files in Raster Folder:")
print(raster_files)

# Load the raster and shapefile
raster_data <- rast("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/arkansasRice2012VPMcumulative.tif")
shapefile_data <- vect("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/ArkansasAmerifluxSiteData/SHP/ArkansasAmerifluxPolygon.shp")

# Calculate the area of each polygon in square meters
polygon_areas <- geom(shapefile_data)$area  # This gives the areas directly

# Calculate the area of each raster pixel
pixel_area <- cellSize(raster_data, unit = "m")

# Intersect raster and shapefile
overlap <- terra::extract(raster_data, shapefile_data, weights = TRUE, cell = TRUE)

# Add the polygon name (using 'Name' as the identifier)
overlap$polygon_name <- shapefile_data$Name[overlap$ID]

# Reproject the shapefile to a projected CRS (e.g., UTM Zone 15N for Arkansas)
shapefile_data_proj <- project(shapefile_data, "EPSG:32615")  # EPSG:32615 is UTM Zone 15N

# Calculate the area of each polygon in square meters
polygon_areas <- expanse(shapefile_data_proj, unit = "m")
# View the results
overlap



library(terra)

