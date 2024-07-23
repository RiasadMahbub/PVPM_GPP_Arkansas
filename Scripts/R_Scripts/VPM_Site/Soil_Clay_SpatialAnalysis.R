
### Export each counties
### Necessary libraries

library(sf)
library(dplyr)
library(sp) ## prerequisite for the raster package
library(raster) #raster(read the rasterfile)
library(terra) # The terra::project function is used in the terra package to project (resample/aggregate) one raster to the grid of another.

## load the shapefile
shapefile <- st_read("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Data/ArkansasShapeFile_OtherShapeFile/ArkansasCounty/COUNTY_BOUNDARY/COUNTY_BOUNDARY.shp")

head(shapefile)
nrow((shapefile))

county_names <- unique(shapefile$OBJECTID)
# Loop through each county
# Loop through each ObjectID
for (id in 1:75) {
  county_subset <- shapefile %>%
  filter(OBJECTID == id)
  
  # Do something with the county subset (e.g., save it, perform analysis, etc.)
  # Replace "output_folder" with the folder where you want to save the subsets
  st_write(county_subset, paste0("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Data/ArkansasShapeFile_OtherShapeFile/ArkansasCounty/IndividualCountyShapefile", id, "_subset.shp"))
}



county_subset <- shapefile %>%
  filter(CountyName == county) %>%
  select(ObjectID)




### upscale the 30m data to 500m
### plot gpp vs clay content
## directory of the clay content file
# Specify the directory and file name
dir <- "/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Data/VPM/SoilClay"
# Specify the file names
file_name_clay <- "ClayContentArkansas.tif"
file_name_cumulative <- "arkansasRice2008VPMcumulative.tif"

# Combine the directory and file names
raster_path_clay <- file.path(dir, file_name_clay)
raster_path_cumulative <- file.path(dir, file_name_cumulative)

# Read the raster images
clay_raster <- raster(raster_path_clay)
cumulative_raster <- raster(raster_path_cumulative)

clay_raster
# Print information about the rasters
plot(clay_raster)
plot(cumulative_raster)

# Convert 'clay_raster' to a terra object
terra_clay <- rast(clay_raster)
plot(terra_clay)
terra_clay

# Project 'clay_raster' to the grid of 'cumulative_raster'
projected_clay <- terra::project(terra_clay, cumulative_raster, method = "average")

plot(values(projected_clay), values(cumulative_raster))

# Convert projected raster to dataframe
#projected_df <- terra::as.data.frame(projected_clay)
projected_df <- terra::as.data.frame(projected_clay, xy=TRUE)

# Convert cumulative raster to dataframe
#cumulative_df <- terra::as.data.frame(cumulative_raster)
cumulative_df <- terra::as.data.frame(cumulative_raster, xy=TRUE)

projected_cumulativedf<- left_join(cumulative_df, projected_df, by = c("x" = "x", "y" = "y"))
##clip a raster by another raster 




View(projected_df)
nrow(cumulative_df)
nrow(projected_df)

ncol(cumulative_df)
ncol(projected_df)

library(sf)

# Convert dataframes to sf objects
small_sf <- st_as_sf(cumulative_df, coords = c("x", "y"))
large_sf <- st_as_sf(projected_df, coords = c("x", "y"))

# Remove rows with NA values in x or y columns for both dataframes
small_sf <- small_sf %>% drop_na(x, y)
large_sf <- large_sf %>% drop_na(x, y)

# Perform a spatial join based on x and y values
merged_sf <- st_join(small_sf, large_sf, join = st_equals)

# Convert the resulting sf object back to a dataframe
merged_df <- as.data.frame(merged_sf)

# Print the merged dataframe
print(merged_df)
