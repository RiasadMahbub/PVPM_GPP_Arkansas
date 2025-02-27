# Load libraries
library(raster)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggpubr)
library(viridis)
library(terra)


### VPM
###Load all the required libraries
library(raster)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(terra)
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

#### all raster using rast function
#first import all files in a single folder as a list 
raster_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter"
rastlist <- list.files(path = raster_dir, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/CountyShapefileFromArkansasOffice/COUNTY_BOUNDARY.shp"
)

# Read the shapefile using st_read
ME2 <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/ME_Shapefile/ME.shp")

setwd(raster_dir)
#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
stacked_raster <- stack(allrasters) ### all raster stack
stacked_raster_rast<-rast(stacked_raster)
#raster::calc
stacked_raster_rast_mean <- terra::app(stacked_raster_rast, "mean", na.rm=TRUE)
names(stacked_raster_rast_mean) <- "GPP"
### Create mean raster data
cumulativerasdf <- as.data.frame(stacked_raster_rast_mean,xy=TRUE)%>%drop_na()
## multiply by 8
cumulativerasdf$mean_8<-cumulativerasdf$GPP*8

# Output file to the exported files
output_file <- "C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Data/ExportedData/CSVCumulative2008_2020.csv"
write.csv(cumulativerasdf, file = output_file, row.names = FALSE)

# Calculate mean and standard deviation of mean_8
mean_mean_8 <- mean(cumulativerasdf$mean_8)
sd_mean_8 <- sd(cumulativerasdf$mean_8)

# Identify values that are 3 standard deviations away from the mean
outlier_threshold <- 3 * sd_mean_8
outliers <- which(abs(cumulativerasdf$mean_8 - mean_mean_8) > outlier_threshold)

# Remove outliers
cleaned_cumulativerasdf <- cumulativerasdf[-outliers, ]
nrow(cumulativerasdf)
nrow(cleaned_cumulativerasdf)

##########PLOTTING  CUMULATIVE ####################
## Plotting cumulative old technique
### cumulativerasdf normal GPP data
### cleaned_cumulativerasdf: 3 standard deviation filtered data
nameColor <- bquote(atop(Mean~Cumulative ~GPP~(2008-2020)~(g~C~m^-2~year^-1)~"  "))
my_breaks <- c(1100, 1500, 1800, 2100, 2500, 2800)
my_breaks <- seq(1000, 3000, by = 500)

##applydegree north with function with x axis label
nwbrks <- seq(31,36,1)
nwlbls <- unlist(lapply(nwbrks, function(x) paste(x, "°N")))

# cumulativemap<-ggplot()+
#   geom_sf(fill='transparent',data=ME2)+
#   geom_raster(aes(x=x,y=y,fill=mean_8),data=cleaned_cumulativerasdf)+
#   #scale_fill_viridis_c( limits = c(0, 300), option = "turbo", breaks = my_breaks, nameColor, direction = -1, oob = scales::squish)+
#   scale_fill_viridis( option = "turbo", nameColor, direction = -1, 
#                       breaks = my_breaks, 
#                       limits = c(1000, 3000)) +
#   labs(x='Longitude',y='Latitude', color = nameColor)+
#   scale_y_continuous(breaks = seq(34, 36, by=1))+
#   cowplot::theme_cowplot(font_size = 24)+
#   theme(legend.key.width=unit(4,"cm"), legend.spacing.x = unit(1, 'cm'))+
#   theme(axis.text = element_text(size = 25))  +
#   
#   ggsn::north(arkansasshp) +
#   ggsn::scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=5, height=0.02,
#                  transform = TRUE, model = "WGS84")

cumulativemap<-ggplot() +
  geom_sf(fill = 'transparent', data = ME2) +  # Add the shapefile
  geom_raster(aes(x = x, y = y, fill = mean_8), data = cleaned_cumulativerasdf) +  # Add the raster data
  scale_fill_viridis(option = "turbo", name = nameColor, direction = -1, 
                     breaks = my_breaks, limits = c(1000, 3000)) +  # Customize the fill scale
  labs(x = 'Longitude', y = 'Latitude') +  # Add axis labels
  scale_y_continuous(breaks = seq(34, 36, by = 1)) +  # Customize y-axis breaks
  cowplot::theme_cowplot(font_size = 24) +  # Apply a custom theme
  theme(legend.key.width = unit(4, "cm"), legend.spacing.x = unit(1, 'cm')) +  # Customize legend
  theme(axis.text = element_text(size = 25)) +  # Customize axis text size
  
  # Add scale bar
  annotation_scale(location = "br", width_hint = 0.5, height = unit(0.5, "cm"),text_cex = 2) +  # Add a scale bar at the bottom-left
  
  # Add north arrow
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_fancy_orienteering,
                         height = unit(2, "cm"),text_cex = 1.2) +  # Add a north arrow at the top-right
  
  # Ensure the coordinate system is set to WGS84 (EPSG:4326)
  coord_sf(crs = 4326)

# Define the breaks for the y-axis to show 34.2, 34.4, 34.6, etc.
latitude_breaks <- seq(33, 36, by = 0.2)
latitude_breaksnwlbls <- unlist(lapply(latitude_breaks, function(x) paste(x, "°N")))
cumulativegam<-  ggplot(cleaned_cumulativerasdf, aes(x=y, y=mean_8)) +
  geom_smooth(level = 0.687)+labs(x='Latitude',y=expression(Mean~Cumulative ~GPP~(g~C~m^{-2}~year^{-1})))+
  scale_x_continuous(breaks = latitude_breaks, labels = latitude_breaksnwlbls, expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(33, 36, by=1))+
  
  coord_flip()+
  cowplot::theme_cowplot(font_size = 23)+
  theme(axis.text = element_text(size = 25))  

cumulativearranged<-ggarrange(cumulativemap, cumulativegam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(1.6,1),
                              common.legend = TRUE, legend = "bottom")
cumulativearranged

cumulativemap
cumulativegam

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMcumulativearrangedsameextent.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMcumulativearranged.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)


#####Latitude with highest GPP
# Sort the data frame by 'mean_8' in descending order and select the top 10 rows
top_10_max_mean_8 <- cumulativerasdf[order(-cumulativerasdf$mean_8), ][1:10, ]

# Print the top 10 rows with the highest 'mean_8' values
print(top_10_max_mean_8)

# Round 'y' to one decimal place to group by it
cumulativerasdf$y_rounded <- round(cumulativerasdf$y, 1)

# Calculate the mean of 'mean_8' for each rounded 'y' value
mean_by_y <- aggregate(mean_8 ~ y_rounded, data = cumulativerasdf, FUN = mean)

# Display the result
print(mean_by_y)
print(mean_by_y[order(-mean_by_y$mean_8), ][1:10, ])


# Filter data based on latitude range
mean_gpp34.234.6 <- mean(subset(cumulativerasdf, y >= 34.2 & y <= 34.6)$mean_8)
sd_gpp34.234.6 <- sd(subset(cumulativerasdf, y >= 34.2 & y <= 34.6)$mean_8)
# Display the mean GPP value
# Using sprintf
print(sprintf("%.0f", mean_gpp34.234.6))
print(sprintf("%.0f", sd_gpp34.234.6))


# Calculate mean and standard deviation of mean_8 for latitude range 36 to 35.5
mean_gpp_36_35.5 <- mean(subset(cumulativerasdf,  y >= 35.5 & y <= 36)$mean_8)
sd_gpp_36_35.5 <- sd(subset(cumulativerasdf, y >= 35.5 & y <= 36)$mean_8)
# Using sprintf
print(sprintf("%.0f", mean_gpp_36_35.5))
print(sprintf("%.0f", sd_gpp_36_35.5))


mean_gpp<-mean(cumulativerasdf$mean_8)
sd_gpp<-sd(cumulativerasdf$mean_8)

print(paste("The spatial distribution of GPP has shown that rice fields located between 33.5° N and 34.5° N have higher GPP values. Mean GPP in this range:", mean_gpp))
print(paste("At the state scale, in the timeframe between 2008 to 2020, the mean photosynthetic carbon uptake of Arkansas rice fields was :", mean_gpp))
sd_gpp
sd_gpp33.534.5
###Lowest




mean(cumulativerasdf$mean_8)
sd(cumulativerasdf$mean_8)

head(cumulativerasdf[order(-cumulativerasdf$mean_8), ], 600)

### export the cumulative raster data for the soil clay content data
# Specify the output file path and name (replace with your desired path and name)
output_file <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM-alltogether/arkansasRice20082020VPMcumulative4.tif"
# Use writeRaster to export the raster
writeRaster(meancumulativeNArmtrue, filename = output_file, format = "GTiff", overwrite = TRUE)
writeRaster(meancumulativeNArmtrue, filename = output_file, format = "GTiff", overwrite = TRUE, options=c("COMPRESS=LZW"), datatype='FLT4S')


### stacked raster to multiple column dataframe to calculate the slope
r_multiple_df <- as.data.frame(stacked_raster, xy = TRUE) ## allraster stack:stacked_raster
nrow(r_multiple_df)
# Remove rows with all NA values
# Specify columns to check for NA values
columns_to_check <- c("gpp_sum.1", "gpp_sum.2", "gpp_sum.3", "gpp_sum.4", "gpp_sum.5", "gpp_sum.6", "gpp_sum.7",
                      "gpp_sum.8", "gpp_sum.9", "gpp_sum.10", "gpp_sum.11", "gpp_sum.12", "gpp_sum.13")

# Remove rows where at least 11 columns have NA values
r_multiple_df <- r_multiple_df[rowSums(is.na(r_multiple_df[, columns_to_check])) <= 5, , drop = TRUE]
### Number of years data present for the pixels are 6 to 13
nrow(r_multiple_df)
View(r_multiple_df)


# Function to calculate slope and p-value for each row
calculate_slope_pvalue <- function(row) {
  model <- lm(unlist(row) ~ c(1:length(row)))  # Fit linear model
  slope <- model$coefficients[[2]]              # Extract slope
  p_value <- summary(model)$coefficients[2, 4]  # Extract p-value
  return(c(slope, p_value))
}

# Apply the function to each row and add as new columns
r_multiple_df <- r_multiple_df %>%
  rowwise() %>%
  mutate(slope = calculate_slope_pvalue(c_across(all_of(columns_to_check)))[1],
         p_value = calculate_slope_pvalue(c_across(all_of(columns_to_check)))[2])

hist(r_multiple_df$p_value)
r_multiple_df_pvalue <- r_multiple_df[r_multiple_df$p_value < 0.05, ]
nrow(r_multiple_df_pvalue)

library(viridis)
library("colorspace")
hcl_palettes(plot = TRUE)
ggplot() +
  geom_sf(fill = 'light gray', data = ME) +
  geom_raster(aes(x = x, y = y, fill = slope), data = r_multiple_df_filtered) +
  labs(x = 'Longitude', y = 'Latitude', color = nameColor) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3", l1 = 30, l2 = 100, p1 = .9, p2 = 1.2, limits = c(-10, 10)) +
  scale_y_continuous(breaks = seq(34, 36, by = 1)) +
  cowplot::theme_cowplot(font_size = 24) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.key.width = unit(4, "cm"), legend.spacing.x = unit(2, 'cm')) +
  theme(axis.text = element_text(size = 25))




# Your existing plot code
your_plot <- ggplot() +
  geom_sf(fill = 'light gray', data = ME) +
  geom_raster(aes(x = x, y = y, fill = slope), data = r_multiple_df_pvalue) +
  labs(x = 'Longitude', y = 'Latitude', color = nameColor) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3", l1 = 30, l2 = 100, p1 = .9, p2 = 1.2, limits = c(-10, 10), rev = TRUE) +
  scale_y_continuous(breaks = seq(34, 36, by = 1)) +
  cowplot::theme_cowplot(font_size = 22) +
  theme(legend.position = "bottom", legend.box = "horizontal", legend.key.width = unit(3, "cm"), legend.spacing.x = unit(1, 'cm')) +
  theme(axis.text = element_text(size = 22))

# Add scale and North arrow
your_plot +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/GPPImages.png",width = 14, height = 8 )
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Scale/Scale.png", width =9, height =8)

#export the pvalue-low and high regions trend dataframe into a raster
### export the cumulative raster data for the soil clay content data
# Assuming r_multiple_df_pvalue is your dataframe
r_multiple_df_pvalue_filtered <- r_multiple_df_pvalue %>%
  select(x, y, slope)

output_file <- "C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Scale"

# Use writeRaster to export the raster
writeRaster(r_multiple_df_pvalue, filename = r_multiple_df_pvalue, format = "GTiff", overwrite = TRUE)





### merge all the shapefiles
library(sf)
# Specify the folder path
folder_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllMergedTogether"
# Read the shapefile
single_shapefile <- st_read(folder_path)
single_shapefile$GPPaggregate<-exact_extract(stacked_raster_rast_mean, single_shapefile, 'mean')
single_shapefile$ricegrowingintensity<-exact_extract(rasterlistCF[[4]], single_shapefile, 'mean')
single_shapefile$clay<-exact_extract(clay_raster, single_shapefile, 'mean')
single_shapefile$soc<-exact_extract(SOC_raster, single_shapefile, 'mean')


ggplot()+
  geom_sf(data = single_shapefile, aes(fill = GPPaggregate)) +
  labs(title = "GPPaggregate", fill = "GPPaggregate") +
  theme_void()

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/GPPpolygon.png",  height=10, width=22, units="in", dpi=150)

single_shapefiledf<-as.data.frame(single_shapefile)
hist(single_shapefiledf$ricegrowingintensity)

plot(single_shapefiledf$ricegrowingintensity, single_shapefiledf$GPPaggregate)
plot(single_shapefiledf$clay$mean.b10, single_shapefiledf$GPPaggregate)
plot(single_shapefiledf$soc$mean.b0, single_shapefiledf$GPPaggregate)




# Assuming you have already created the plot and single_shapefiledf is your data
library(lm)

# Fit the linear regression model
model <- lm(single_shapefiledf$GPPaggregate ~ single_shapefiledf$ricegrowingintensity)

# Extract regression coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Calculate R-squared and p-value
summary(model)  # View detailed summary

# Add the regression line to the plot
abline(intercept, slope, col = "red", lwd = 2)

# Annotate the R-squared and p-value on the plot
text(x = max(single_shapefiledf$ricegrowingintensity), y = max(single_shapefiledf$GPPaggregate) * 0.8,
     paste0("R-squared:", round(summary(model)$r.squared, 3)), col = "red")
text(x = max(single_shapefiledf$ricegrowingintensity), y = max(single_shapefiledf$GPPaggregate) * 0.7,
     paste0("p-value:", round(summary(model)$coefficients[,4], 3)), col = "red")

# Customize the plot further (optional)
title("GPPaggregate vs. Clay Mean B10 with Regression Line")
xlabel("Clay Mean B10")
ylabel("GPPaggregate")

###Export ME
# Save the SpatialPolygon object as a shapefile
writeSpatialShape(polygon, "path/to/your/folder/file_name.shp")
# Assuming "ME" is an sf object (if not, adjust accordingly)
st_write(ME, "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/ME_Shapefile/ME.shp")
# Read the shapefile using st_read
ME2 <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/ME_Shapefile/ME.shp")



########## OLD PLOTTING ####################
nameColor <- bquote(atop(Mean~Cumulative ~GPP~(2008-2020)~(g~C~m^-2~season^-1)~"  "))
my_breaks <- c(0, 800, 1000, 1200, 1600, 2000, 2400)

##applydegree north with function with x axis label
nwbrks <- seq(31,36,1)
nwlbls <- unlist(lapply(nwbrks, function(x) paste(x, "°N")))

cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME2)+
  geom_raster(aes(x=x,y=y,fill=mean_8),data=cleaned_cumulativerasdf)+
  #scale_fill_viridis_c( limits = c(0, 300), option = "turbo", breaks = my_breaks, nameColor, direction = -1, oob = scales::squish)+
  scale_fill_viridis(rescaler = function(x, to = c(0, 0.8), from = NULL) {
    ifelse(x<100, 
           scales::rescale(x,
                           to = c(0,0.2),
                           from = c(min(x, na.rm = TRUE), 500)),
           ifelse(x>1600, 
                  scales::rescale(x,
                                  to = c(0.8, 0.8),
                                  from = c(0.8, 1)), 
                  ifelse(x>100 & x<1600, 
                         scales::rescale(x,
                                         to = to,
                                         from = c(min(x, na.rm = TRUE), 1600)), 0.8)))
    
  },limits = c(0, 2400), option = "turbo", breaks = my_breaks, nameColor, direction = -1) +
  labs(x='Longitude',y='Latitude', color = nameColor)+
  scale_y_continuous(breaks = seq(34, 36, by=1))+
  cowplot::theme_cowplot(font_size = 24)+
  theme(legend.key.width=unit(4,"cm"), legend.spacing.x = unit(2, 'cm'))+
  theme(axis.text = element_text(size = 25))  +
  
  ggsn::north(arkansasshp) +
  ggsn::scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=5, height=0.02,
                 transform = TRUE, model = "WGS84")

cumulativegam<-  ggplot(cleaned_cumulativerasdf, aes(x=y, y=mean_8)) +
  geom_smooth(level = 0.687)+labs(x='Latitude',y=expression(Mean~Cumulative ~GPP~(g~C~m^{-2}~year^{-1})))+
  scale_x_continuous(breaks = nwbrks, labels = nwlbls, expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(33, 36, by=1))+
  
  coord_flip()+
  cowplot::theme_cowplot(font_size = 23)+
  theme(axis.text = element_text(size = 25))  

cumulativearranged<-ggarrange(cumulativemap, cumulativegam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(1.6,1),
                              common.legend = TRUE, legend = "bottom")
cumulativearranged
cumulativearranged

# Load necessary libraries

# Load necessary libraries
library(raster)
library(sf)
library(exactextractr)
library(terra)

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
  
  return(filtered_raster)
}

###### Iterate through each year (2008 to 2020) and filter rasters ######
years <- 2008:2020
filtered_rasters <- list()

for (i in seq_along(years)) {
  # Construct file paths for raster and polygon files
  raster_file <- file.path(raster_folder, paste0("arkansasRice", years[i], "VPMcumulative.tif"))
  polygon_file <- file.path(polygon_folder, paste0("ArkansasRice", years[i], "_Shapefile_corrected.shp"))
  
  # Filter raster based on coverage fraction threshold
  filtered_raster <- filter_raster_based_on_coverage(raster_file, polygon_file, threshold = 0.5)
  
  # Store the filtered raster in the list
  filtered_rasters[[years[i]]] <- filtered_raster
  
  # Save the filtered raster to the output folder
  filtered_raster_path <- file.path(output_folder, paste0("filtered_arkansasRice", years[i], "VPMcumulative.tif"))
  writeRaster(filtered_raster, filename = filtered_raster_path,  overwrite = TRUE)
}

###### Print a message ######
print("Filtering and saving of rasters completed successfully.")

############################################################################## 
############################################################################## 
############################################################################## 
############################################################################## 
###############3 Rice production region########################
##############Merging Shapefile with yield dataset########################
## Read the arkansas shpaefile of the rice production ecological regions######
##############################################################################
############################################################################## 
############################################################################## 
############################################################################## 
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions6regions/RiceProductionRegions.shp")

# Ensure both datasets are in the same CRS
riceproductionregion <- st_transform(riceproductionregion, crs(stacked_raster_rast_mean))
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/CountyShapefileFromArkansasOffice/COUNTY_BOUNDARY.shp"
)
# Extract raster values for each polygon in the shapefile
# Fun can be mean, median, sum, etc. depending on how you want to aggregate
raster_agg <- terra::extract(stacked_raster_rast_mean, riceproductionregion, fun = mean, na.rm = TRUE, exact = TRUE)
# Extract standard deviation without exact=TRUE
raster_agg_sd <- terra::extract(stacked_raster_rast_mean, riceproductionregion, fun = sd, na.rm = TRUE)


# Add the extracted values as a new column in the shapefile
riceproductionregion$GPP_mean <- raster_agg$GPP
riceproductionregion$GPP_SD <- raster_agg_sd$GPP
riceproductionregion$GPP_mean[is.nan(riceproductionregion$GPP_mean)] <- NA

# List of counties where we want to retain the GPP_mean values
retain_counties <- c("Grand Prairie", "White River", "South Delta", "North Delta", "West of Crawleys Ridge", "Middle Delta")

# Update GPP_mean: set it to NA if RiceProduc is not in the retain_counties list
riceproductionregion$GPP_mean <- ifelse(riceproductionregion$layer %in% retain_counties, 
                                        riceproductionregion$GPP_mean, 
                                        NA)
riceproductionregion$GPP_mean <- riceproductionregion$GPP_mean*8
riceproductionregion$GPP_SD <- riceproductionregion$GPP_SD*8
library(ggplot2)
library(dplyr)
library(sf)
library(viridis)  # For the viridis color palette
library(ggspatial)  # For north arrow and scale bar

ggplot() +
  # Plot the base layer for rice production regions
  geom_sf(data = riceproductionregion) +
  # Filter for polygons with non-NA GPP_mean values and fill based on GPP_mean
  geom_sf(data = riceproductionregion %>% dplyr::filter(!is.na(GPP_mean)), aes(fill = GPP_mean)) +
  # Add region labels with a vertical orientation for "West of Crawleys Ridge"
  geom_sf_label(data = riceproductionregion %>% dplyr::filter(!is.na(GPP_mean)), 
                aes(label = layer, angle = ifelse(layer == "West of Crawleys Ridge", 75, 0)), size = 3) +
  # Use viridis color scale for GPP_mean with custom breaks and limits
  scale_fill_viridis_c(option = "magma", 
                       direction = -1, 
                       breaks = c(1600, 1700, 1800, 1900, 2000, 2100), 
                       limits = c(1650, 2100)) +
  # Add labels and formatting
  labs(fill = expression(atop("Mean Cumulative GPP", "(g C m"^ -2~"year"^ -1~")"))) +
  # Use a clean theme
  theme_bw() +
  # Adjust plot margins
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 15),  # Adjust the legend text size
        legend.title = element_text(size = 15)   # Adjust the legend title size
  ) +
  # Adjust legend size (height and width) using guides
  guides(fill = guide_colorbar(
    barwidth = 1.5,               # Increase the width of the color bar
    barheight = 10,               # Increase the height of the color bar
    title.position = "top",       # Position the title at the top
    title.hjust = 0,              # Center the title horizontally
    label.vjust = 1           # Adjust legend label position to balance with the title
  )) +
  # Add a north arrow for orientation
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  # Add a scale bar
  annotation_scale(location = "br", width_hint = 0.4) +
  # Optionally remove axis labels
  labs(x = NULL, y = NULL)



ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/GPPriceproductionregion.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 9,           # Adjust width and height as needed
       height = 6,
       dpi = 300)

# Filter and print GPP_mean for the specified RiceProduc names
regions <- c("Grand Prairie", "White River", "South Delta", "North Delta", "West of Crawleys Ridge", "Middle Delta")

# Assuming riceproductionregion is your data frame
gpp_values <- riceproductionregion[riceproductionregion$RiceProduc %in% regions, c("RiceProduc", "GPP_mean")]

# Print the result
print(gpp_values)
# List of target regions
regions <- c("Grand Prairie", "White River", "South Delta", "North Delta", "West of Crawleys Ridge", "Middle Delta")

# Loop through each region and print a custom message with GPP mean and GPP SD
# Loop through each region and print a custom message with GPP mean and GPP SD
# Loop through each region and print a custom message with GPP mean and GPP SD
for (region in regions) {
  gpp_value <- riceproductionregion$GPP_mean[riceproductionregion$layer == region]
  gpp_sd_value <- riceproductionregion$GPP_SD[riceproductionregion$layer == region]  # Extract GPP_SD
  
  # Round mean GPP and GPP SD, rounding up if 0.5 or greater
  formatted_gpp_value <- round(gpp_value)  
  formatted_gpp_sd_value <- round(gpp_sd_value)  # Round GPP SD
  
  # Create the message with mean GPP and GPP SD
  message <- paste("The GPP_cum_mean of", region, "is", formatted_gpp_value, "±", formatted_gpp_sd_value)
  print(message)
}


library(sf)
library(dplyr)

# Read the shapefiles
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions5regions/RiceProductionRegions.shp"
)
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/CountyShapefileFromArkansasOffice/COUNTY_BOUNDARY.shp"
)

# Ensure both shapefiles use the same coordinate reference system (CRS)
riceproductionregion <- st_transform(riceproductionregion, st_crs(arkansasshp))

# Perform spatial join to find which counties fall in which rice production regions
counties_in_regions <- st_join(arkansasshp, riceproductionregion, right = TRUE)

# Select relevant columns (e.g., county name and region)
result <- counties_in_regions %>%
  dplyr::select(COUNTY, layer)  # Assuming 'NAME' is the county name and 'layer' is the region name

# Filter out rows with NA values in 'layer' column (if needed)
valid_data <- counties_in_regions %>%
  dplyr::filter(!is.na(layer))

# Group by 'layer' and list all counties for each unique layer
regions_with_counties <- valid_data %>%
  dplyr::group_by(layer) %>%
  dplyr::summarise(counties = paste(COUNTY, collapse = ", "))
# Assuming your regions_with_counties is a data frame or tibble
for (i in 1:nrow(regions_with_counties)) {
  region <- regions_with_counties$layer[i]
  counties <- regions_with_counties$counties[i]
  cat(region, "has these counties:", counties, "\n")
}


################################################
######Regionbased analysis########
################################################
# Define the directory containing the EVI files
evi_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/EVI"
# List all .tif files in the directory
evi_files <- list.files(evi_dir, pattern = "\\.tif$", full.names = TRUE)
# Read the shapefile
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions6regions/RiceProductionRegions.shp"
)

# Check and align CRS of shapefile with the raster CRS
first_evi_raster <- terra::rast(evi_files[1])
raster_crs <- terra::crs(first_evi_raster)

if (st_crs(riceproductionregion)$proj4string != raster_crs) {
  riceproductionregion <- st_transform(riceproductionregion, crs = raster_crs)
}

# Convert sf to SpatVector for compatibility with terra
riceproductionregion_terra <- terra::vect(riceproductionregion)
# Initialize an empty list to store results
all_evi_results <- list()
# Loop through each EVI file and calculate mean EVI for each region
for (evi_file in evi_files) {# Read the raster file
  evi_raster <- terra::rast(evi_file) # Extract mean EVI values by region
  mean_evi <- terra::extract(evi_raster, riceproductionregion_terra, fun = mean, na.rm = TRUE)
  # Add region names and the year to the result
  mean_evi$Region <- riceproductionregion$layer
  # Extract the year from the file name (assuming consistent naming)
  year <- gsub(".*(\\d{4})meanEVI\\.tif$", "\\1", evi_file)
  mean_evi$Year <- as.numeric(year)
  # Append to the results list
  all_evi_results[[year]] <- mean_evi
}

# Combine all results into a single data frame
final_evi_df <- do.call(rbind, all_evi_results)

# Organize columns
final_evi_df <- final_evi_df[, c("Year", "Region", "EVI_SG_mean")]
colnames(final_evi_df) <- c("Year", "Region", "Mean_EVI")

# Print the results
print(final_evi_df)

# Optionally save the results to a CSV file
write.csv(final_evi_df, "Mean_EVI_by_Region_2008-2020.csv", row.names = FALSE)


################################
############LSWI################
################################
# Define the directory containing the LSWI files
LSWI_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/LSWI"

# List all .tif files in the directory
LSWI_files <- list.files(LSWI_dir, pattern = "\\.tif$", full.names = TRUE)
LSWI_files
# Read the shapefile
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions6regions/RiceProductionRegions.shp"
)

# Check and align CRS of shapefile with the raster CRS
first_LSWI_raster <- terra::rast(LSWI_files[1])
raster_crs <- terra::crs(first_LSWI_raster)

if (st_crs(riceproductionregion)$proj4string != raster_crs) {
  riceproductionregion <- st_transform(riceproductionregion, crs = raster_crs)
}

# Convert sf to SpatVector for compatibility with terra
riceproductionregion_terra <- terra::vect(riceproductionregion)

# Initialize an empty list to store results
all_LSWI_results <- list()

# Loop through each LSWI file and calculate mean LSWI for each region
for (LSWI_file in LSWI_files) {
  # Read the raster file
  LSWI_raster <- terra::rast(LSWI_file)
  
  # Extract mean LSWI values by region
  mean_LSWI <- terra::extract(LSWI_raster, riceproductionregion_terra, fun = mean, na.rm = TRUE)
  
  # Add region names
  mean_LSWI$Region <- riceproductionregion$layer
  
  # Extract the year from the current file name
  year <- gsub(".*(\\d{4})meanlswi\\.tif$", "\\1", LSWI_file, ignore.case = TRUE)
  mean_LSWI$Year <- as.numeric(year)
  
  # Append to the results list
  all_LSWI_results[[year]] <- mean_LSWI
}


# Combine all results into a single data frame
final_LSWI_df <- do.call(rbind, all_LSWI_results)

# Organize columns
final_LSWI_df <- final_LSWI_df[, c("Year", "Region", "LSWI_SG_mean")]
colnames(final_LSWI_df) <- c("Year", "Region", "Mean_LSWI")

# Print the results
View(final_LSWI_df)

# Optionally save the results to a CSV file
write.csv(final_LSWI_df, "Mean_LSWI_by_Region_2008-2020.csv", row.names = FALSE)

################################
############Temp################
################################
# Define the directory containing the Temp files
Temp_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature"

# List all .tif files in the directory
Temp_files <- list.files(Temp_dir, pattern = "\\.tif$", full.names = TRUE)
Temp_files
# Read the shapefile
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions6regions/RiceProductionRegions.shp"
)

# Check and align CRS of shapefile with the raster CRS
first_Temp_raster <- terra::rast(Temp_files[1])
raster_crs <- terra::crs(first_Temp_raster)

if (st_crs(riceproductionregion)$proj4string != raster_crs) {
  riceproductionregion <- st_transform(riceproductionregion, crs = raster_crs)
}

# Convert sf to SpatVector for compatibility with terra
riceproductionregion_terra <- terra::vect(riceproductionregion)

# Initialize an empty list to store results
all_Temp_results <- list()

# Loop through each Temp file and calculate mean Temp for each region
for (Temp_file in Temp_files) {
  # Read the raster file
  Temp_raster <- terra::rast(Temp_file)
  
  # Extract mean Temp values by region
  mean_Temp <- terra::extract(Temp_raster, riceproductionregion_terra, fun = mean, na.rm = TRUE)
  
  # Add region names
  mean_Temp$Region <- riceproductionregion$layer
  
  # Extract the year from the current file name
  year <- gsub(".*(\\d{4})meantmean\\.tif$", "\\1", Temp_file, ignore.case = TRUE)
  mean_Temp$Year <- as.numeric(year)
  
  # Append to the results list
  all_Temp_results[[year]] <- mean_Temp
}


# Combine all results into a single data frame
final_Temp_df <- do.call(rbind, all_Temp_results)

# Organize columns
final_Temp_df <- final_Temp_df[, c("Year", "Region", "tmean_mean")]
colnames(final_Temp_df) <- c("Year", "Region", "Mean_Temp")

# Print the results
View(final_Temp_df)

# Optionally save the results to a CSV file
write.csv(final_Temp_df, "Mean_Temp_by_Region_2008-2020.csv", row.names = FALSE)

################################
############PAR################
################################
# Define the directory containing the PAR files
PAR_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/PAR"

# List all .tif files in the directory
PAR_files <- list.files(PAR_dir, pattern = "\\.tif$", full.names = TRUE)
PAR_files
# Read the shapefile
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions6regions/RiceProductionRegions.shp"
)

# Check and align CRS of shapefile with the raster CRS
first_PAR_raster <- terra::rast(PAR_files[1])
raster_crs <- terra::crs(first_PAR_raster)

if (st_crs(riceproductionregion)$proj4string != raster_crs) {
  riceproductionregion <- st_transform(riceproductionregion, crs = raster_crs)
}

# Convert sf to SpatVector for compatibility with terra
riceproductionregion_terra <- terra::vect(riceproductionregion)

# Initialize an empty list to store results
all_PAR_results <- list()

# Loop through each PAR file and calculate mean PAR for each region
for (PAR_file in PAR_files) {
  # Read the raster file
  PAR_raster <- terra::rast(PAR_file)
  
  # Extract mean PAR values by region
  mean_PAR <- terra::extract(PAR_raster, riceproductionregion_terra, fun = mean, na.rm = TRUE)
  
  # Add region names
  mean_PAR$Region <- riceproductionregion$layer
  
  # Extract the year from the current file name
  year <- gsub(".*(\\d{4})meanpar\\.tif$", "\\1", PAR_file, ignore.case = TRUE)
  mean_PAR$Year <- as.numeric(year)
  
  # Append to the results list
  all_PAR_results[[year]] <- mean_PAR
}


# Combine all results into a single data frame
final_PAR_df <- do.call(rbind, all_PAR_results)

# Organize columns
final_PAR_df <- final_PAR_df[, c("Year", "Region", "par_mean")]
colnames(final_PAR_df) <- c("Year", "Region", "Mean_PAR")

# Print the results
View(final_PAR_df)

# Optionally save the results to a CSV file
write.csv(final_PAR_df, "Mean_PAR_by_Region_2008-2020.csv", row.names = FALSE)



final_evi_df <- final_evi_df %>% dplyr::filter(Region %in% c("Middle Delta", "Grand Prairie"))
final_LSWI_df <- final_LSWI_df %>% dplyr::filter(Region %in% c("Middle Delta", "Grand Prairie"))
final_Temp_df <- final_Temp_df %>% dplyr::filter(Region %in% c("Middle Delta", "Grand Prairie"))
final_PAR_df <- final_PAR_df %>%dplyr:: filter(Region %in% c("Middle Delta", "Grand Prairie"))


library(ggplot2)
library(patchwork)
library(ggplot2)
library(patchwork)
# Define a base theme with increased font sizes
custom_theme <- theme_minimal(base_size = 12) +  # Increase base font size by 2 points
  theme(
    axis.text.x = element_text(hjust = 1, size = 12),  # Increase axis text size
    axis.title = element_text(size = 12),  # Increase axis title size
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 12),  # Increase legend title size
    plot.caption = element_text(size = 16, hjust = 0.5)  # Increase caption size
  )
#https://craig.rbind.io/post/2021-05-17-asgr-3-1-data-visualization/
# Define the colorblind-friendly palettes
cbp1 <- c("#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

cbp2 <- c("#51127CFF","#FEC287FF", "#FCFDBFFF","#000000", "#EDD9A3", "#F0E442", "#E69F00", "#56B4E9", "#009E73",
          "#0072B2", "#D55E00", "#CC79A7")

# Define a custom label format for the x-axis
custom_labels <- c("Grand Prairie" = "Grand\nPrairie", "Middle Delta" = "Middle\nDelta")

# Create each individual plot, suppressing the legend in all but one plot
plot1 <- ggplot(final_evi_df, aes(x = Region, y = Mean_EVI, fill = Region)) +
  geom_boxplot(color = "#D55E00") +
  labs(x = NULL, y = "Annual Mean EVI") +
  custom_theme +
  scale_fill_manual(values = cbp2) +
  scale_x_discrete(labels = custom_labels) +  # Apply custom x-axis labels
  theme(legend.position = "none")  # Suppress legend for this plot

plot2 <- ggplot(final_LSWI_df, aes(x = Region, y = Mean_LSWI, fill = Region)) +
  geom_boxplot(color = "#D55E00") +
  labs(x = NULL, y = "Annual Mean LSWI") +
  custom_theme +
  scale_fill_manual(values = cbp2) +
  scale_x_discrete(labels = custom_labels) +  # Apply custom x-axis labels
  theme(legend.position = "none")  # Suppress legend for this plot

plot3 <- ggplot(final_Temp_df, aes(x = Region, y = Mean_Temp, fill = Region)) +
  geom_boxplot(color = "#D55E00") +
  labs(x = NULL, y = "Annual Mean Temperature (°C)") +
  custom_theme +
  scale_fill_manual(values = cbp2) +
  scale_x_discrete(labels = custom_labels) +  # Apply custom x-axis labels
  theme(legend.position = "none")  # Suppress legend for this plot

plot4 <- ggplot(final_PAR_df, aes(x = Region, y = Mean_PAR, fill = Region)) +
  geom_boxplot(color = "#D55E00") +
  labs(x = NULL, y = "Annual Mean PAR (μmol m⁻² s⁻¹)") +
  custom_theme +
  scale_fill_manual(values = cbp2) +
  scale_x_discrete(labels = custom_labels) +  # Apply custom x-axis labels
  theme(legend.position = "none")  # Suppress legend for this plot

# Combine the plots into a 2x2 grid
boxplot <-  (plot3 | plot4) /(plot1 | plot2) 

# Add the custom caption for x-axis, centered, below the plots
combined_plot <- boxplot + plot_annotation(
  caption = "Rice Ecological Production Zone",  
  theme = theme(plot.caption = element_text(hjust = 0.5, size = 16))  # Increase caption size
)
combined_plot
# Print the final plot
print(combined_plot)

# Save the combined plot as a PNG file
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regionboxplot.png", 
       plot = combined_plot, 
       width = 10, height = 8, dpi = 300)


#######Combine Rice production region and boxplot##########
riceproductionregion <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/RiceProductionRegions/RiceProductionRegions6regions/RiceProductionRegions.shp")

# Ensure both datasets are in the same CRS
riceproductionregion <- st_transform(riceproductionregion, crs(stacked_raster_rast_mean))
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/CountyShapefileFromArkansasOffice/COUNTY_BOUNDARY.shp"
)
# Extract raster values for each polygon in the shapefile
# Fun can be mean, median, sum, etc. depending on how you want to aggregate
raster_agg <- terra::extract(stacked_raster_rast_mean, riceproductionregion, fun = mean, na.rm = TRUE, exact = TRUE)
# Extract standard deviation without exact=TRUE
raster_agg_sd <- terra::extract(stacked_raster_rast_mean, riceproductionregion, fun = sd, na.rm = TRUE)


# Add the extracted values as a new column in the shapefile
riceproductionregion$GPP_mean <- raster_agg$GPP
riceproductionregion$GPP_SD <- raster_agg_sd$GPP
riceproductionregion$GPP_mean[is.nan(riceproductionregion$GPP_mean)] <- NA

# List of counties where we want to retain the GPP_mean values
retain_counties <- c("Grand Prairie", "White River", "South Delta", "North Delta", "West of Crawleys Ridge", "Middle Delta")

# Update GPP_mean: set it to NA if RiceProduc is not in the retain_counties list
riceproductionregion$GPP_mean <- ifelse(riceproductionregion$layer %in% retain_counties, 
                                        riceproductionregion$GPP_mean, 
                                        NA)
riceproductionregion$GPP_mean <- riceproductionregion$GPP_mean*8
riceproductionregion$GPP_SD <- riceproductionregion$GPP_SD*8

combined_plot


# Load the required package
library(patchwork)

# Create the first plot (your modified GPP_mean map)
gpp_plot <- 
  ggplot() +
  # Plot the base layer for rice production regions
  geom_sf(data = riceproductionregion) +
  # Filter for polygons with non-NA GPP_mean values and fill based on GPP_mean
  geom_sf(data = riceproductionregion %>% dplyr::filter(!is.na(GPP_mean)), aes(fill = GPP_mean)) +
  # Add region labels with a vertical orientation for "West of Crawleys Ridge"
  geom_sf_label(data = riceproductionregion %>% dplyr::filter(!is.na(GPP_mean)), 
                aes(label = layer, angle = ifelse(layer == "West of Crawleys Ridge", 75, 0)), size = 3) +
  # Use viridis color scale for GPP_mean with custom breaks and limits
  scale_fill_viridis_c(option = "magma", 
                       direction = -1, 
                       breaks = c(1600, 1700, 1800, 1900, 2000, 2100), 
                       limits = c(1650, 2100)) +
  # Add labels and formatting
  labs(fill = expression(atop("Mean Cumulative GPP", "(g C m"^ -2~"year"^ -1~")"))) +
  # Use a clean theme
  theme_bw() +
  # Adjust plot margins
  theme(plot.margin = margin(0, 0, 0, 0),
        legend.text = element_text(size = 15),  # Adjust the legend text size
        legend.title = element_text(size = 15)   # Adjust the legend title size
  ) +
  # Adjust legend size (height and width) using guides
  guides(fill = guide_colorbar(
    barwidth = 1.5,               # Increase the width of the color bar
    barheight = 10,               # Increase the height of the color bar
    title.position = "top",       # Position the title at the top
    title.hjust = 0,              # Center the title horizontally
    label.vjust = 1           # Adjust legend label position to balance with the title
  )) +
  # Add a north arrow for orientation
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  # Add a scale bar
  annotation_scale(location = "br", width_hint = 0.4) +
  # Optionally remove axis labels
  labs(x = NULL, y = NULL)




# Arrange the two plots side by side with adjusted width
final_plot <- (gpp_plot + combined_plot) +
  plot_layout(ncol = 2, widths = c(1.3, 1.2)) +
  plot_annotation(tag_levels = 'A')  # Adds "A" and "B" labels automatically

# Print the final combined plot
print(final_plot)

# Save the combined plot as a PNG file
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regionboxplot.png", 
       plot = final_plot, 
       width = 12, height = 8, dpi = 300)



library(dplyr)

# Function to perform an unpaired t-test between Grand Prairie and Middle Delta
perform_t_test <- function(df, variable_name) {
  # Step 1: Filter data for Middle Delta and Grand Prairie
  middle_delta <- df[df$Region == "Middle Delta", ]
  grand_prairie <- df[df$Region == "Grand Prairie", ]
  
  # Step 2: Ensure both regions are aligned by year
  merged_data <- merge(middle_delta, grand_prairie, by = "Year", suffixes = c("_MiddleDelta", "_GrandPrairie"))
  
  # Step 3: Perform an unpaired t-test
  t_test_result <- t.test(
    merged_data[[paste0(variable_name, "_MiddleDelta")]], 
    merged_data[[paste0(variable_name, "_GrandPrairie")]], 
    paired = TRUE
  )
  
  return(t_test_result$p.value)
}

# Perform t-tests for each variable
p_value_EVI <- perform_t_test(final_evi_df, "Mean_EVI")
p_value_Temp <- perform_t_test(final_Temp_df, "Mean_Temp")
p_value_PAR <- perform_t_test(final_PAR_df, "Mean_PAR")
p_value_LSWI <- perform_t_test(final_LSWI_df, "Mean_LSWI")

# Format the output text
text_output <- sprintf(
  "Unpaired t-test between these two regions revealed no significant differences between 
the annual mean temperature (p-value = %.4f), annual mean PAR (p-value = %.4f), 
and annual mean LSWI (p-value = %.4f). However, there has been significant differences 
between EVI values of Grand Prairie regions and Middle Delta regions (p-value = %.4f).",
  p_value_Temp, p_value_PAR, p_value_LSWI, p_value_EVI
)

# Print the final text
cat(text_output)
p_value_EVI
p_value_LSWI

library(dplyr)

final_evi_df %>%
  group_by(Region) %>%
  summarise(
    Min_EVI = min(Mean_EVI, na.rm = TRUE),
    Mean_EVI_EVI = mean(Mean_EVI, na.rm = TRUE),
    Max_EVI = max(Mean_EVI, na.rm = TRUE)
  )
final_evi_df %>%
  group_by(Region) %>%
  summarise(Unique_EVI = list(sort(unique(Mean_EVI)) )) %>%
  pull(Unique_EVI)


library(dplyr)

# Extract EVI statistics
evi_stats <- final_evi_df %>%
  group_by(Region) %>%
  summarise(
    Min_EVI = min(Mean_EVI, na.rm = TRUE),
    Mean_EVI = mean(Mean_EVI, na.rm = TRUE),
    Max_EVI = max(Mean_EVI, na.rm = TRUE)
  )

# Extract LSWI statistics
lswi_stats <- final_LSWI_df %>%
  group_by(Region) %>%
  summarise(
    Min_LSWI = min(Mean_LSWI, na.rm = TRUE),
    Mean_LSWI = mean(Mean_LSWI, na.rm = TRUE),
    Max_LSWI = max(Mean_LSWI, na.rm = TRUE)
  )

# Extract Temperature statistics
temp_stats <- final_Temp_df %>%
  group_by(Region) %>%
  summarise(
    Min_Temp = min(Mean_Temp, na.rm = TRUE),
    Mean_Temp = mean(Mean_Temp, na.rm = TRUE),
    Max_Temp = max(Mean_Temp, na.rm = TRUE)
  )

# Extract PAR statistics
par_stats <- final_PAR_df %>%
  group_by(Region) %>%
  summarise(
    Min_PAR = min(Mean_PAR, na.rm = TRUE),
    Mean_PAR = mean(Mean_PAR, na.rm = TRUE),
    Max_PAR = max(Mean_PAR, na.rm = TRUE)
  )

# Format the text
text_output <- sprintf(
  "The average annual mean EVI of Grand Prairie and Middle Delta across 13 years were %.2f (%.2f-%.2f) and %.2f (%.2f-%.2f) respectively. 
The average annual mean LSWI of Grand Prairie and Middle Delta across 13 years were %.2f (%.2f-%.2f) and %.2f (%.2f-%.2f) respectively. 
The average annual mean temperature of Grand Prairie and Middle Delta across 13 years were %.2f (%.2f-%.2f) and %.2f (%.2f-%.2f) respectively. 
The average annual mean PAR of Grand Prairie and Middle Delta across 13 years were %.2f (%.2f-%.2f) and %.2f (%.2f-%.2f) respectively.",
  
  evi_stats$Mean_EVI[1], evi_stats$Min_EVI[1], evi_stats$Max_EVI[1], 
  evi_stats$Mean_EVI[2], evi_stats$Min_EVI[2], evi_stats$Max_EVI[2],
  
  lswi_stats$Mean_LSWI[1], lswi_stats$Min_LSWI[1], lswi_stats$Max_LSWI[1], 
  lswi_stats$Mean_LSWI[2], lswi_stats$Min_LSWI[2], lswi_stats$Max_LSWI[2],
  
  temp_stats$Mean_Temp[1], temp_stats$Min_Temp[1], temp_stats$Max_Temp[1], 
  temp_stats$Mean_Temp[2], temp_stats$Min_Temp[2], temp_stats$Max_Temp[2],
  
  par_stats$Mean_PAR[1], par_stats$Min_PAR[1], par_stats$Max_PAR[1], 
  par_stats$Mean_PAR[2], par_stats$Min_PAR[2], par_stats$Max_PAR[2]
)

# Print the final text
cat(text_output)

par_stats
