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
library(dplyr)
library(conflicted)
library(exactextractr)
library(ggspatial)
library(osmdata)

#### all raster using rast function
#first import all files in a single folder as a list 
raster_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
rastlist <- list.files(path = raster_dir, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
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
nameColor <- bquote(atop(Mean~Cumulative ~GPP~(2008-2020)~(g~C~m^-2~year^-1)~"  "))
my_breaks <- c(0, 800, 1000, 1200, 1600, 2000, 2400)

##applydegree north with function with x axis label
nwbrks <- seq(31,36,1)
nwlbls <- unlist(lapply(nwbrks, function(x) paste(x, "°N")))

cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME2)+
  geom_raster(aes(x=x,y=y,fill=mean_8),data=cleaned_cumulativerasdf)+
  #scale_fill_viridis_c( limits = c(0, 300), option = "turbo", breaks = my_breaks, nameColor, direction = -1, oob = scales::squish)+
  scale_fill_viridis( option = "turbo", nameColor, direction = -1) +
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

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMcumulativearrangedsameextent.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMcumulativearranged.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)

# Filter data based on latitude range
mean_gpp33.534.5 <- mean(subset(cleaned_cumulativerasdf, y >= 33.5 & y <= 34.5)$mean_8)
sd_gpp33.534.5 <- sd(subset(cleaned_cumulativerasdf, y >= 33.5 & y <= 34.5)$mean_8)
# Display the mean GPP value
mean_gpp33.534.5
sd_gpp33.534.5
mean_gpp<-mean(cleaned_cumulativerasdf$mean_8)
sd_gpp<-sd(cleaned_cumulativerasdf$mean_8)

print(paste("The spatial distribution of GPP has shown that rice fields located between 33.5° N and 34.5° N have higher GPP values. Mean GPP in this range:", mean_gpp33.534.5))
print(paste("At the state scale, in the timeframe between 2008 to 2020, the mean photosynthetic carbon uptake of Arkansas rice fields was :", mean_gpp))
sd_gpp
sd_gpp33.534.5
###Lowest
# Calculate mean and standard deviation of mean_8 for latitude range 36 to 35.5
mean_gpp_36_35.5 <- mean(subset(cleaned_cumulativerasdf, y >= 35.5 & y <= 36)$mean_8)
sd_gpp_36_35.5 <- sd(subset(cleaned_cumulativerasdf, y >= 35.5 & y <= 36)$mean_8)

# Display the mean and standard deviation
print(mean_gpp_36_35.5)
print(sd_gpp_36_35.5)



### export the cumulative raster data for the soil clay content data
# Specify the output file path and name (replace with your desired path and name)
output_file <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM-alltogether/arkansasRice20082020VPMcumulative.tif"
# Use writeRaster to export the raster
writeRaster(meancumulativeNArmtrue, filename = output_file, format = "GTiff", overwrite = TRUE)


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
