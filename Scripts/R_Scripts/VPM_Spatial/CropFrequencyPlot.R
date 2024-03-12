library(raster)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(sf)
library(terra)
library(lubridate)
library(maptools)  ## For wrld_simpl
library(sp)
library(rgdal)
library(cowplot)
library(ggsn)
library(ggpubr)
library(gridExtra)
library(grid)
library(rasterVis)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(RColorBrewer)
library(magrittr)
library(ggpointdensity)

dirCF<- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CropFrequency"
dirraster <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM" ##VPM images

filesCF<- list.files(dirCF, pattern = "\\.tif$", full.names = TRUE)
filesraster <- list.files(dirraster, pattern = "\\.tif$", full.names = TRUE)
rasterlistCF<-vector ("list", 6)

for (i in 1:6){
    rasterlistCF[i]<-rast(filesCF[i])
}

### Get the raster images
# Define file paths for your rasters

### One raster file to convert the spatial non-temporal data to MODIS projection and upscaling
### File : stacked_raster_rast_mean from VPMMeanRasterImageAnalysis2008_2020
stacked_raster_rast_mean
gppaggregate
projected_CF<-vector("list", 6)
for (i in 1:6){
  projected_CF[i]<- terra::project(rasterlistCF[[i]], gppaggregate, method = "average")
}

projected_CF_df<-vector("list", 6)
for (i in 1:6){
  projected_CF_df[[i]] <- terra::as.data.frame(projected_CF[[i]], xy=TRUE)
}

plot(projected_CF[[4]])

####Merge the files
merged_CF_df <- projected_CF_df[[1]]
# Iterate through the remaining DataFrames and merge them
for (i in 2:6) {
  merged_CF_df <- merge(merged_CF_df, projected_CF_df[[i]], by = c("x", "y"), all = TRUE)
}

### One raster file to convert the spatial non-temporal data to MODIS projection and upscaling
### File : meancumulativeNArmtrue from VPMMeanRasterImageAnalysis2008_2020
merged_CF_df_GPP<- left_join(cumulativerasdf, merged_CF_df, by = c("x" = "x", "y" = "y"))
plot(merged_CF_df_GPP$ar_rice, merged_CF_df_GPP$GPP)

### Plot with custom labels and title
plot(merged_CF_df_GPP$ar_rice, merged_CF_df_GPP$mean_8,
     main = "Rice Grown Frequency vs. Mean GPP",
     xlab = "Rice Grown Frequency",
     ylab = "Mean GPP (units)",
     pch = 19,  # Choose a suitable plot character (e.g., circle, triangle)
     col = "blue",  # Set a distinctive color for the plot points
     cex = 0.7)  # Adjust point size for better visibility)


#### Analysis on the low and high regions
View(merged_df_df_slope)
merged_CF_df_GPP

merged_CF_df_GPP_slope<- merge(merged_df_df_slope_pvalue, merged_CF_df_GPP, by = c("x" = "x", "y" = "y"))
View(merged_CF_df_GPP_slope)

plot(merged_CF_df_GPP_slope$ar_rice, merged_CF_df_GPP_slope$slope)
plot(merged_CF_df_GPP_slope$ar_soy, merged_CF_df_GPP_slope$slope)
plot(merged_CF_df_GPP_slope$ar_dblWhtSoy, merged_CF_df_GPP_slope$slope)
plot(merged_CF_df_GPP_slope$ar_corn, merged_CF_df_GPP_slope$slope)
plot(merged_CF_df_GPP_slope$ar_wht, merged_CF_df_GPP_slope$slope)

## replace function to specifically target and replace 0s with NA values.
mergedCF_Raster<-as.data.frame(rasterlistCF[[4]]) 
merged_CF_df[merged_CF_df == 0] <- NA
merged_CF_df_NA <- na.omit(merged_CF_df)
###plot dual plots
cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=ar_rice),data=mergedCF_Raster)+
  #scale_fill_viridis_c( limits = c(0, 300), option = "turbo", breaks = my_breaks, nameColor, direction = -1, oob = scales::squish)+
  scale_fill_viridis(limits = c(0, 12), option = "turbo", nameColor, direction = -1) +
  labs(x='Longitude',y='Latitude', color = nameColor)+
  scale_y_continuous(breaks = seq(34, 36, by=1))+
  cowplot::theme_cowplot(font_size = 24)+
  theme(legend.key.width=unit(4,"cm"), legend.spacing.x = unit(2, 'cm'))+
  theme(axis.text = element_text(size = 25))  +
  
  ggsn::north(arkansasshp) +
  ggsn::scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=5, height=0.02,
                 transform = TRUE, model = "WGS84")

cumulativegam<-  ggplot(mergedCF_Raster, aes(x=y, y=ar_rice)) +
  geom_smooth(level = 0.687)+labs(x='Latitude',y=expression("Crop Frequency"))+
  scale_x_continuous(breaks = nwbrks, labels = nwlbls, expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(33, 36, by=1))+
  
  coord_flip()+
  cowplot::theme_cowplot(font_size = 23)+
  theme(axis.text = element_text(size = 25))  

cumulativearranged<-ggarrange(cumulativemap, cumulativegam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(1.6,1),
                              common.legend = TRUE, legend = "bottom")
cumulativearranged

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/CropFrequency.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)


