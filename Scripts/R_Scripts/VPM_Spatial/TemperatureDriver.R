### ### VPM
###Load all the required libraries
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



### path of the images (list.files (base function) takes the name of the files)
## lapply(base): Apply a Function over a List or Vector
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature"
setwd(path)

rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature", pattern='.tif$', 
                                 all.files=TRUE, full.names=FALSE)
allrasterscumulative <- lapply(rastlistcumulative, raster)


## Clip the raster image
alignExtent(allrasterscumulative, object, snap='near')
same_as_r1 <- sapply(allrasterscumulative, function(x) extent(x) == extent(allrasterscumulative[[1]]))
## Create one raster image taking the mean
extent(allrasterscumulative[[1]])
st_ext <- extent(-94.86209 , -89.14432 , 32.77952 , 36.67372 )
allrasterscumulativeextent <- vector("list", 13)
for(i in 1:13){
  allrasterscumulativeextent[[i]] = setExtent(allrasterscumulative[[i]], st_ext)
}

for(i in 1:13){
  allrasterscumulativeextent[[i]] = resample(allrasterscumulative[[i]], allrasterscumulative[[1]])
}
allrasterscumulativeextent

allrasterscumulativestack<-stack(allrasterscumulativeextent)

meancumulative <- calc(allrasterscumulativestack, fun = mean)

meancumulativeNArmtrue <- calc(allrasterscumulativestack, fun = mean, na.rm=TRUE)

### export the cumulative raster data for the soil clay content data
# Specify the output file path and name (replace with your desired path and name)
output_file <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature/MeanTemperature/arkansasRice20082020temperature.tif"

# Use writeRaster to export the raster
writeRaster(meancumulativeNArmtrue, filename = output_file, format = "GTiff", overwrite = TRUE)

# Print a message indicating successful export
cat("Raster exported to:", output_file, "\n")




### upscale the 30m data to 500m
### plot gpp vs clay content
## directory of the clay content file
# Specify the directory and file name
#dir <- "/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Data/VPM/SoilClay" ## mac directory
dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature/MeanTemperature" ## windows directory
dircumulative <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM-alltogether"


# Specify the file names
file_name_temperature <- "arkansasRice20082020temperature.tif"
file_name_cumulative <- "arkansasRice20082020VPMcumulative.tif"

# Combine the directory and file names
raster_path_temperature <- file.path(dir, file_name_temperature)
raster_path_cumulative <- file.path(dircumulative, file_name_cumulative)

# Read the raster images
temperature_raster <- rast(raster_path_temperature)
cumulative_raster <- rast(raster_path_cumulative)

# Convert 'clay_raster' to a terra object
terra_temperature <- rast(temperature_raster)
terra_temperature <-temperature_raster

# Project 'clay_raster' to the grid of 'cumulative_raster'
projected_temperature<- terra::project(terra_temperature, cumulative_raster, method = "average")


# Convert projected raster to dataframe
#projected_df <- terra::as.data.frame(projected_clay)
projected_df <- terra::as.data.frame(projected_temperature, xy=TRUE)

# Convert cumulative raster to dataframe
#cumulative_df <- terra::as.data.frame(cumulative_raster)
cumulative_df <- terra::as.data.frame(cumulative_raster, xy=TRUE)

projected_cumulativedf<- left_join(cumulative_df, projected_df, by = c("x" = "x", "y" = "y"))

#3 Multiply by 8
projected_cumulativedf$layer_8<- projected_cumulativedf$layer.x*8

# Create a scatter plot
ggscatter(projected_cumulativedf, x = "layer.y", y = "layer_8", add = "reg.line", size =3) +
  stat_cor(label.x = 18, label.y = 2500, size =8) +
  stat_regline_equation(label.x = 20, label.y = 2600, size =8)+
  #scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Mean Temperature (deg Celsius)")+
  font("xy.text", size = 24)+
  font("xlab", size = 24)+
  font("ylab", size = 24)+
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1,")")))
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/TemperatureR.png",width = 14, height = 8 )



# Create a scatter plot
ggscatter(projected_cumulativedf, x = "layer.y", y = "layer_8", add = "reg.line", size = 2) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 18, label.y =2600, size =8
  )+
  stat_regline_equation(label.x = 18, label.y = 2800, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  xlab("Mean Temperature (°C)") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1,")")))

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/TemperatureR2.png",width = 14, height = 8 )
projected_cumulativedf$layer.y

projected_cumulativedf

sumcumulative <- calc(allrasterscumulativestack, fun = sum)
plot(sumcumulative)



##Plot multiple raster
## Multiple raster stack file = allrasterscumulativestack
r_stack_df <- as.data.frame(allrasterscumulativestack, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

ggplot() + 
  geom_raster(data = r_stack_df, 
              aes(x = x, y = y, fill = value), alpha = 0.2) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal() + 
  theme_minimal() 


### Create mean raster data
cumulativerasdf <- as.data.frame(meancumulativeNArmtrue,xy=TRUE)%>%drop_na()

## multiply by 8
##cumulativerasdf$layer<-cumulativerasdf$layer*8

## Plotting cumulative old technique
plot(meancumulative)
nameColor <- bquote(atop(Mean~Cumulative ~GPP~(2008-2020)~(g~C~m^-2~season^-1)~"  "))



my_breaks <- c(0, 800, 1000, 1200, 1600, 2000, 2400)

##applydegree north with function with x axis label
nwbrks <- seq(31,36,1)
nwlbls <- unlist(lapply(nwbrks, function(x) paste(x, "°N")))


cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=layer),data=cumulativerasdf)+
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
  
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=5, height=0.02,
           transform = TRUE, model = "WGS84")

cumulativegam<-  ggplot(cumulativerasdf, aes(x=y, y=layer)) +
  geom_smooth(level = 0.687)+labs(x='Latitude',y=expression(Mean~Cumulative ~GPP~(g~C~m^{-2}~season^{-1})))+
  scale_x_continuous(breaks = nwbrks, labels = nwlbls, expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(33, 36, by=1))+
  
  coord_flip()+
  cowplot::theme_cowplot(font_size = 23)+
  theme(axis.text = element_text(size = 25))  

cumulativearranged<-ggarrange(cumulativemap, cumulativegam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(1.6,1),
                              common.legend = TRUE, legend = "bottom")
cumulativearranged
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMcumulativearranged.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)


##CUMULATIVE WITH ERROR BAR
cumulativerasdf$lat <- cumulativerasdf$y
cumulativerasdf$lat<-format(round(cumulativerasdf$lat, 1), nsmall = 1)
cumulativerasdf$lat<- as.character(cumulativerasdf$lat)

Data_summary <- aggregate(layer ~ lat, cumulativerasdf,       # Create summary data
                          function(x) c(mean = mean(x),
                                        se = sd(x) / sqrt(length(x))))
data_summary <- data.frame(group = Data_summary[ , 1], Data_summary$layer)
# Print summary data

data_summary <-data.frame(unclass((data_summary)), check.names = FALSE, stringsAsFactors = FALSE)


data_summary