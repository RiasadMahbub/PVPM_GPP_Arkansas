### VPM
###Load all the required libraries
library(raster)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
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
library(raster)
library(conflicted)
library(sf)
library(exactextractr)



### path of the images (list.files (base function) takes the name of the files)
## lapply(base): Apply a Function over a List or Vector
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
setwd(path)

rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/", pattern='.tif$', 
                                 all.files=TRUE, full.names=FALSE)
allrasterscumulative <- lapply(rastlistcumulative, raster)


## Clip the raster images
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
allrasterscumulativestack<-stack(allrasterscumulativeextent)
meancumulative <- calc(allrasterscumulativestack, fun = mean)
meancumulativeNArmtrue <- calc(allrasterscumulativestack, fun = mean, na.rm=TRUE)
plot(meancumulativeNArmtrue)
outdir<- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CodeCreatedFile/RasterCumulativeFile2008_2020/meancumulativeNArmtrue.tiff"
# Write the raster as TIFF
writeRaster(meancumulativeNArmtrue, outdir, format = "GTiff", driver = "GTiff")
plot(meancumulativeNArmtrue)
sumcumulative <- calc(allrasterscumulativestack, fun = sum, na.rm=TRUE)
plot(sumcumulative)
outdir<- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CodeCreatedFile/RasterCumulativeFile2008_2020/meancumulativeNArmtrue.tiff"



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
cumulativerasdf$layer<-cumulativerasdf$layer*8

# Define output file path
output_file <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CodeCreatedFile/CsvCumulative2008_2020/CSVCumulative2008_2020.csv"
write.csv(cumulativerasdf, file = output_file, row.names = FALSE)


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

### export the cumulative raster data for the soil clay content data
# Specify the output file path and name (replace with your desired path and name)
output_file <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM-alltogether/arkansasRice20082020VPMcumulative.tif"

# Use writeRaster to export the raster
writeRaster(meancumulativeNArmtrue, filename = output_file, format = "GTiff", overwrite = TRUE)

# Print a message indicating successful export
cat("Raster exported to:", output_file, "\n")

r_multiple_df_filtered <- r_multiple_df %>% dplyr::select(x, y, slope)
#### plot the slope

### stacked raster to multiple column dataframe to calculate the slope
r_multiple_df <- as.data.frame(allrasterscumulativestack, xy = TRUE)
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



View(r_multiple_df)
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
hist(r_multiple_df_filtered$slope)



library(ggplot2)
library(ggspatial)
library(osmdata)

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

r_multiple_df_pvalue_filtered

nrow(r_multiple_df_pvalue)

table(is.na(r_multiple_df_pvalue$gpp_sum.13))
table(is.na(r_multiple_df_pvalue$slope))
table(is.na(sloperasterdflist[[13]]$`2020AnnualGPP`))
table(is.na(sloperasterdflist[[13]]$`2020AnnualGPP`))


#### all raster using rast function

#first import all files in a single folder as a list 
raster_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"

rastlist <- list.files(path = raster_dir, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
setwd(raster_dir)
#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
stacked_raster <- stack(allrasters) 
stacked_raster_rast<-rast(stacked_raster)
#raster::calc
stacked_raster_rast_mean <- terra::app(stacked_raster_rast, "mean", na.rm=TRUE)
names(stacked_raster_rast_mean) <- "GPP"


plot(stacked_raster_rast_mean, main= "GPP/8")
### Create mean raster data
cumulativerasdf <- as.data.frame(stacked_raster_rast_mean,xy=TRUE)%>%drop_na()

## multiply by 8
cumulativerasdf$mean_8<-cumulativerasdf$mean*8


cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=mean_8),data=cumulativerasdf)+
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

cumulativegam<-  ggplot(cumulativerasdf, aes(x=y, y=mean_8)) +
  geom_smooth(level = 0.687)+labs(x='Latitude',y=expression(Mean~Cumulative ~GPP~(g~C~m^{-2}~season^{-1})))+
  scale_x_continuous(breaks = nwbrks, labels = nwlbls, expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(33, 36, by=1))+
  
  coord_flip()+
  cowplot::theme_cowplot(font_size = 23)+
  theme(axis.text = element_text(size = 25))  

cumulativearranged<-ggarrange(cumulativemap, cumulativegam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(1.6,1),
                              common.legend = TRUE, legend = "bottom")
cumulativearranged

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMcumulativearrangedsameextent.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)


### merge all the shapefiles
library(sf)
# Specify the folder path
folder_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllMergedTogether"

# Read the shapefile
single_shapefile <- st_read(folder_path)


plot(stacked_raster_rast_mean)
plot(gppaggregate)
hist(gppaggregate)
cumulativerasdf <- as.data.frame(gppaggregate,xy=TRUE)%>%drop_na()

single_shapefile$GPPaggregate<-exact_extract(stacked_raster_rast_mean, single_shapefile, 'mean')
single_shapefile$ricegrowingintensity<-exact_extract(rasterlistCF[[4]], single_shapefile, 'mean')
single_shapefile$clay<-exact_extract(clay_raster, single_shapefile, 'mean')
single_shapefile$soc<-exact_extract(SOC_raster, single_shapefile, 'mean')


plot(single_shapefile)

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
model <- lm(single_shapefiledf$GPPaggregate ~ single_shapefiledf$clay$mean.b10)

# Extract regression coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

# Calculate R-squared and p-value
summary(model)  # View detailed summary

# Add the regression line to the plot
abline(intercept, slope, col = "red", lwd = 2)

# Annotate the R-squared and p-value on the plot
text(x = max(single_shapefiledf$clay$mean.b10), y = max(single_shapefiledf$GPPaggregate) * 0.8,
     paste0("R-squared:", round(summary(model)$r.squared, 3)), col = "red")
text(x = max(single_shapefiledf$clay$mean.b10), y = max(single_shapefiledf$GPPaggregate) * 0.7,
     paste0("p-value:", round(summary(model)$coefficients[,4], 3)), col = "red")

# Customize the plot further (optional)
title("GPPaggregate vs. Clay Mean B10 with Regression Line")
xlabel("Clay Mean B10")
ylabel("GPPaggregate")

slope
