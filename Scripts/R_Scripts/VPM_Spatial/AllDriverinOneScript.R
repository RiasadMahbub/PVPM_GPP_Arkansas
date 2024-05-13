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
library(exactextractr)
library(base)
library("plot3D")

#### GPP
raster_dir <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
rastlist <- list.files(path = raster_dir, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
ME2 <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/ME_Shapefile/ME.shp")
setwd(raster_dir)
allrasters <- lapply(rastlist, raster)#import all raster files in folder using lapply
stacked_raster <- stack(allrasters) ### all raster stack
stacked_raster_rast<-rast(stacked_raster)
stacked_raster_rast_mean <- terra::app(stacked_raster_rast, "mean", na.rm=TRUE)

# SOIL 
dirclay <- "C://Users//rbmahbub//Box//Research//Data//VPM//SoilClay" ## windows directory
dirSOC <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/SoilOrganicCarbon"## soilorganiccarbon
file_name_clay <- "ClayContentArkansas.tif"
file_name_SOC <- "SoilOrganicCarbonArkansas.tif"

# List all files in the directory with a .tif extension
files <- list.files(dirraster, pattern = "\\.tif$", full.names = TRUE)
raster_path_clay <- file.path(dirclay, file_name_clay)
clay_raster <- rast(raster_path_clay)
raster_path_SOC <- file.path(dirSOC, file_name_SOC)
SOC_raster <- rast(raster_path_SOC)
clay_raster_b0<-clay_raster$b0
clay_raster_b10<-clay_raster$b10
clay_raster_b30<- clay_raster$b30
clay_raster_meanb30<-mean(clay_raster$b0, clay_raster$b10, clay_raster$b30)


#####CROP FREQUENCY################
dirCF<- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CropFrequency"
dirraster <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM" ##VPM images

filesCF<- list.files(dirCF, pattern = "\\.tif$", full.names = TRUE)
filesraster <- list.files(dirraster, pattern = "\\.tif$", full.names = TRUE)
rasterlistCF<-vector ("list", 6)

for (i in 1:6){
  rasterlistCF[i]<-rast(filesCF[i])
}


#####EVI##############
dirEVI <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/EVI" ## # Specify the directory
filesEVI <- list.files(path = dirEVI, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE) # List all files in the directory with a .tif extension
setwd(dirEVI)
allrastersEVI <- lapply(filesEVI, raster)#import all raster files in folder using lapply
stacked_rasterEVI <- stack(allrastersEVI) ### all raster stack
stacked_raster_rastEVI<-rast(stacked_rasterEVI)
stacked_raster_rast_meanEVI <- terra::app(stacked_raster_rastEVI, "mean", na.rm=TRUE)

dirLSWI <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/LSWI" ## # Specify the directory
filesLSWI <- list.files(path = dirLSWI, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE) # List all files in the directory with a .tif extension
setwd(dirLSWI)
allrastersLSWI <- lapply(filesLSWI, raster)#import all raster files in folder using lapply
stacked_rasterLSWI <- stack(allrastersLSWI) ### all raster stack
stacked_raster_rastLSWI<-rast(stacked_rasterLSWI)
stacked_raster_rast_meanLSWI <- terra::app(stacked_raster_rastLSWI, "mean", na.rm=TRUE)

names(stacked_raster_rast_meanLSWI) <- "meanLSWI"
names(stacked_raster_rast_meanEVI) <- "meanEVI"

# Specify the folder path
folder_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/ArkansasRiceClipped/AllMergedTogether"

custom_mean <- function(values, coverage_fractions) {
  covf <- coverage_fractions[!is.na(values)]
  vals <- values[!is.na(values)]
  return(round(sum(vals * covf) / sum(covf), 2))
}

# Read the shapefile
single_shapefile <- st_read(folder_path)
single_shapefile$GPPaggregate<-exactextractr::exact_extract(stacked_raster_rast_mean, single_shapefile, fun = custom_mean) ###GPP
single_shapefile$ricegrowingintensity<-exact_extract(rasterlistCF[[4]], single_shapefile, fun = custom_mean) ###CropIntensity
#single_shapefile$clayb0<-exact_extract(clay_raster_b0, single_shapefile, 'mean') ### Clay Raster
#single_shapefile$clayb10<-exact_extract(clay_raster_b10, single_shapefile, 'mean') ### Clay Raster
#single_shapefile$clayb30<-exact_extract(clay_raster_b30, single_shapefile, 'mean') ### Clay Raster
single_shapefile$clayb01030<-exactextractr::exact_extract(clay_raster_meanb30, single_shapefile,  fun = custom_mean) ### Clay Raster
#single_shapefile$soc<-exact_extract(SOC_raster, single_shapefile, fun = custom_mean) ### SOC raster
single_shapefile$EVI<-exact_extract(stacked_raster_rast_meanEVI, single_shapefile, fun = custom_mean) ### SOC raster
single_shapefile$LSWI<-exact_extract(stacked_raster_rast_meanLSWI, single_shapefile, fun = custom_mean) ### SOC raster

st_write(single_shapefile, "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/SingleShapefileAlldataframes.shp")
# Read the shapefile using st_read
single_shapefile <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/SingleShapefileAlldataframes.shp")

###FIlter GPP based on 3 SD
single_shapefiledf<-as.data.frame(single_shapefile)
write.csv(single_shapefiledf, "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/singleshapefiledf.csv")

# Calculate the mean and standard deviation of GPPaggregate with na.rm = TRUE
mean_gppaggregate <- mean(single_shapefiledf$GPPggrg, na.rm = TRUE)
sd_gppaggregate <- sd(single_shapefiledf$GPPggrg, na.rm = TRUE)
# Calculate the upper and lower bounds for filtering
upper_bound <- mean_gppaggregate + 3 * sd_gppaggregate
lower_bound <- mean_gppaggregate - 3 * sd_gppaggregate
# Filter out rows outside the bounds, ignoring NAs in calculations
filtered_df <- single_shapefiledf[single_shapefiledf$ricegrowingintensity >= lower_bound &
                                    single_shapefiledf$ricegrowingintensity <= upper_bound, ]
# Print the filtered dataframe
# Assuming df is your dataframe
filtered_df_no_na <- na.omit(filtered_df)
### Residual Plots
GPPclay.lm = lm(GPPggrg ~ cl01030, data=filtered_df_no_na) 
GPPcf.lm<-lm(GPPggrg ~ ricegrowingintensity, data=filtered_df_no_na)
filtered_df_no_na$predGPPclay<-predict(GPPclay.lm)
filtered_df_no_na$predGPPCF<-predict(GPPcf.lm)
filtered_df_no_na$residGPPClay<-filtered_df_no_na$GPPggrg-filtered_df_no_na$predGPPclay
filtered_df_no_na$residGPPCF<-filtered_df_no_na$GPPggrg-filtered_df_no_na$predGPPCF

EVIclay.lm = lm(EVI ~ clayb01030, data=filtered_df_no_na) 
filtered_df_no_na$predEVIclay<-predict(EVIclay.lm)
filtered_df_no_na$residEVIClay<-filtered_df_no_na$EVI-filtered_df_no_na$predEVIclay

########################
# slope graph
###################
library(ggplot2)
library(ggpmisc)

### High Low Regions
# Read the shapefile using st_read
single_shapefile <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/SingleShapefileAlldataframes.shp")
single_shapefiledf<-as.data.frame(single_shapefile)
single_shapefile$EVImultiyear<-exact_extract(stacked_rasterEVI, single_shapefile, 'mean')
single_shapefile$GPPmultiyear<-exact_extract(stacked_raster_rast, single_shapefile, 'mean') 
EVIdataframe<-single_shapefile$EVImultiyear
GPPdataframe<-single_shapefile$GPPmultiyear
EVIdataframe<-as.data.frame(EVIdataframe)
GPPdataframe<-as.data.frame(GPPdataframe)
EVIdataframe$geometry<-(single_shapefiledf$geometry)
GPPdataframe$geometry<-(single_shapefiledf$geometry)
GPPdataframe$rcgrwng <-single_shapefiledf$rcgrwng
EVIdataframe$rcgrwng <-single_shapefiledf$rcgrwng

columns_to_EVI <- c( "mean.EVI_SG_mean.1", "mean.EVI_SG_mean.2" , "mean.EVI_SG_mean.3", 
                          "mean.EVI_SG_mean.4","mean.EVI_SG_mean.5" , "mean.EVI_SG_mean.6" ,
                           "mean.EVI_SG_mean.7", "mean.EVI_SG_mean.8" , "mean.EVI_SG_mean.9", 
                          "mean.EVI_SG_mean.10","mean.EVI_SG_mean.11", "mean.EVI_SG_mean.12",
                           "mean.EVI_SG_mean.13")
columns_to_GPP <- c( "mean.gpp_sum.1",  "mean.gpp_sum.2"  ,"mean.gpp_sum.3",  "mean.gpp_sum.4" ,
                      "mean.gpp_sum.5",  "mean.gpp_sum.6"  ,"mean.gpp_sum.7",  "mean.gpp_sum.8" ,
                      "mean.gpp_sum.9" , "mean.gpp_sum.10", "mean.gpp_sum.11", "mean.gpp_sum.12",
                     "mean.gpp_sum.13")

# Remove rows where at least 11 columns have NA values
EVIdataframehl <- filter(EVIdataframe, rcgrwng >5)
GPPdataframehl <- filter(GPPdataframe, rcgrwng >5)

# Remove columns with 12 or more NaN values in each row
GPPdataframehl <- GPPdataframehl[rowSums(is.na(GPPdataframehl)) < 12, ]
EVIdataframehl <- EVIdataframehl[rowSums(is.na(EVIdataframehl)) < 12, ]


# Function to calculate slope and p-value for each row
calculate_slope_pvalue <- function(row) {
  model <- lm(unlist(row) ~ c(1:length(row)))  # Fit linear model
  slope <- model$coefficients[[2]]              # Extract slope
  p_value <- summary(model)$coefficients[2, 4]  # Extract p-value
  return(c(slope, p_value))
}

# Apply the function to each row and add as new columns
GPPdataframehl_slope <- GPPdataframehl %>%
  rowwise() %>%
  dplyr::mutate(slope = calculate_slope_pvalue(c_across(all_of(columns_to_GPP)))[1],
         p_value = calculate_slope_pvalue(c_across(all_of(columns_to_GPP)))[2])

EVIdataframehl_slope <- EVIdataframehl %>%
  rowwise() %>%
  dplyr::mutate(slopeEVI = calculate_slope_pvalue(c_across(all_of(columns_to_EVI)))[1],
                p_valueEVI = calculate_slope_pvalue(c_across(all_of(columns_to_EVI)))[2])


GPPdataframehl_slope_pvalue <- GPPdataframehl_slope[GPPdataframehl_slope$p_value < 0.05, ]

merged_data <- left_join(GPPdataframehl_slope_pvalue, EVIdataframehl_slope, by = "geometry")
hist(merged_data$rcgrwng.y)      

# Read the shapefile using st_read
single_shapefile <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/SingleShapefileAlldataframes.shp")
single_shapefiledf<-as.data.frame(single_shapefile)
merged_data_clay <- left_join(merged_data, single_shapefile, by = "geometry")
plot(merged_data_clay$slopeEVI, merged_data_clay$slope)
merged_data_clay$cl01030


###SlopeGPP SlopeEVI Relationship
slopeGPP<-ggscatter(merged_data_clay, x = "slopeEVI", y = "slope", add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 9, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 4, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI against time (2008-2020)") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP against time (2008-2020)")))
slopeGPP
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/EVISlope.png"), width = 14, height = 8)



### save the csv file:
write.csv(merged_data_clay, "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/merged_data_clay.csv")
# Create a ggplot object with viridis color scale
ggplot(merged_data_clay, aes(x = GPPggrg, y = EVI,col =EVI)) +
  geom_point() +
  labs(x = "slopeEVI", y = "SlopeGPP") +
  scale_color_viridis(name = "Clay Content")

merged_data_clay$cl01030
hist(merged_data_clay$GPPggrg)
mean(single_shapefiledf$rcgrwng, na.rm = TRUE)
# Count the number of rows where rcgrwng is lower than 1
count_lower_than_1rg <- sum(single_shapefiledf$rcgrwng > 1, na.rm = TRUE)

count_lower_than_1rg

# Filter rows where rcgrwng is lower than 1 into a new dataframe called filtered_df
count_lower_than_1rg <- filter(single_shapefile, rcgrwng < 1)
plot(count_lower_than_1rg$rcgrwng)
hist(count_lower_than_1rg$GPPggrg)
plot(count_lower_than_1rg$cl01030, count_lower_than_1rg$EVI)

######################
######Plotting######################
######################
# Create a ggplot object with viridis color scale
###Multiply GPP by 8
filtered_df_no_na$GPPggrg_8<-filtered_df_no_na$GPPggrg*8
merged_data_clay$GPPggrg_8<-merged_data_clay$GPPggrg*8


###Rice Growing Region and GPP
ggscatter(filtered_df_no_na, x = "rcgrwng", y = "GPPggrg_8", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 0, label.y = 2450, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = 0, label.y = 2350, size = 10) +
  scale_y_continuous(expand = c(0, 0), limits = c(700, 2600)) +
  xlab("Rice Growing Frequency") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/RGRGPPCumulative.png"), width = 14, height = 8)


###SlopeGPP SlopeEVI Relationship
ggscatter(merged_data_clay, x = "slopeEVI", y = "slope", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 9, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 4, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP")))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/EVISlope.png"), width = 14, height = 8)



ggscatter(merged_data_clay, x = "rcgrwng", y = "GPPggrg_8", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 9, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 4, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP")))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/EVISlope.png"), width = 14, height = 8)

# Assuming "filtered_df" is your dataframe
clay_bins <- seq(min(filtered_df_no_na$cl01030), max(filtered_df_no_na$cl01030), by = 10)

# Create a list to store dataframes
clay_dfs <- list()
# Loop through bins and create dataframes
for (i in 1:length(clay_bins)) {
  start <- clay_bins[i]
  end <- clay_bins[i + 1]
  # Use double square brackets for subsetting by column names
  clay_dfs[[i]] <- filtered_df_no_na[filtered_df_no_na$cl01030 >= start & filtered_df_no_na$cl01030 < end, ]
}
# Add names to the dataframes
names(clay_dfs) <- paste0("clay_content_", clay_bins[1:length(clay_bins) - 1], "-", clay_bins[2:length(clay_bins)])
# Accss specific dataframes (optional)
# For example, access the dataframe for clay content between 20 and 30:
clay_df_0_10 <- clay_dfs[["clay_content_0-10"]]
clay_df_20_30 <- clay_dfs[["clay_content_20-30"]]
clay_df_30_40 <- clay_dfs[["clay_content_30-40"]]
clay_df_40_50 <- clay_dfs[["clay_content_40-50"]]


ggscatter(clay_df_0_10, x = "LSWI", y = "GPPggrg_8", color = "rcgrwng",add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 9, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 4, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP")))


ggscatter(clay_df_20_30, x = "LSWI", y = "GPPggrg_8", color = "rcgrwng",add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 2000, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 1900, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP")))


ggscatter(clay_df_30_40, x = "LSWI", y = "GPPggrg_8", color = "rcgrwng",add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 2000, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 1900, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP")))


ggscatter(clay_df_40_50, x = "LSWI", y = "GPPggrg_8", color = "rcgrwng",add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.1, label.y = 2000, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.1, label.y = 1800, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP")))



library(ggpubr)

###LSWI CLay data at different intervals
# Assuming "filtered_df_no_na" is your dataframe
# Assuming "cl01030" is the column to categorize
# Assuming "LSWI", "GPPggrg_8", and "rcgrwng" are columns for plotting

# Create bins with a 2.5% increment
clay_bins <- seq(min(filtered_df_no_na$cl01030), max(filtered_df_no_na$cl01030), by = diff(range(filtered_df_no_na$cl01030)) * 0.025)
# Create an empty dataframe to store R-squared and slope values
result_df <- data.frame(Interval = character(), R_squared = numeric(), Slope = numeric(), stringsAsFactors = FALSE)

# Create a list to store dataframes
clay_dfs <- lapply(seq_along(clay_bins)[-length(clay_bins)], function(i) {
  start <- clay_bins[i]
  end <- clay_bins[i + 1]
  filtered_df_no_na %>%
    filter(cl01030 >= start & cl01030 < end)
})

# Add names to the dataframes
names(clay_dfs) <- paste0("clay_content_", round(clay_bins[-length(clay_bins)], 2), "-", round(clay_bins[-1], 2))

# Create scatter plots for each dataframe with viridis color and save them
# Create scatter plots for each dataframe with viridis color and save them
for (i in seq_along(clay_dfs)) {
  interval_name <- names(clay_dfs)[i]
  plot <- ggscatter(clay_dfs[[i]], x = "LSWI", y = "GPPggrg_8", color = "rcgrwng", add = "reg.line",
                     size = 3)  +
    gradient_color(c("blue", "white", "red"))+
    geom_smooth(method = "lm", formula = y ~ x, color = viridis(1), se = FALSE) +  # Set color here
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 0.01, label.y = 2100, p.accuracy = 0.001, size = 8
    ) +
    stat_regline_equation(label.x = 0.01 , label.y = 2300, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(500, 2500)) +
    xlab("LSWI") +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("GPP"))) +
    labs(color = "Rice Growing Frequency") +  # Legend title
    theme_minimal()  +  # Adjust the theme if needed
    ggtitle(paste("Clay Content Interval:", interval_name, "%"))
  
  # Calculate R-squared and slope for LSWI vs GPPggrg_8
  lm_model_lswi <- lm(GPPggrg_8 ~ LSWI, data = clay_dfs[[i]])
  clay_dfs[[i]]$gpplswipredict <- predict(lm_model_lswi)
  clay_dfs[[i]]$residual <- clay_dfs[[i]]$GPPggrg_8 - clay_dfs[[i]]$gpplswipredict
  r_squared_lswi <- summary(lm_model_lswi)$r.squared
  
  # Calculate R-squared for residual vs rcgrwng
  lm_model_rgf <- lm(residual ~ rcgrwng, data = clay_dfs[[i]])
  r_squared_rgf <- summary(lm_model_rgf)$r.squared
  
  # Slope values
  slope_lswi <- coef(lm_model_lswi)[2]
  slope_rgf <- coef(lm_model_rgf)[2]
  
  # Determine if the slope is positive or negative and add to the dataframe
  slope_sign <- ifelse(slope_lswi > 0, "Positive", "Negative")
  slopergf_sign <- ifelse(slope_rgf > 0, "Positive", "Negative")
  
  # Add R-squared, slope, and sign values to the dataframe
  result_df <- rbind(result_df, data.frame(Interval = interval_name,
                                           R_squared = r_squared_lswi,
                                           Slope = slope_lswi,
                                           Slope_Sign = slope_sign,
                                           r2rgf = r_squared_rgf,
                                           Slope_RGF = slope_rgf,
                                           Slope_RGF_Sign = slopergf_sign,
                                           Sample_Count = nrow(clay_dfs[[i]])))
  
  # Save the plot with a filename indicating the 2.5% range of clay content
  ggsave(paste0("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/SoilClayplot_", names(clay_dfs)[i], ".png"), plot, width = 10, height = 6)
}

# Extract numeric values from the Interval column
numeric_intervals <- as.numeric(gsub("clay_content_|-.*", "", result_df$Interval))
# Replace the Interval column with numeric values
result_df$Interval <- numeric_intervals
result_df <- result_df[-c(nrow(result_df)-1, nrow(result_df)), ]



ggscatter(result_df, x = "Interval", y = "R_squared", color = "Sample_Count", 
          size = 5)  +
  gradient_color(c("blue", "white", "red"))+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  xlab(expression(paste("Clay content % (kg / kg)")))+
  ylab(expression(paste("R-Squared of LSWI vs GPP"))) +
  labs(color = "Number of Samples")+
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.key.width = unit(3.5, "lines"))

claylswigpp<-ggscatter(result_df, x = "Interval", y = "R_squared", color = "Sample_Count", 
          shape = "Slope_Sign", size = 5) +
  gradient_color(c("blue", "violet", "red")) +
  font("xy.text", size = 28) +
  font("xlab", size = 28) +
  font("ylab", size = 28) +
  xlab(expression(paste("Clay content % (kg / kg)"))) +
  ylab(expression(paste("R-Squared of LSWI vs GPP"))) +
  labs(color = "Number of Samples", shape = "Slope Sign") +
  guides(shape = guide_legend(title.position = "top", ncol = 1)) +  # Set shape legend to vertical
  theme(legend.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.key.width = unit(3.5, "lines"))

ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/ClayContentLSWI.png"), width = 14, height = 8)
claylswigpprgf<-ggscatter(result_df, x = "Interval", y = "r2rgf", color = "Sample_Count",  shape = "Slope_RGF_Sign",
          size = 5)  +
  gradient_color(c("blue", "violet", "red")) +
  font("xy.text", size = 28) +
  font("xlab", size = 28) +
  font("ylab", size = 28) +
  xlab(expression(paste("Clay content % (kg / kg)"))) +
  ylab((paste("R-Squared of Rice Growing frequency and", "\nResiduals of LSWI vs GPP"))) +
  labs(color = "Number of Samples") +
  labs(shape = "Slope Sign") +
  guides(shape = guide_legend(title.position = "top", ncol = 1)) +  # Set shape legend to vertical
  theme(legend.title = element_text(size = 28),
        legend.text = element_text(size = 28),
        legend.key.width = unit(3.5, "lines"))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/RiceGrowingRegion.png"), width = 14, height = 8)
# Assuming "filtered_df_no_na" is your dataframe
distclaylswi<-ggplot(filtered_df_no_na, aes(x = cl01030)) +
  geom_histogram(fill = "blue", bins = 30, alpha = 0.7) +  # Set transparency for overlapping bars
  geom_vline(xintercept = mean(filtered_df_no_na$cl01030), color = "red", linetype = "dashed", size = 0.8) +  # Add mean line
  labs(x = "Clay content % (kg / kg)", y = "Frequency", title = "Distribution of Clay Content") +
  scale_x_continuous(breaks = seq(min(filtered_df_no_na$cl01030), max(filtered_df_no_na$cl01030), by = 10)) +  # Add breaks for clarity
  theme_bw() +
  theme(  # Center and adjust title size and face
    axis.title = element_text(size = 28),  # Adjust axis title size
    axis.text = element_text(size = 28),  # Adjust axis text size
  )

ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Histogramn.png"), width = 14, height = 8)

ggarrange(distclaylswi,                                                 # First row with scatter plot
          ggarrange(claylswigpp, claylswigpprgf,  ncol = 2, labels = c("B", "C"),
                    font.label = list(size = 26, color = "black", face = "bold", family = NULL)), # Second row with box and dot plots
          nrow = 2, 
          labels = "A",
          font.label = list(size = 26, color = "black", face = "bold", family = NULL)
) 


ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/ricelswi.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 25,           # Adjust width and height as needed
       height = 18,
       dpi = 300)


ggscatter(result_df, x = "Interval", y = "Slope", color = "Sample_Count", 
          size = 5)  +
  gradient_color(c("blue", "violet", "red")) +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  xlab(expression(paste("Clay content % (kg / kg)"))) +
  ylab((paste("Slope of LSWI vs GPP"))) +
  labs(color = "Number of Samples") +
  theme(legend.title = element_text(size = 24),
        legend.text = element_text(size = 24),
        legend.key.width = unit(3.5, "lines"))

ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/SlopeLSWIGPP.png"), width = 14, height = 8)

#### percentage of data between 15 and 30 percent
prop_between <- sum(filtered_df_no_na$cl01030 >= 15 & filtered_df_no_na$cl01030 <= 30) / nrow(filtered_df_no_na)
percent_between <- prop_between * 100 %>% round(2)
cat("Percentage of data between 15 and 30 in cl01030:", percent_between, "%")

print(round(result_df$r2rgf, 2))
print(round(result_df$Interval, 2))


# Read the shapefile using st_read
single_shapefile <- st_read("C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/SingleShapefileAlldataframes.shp")
single_shapefiledf<-as.data.frame(single_shapefile)
#"single_shapefiledf" in the dataframe remove the rows where rcgrwng=0
single_shapefiledf <- single_shapefiledf[single_shapefiledf$rcgrwng != 0, ]

single_shapefiledf$GPPggrg_8<-single_shapefiledf$GPPggrg * 8
# Assuming single_shapefiledf is your dataframe
single_shapefiledf <- single_shapefiledf[single_shapefiledf$rcgrwng >= 1, ]

grgpp<-ggscatter(single_shapefiledf, x = "rcgrwng", y = "GPPggrg_8", size =0)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 7, label.y = 500, size = 8, p.accuracy = 0.001) +
  stat_regline_equation(label.x = 7, label.y = 250, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
  xlab(paste0("Rice Growing Frequency (season)")) +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_classic()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Annual Cumulative GPP (g C ", m^-2, year^-1, ")")))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/RiceGrowingFrequency.png"), width = 14, height = 8)


###Clay GPP Relationship
claygpp<-ggscatter(single_shapefiledf, x = "cl01030", y = "GPPggrg_8",size = 0) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 0, label.y = 2850, p.accuracy = 0.001, size = 8
  ) +
  stat_regline_equation(label.x = 0, label.y = 2650, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(700, 3000)) +
  scale_x_continuous( breaks = seq(min(filtered_df_no_na$cl01030), max(filtered_df_no_na$cl01030), by = 10)) +
  
  xlab("Clay content % (kg / kg) ") +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_classic()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Annual Cumulative GPP (g C ", m^-2, year^-1, ")")))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/ClayGPPCumulative.png"), width = 14, height = 8)

###Clay EVI Relationship
clayEVI<-ggscatter(filtered_df_no_na, x = "cl01030", y = "EVI", add = "reg.line", size = 0) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 0, label.y = 0.45, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = 0, label.y = 0.5, size = 10) +
  scale_y_continuous(expand = c(0, 0), limits = c(0.1, 0.55)) +
  scale_x_continuous( breaks = seq(min(filtered_df_no_na$cl01030), max(filtered_df_no_na$cl01030), by = 10)) +
  
  xlab("Clay content % (kg / kg) ") +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_classic()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("EVI")))+

  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/EVIClayCumulative.png"), width = 14, height = 8)


# Read the CSV file
# Assuming merged_data_clay is your dataframe
selected_columns <- merged_data_clay[, c("slopeEVI", "p_valueEVI", "DN", 
                                         "GPPggrg", "rcgrwng", "clayb0", 
                                         "clayb10", "clayb30", "cl01030", 
                                         "EVI", "LSWI", "slope")]
write.csv(selected_columns, "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/merged_data_clay.csv", row.names = FALSE)
merged_data_clay <- read.csv("C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapefile/SingleShapefileAllDriver/merged_data_clay.csv")
###SlopeGPP SlopeEVI Relationship
slopeGPP<-ggscatter(merged_data_clay, x = "slopeEVI", y = "slope", add = "reg.line",
          add.params = list(color = "blue", fill = "lightgray"), size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.02, label.y = 9, p.accuracy = 0.001, size = 10
  ) +
  stat_regline_equation(label.x = -0.02, label.y = 4, size = 10) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Slope of EVI against time (2008-2020)") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP against time (2008-2020)")))
ggsave(("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/EVISlope.png"), width = 14, height = 8)

ggarrange(claygpp,clayEVI,slopeGPP, grgpp,
          labels = c("A", "B", "C", "D"),
          font.label = list(size = 24, color = "black", face = "bold", family = NULL),
          ncol = 2, nrow = 2)

ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/ricegrclayrgr.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 25,           # Adjust width and height as needed
       height = 18,
       dpi = 300)


# Assuming single_shapefile is your sf object
# Convert the sf object to a raster extent
CFdf<- raster(rasterlistCF[[4]])
# Transform the raster to lon/lat WGS 84
CFdf <- projectRaster(CFdf, crs = "+proj=longlat +datum=WGS84")

CFdf<- as.data.frame(CFdf, xy=TRUE)
CFdf
CFdf_filtered <- na.omit(CFdf)

###plot dual plots
cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=ar_rice),data=CFdf_filtered)+
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


library(Metrics)
actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
percent_bias(actual, predicted)

