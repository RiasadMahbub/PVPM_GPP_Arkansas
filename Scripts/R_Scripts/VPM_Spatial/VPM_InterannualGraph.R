library(trend) ## functions sens.slope and mk.test: Mann-Kendall Trend Test
library(ggpubr)
library(ggtext)
library(ggplot2)

##mann kendall test: https://www.rdocumentation.org/packages/trend/versions/1.1.6/topics/mk.test
## sens.slope test: https://www.rdocumentation.org/packages/trend/versions/1.1.6/topics/sens.slope
## sens.slope test: https://search.r-project.org/CRAN/refmans/trend/html/sens.slope.html



### path of the images
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/"
setwd(path)
rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/", pattern='.tif$', 
                                 all.files=TRUE, full.names=FALSE)
## read it as list
allrasterscumulative <- lapply(rastlistcumulative, raster)
### Since the geometry region is same now we do not need to set the extent manually
#st_ext <- extent(-94.86209 , -89.14432 , 32.77952 , 36.67372 )
#allrasterscumulativeextent <- vector("list", 13)
#for(i in 1:13){
  #allrasterscumulativeextent[[i]] = setExtent(allrasterscumulative[[i]], st_ext)
#}
# for(i in 1:13){
#   allrasterscumulativeextent[[i]] = resample(allrasterscumulative[[i]], allrasterscumulative[[1]])
# }
#allrasterscumulativestack<-stack(allrasterscumulativeextent)
##In R, the stack() function is used to combine multiple raster layers into a single stacked raster object. 
allrasterscumulativestack<-stack(allrasterscumulative)

##calculates the mean value across all cells in each layer of the stacked raster object allrasterscumulativestack, ignoring any cells with missing values (na.rm = TRUE).
allrasterscumulativestack_mean <-cellStats(allrasterscumulativestack, 'mean', na.rm = TRUE)
## Convert to dataframe
allrasterscumulativestack_meandf<-as.data.frame(allrasterscumulativestack_mean)

## calculate it to 8-day
allrasterscumulativestack_meandf$meancumgpp8dayagg<-allrasterscumulativestack_meandf$allrasterscumulativestack_mean*8
allrasterscumulativestack_meandf$year<-seq(2008, 2020, by =1)

result = cor(allrasterscumulativestack_meandf$year, allrasterscumulativestack_meandf$meancumgpp8dayagg, method = "kendall")
result
meanRallrasterscumulativeextent

## To add the error bars we will calculate the standard deviation and add it to the data frame at the end
## calculate the standard deviation from the cellstats function
allrasterscumulativestack_sd <-cellStats(allrasterscumulativestack, 'sd')
allrasterscumulativestack_sddf<-as.data.frame(allrasterscumulativestack_sd)
allrasterscumulativestack_sddf$sdcumgpp8dayagg<-allrasterscumulativestack_sddf$allrasterscumulativestack_sd*8
allrasterscumulativestack_meansddf<- cbind(allrasterscumulativestack_meandf, allrasterscumulativestack_sddf)

## making the graph of interannual variability of GPP
mktestpvaluegpp<-mk.test(allrasterscumulativestack_meansddf$meancumgpp8dayagg)$p.value ##mk.test: Mann-Kendall Trend Test trend (version 1.1.6)
senslopegpp<-sens.slope(allrasterscumulativestack_meansddf$meancumgpp8dayagg)$estimates["Sen's slope"] ##trend (version 1.1.6) and sens.slope: Sen's slope
 
ggscatter(allrasterscumulativestack_meansddf, x = "year", y = "meancumgpp8dayagg", add = "reg.line", size =7) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 2016, label.y = 400, size = 8) +
  stat_regline_equation(label.x = 2016, label.y = 500, size =8)+
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  geom_errorbar(aes(ymin=meancumgpp8dayagg-sdcumgpp8dayagg , ymax=meancumgpp8dayagg+sdcumgpp8dayagg ), width=.2,
                position=position_dodge(90))+ 
  geom_text(x = 2016, y = 300, label = paste("Mann-Kendall p-value =", round(mktestpvaluegpp, 4)), size = 8) +
  geom_text(x = 2016, y = 200, label = paste("Sens Slope =", round(senslopegpp, 2)), size = 8) + 
  xlab("Year")+
  font("xy.text", size = 24)+
  font("xlab", size = 24)+
  font("ylab", size = 24)+
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1,")")))


ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMyearwisespatialGPP.png",width = 14, height = 8 )
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMyearwisespatialGPP.png",width = 14, height = 8 )


allrasterscumulativestack_meansddf

#At the state scale, in the timeframe between 2008 to 2020, the mean photosynthetic carbon uptake of Arkansas rice fields was 
mean(allrasterscumulativestack_meansddf$meancumgpp8dayagg)
#At the state scale, in the timeframe between 2008 to 2020, the mean photosynthetic carbon uptake of Arkansas rice fields was 
sqrt(var(allrasterscumulativestack_meansddf$meancumgpp8dayagg))

### 3 minimum GPP years
top_3_min <- allrasterscumulativestack_meansddf %>%
  top_n(-3, meancumgpp8dayagg)
print(top_3_min)

### 3 maximum GPP years
top_3_max <- allrasterscumulativestack_meansddf %>%
  top_n(3, meancumgpp8dayagg)
print(top_3_max)

allrasterscumulativestack_meansddf %>%
  arrange(desc(meancumgpp8dayagg)) %>%
  head(3)

allrasterscumulativestack_meansddf %>%
  arrange(meancumgpp8dayagg) %>%
  head(3)

# Function to read raster files into a list of raster objects
read_rasters <- function(directory_path) {
  files <- list.files(path = directory_path, pattern = '.tif$', full.names = TRUE)
  rasters <- lapply(files, raster)
  return(rasters)
}

# Function to calculate mean and standard deviation across all cells in each layer of a raster stack
calculate_stats <- function(raster_stack) {
  mean_vals <- cellStats(raster_stack, 'mean', na.rm = TRUE)
  sd_vals <- cellStats(raster_stack, 'sd')
  return(list(mean = mean_vals, sd = sd_vals))
}

# Function to create a data frame with years and statistics
create_stats_df <- function(stats_list) {
  mean_df <- as.data.frame(stats_list$mean)
  sd_df <- as.data.frame(stats_list$sd)
  mean_df$year <- seq(2008, 2020, by = 1)
  stats_df <- cbind(mean_df, sd_df)
  return(stats_df)
}

# Read temperature rasters
temperature_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature"
setwd(temperature_path)
temperature_rasters <- read_rasters(temperature_path)
temperature_stack <- stack(temperature_rasters)
temperature_stats <- calculate_stats(temperature_stack)
temperature_df <- create_stats_df(temperature_stats)

# Read Ts rasters
ts_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/Ts"
setwd(ts_path)
ts_rasters <- read_rasters(ts_path)
ts_stack <- stack(ts_rasters)
ts_stats <- calculate_stats(ts_stack)
ts_df <- create_stats_df(ts_stats)

# Merge the data frames based on the common column (year)
merged_df <- merge(temperature_df, ts_df, by = "year", all.x = TRUE, all.y = TRUE)
merged_df
# Rename columns in the merged dataframe
colnames(merged_df) <- c("year", "tempmean", "tempsd", "ts_mean", "ts_sd")

# Print the updated dataframe
print(merged_df)


ggscatter(merged_df, x = "ts", y = "meancumgpp8dayagg.x", add = "reg.line", size =12, col = "year") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 15, label.y = 1700, size =12) +
  stat_regline_equation(label.x = 15, label.y = 1600, size =12)+
  scale_x_continuous(breaks = seq(15, 20, by = 1), limits = c(15, 20)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Mean Temperature (°C)")+
  font("xy.text", size = 24)+
  font("xlab", size = 24)+
  font("ylab", size = 24)+
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1,")")))


ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/TEMPERATUREGPPinterannual.png",width = 14, height = 8 )

# Filter the dataframe for the years 2010 and 2011
years_2010_2011 <- merged_df[merged_df$year %in% c(2010, 2011), ]
mean(merged_df$tempmean)

# Calculate the differences in tempmean for each year compared to all years
diff_tempmean_2010 <- merged_df$tempmean[merged_df$year == 2010] - mean(merged_df$tempmean)
diff_tempmean_2011 <- merged_df$tempmean[merged_df$year == 2011] - mean(merged_df$tempmean)

# Print the differences
cat("Difference in tempmean for 2010 compared to all years:", diff_tempmean_2010, "\n")
cat("Difference in tempmean for 2011 compared to all years:", diff_tempmean_2011, "\n")
allrasterscumulativestack_meansddf


# Calculate the mean GPP for all years
mean_gpp_all_years <- mean(allrasterscumulativestack_meansddf$meancumgpp8dayagg)

# Calculate the differences in GPP for each year compared to all years
diff_gpp_2010 <- mean_gpp_all_years - allrasterscumulativestack_meansddf$meancumgpp8dayagg[allrasterscumulativestack_meansddf$year == 2010]
diff_gpp_2011 <- mean_gpp_all_years - allrasterscumulativestack_meansddf$meancumgpp8dayagg[allrasterscumulativestack_meansddf$year == 2011]

# Print the differences
cat("Difference in GPP for 2010 compared to all years:", diff_gpp_2010, "\n")
cat("Difference in GPP for 2011 compared to all years:", diff_gpp_2011, "\n")


# Calculate the mean GPP for all years
mean_gpp_all_years <- mean(allrasterscumulativestack_meansddf$meancumgpp8dayagg)

# Calculate the percentage differences in GPP for each year compared to all years
diff_percent_gpp_2010 <- ((mean_gpp_all_years - allrasterscumulativestack_meansddf$meancumgpp8dayagg[allrasterscumulativestack_meansddf$year == 2010]) / mean_gpp_all_years) * 100
diff_percent_gpp_2011 <- ((mean_gpp_all_years - allrasterscumulativestack_meansddf$meancumgpp8dayagg[allrasterscumulativestack_meansddf$year == 2011]) / mean_gpp_all_years) * 100

# Print the percentage differences
cat("Percentage difference in GPP for 2010 compared to all years:", diff_percent_gpp_2010, "%\n")
cat("Percentage difference in GPP for 2011 compared to all years:", diff_percent_gpp_2011, "%\n")

