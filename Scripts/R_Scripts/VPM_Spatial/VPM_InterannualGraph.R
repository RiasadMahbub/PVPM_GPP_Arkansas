library(trend) ## functions sens.slope and mk.test: Mann-Kendall Trend Test
library(ggpubr)
library(ggtext)
library(ggplot2)
library(raster)

##mann kendall test: https://www.rdocumentation.org/packages/trend/versions/1.1.6/topics/mk.test
## sens.slope test: https://www.rdocumentation.org/packages/trend/versions/1.1.6/topics/sens.slope
## sens.slope test: https://search.r-project.org/CRAN/refmans/trend/html/sens.slope.html



### path of the images
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter/"
setwd(path)
rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter/", pattern='.tif$', 
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
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 2013, label.y = 750, size = 8) +
  stat_regline_equation(label.x = 2013, label.y = 600, size =8)+
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  geom_errorbar(aes(ymin=meancumgpp8dayagg-sdcumgpp8dayagg , ymax=meancumgpp8dayagg+sdcumgpp8dayagg ), width=.2,
                position=position_dodge(90))+ 
  geom_text(x = 2015.3, y = 450, label = paste("Mann-Kendall p-value =", round(mktestpvaluegpp, 4)), size = 8) +
  geom_text(x = 2014.4, y = 300, label = paste("Sens Slope =", round(senslopegpp, 2)), size = 8) + 
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

### Standarddeviation of temperature
# Read SD Temperature rasters
sdtemp_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/SDtemp"
setwd(sdtemp_path)
sdtemp_rasters <- read_rasters(sdtemp_path)
sdtemp_stack <- stack(sdtemp_rasters)
sdtemp_stats <- calculate_stats(sdtemp_stack)
sdtemp_df <- create_stats_df(sdtemp_stats)

### Standarddeviation of PAR
# Read SD PAR rasters
sdpar_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/SDPAR"
setwd(sdpar_path)
sdpar_rasters <- read_rasters(sdpar_path)
sdpar_stack <- stack(sdpar_rasters)
sdpar_stats <- calculate_stats(sdpar_stack)
sdpar_df <- create_stats_df(sdpar_stats)


# Merge the data frames based on the common column (year)
merged_df <- merge(temperature_df, ts_df, by = "year", all.x = TRUE, all.y = TRUE)
merged_df <- merge(merged_df, sdtemp_df, by = "year", all.x = TRUE, all.y = TRUE)
merged_df <- merge(merged_df, sdpar_df, by = "year", all.x = TRUE, all.y = TRUE)

# Rename columns in the merged dataframe
colnames(merged_df) <- c("year", "tempmean", "tempsd", "ts_mean", "ts_sd", "sd_temp_mean", "sd_temp_sd", "sd_par_mean", "sd_par_sd")
merged_df_tsTemp <- merged_df
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


#####################################################################
####################primary and secondary axis#######################
#####################################################################
##allrasterscumulativestack_meansddf = VPM_InterannaualGraph
library(ggpubr)
library(ggtext)
library(ggplot2)
library(trend)
library(remotes)
library(lterdatasampler)

# Define the file path
file_path <- "C:/Users/rbmahbub/Documents/Data/YieldData/Harvested_ACre_Rice_Arkansas.csv"
# Read the CSV file
stateyield <- read.csv(file_path)
# Remove commas and convert to numeric
stateyield$Value <- as.numeric(gsub(",", "", stateyield$Value))
stateyield$Year<-as.numeric(stateyield$Year)

stateyield <- stateyield %>%
  dplyr::rename(yield = Value)

stateyield$yield <- stateyield$yield * 0.111111

stateyield_selected <- stateyield %>%
  dplyr::select(Year, yield)
allrasterscumulativestack_meansddf$Year<-allrasterscumulativestack_meansddf$year

GPPyieldmerge <- merge(allrasterscumulativestack_meansddf, stateyield_selected, by = "Year")
mktestpvalueyield<-mk.test(GPPyieldmerge$yield)$p.value
senslopeyield<-sens.slope(GPPyieldmerge$yield)$estimates["Sen's slope"]

mktestpvaluegpp<-mk.test(GPPyieldmerge$meancumgpp8dayagg)$p.value
senslopegpp<-sens.slope(GPPyieldmerge$meancumgpp8dayagg)$estimates["Sen's slope"]

ggscatter(GPPyieldmerge, x = "year", y = "yield", add = "reg.line", size = 7) +
  geom_point(size = 7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 2013.8, label.y = 400, size = 8) +
  stat_regline_equation(label.x = 2013.8, label.y = 500, size = 8) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100)) + 
  geom_text(x = 2016, y = 300, label = paste("Mann-Kendall p-value =", round(mktestpvalueyield, 4)), size = 8) +
  geom_text(x = 2015.1, y = 200, label = paste("Sens Slope =", round(senslopeyield, 2)), size = 8) +
  xlab("Year") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("State wise yield (g ", m^-2, season^-1, ")")))


# Load required libraries
library(ggplot2)

# Plot GPP on primary y-axis and Yield on secondary y-axis
ggplot(GPPyieldmerge, aes(x = year)) + 
  # Primary y-axis for GPP
  geom_line(aes(y = meancumgpp8dayagg, color = "GPP"), size = 1.2) +
  geom_point(aes(y = meancumgpp8dayagg, color = "GPP"), size = 3) +
  geom_smooth(aes(y = meancumgpp8dayagg, color = "GPP"), method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin = meancumgpp8dayagg - sdcumgpp8dayagg, 
                    ymax = meancumgpp8dayagg + sdcumgpp8dayagg, color = "GPP"), 
                width = 0.2) +
  
  # Secondary y-axis for Yield
  geom_line(aes(y = yield * (max(GPPyieldmerge$meancumgpp8dayagg) / max(GPPyieldmerge$yield)), color = "Yield"), 
            size = 1.2, linetype = "dashed") +
  geom_point(aes(y = yield * (max(GPPyieldmerge$meancumgpp8dayagg) / max(GPPyieldmerge$yield)), color = "Yield"), 
             size = 3) +
  geom_smooth(aes(y = yield * (max(GPPyieldmerge$meancumgpp8dayagg) / max(GPPyieldmerge$yield)), color = "Yield"), 
              method = "lm", se = FALSE, linetype = "dashed") +
  
  # Define primary y-axis for GPP and secondary y-axis for Yield
  scale_y_continuous(
    name = expression(Gross~Primary~Productivity~(g~C~m^-2~year^-1)),
    limits = c(0, NA),
    sec.axis = sec_axis(~ . / (max(GPPyieldmerge$meancumgpp8dayagg) / max(GPPyieldmerge$yield)), 
                        name = expression(State~wise~yield~(g~m^-2~season^-1))
    ),
    breaks = seq(0, 2500, by = 500),  # Major ticks at 0, 1000, and 2000
    minor_breaks = c(1500, 1600, 1700, 1800, 1900)  # Specify the minor ticks explicitlye
  ) +
  
  # Statistical annotations for GPP

  stat_cor(aes(y = meancumgpp8dayagg, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 2008, label.y = 700, size = 8, color = "black") +
  stat_regline_equation(aes(y = meancumgpp8dayagg), 
                        label.x = 2008, label.y = 500, size = 8, color = "black") +
  geom_text(x = 2010.6, y = 300, label = paste("Mann-Kendall p-value =", round(mktestpvaluegpp, 4)), size = 8, color = "black") +
  geom_text(x = 2009.6, y = 100, label = paste("Sens Slope =", round(senslopegpp, 2)), size = 8, color = "black") + 
  
  # Statistical annotations for Yield
  stat_cor(aes(y = yield * (max(GPPyieldmerge$meancumgpp8dayagg) / max(GPPyieldmerge$yield)), 
               label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 2014.8, label.y = 700, size = 8, color = "blue") +
  stat_regline_equation(aes(y = yield * (max(GPPyieldmerge$meancumgpp8dayagg) / max(GPPyieldmerge$yield))), 
                        label.x = 2014.8, label.y = 500, size = 8, color = "blue") +
  
  # Customize scales and labels
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  geom_text(x = 2018.2, y = 300, label = paste("Mann-Kendall p-value =", round(mktestpvalueyield, 4)), size = 8,  color = "blue") +
  geom_text(x = 2016.6, y = 100, label = paste("Sens Slope =", round(senslopeyield, 2)), size = 8,  color = "blue") +
  
  # Customize plot theme
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y.left = element_text(size = 24, color = "black"),  # Increased font size by 10
    axis.title.y.right = element_text(size = 24), # Increased font size by 10
    axis.text.x = element_text(size = 22, color = "black"),        # Increased font size by 10
    axis.text.y = element_text(size = 22, color = "black"),        # Increased font size by 10
    legend.position = c(0.1, 0.9),                 # Position legend in the top-left corner
    legend.background = element_rect(fill = "white"), # Legend background color
    legend.text = element_text(size = 18)          # Increased legend font size by 2
  ) +
  
  # Legend and colors for the two lines
  scale_color_manual(values = c("GPP" = "black", "Yield" = "blue")) +
  labs(color = NULL, x = "Year") 


ggplot(GPPyieldmerge, aes(x = year)) + 
  # Line and point for GPP
  geom_line(aes(y = meancumgpp8dayagg, color = "GPP"), size = 1.2) +
  geom_point(aes(y = meancumgpp8dayagg, color = "GPP"), size = 3) +
  geom_smooth(aes(y = meancumgpp8dayagg, color = "GPP"), method = "lm", se = FALSE) +
  geom_errorbar(aes(ymin = meancumgpp8dayagg - sdcumgpp8dayagg, 
                    ymax = meancumgpp8dayagg + sdcumgpp8dayagg, color = "GPP"), 
                width = 0.2) +
  
  # Line and point for Yield
  geom_line(aes(y = yield, color = "Yield"), size = 1.2, linetype = "dashed") +
  geom_point(aes(y = yield, color = "Yield"), size = 3) +
  geom_smooth(aes(y = yield, color = "Yield"), method = "lm", se = FALSE, linetype = "dashed") +
  
  # Define y-axis label
  scale_y_continuous(
    name = expression(atop("Gross Primary Productivity (g C m"^{-2}~"year"^{-1}~")", 
                           "State-wise Yield (g m"^{-2}~"season"^{-1}~")")),
    limits = c(0, NA),
    breaks = seq(0, 2500, by = 200),  # Major ticks at 0, 1000, and 2000
    minor_breaks = c(1500, 1600, 1700, 1800, 1900)  # Minor ticks
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2020, by = 1),  # Ensure full integers from 2008 to 2020
    labels = seq(2008, 2020, by = 1)   # Set labels to match the integer sequence
  )+
  
  # Statistical annotations for GPP
  stat_cor(aes(y = meancumgpp8dayagg, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 2008, label.y = 500, size = 7, color = "black") +
  stat_regline_equation(aes(y = meancumgpp8dayagg), 
                        label.x = 2008, label.y = 350, size = 7, color = "black") +
  geom_text(x = 2010.1, y = 200, label = paste("Mann-Kendall p-value =", round(mktestpvaluegpp, 4)), size = 7, color = "black") +
  geom_text(x = 2009.3, y = 50, label = paste("Sens Slope =", round(senslopegpp, 2)), size = 7, color = "black") + 
  
  # Statistical annotations for Yield
  stat_cor(aes(y = yield, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.x = 2014.8, label.y = 500, size =7, color = "blue") +
  stat_regline_equation(aes(y = yield), 
                        label.x = 2014.8, label.y = 350, size = 7, color = "blue") +
  geom_text(x = 2016.9, y = 200, label = paste("Mann-Kendall p-value =", round(mktestpvalueyield, 4)), size = 7, color = "blue") +
  geom_text(x = 2016.05, y = 50, label = paste("Sens Slope =", round(senslopeyield, 2)), size = 7, color = "blue") +
  
  # Customize plot theme
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24, color = "black"),
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    legend.position = c(0.1, 0.9),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(size = 18)
  ) +
  
  # Legend and colors for the two lines
  scale_color_manual(values = c("GPP" = "black", "Yield" = "blue")) +
  labs(color = NULL, x = "Year")

ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMinterannualYieldGPP.png",width = 14, height = 8 )

library(Metrics)

# Calculate RMSE, MAE, and Bias
rmse_value <- Metrics::rmse(GPPyieldmerge$meancumgpp8dayagg, GPPyieldmerge$yield)
mae_value <- Metrics::mae(GPPyieldmerge$meancumgpp8dayagg, GPPyieldmerge$yield)
bias_value <- Metrics::bias(GPPyieldmerge$meancumgpp8dayagg, GPPyieldmerge$yield)
r2_value <- cor(GPPyieldmerge$meancumgpp8dayagg, GPPyieldmerge$yield)^2

# Print the results
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("Bias:", bias_value, "\n")
cat("R2:", r2_value, "\n")

#### spatial variation of temperature vs gpp yield relationship
#### GPP-yield relation data
### Temperature data 
##temperature_df: merged_df_tsTemp

###YieldGPP data: merged_df_gppyield (VPMYieldGPP_CountyMaps_Yearwise)
### there is a merged_df dataframe there too so run both scripts

# Rename the column "year" in merged_df_tsTemp to "Year" for consistency
merged_df_tsTemp <- merged_df_tsTemp %>% dplyr::rename(Year = year)

# Perform a left join and name the result as gppyieldtem
gppyieldtem <- left_join(merged_df_gppyield, merged_df_tsTemp, by = "Year")

# Display the merged data frame
plot(gppyieldtem$tempmean, gppyieldtem$r_squared)
plot(gppyieldtem$sd_temp_mean, gppyieldtem$r_squared)
plot(gppyieldtem$sd_par_mean, gppyieldtem$r_squared)

plot(gppyieldtem$tempmean, gppyieldtem$mean_yield)
plot(gppyieldtem$sd_temp_mean, gppyieldtem$mean_yield)
plot(gppyieldtem$sd_par_mean, gppyieldtem$mean_yield)


library(ggplot2)
library(viridis)
library(ggpubr)

# Plot 1: tempmean vs r_squared
p1 <- ggplot(gppyieldtem, aes(x = tempmean, y = r_squared, color = Year)) +
  geom_point(size = 3) +  # Increase point size
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add regression line
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top") +  # Add R²
  scale_color_viridis_c(breaks = seq(min(gppyieldtem$Year), max(gppyieldtem$Year), by = 2),
                        labels = function(x) round(x)) +
  labs(x = "Annual Mean Temperature", y = "R-squared between GPP and Yield", color = "Year") +
  theme_minimal() 


# Plot 1: tempmean vs r_squared
p2 <- ggplot(gppyieldtem, aes(x = sd_par_mean, y = r_squared, color = Year)) +
  geom_point(size = 3) +  # Increase point size
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add regression line
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top") +  # Add R²
  scale_color_viridis_c(breaks = seq(min(gppyieldtem$Year), max(gppyieldtem$Year), by = 2),
                        labels = function(x) round(x)) +
  labs(x = "Annual standard deviation of PAR", y = "R-squared between GPP and Yield", color = "Year") +
  theme_minimal() 


# Plot 3: sd_temp_mean vs r_squared
p3 <- ggplot(gppyieldtem, aes(x = sd_temp_mean, y = r_squared, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top") +
  scale_color_viridis_c(breaks = seq(min(gppyieldtem$Year), max(gppyieldtem$Year), by = 2),
                        labels = function(x) round(x)) +
  labs(x = "Annual standard deviation of Mean Temperature", y = "R-squared between GPP and Yield", color = "Year") +
  theme_minimal() 




# Plot 2: tempmean vs mean_yield
p4 <- ggplot(gppyieldtem, aes(x = tempmean, y = mean_yield, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top") +
  scale_color_viridis_c(breaks = seq(min(gppyieldtem$Year), max(gppyieldtem$Year), by = 2),
                        labels = function(x) round(x)) +
  labs(x = "Annual Mean Temperature", y = "Mean Yield", color = "Year") +
  theme_minimal()

# Plot 4: sd_par_sd vs r_squared
p5 <- ggplot(gppyieldtem, aes(x = sd_par_mean, y = mean_yield, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top") +
  scale_color_viridis_c(breaks = seq(min(gppyieldtem$Year), max(gppyieldtem$Year), by = 2),
                        labels = function(x) round(x)) +
  labs(x = "Annual standard deviation of PAR", y = "Mean Yield", color = "Year") +
  theme_minimal() 


# Plot 4: sd_par_sd vs r_squared
p6 <- ggplot(gppyieldtem, aes(x = sd_temp_mean, y = mean_yield, color = Year)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  stat_cor(aes(label = paste(..rr.label.., sep = "~`,`~")), label.x.npc = "left", label.y.npc = "top") +
  scale_color_viridis_c(breaks = seq(min(gppyieldtem$Year), max(gppyieldtem$Year), by = 2),
                        labels = function(x) round(x)) +
  labs(x = "Annual standard deviation of temperature", y = "Mean Yield", color = "Year") +
  theme_minimal() 

# Display the plots
p1
p2
p3
p4
p5
p6

