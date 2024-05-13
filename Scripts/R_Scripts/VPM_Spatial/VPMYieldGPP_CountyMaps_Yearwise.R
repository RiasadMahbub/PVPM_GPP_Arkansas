### Load the libraries
library(raster)
library(maptools)
library(terra)
library(cowplot)
library(tidyverse)
library(ggsn)
library(ggpubr)
library(sf)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(sp)
library(ggpmisc)
library(ggrepel)
library(broom)
require(ggplot2)
require(plyr)
library(Metrics)
library(ggspatial)
library(ggplot2)
library(viridis)
library(trend)
library(gridExtra)
library(dplyr)

#######VPM Raster Images#######################
## Set the path where the raster files are located
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/"
## Get the list of image file names with the .tif extension from the specified path
Imagenameyield <-list.files(path = path, pattern='.tif$', 
                            all.files=TRUE, full.names=FALSE)
## Initialize lists to store paths and raster objects
pathlistyield <- vector("list", 13)
rastlistyield<- vector("list", 13)
## Iterate over the image file names to construct their full paths
for (i in 1:13) {
  pathlistyield[i] = paste('C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/', Imagenameyield[i], sep = "")
  year = year+1 ### image names are like this "arkansasRice2008VPMcumulative.tif", "arkansasRice2009VPMcumulative.tif
} 
## Read each raster file from its corresponding path and store them in a list
for (i in 1:13) {
  rastlistyield[[i]] <- raster(pathlistyield[[i]])
}

# Initialize year
year <- 2008
# Loop through each file
for (i in 1:13) {
  # Construct the full path to the raster file
  pathlistyield[i] <- paste0('C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/', Imagenameyield[i])
  # Increment the year based on the file name
  year <- year + 1
}
for (i in 1:13) {
  rastlistyield[[i]] <- raster(pathlistyield[[i]])
}
## Yield data in csv format
yield20082020<-read.csv("C:/Users/rbmahbub/Documents/Data/YieldData/ArkansasCounty2008_2020lb_acre_areayield.csv")
## Extract COUNTYFP from County.ANSI 9 (Merge yield data with county shapefile based on COUNTYFP)
yield20082020$COUNTYFP<-yield20082020$County.ANSI
## Pad COUNTYFP with zeros 45 yto 045
yield20082020$COUNTYFP<-str_pad(yield20082020$COUNTYFP, 3, pad = "0")
## Remove commas from the Value column and convert it to numeric
yield20082020$Yield<-as.numeric(gsub(",", "", yield20082020$Value))

for(i in unique(yield20082020$Year)) {
  nam <- paste("yield", i, sep = ".")
  assign(nam, yield20082020[yield20082020$Year==i,])
}

yieldlist<-list(yield.2008, yield.2009, yield.2010, yield.2011, yield.2012, yield.2013, yield.2014, yield.2015, yield.2016, yield.2017, yield.2018, yield.2019, yield.2020)
yieldlist

##Merging Shapefile with yield dataset
## Read the arkansas shpaefile
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Documents/Data/GeospatialData/ArkansasShapefile/tl_2016_05_cousub/tl_2016_05_cousub.shp")

### Make a county map
county <- aggregate(arkansasshp["COUNTYFP"], by = list(diss = arkansasshp$COUNTYFP), 
                    FUN = function(x)x[1], do_union = TRUE)

##County with one variable
countyonevariable <- county[ ,c(2,3)]

##List of shapefile
yieldlistshp<-vector("list", 13)
for (i in 1:13) {
  yieldlistshp[[i]] <- merge(countyonevariable, yieldlist[i], by='COUNTYFP')
  yieldlistshp[[i]] <- yieldlistshp[[i]][ ,c("COUNTYFP", "Year", "Yield")]
  yieldlistshp[[i]] <- st_transform(yieldlistshp[[i]], crs(rastlistyield[[i]])) #st_transform() is then used to transform the spatial object yieldlistshp[[i]] to the CRS of the raster object.
} 

# Check if CRS are the same
# Print CRS of yieldlistshp
# Check CRS for all files
for (i in 1:13) {
  cat("File", i, ":", st_crs(yieldlistshp[[i]]) == st_crs(rastlistyield[[i]]), "\n")
}


### Extract the rasters or raster values on all the counties
extractedvalueslist<-vector("list", 13)
extractedcombinelist<-vector("list", 13)

###29 counties and 13 years
for (i in 1:13) {
  extractedvalueslist[[i]] <- terra::extract(rastlistyield[[i]], yieldlistshp[[i]]) ### 29 in each of them
  extractedcombinelist[[i]]<-c(1:nrow(yieldlistshp[[i]]))
  extractedcombinelist[[i]]<-as.data.frame(extractedcombinelist[[i]])
  extractedcombinelist[[i]]$meanGPP<-NA
}

for (i in 1: 13){ ### for 13 years
  for (j in 1: nrow(yieldlistshp[[i]])){ ### 29 counties
    extractedcombinelist[[i]]$meanGPP[j]<-lapply((extractedvalueslist[[i]][j]), mean, na.rm = TRUE)
  }
}

for (i in 1:13) {
  yieldlistshp[[i]]$extractedcombine<- c(1:nrow(yieldlistshp[[i]]))
  names(yieldlistshp[[i]])[names(yieldlistshp[[i]]) == 'extractedcombine'] <- "extractedcombinelist[[i]]"
}
##rename the column
mgppyieldlist<-vector("list", 13)
for (i in 1:13) {
  mgppyieldlist[[i]]<-merge(yieldlistshp[[i]], extractedcombinelist[[i]], by='extractedcombinelist[[i]]')
}

## Drop the null values
gppricecountydfnaomit<-vector("list", 13)
for (i in 1:13) {
  gppricecountydfnaomit[[i]]<-mgppyieldlist[[i]] %>% drop_na(Yield)
} 

## Convert the shapefile to dataframe
gppricecountydfnaomit_dataframe<-vector("list", 13)
for (i in 1:13) {
  gppricecountydfnaomit_dataframe[[i]]<-gppricecountydfnaomit[[i]] %>% st_drop_geometry()
} 

## Bind the rows convert the list of list to a single dataframe
gppricecounty<-do.call(rbind, Map(data.frame, gppricecountydfnaomit_dataframe))
### unit Conversion
## COnvert the yield to gram ##
gppricecounty$yieldgm<-(gppricecounty$Yield)*0.112085
## Convert the gpp to 8 day cumulative (the gpp was giving non numeric argument error so converted to binary operator==)
gppricecounty$meanGPP<-as.numeric(gppricecounty$meanGPP)
gppricecounty$gpp_sum_8day<-(gppricecounty$meanGPP)*8

plot(gppricecounty$gpp_sum_8day, gppricecounty$yieldgm)
(cor(gppricecounty$gpp_sum_8day, gppricecounty$yieldgm))^2

# Fit linear regression model
model <- lm(yieldgm ~ gpp_sum_8day, data = gppricecounty)
# Extract intercept and slope
intercept <- coef(model)[1]
slope <- coef(model)[2]
# Display intercept and slope
print(intercept)
print(slope)

gppricecounty$predictedyield<- intercept+ slope*gppricecounty$gpp_sum_8day
rmseyield<-rmse(gppricecounty$yieldgm, gppricecounty$predictedyield)
rmseyield


lb1 <- paste("RMSE == rmseyield", round(runif(1),4))

gppcountyyieldplot<-ggplot(data=gppricecounty, aes(x =gpp_sum_8day , y =yieldgm, col = Year))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean Cumulative GPP ('*g~ 'C'~ m^-2~year^-1*')'))+
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_y_continuous(limits = c(500,1000))+
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2008, 2012, 2016, 2020), labels = c("2008","2012", "2016", "2020"))+
  
  stat_regline_equation(label.x = 1750, label.y = 580, size = 10)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = 1750, label.y = 625, size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 30))+
  ggplot2::annotate(geom = "text", x = 1850, y = 520, label = "RMSE = 61", size = 10)+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(axis.ticks.length = unit(.25, "cm"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

gppcountyyieldplot
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMyield.png",width = 14, height = 8 )

### Yearwise state scale increase
statearyield<- read.csv("C:/Users/rbmahbub/Documents/Data/YieldData/Harvested_ACre_Rice_Arkansas.csv")
###
# Remove commas from the values
statearyield$Value <- gsub(",", "", statearyield$Value)
# Convert the column to numeric
statearyield$Value <- as.numeric(statearyield$Value)
# Convert lb/acre to g/m^2
conversion_factor <- 453.59237 / 4046.85642  # Calculating the conversion factor
statearyield$yieldgm <- statearyield$Value * conversion_factor

library(dplyr)
statearyield$Year<-as.character(statearyield$Year)
statearyield$yieldgm<-as.numeric(statearyield$yieldgm)
# Group by Year and calculate mean yieldgm, dropping empty groups
agg_tbl <- statearyield %>%
  group_by(Year) %>%
  dplyr::summarise(mean_yield = mean(yieldgm),
                   sd_yield = sd(yieldgm))

agg_tbl
agg_tbl$Year<-as.numeric(agg_tbl$Year)
# Calculate differences in years
agg_tbl$Diff_year <- c(NA, diff(agg_tbl$Year))
# Calculate differences in mean_yield yields
agg_tbl$Diff_growth <- c(NA, diff(agg_tbl$mean_yield))
# Calculate growth rate in percent
agg_tbl$Rate_percent <- (agg_tbl$Diff_growth / agg_tbl$mean_yield) / agg_tbl$Diff_year * 100

# Print the updated data frame
print(agg_tbl)




agg_tbl
write.csv(agg_tbl, 
          "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/growth_rate.csv", 
          row.names = FALSE)
allrasterscumulativestack_meansddf$Year<-allrasterscumulativestack_meansddf$year
# Assuming both data frames have a column named "Year"
merged_df <- merge(agg_tbl, allrasterscumulativestack_meansddf, by = "Year")


data <- gppricecounty %>% 
  dplyr::select(Year, gpp_sum_8day, yieldgm)
plot(gppricecounty$Year, gppricecounty$yieldgm)
plot(gppricecounty$Year, gppricecounty$gpp_sum_8day)

# Group by year and calculate correlation and R-squared
year_wise_correlations <- data %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(correlation = cor(gpp_sum_8day, yieldgm),
            r_squared = cor(gpp_sum_8day, yieldgm)^2)

year_wise_correlations$Year<-as.numeric(year_wise_correlations$Year)

# Merge the dataframes
merged_df <- year_wise_correlations %>%
  left_join(agg_tbl, by = "Year")

# Print the merged dataframe
print(merged_df)
plot(merged_df$correlation, merged_df$Diff_growth)
write.csv(merged_df, file = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/yieldyearwisecorrelation.csv", row.names = FALSE)
mean(merged_df$Rate_percent, na.rm = TRUE)

########### Mann Kendall Test############# at state scale yield 
# Load packages
# Assuming gppricecounty is your data frame
# Define the file path
file_path <- "C:/Users/rbmahbub/Documents/Data/YieldData/Harvested_ACre_Rice_Arkansas.csv"

# Read the CSV file
stateyield <- read.csv(file_path)
# Remove commas and convert to numeric
stateyield$Value <- as.numeric(gsub(",", "", stateyield$Value))
stateyield$Year<-as.numeric(stateyield$Year)

ggplot(data = stateyield, aes(x = Year, y = Value)) +
  geom_point() +  # Add points
  geom_line() +   # Add lines
  xlab("Year") +  # Label for x-axis
  ylab("County Yield") 

library(trend)
library(tidyverse)
library(remotes)
library(lterdatasampler)
###mk test:https://www.nataliechazal.info/post/mk_rtrend/
aggregated <- gppricecounty %>%
  dplyr::mutate(year = Year) %>%
  group_by(year) %>%
  summarize(meanyield = mean(gpp_sum_8day, na.rm=TRUE)) %>%
  ungroup()
aggregated
##############################
### Mann kendall test of the yield
##############################
mk.test(stateyield$Value)
sens.slope(stateyield$Value)

### Aggregation GPP from the rasterfiles script :allrasterscumulativestack_meansddf
### Mann kendall test of GPP
##############################
allrasterscumulativestack_meansddf
mk.test(allrasterscumulativestack_meansddf$meancumgpp8dayagg)
sens.slope(allrasterscumulativestack_meansddf$meancumgpp8dayagg)

# Calculate increasing rate of GPP
increasing_rate_gpp <- round(((tail(allrasterscumulativestack_meansddf$meancumgpp8dayagg, 1) - allrasterscumulativestack_meansddf$meancumgpp8dayagg[1]) / allrasterscumulativestack_meansddf$meancumgpp8dayagg[1]) * 100, 2)
# Calculate increasing rate of yield
increasing_rate_yield <- round(((tail(stateyield$Value, 1) - stateyield$Value[1]) / stateyield$Value[1]) * 100, 2)
# Print the results
print(paste("Increasing rate of GPP:", increasing_rate_gpp))
print(paste("Increasing rate of yield:", increasing_rate_yield))

### Area values of the 
# Set the working directory
setwd("C:/Users/rbmahbub/Documents/Data/YieldData")
# Read the CSV file
AreaHarvested <- read.csv("Rice_Planted_Harvested_Acres.csv")
AreaHarvested


### Under Data.item column select the rows where Area_planted rows
Areaplanted<-AreaHarvested%>% dplyr::filter(Data.Item == "RICE - ACRES PLANTED")
# Assuming 'Areaplanted' is the name of your dataframe
Areaplanted <- Areaplanted[!(Areaplanted$County %in% c("OTHER COUNTIES", "OTHER (COMBINED) COUNTIES")), ]
Areaplanted$County.ANSI <- sprintf("%03d", Areaplanted$County.ANSI)

# Concatenate COUNTYFP and Year columns to create a new column COUNTYFPYear, which will be used for merging with another dataframe
Areaplanted$COUNTYFPYear <- paste0(Areaplanted$County.ANSI, Areaplanted$Year)
gppricecounty$COUNTYFPYear <- paste0(gppricecounty$COUNTYFP, gppricecounty$Year)

### rename the area column as AreaPlanted from the Value column and convert it to numeric
# Remove comma, rename column, and convert to numeric
# Select specified columns
Areaplanted_selected <- Areaplanted %>%
  dplyr::mutate(AreaPlanted = as.numeric(gsub(",", "", Value))) %>%
  dplyr::select(-Value) %>%
  dplyr::select(County, AreaPlanted, COUNTYFPYear)

gppricecountyarea <- merge(gppricecounty, Areaplanted_selected, by = "COUNTYFPYear")

gppcountyyieldplot <- ggplot(data = gppricecountyarea, aes(x = gpp_sum_8day, y = yieldgm, col = Year, size = AreaPlanted)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  xlab(bquote('Mean Cumulative GPP ('*g~ 'C'~ m^-2~year^-1*')')) +
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')')) +
  scale_y_continuous(limits = c(500, 1000)) +
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020")) +
  scale_size_continuous(name = "Area planted (acre)", labels = scales::comma, breaks = c(25000, 50000, 75000, 100000)) +  # Set the legend breaks and format the labels
  stat_regline_equation(label.x = 1200, label.y = 1000, size = 8) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), 
           p.accuracy = 0.001, label.x = 1200, label.y = 970, size = 8) +
  theme_classic() +
  theme(text = element_text(size = 25)) +
  ggplot2::annotate("text", x = 1800, y = 1000, label = ", RMSE = 61", size = 8) +
  ggplot2::annotate("text", x = 2210, y = 1000, label = "g~C~m^-2~season^-1",size = 8, parse = TRUE)+
  #annotate(geom = "text", x = 1800, y = 1000, label = "g C[2]", size = 8) +
  theme(legend.key.size = unit(1.5, 'cm')) +
  theme(axis.line = element_line(size = 1.7)) +
  theme(axis.ticks.length = unit(.25, "cm")) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

gppcountyyieldplot
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMyieldArea.png",width = 14, height = 8 )
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMyieldArea.png",width = 14, height = 8 )

# Define the breaks and labels
breaks <- c(0, 25000, 50000, 75000, 100000, 144500)
labels <- c("0-25000", "25000-50000", "50000-75000", "75000-100000", "100000-150000")

# Create a data frame to store results
performance_metrics <- data.frame(AreaPlantedRange = labels, MAE = NA, RMSE = NA, Bias = NA, R2 = NA, n = NA)

# Calculate performance metrics for each group
for (i in seq_along(labels)) {
  # Filter the data for the current range
  data_range <- gppricecountyarea %>%
    filter(AreaPlanted > breaks[i] & AreaPlanted <= breaks[i+1])
  
  # Fit linear regression model
  model <- lm(yieldgm ~ gpp_sum_8day, data = data_range)
  
  # Calculate predictions
  predictions <- predict(model)
  
  # Calculate mean absolute error (MAE)
  mae <- mean(abs(predictions - data_range$yieldgm))
  
  # Calculate root mean square error (RMSE)
  rmse <- sqrt(mean((predictions - data_range$yieldgm)^2))
  
  # Calculate bias
  bias <- mean(predictions - data_range$yieldgm)
  
  # Calculate R-squared (R2)
  r2 <- summary(model)$r.squared
  
  # Number of observations
  n <- nrow(data_range)
  
  # Store the metrics in the data frame
  performance_metrics[i, "MAE"] <- mae
  performance_metrics[i, "RMSE"] <- rmse
  performance_metrics[i, "Bias"] <- bias
  performance_metrics[i, "R2"] <- r2
  performance_metrics[i, "n"] <- n
}

# View the performance metrics
print(performance_metrics)
performance_metrics$AreaPlantedRange <- factor(performance_metrics$AreaPlantedRange,
                                               levels = c("0-25000", "25000-50000", "50000-75000",
                                                          "75000-100000", "100000-150000"))

# Plot the boxplot with n values as

# Create a box plot
boxplot <- ggplot(performance_metrics, aes(x = AreaPlantedRange, y = RMSE)) +
  geom_boxplot(fill = "#56B1F7", color = "#132B43") +
  geom_point(aes(y = RMSE), color = "red", size = 8) +  # Add individual data points
  geom_text(aes(label = paste("n =", n)), vjust = -2.5, hjust = 0.5, size = 10) +  # Add n values as labels
  labs(x = "Area Planted Range (acre)", y = expression("Root Mean Squared Error (RMSE) of the \n relation between GPPcum and yield")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24),  # Rotate x-axis labels and set text size
        axis.text.y = element_text(size = 22),  # Set text size for y-axis labels
        axis.title.x = element_text(size = 24),  # Set text size for x-axis title
        axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 10, b = 0, l = 25))) +  # Set text size and adjust position for y-axis title
  scale_y_continuous(limits = c(0, 72))  # Set y-axis limits



# Save the plot
ggsave(filename = "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/boxplot.png", 
       plot = boxplot, width = 18, height = 12)
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/boxplot.png", 
       plot = boxplot, width = 18, height = 12)

# Create a box plot for R2
# Reorder the levels of the "AreaPlantedRange" factor variable
performance_metrics$AreaPlantedRange <- factor(performance_metrics$AreaPlantedRange,
                                               levels = c("0-25000", "25000-50000", "50000-75000",
                                                          "75000-100000", "100000-150000"))

# Plot the boxplot with n values as labels
boxplotr2<-ggplot(performance_metrics, aes(x = AreaPlantedRange, y = R2)) +
  geom_boxplot(fill = "#56B1F7", color = "#132B43") +
  geom_point(aes(y = R2), color = "red", size = 8) +
  geom_text(aes(label = paste("n =", n)), vjust = -1.5, hjust = 0.5, size = 10) +  # Add n values as labels
  labs(x = "Area Planted Range (acre)",  y = expression("R-squared (R"^2*") of the relationship between GPP and yield")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 24),  # Rotate x-axis labels and set text size
        axis.text.y = element_text(size = 22),  # Set text size for y-axis labels
        axis.title.x = element_text(size = 24),  # Set text size for x-axis title
        axis.title.y = element_text(size = 24, margin = margin(t = 0, r = 10, b = 0, l = 25))) +  # Set text size and adjust position for y-axis title
  scale_y_continuous(limits = c(0, 0.6))  # Set y-axis limits
ggsave(filename = "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/boxplotr2.png", 
       plot = boxplotr2, width = 18, height = 12)
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/boxplotr2.png", 
       plot = boxplot, width = 18, height = 12)

# Assuming you have separate plots named plot_rmse and plot_r2
# Assuming you have separate plots named boxplot and boxplotr2
boxplot_no_x_axis <- boxplot + theme(axis.title.x = element_blank(),
                                     axis.text.x = element_blank())
boxplotr2rmse <- grid.arrange(boxplot_no_x_axis + labs(x = NULL) + annotate("text", x = 5, y = 70, label = "A", size = 12),
                               boxplotr2 + annotate("text", x = 5, y =0.6, label = "B", size = 12), 
                               nrow = 2, 
                               ncol = 1)
ggsave(filename = "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/boxplotr2emse.png", 
       plot = boxplotr2rmse, width = 18, height = 24)
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/boxplotr2emse.png", 
       plot = boxplotr2rmse, width = 18, height = 24)
# 'x' must have at least 3 elements
county_counts <- table(gppricecountyarea$County) # Count the occurrences of each county
gppricecountyarea$count_occurrences <- county_counts[gppricecountyarea$County] # Add a new column to the dataframe with the counts
gppricecountyarea <- gppricecountyarea[gppricecountyarea$count_occurrences >= 3, ] # Filter the dataframe to remove observations where the county count is less than 3

#### County basedmann kendall test
# Group by COUNTYFP
# Group by COUNTYFP
mktestacrosscounty <- gppricecountyarea %>%
  dplyr::group_by(COUNTYFP) %>%
  dplyr::summarise(pvalueyield = mk.test(yieldgm)$p.value,
                   trendyield = mk.test(yieldgm)$estimates[["tau"]],
                   pvaluegpp = mk.test(gpp_sum_8day)$p.value,
                   trendgpp = mk.test(gpp_sum_8day)$estimates[["tau"]],
                   meanarea = mean(AreaPlanted),
                   meanGPP = mean(gpp_sum_8day),
                   meanYield = mean(yieldgm),
                   correlatiopvalue = cor.test(yieldgm, gpp_sum_8day)$p.value,
                   correlatiorvalue = cor.test(yieldgm, gpp_sum_8day)$estimate["cor"])
View(mktestacrosscounty)
gppricecountyarea$COUNTYFP
plot(mktestacrosscounty$pvalueyield, mktestacrosscounty$pvaluegpp)

### merge them with the shapefile
# Merge datasets by COUNTYFP
countypvalues <- merge(countyonevariable, mktestacrosscounty, by = "COUNTYFP", all.x = TRUE)
countypvaluesdf<-as.data.frame(countypvalues)



countypvalues <- countypvalues %>%
        dplyr::mutate(pvalueyieldclass = ifelse(pvalueyield < 0.05 & trendgpp > 0, "positive significant",
                                            ifelse(pvalueyield < 0.05 & trendgpp < 0, "negative significant",
                                                   ifelse(pvalueyield >= 0.05 & trendgpp > 0, "positive insignificant", 
                                                          ifelse(pvalueyield >= 0.05 & trendgpp < 0, "negative insignificant", "insignificant")))),
         pvaluegppclass  = ifelse(pvaluegpp < 0.05 & trendyield > 0, "positive significant",
                                  ifelse(pvaluegpp < 0.05 & trendyield < 0, "negative significant",
                                         ifelse(pvaluegpp >= 0.05 & trendyield > 0, "positive insignificant", 
                                                ifelse(pvaluegpp >= 0.05 & trendyield < 0, "negative insignificant", "insignificant")))),
         pcorrelatiopvalue  = ifelse(correlatiopvalue < 0.05 & correlatiorvalue > 0, "positive significant",
                                     ifelse(correlatiopvalue < 0.05 & correlatiorvalue < 0, "negative significant",
                                            ifelse(correlatiopvalue >= 0.05 & correlatiorvalue > 0, "positive insignificant", 
                                                   ifelse(correlatiopvalue >= 0.05 & correlatiorvalue < 0, "negative insignificant", "insignificant")))))


### for the names of the county we will merge two datframes
# Perform left join and keep only unique rows
gppricecountyareacounty <- gppricecountyarea %>%
  dplyr::select(County, COUNTYFP)
countypvalues <- countypvalues %>%
  left_join(gppricecountyareacounty, by = "COUNTYFP", all.x = TRUE) %>%
  distinct(COUNTYFP, .keep_all = TRUE)

##color generator :https://hauselin.github.io/colorpalettejs/
# Plot map of p-values using ggplot and sf



countypvalues$meanaream2<-countypvalues$meanarea * 4046.86
# Create plots
plot1 <- ggplot() +
  geom_sf(data = countypvalues) +
  geom_sf(data = countypvalues %>% filter(!is.na(pvaluegppclass)), aes(fill = pvaluegppclass)) +
  geom_sf_label(data = countypvalues %>% filter(!is.na(pcorrelatiopvalue)), aes(label = County), size = 2)+
  scale_fill_manual(values = c("negative insignificant" = "#FFDB6D", "negative significant"="#D16103","positive insignificant" ="#52854C","positive significant" = "#4E84C4")) +
  labs(fill = paste("p-value (<0.05)\nof Mann-Kendall test\nof county GPP \nacross 13 years", sep = "")) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4)+
  labs(x = NULL, y = NULL)  # Remove "x" and "y" labels
plot1


plot2 <- ggplot() +
  geom_sf(data = countypvalues) +
  geom_sf(data = countypvalues %>% filter(!is.na(pvalueyieldclass)), aes(fill = pvalueyieldclass)) +
  scale_fill_manual(values = c("negative insignificant" = "#FFDB6D", "negative significant"="#D16103","positive insignificant" ="#52854C","positive significant" = "#4E84C4")) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))+
  labs(fill = paste("p-value (<0.05)\nof Mann-Kendall test\nof county yield \nacross 13 years", sep = "")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4)


plot3 <- ggplot() +
  geom_sf(data = countypvalues) +
  geom_sf(data = countypvalues %>% filter(!is.na(meanYield)), aes(fill = meanYield)) +
  
  scale_fill_gradientn(colours = rev(viridis(6))) +
  labs(fill = paste("Mean yield\n(g m\u207b\u00b2 season\u207b\u00b9)", sep = "")) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4)


plot4 <- ggplot() +
  geom_sf(data = countypvalues) +
  geom_sf(data = countypvalues %>% filter(!is.na(meanGPP)), aes(fill = meanGPP)) +
  scale_fill_gradientn(colours = rev(viridis(6))) +
  labs(fill = paste("Mean Cumulative GPP\n(g C m\u207b\u00b2 year\u207b\u00b9)", sep = "")) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4)


plot5 <- ggplot() +
  geom_sf(data = countypvalues) +
  geom_sf(data = countypvalues %>% filter(!is.na(meanarea)), aes(fill = meanarea)) +
  scale_fill_gradientn(colours = rev(viridis(6))) +
  labs(fill = paste("Planted Area \n (Acre)", sep = "")) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))+
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4)

plot6 <- ggplot() +
  geom_sf(data = countypvalues) +
  
  geom_sf(data = countypvalues %>% filter(!is.na(pcorrelatiopvalue)), aes(fill = pcorrelatiopvalue), size =3) +
  
  scale_fill_manual(values = c("negative insignificant" = "#FFDB6D", "negative significant"="#D16103","positive insignificant" ="#52854C","positive significant" = "#4E84C4")) +
  theme_bw() +
  theme(plot.margin = margin(0, 0, 0, 0))+
  labs(fill = paste("p-value (<0.05)\nof Correlation test\nbetween county yield \nand GPP", sep = "")) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.185, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.4)

# Combine plots
combined_plot <- cowplot::plot_grid(plot4, plot3, plot5, plot1, plot2, plot6, ncol = 3, labels = c("A", "B", "C", "D", "E", "F"))

# Save the combined plot as an image file
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/mapgppyieldsignificance.jpg", 
       combined_plot, 
       width = 20, 
       height = 10, 
       dpi = 1000)

ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/mapgppyieldsignificance.jpg", 
       combined_plot, 
       width = 20, 
       height = 10, 
       dpi = 1000)


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
  select(Year, yield)
allrasterscumulativestack_meansddf$Year<-allrasterscumulativestack_meansddf$year

GPPyieldmerge <- merge(allrasterscumulativestack_meansddf, stateyield_selected, by = "Year")
mktestpvalueyield<-mk.test(GPPyieldmerge$yield)$p.value
senslopeyield<-sens.slope(GPPyieldmerge$yield)$estimates["Sen's slope"]

mktestpvaluegpp<-mk.test(GPPyieldmerge$meancumgpp8dayagg)$p.value
senslopegpp<-sens.slope(GPPyieldmerge$meancumgpp8dayagg)$estimates["Sen's slope"]

ggscatter(GPPyieldmerge, x = "year", y = "yield", add = "reg.line", size = 7) +
  geom_point(size = 7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 2016, label.y = 400, size = 8) +
  stat_regline_equation(label.x = 2016, label.y = 500, size = 8) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  scale_y_continuous(limits = c(0, 900), breaks = seq(0, 900, by = 100)) + 
  geom_text(x = 2016, y = 300, label = paste("Mann-Kendall p-value =", round(mktestpvalueyield, 4)), size = 8) +
  geom_text(x = 2016, y = 200, label = paste("Sens Slope =", round(senslopeyield, 2)), size = 8) +
  xlab("Year") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("State wise yield (g ", m^-2, season^-1, ")")))


ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/Yield.png",width = 14, height = 9 )
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/Yield.png",width = 14, height = 9 )



## finding lowest gpp and highest gpp
top_5_df <- countypvalues %>%
  dplyr::select(meanGPP, County, meanYield, meanarea) %>%
  dplyr::arrange(desc(meanGPP)) %>%
  head(10)
top_5_df
top_5_df <- countypvalues %>%
  dplyr::select(meanGPP, County, meanYield, meanarea) %>%
  dplyr::arrange(desc(meanYield)) %>%
  head(5)
top_5_df

top_5_df <- countypvalues %>%
  dplyr::select(meanGPP, County, meanYield, meanarea) %>%
  dplyr::arrange(desc(meanarea)) %>%
  head(5)

# Select relevant columns and sort by meanGPP in ascending order (for lowest values)
top_5_df <- countypvalues %>%
  dplyr::select(meanGPP, County, meanYield, meanarea) %>%
  dplyr::arrange(meanGPP) %>%  # Sort by meanGPP in ascending order
  head(5)
top_5_df
top_5_df <- countypvalues %>%
  dplyr::select(meanGPP, County, meanYield, meanarea) %>%
  dplyr::arrange(meanYield) %>%  # Sort by meanGPP in ascending order
  head(5)
top_5_df


# Define the breaks and labels
breaks <- c(0, 25000, 50000, 75000, 100000, 144500)
labels <- c("0-25000", "25000-50000", "50000-75000", "75000-100000", "100000-150000")

# Create a data frame to store results
top5dataframe <- data.frame(AreaPlantedRange = labels,
                                  Top5Counties = NA, Bottom5Counties = NA)

# Calculate performance metrics for each group
for (i in seq_along(labels)) {
  # Filter the data for the current range
  data_range <- countypvalues %>%
    filter(meanarea > breaks[i] & meanarea <= breaks[i+1])
  
  
  # Find the top five meanGPP counties
  top5 <- head(dplyr::arrange(data_range, desc(meanGPP)), 5)$County
  
  # Find the lowest five meanGPP counties
  bottom5 <- head(dplyr::arrange(data_range, meanGPP), 5)$County
  
  top5dataframe[i, "Top5Counties"] <- toString(top5)
  top5dataframe[i, "Bottom5Counties"] <- toString(bottom5)
}

# View the performance metrics
View(top5dataframe)

