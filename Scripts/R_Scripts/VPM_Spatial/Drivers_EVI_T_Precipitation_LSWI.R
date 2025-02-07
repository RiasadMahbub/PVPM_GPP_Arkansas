### ### VPM
#### Export all the images from google earth engine, the script exports all the images
#### PD data: C:\Users\rbmahbub\Documents\Data\GeospatialData\CumulativeVPM\PD
#### PAR data: C:\Users\rbmahbub\Documents\Data\GeospatialData\CumulativeVPM\PAR
### LSWI data: C:\Users\rbmahbub\Documents\Data\GeospatialData\CumulativeVPM\LSWI
### EVI data: C:\Users\rbmahbub\Documents\Data\GeospatialData\CumulativeVPM\EVI



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
## script: VPMMeanRasterImageAnalysis2008_2020.R

# Specify the directory
dirEVI <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/EVI" ## EVI
dirLSWI <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/LSWI" ## LSWI
dirPAR <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/PAR" ## PAR
dirTemp <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature/MeanTemperature" ## Temperature
dirPD<- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/PD"
#dirraster <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM" ##VPM images at 100 coverage
dirraster <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPMRasterPolygonCoverageFilter" ##VPM images at 100 coverage
dirCF<- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CropFrequency"


# List all files in the directory with a .tif extension
filesEVI <- list.files(dirEVI, pattern = "\\.tif$", full.names = TRUE)
filesLSWI <- list.files(dirLSWI, pattern = "\\.tif$", full.names = TRUE)
filesPAR <- list.files(dirPAR, pattern = "\\.tif$", full.names = TRUE)
filesTemp <- list.files(dirTemp, pattern = "\\.tif$", full.names = TRUE)
filesPD <- list.files(dirPD, pattern = "\\.tif$", full.names = TRUE)
filesraster <- list.files(dirraster, pattern = "\\.tif$", full.names = TRUE)
filesCF<- list.files(dirCF, pattern = "\\.tif$", full.names = TRUE)

# Read the raster images
rasterlistEVI <- vector("list", 13)
rasterlistLSWI <- vector("list", 13)
rasterlistPAR<- vector("list", 13)
rasterlistTemp <-vector("list", 13)
rasterlistPD <-vector("list", 13)
rasterlistraster<-vector ("list", 13)
rasterlistCF<-vector ("list", 6)
file_path<-vector("list", 13)
year<-vector("list", 13)

for (i in 1:13){
  rasterlistEVI[i]<-rast(filesEVI[i])
  rasterlistLSWI[i]<-rast(filesLSWI[i])
  rasterlistPAR[i]<-rast(filesPAR[i])
  rasterlistTemp[i]<-rast(filesTemp[i])
  rasterlistPD[i]<-rast(filesPD[i])
  rasterlistraster[i]<-rast(filesraster[i])
  file_path[i] <- sources(rasterlistraster[[i]])
  year[i] <- as.numeric(sub(".*([0-9]{4})VPMcumulative.*", "\\1", file_path[i])) ### for the VPM images we are changing the name of the bands to year so that it is easy to plot it later
  rasterlistraster[[i]] <- setNames(rasterlistraster[[i]], paste0("AnnualGPP", year[i]))
  
}

##Project the raster images on the projection of 
projected_EVI<-vector("list", 13)
projected_LSWI<-vector("list", 13)
projected_PAR<-vector("list", 13)
projected_Temp<-vector("list", 13)
projected_PD<-vector("list", 13)
projected_raster<-vector("list", 13)
projected_CF<-vector("list", 6)

for (i in 1:13){
  projected_EVI[i] <- terra::project(rasterlistEVI[[i]], rasterlistraster[[i]], method = "average")
  projected_LSWI[i] <- terra::project(rasterlistLSWI[[i]], rasterlistraster[[i]], method = "average")
  projected_PAR[i] <- terra::project(rasterlistPAR[[i]], rasterlistraster[[i]], method = "average")
  projected_PD[i] <- terra::project(rasterlistPD[[i]], rasterlistraster[[i]], method = "average")
  projected_Temp[i] <- terra::project(rasterlistTemp[[i]], rasterlistraster[[i]], method = "average")
}


##Convert the raster images to data frames
projected_EVIdflist<-vector("list", 13)
projected_LSWIdflist<-vector("list", 13)
projected_PARdflist<-vector("list", 13)
projected_Tempdflist<-vector("list", 13)
projected_PDdflist<-vector("list", 13)
projected_rasterdflist<-vector("list", 13)

###
filter_by_3sigma <- function(df, col_index = 3, sd_multiplier = 3) {
  # Calculate mean and standard deviation
  mean_value <- mean(as.numeric(df[, col_index]))
  sd_value <- sd(as.numeric(df[, col_index]))
  # Calculate upper and lower bounds for 3-sigma filtering
  mean_value3sdp <- mean_value + (sd_multiplier * sd_value)
  mean_value3sdn <- mean_value - (sd_multiplier * sd_value)
  # Filter DataFrame within 3-sigma range
  filtered_df <- df[df[, col_index] < mean_value3sdp & df[, col_index] > mean_value3sdn, ]
  return(filtered_df)
}

for (i in 1:13){
  projected_EVIdflist[[i]] <- terra::as.data.frame(projected_EVI[[i]], xy=TRUE)
  projected_EVIdflist[[i]] <- filter_by_3sigma(projected_EVIdflist[[i]])
  projected_LSWIdflist[[i]] <- terra::as.data.frame(projected_LSWI[[i]], xy=TRUE)
  projected_LSWIdflist[[i]] <- filter_by_3sigma(projected_LSWIdflist[[i]])
  projected_PARdflist[[i]] <- terra::as.data.frame(projected_PAR[[i]], xy=TRUE)
  projected_PARdflist[[i]] <- filter_by_3sigma(projected_PARdflist[[i]])
  projected_Tempdflist[[i]] <- terra::as.data.frame(projected_Temp[[i]], xy=TRUE)
  projected_Tempdflist[[i]] <- filter_by_3sigma(projected_Tempdflist[[i]])
  projected_PDdflist[[i]] <- terra::as.data.frame(projected_PD[[i]], xy=TRUE)
  projected_PDdflist[[i]] <- filter_by_3sigma(projected_PDdflist[[i]])
  projected_rasterdflist[[i]] <- terra::as.data.frame(rasterlistraster[[i]], xy=TRUE)
  projected_rasterdflist[[i]] <- filter_by_3sigma(projected_rasterdflist[[i]])
}


##Convert the raster images to data frames
projected_cumulativeEVIdflist<-vector("list", 13)
projected_cumulativeLSWIdflist<-vector("list", 13)
projected_cumulativePARdflist<-vector("list", 13)
projected_cumulativeTempdflist<-vector("list", 13)
projected_cumulativePDdflist<-vector("list", 13)


for (i in 1:13){
  projected_cumulativeEVIdflist[[i]]<- left_join(projected_rasterdflist[[i]], projected_EVIdflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativeEVIdflist[[i]][,3]<- projected_cumulativeEVIdflist[[i]][,3]*8
  projected_cumulativeLSWIdflist[[i]]<- left_join(projected_rasterdflist[[i]], projected_LSWIdflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativeLSWIdflist[[i]][,3]<- projected_cumulativeLSWIdflist[[i]][,3]*8
  projected_cumulativePARdflist[[i]]<- left_join(projected_rasterdflist[[i]], projected_PARdflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativePARdflist[[i]][,3]<- projected_cumulativePARdflist[[i]][,3]*8
  projected_cumulativeTempdflist[[i]]<- left_join(projected_rasterdflist[[i]], projected_Tempdflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativeTempdflist[[i]][,3]<- projected_cumulativeTempdflist[[i]][,3]*8
  projected_cumulativePDdflist[[i]]<- left_join(projected_rasterdflist[[i]], projected_PDdflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativePDdflist[[i]][,3]<- projected_cumulativePDdflist[[i]][,3]*8
}

# Create an empty dataframe to store regression metrics
###EVI
regression_metrics_df_EVI <- data.frame()

for (i in 1:13) {
  # Extract the predictive column name explicitly
  x_col <- names(projected_cumulativeEVIdflist[[i]][4])
  # Fit the linear model
  model <- lm(projected_rasterdflist[[i]][,3] ~ projected_cumulativeEVIdflist[[i]][, 4])
  
  # Extract only the p-value for the slope coefficient
  p_value <- coef(summary(model))[2, 4]
  
  # Store metrics in the dataframe, excluding the intercept row
  regression_metrics_df_EVI <- rbind(
    regression_metrics_df_EVI,
    data.frame(
      i = i,
      rsquared = summary(model)$r.squared,
      p_value = p_value,
      rmse = sqrt(mean(model$residuals^2)),
      slope = model$coefficients[2],
      intercept = model$coefficients[1],
      predictor = x_col  # Add column for predictor name
    )
  )
  colnamey<-colnames((projected_cumulativeEVIdflist[[i]])[3])
  # Create the ggscatter plot
  plot <- ggscatter(projected_cumulativeEVIdflist[[i]], x = "EVI_SG_mean", y = colnamey, add = "reg.line", size = 3) +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 0, label.y = 3000, size = 8
    ) +
    stat_regline_equation(label.x = 0, label.y = 2800, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
    xlab(paste0("Annual mean EVI")) +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("Annual Cumulative Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
  
  # Save the plot with a specific filename
  filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/EVIFigure/EVIFigureEVIGPP_", i, "_", x_col, ".png")
  ggsave(filename, plot, width = 14, height = 8)
}


# Assuming 'i' is an integer column
regression_metrics_df_EVI <- mutate(regression_metrics_df_EVI,
                                i = ifelse(i == 1, 2008,
                                           ifelse(i == 13, 2020,
                                                  ifelse(i >= 2 & i <= 12, i + 2007, i))))
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_df_EVI, "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regression_metrics_df_EVI.csv", row.names = FALSE)




###LSWI
# Create an empty dataframe to store regression metrics
regression_metrics_df_LSWI <- data.frame()

for (i in 1:13) {
  # Extract the predictive column name explicitly
  x_col <- names(projected_cumulativeLSWIdflist[[i]][4])
  
  # Fit the linear model
  model <- lm(projected_rasterdflist[[i]][,3] ~ projected_cumulativeLSWIdflist[[i]][, 4])
  
  # Extract only the p-value for the slope coefficient
  p_value <- coef(summary(model))[2, 4]
  
  # Store metrics in the dataframe, excluding the intercept row
  regression_metrics_df_LSWI <- rbind(
    regression_metrics_df_LSWI,
    data.frame(
      i = i,
      rsquared = summary(model)$r.squared,
      p_value = p_value,
      rmse = sqrt(mean(model$residuals^2)),
      slope = model$coefficients[2],
      intercept = model$coefficients[1],
      predictor = x_col  # Add column for predictor name
    )
  )
  colnamey<-colnames((projected_cumulativeLSWIdflist[[i]])[3])
  # Create the ggscatter plot
  plot <- ggscatter(projected_cumulativeLSWIdflist[[i]], x = "LSWI_SG_mean", y = colnamey, add = "reg.line", size = 3) +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 0, label.y = 3000, size = 8
    ) +
    stat_regline_equation(label.x = 0, label.y = 2800, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
    xlab(paste0("Annual mean LSWI")) +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("Annual Cumulative Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
  
  # Save the plot with a specific filename
  filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/LSWIFigure/LSWIGPP_", i, "_", x_col, ".png")
  ggsave(filename, plot, width = 14, height = 8)
}


# Assuming 'i' is an integer column
regression_metrics_df_LSWI <- mutate(regression_metrics_df_LSWI,
                                    i = ifelse(i == 1, 2008,
                                               ifelse(i == 13, 2020,
                                                      ifelse(i >= 2 & i <= 12, i + 2007, i))))
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_df_LSWI, "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regression_metrics_df_LSWI.csv", row.names = FALSE)




###PAR
# Create an empty dataframe to store regression metrics
regression_metrics_df_PAR <- data.frame()

for (i in 1:13) {
  # Extract the predictive column name explicitly
  x_col <- names(projected_cumulativePARdflist[[i]][4])
  
  # Fit the linear model
  model <- lm(projected_rasterdflist[[i]][,3] ~ projected_cumulativePARdflist[[i]][, 4])
  
  # Extract only the p-value for the slope coefficient
  p_value <- coef(summary(model))[2, 4]
  
  # Store metrics in the dataframe, excluding the intercept row
  regression_metrics_df_PAR <- rbind(
    regression_metrics_df_PAR,
    data.frame(
      i = i,
      rsquared = summary(model)$r.squared,
      p_value = p_value,
      rmse = sqrt(mean(model$residuals^2)),
      slope = model$coefficients[2],
      intercept = model$coefficients[1],
      predictor = x_col  # Add column for predictor name
    )
  )
  colnamey<-colnames((projected_cumulativePARdflist[[i]])[3])
  # Create the ggscatter plot
  plot <- ggscatter(projected_cumulativePARdflist[[i]], x = "par_mean", y = colnamey, add = "reg.line", size = 3) +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 30, label.y = 3000, size = 8
    ) +
    stat_regline_equation(label.x = 30, label.y = 2800, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
    xlab(paste0("Annual mean PAR")) +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("Annual Cumulative Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
  
  # Save the plot with a specific filename
  filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/PARFigure/PARGPP_", i, "_", x_col, ".png")
  ggsave(filename, plot, width = 14, height = 8)
}

# Assuming 'i' is an integer column
regression_metrics_df_PAR <- mutate(regression_metrics_df_PAR,
                                     i = ifelse(i == 1, 2008,
                                                ifelse(i == 13, 2020,
                                                       ifelse(i >= 2 & i <= 12, i + 2007, i))))
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_df_PAR, "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regression_metrics_df_PAR.csv", row.names = FALSE)


###Temp
# Create an empty dataframe to store regression metrics
regression_metrics_df_Temp <- data.frame()

for (i in 1:13) {
  # Extract the predictive column name explicitly
  x_col <- names(projected_cumulativeTempdflist[[i]][4])
  
  # Fit the linear model
  model <- lm(projected_rasterdflist[[i]][,3] ~ projected_cumulativeTempdflist[[i]][, 4])
  
  # Extract only the p-value for the slope coefficient
  p_value <- coef(summary(model))[2, 4]
  
  # Store metrics in the dataframe, excluding the intercept row
  regression_metrics_df_Temp <- rbind(
    regression_metrics_df_Temp,
    data.frame(
      i = i,
      rsquared = summary(model)$r.squared,
      p_value = p_value,
      rmse = sqrt(mean(model$residuals^2)),
      slope = model$coefficients[2],
      intercept = model$coefficients[1],
      predictor = x_col  # Add column for predictor name
    )
  )
  colnamey<-colnames((projected_cumulativeTempdflist[[i]])[3])
  # Create the ggscatter plot
  plot <- ggscatter(projected_cumulativeTempdflist[[i]], x = "tmean_mean", y = colnamey, add = "reg.line", size = 3) +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 30, label.y = 3000, size = 8
    ) +
    stat_regline_equation(label.x = 30, label.y = 2800, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
    xlab(paste0("Annual mean Temperature")) +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("Annual Cumulative Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
  
  # Save the plot with a specific filename
  filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/TemperatureFigure/TempGPP_", i, "_", x_col, ".png")
  ggsave(filename, plot, width = 14, height = 8)
}


# Assuming 'i' is an integer column
regression_metrics_df_Temp <- mutate(regression_metrics_df_Temp,
                                    i = ifelse(i == 1, 2008,
                                               ifelse(i == 13, 2020,
                                                      ifelse(i >= 2 & i <= 12, i + 2007, i))))
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_df_Temp, "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regression_metrics_df_Temp.csv", row.names = FALSE)



# Create an empty dataframe to store regression metrics
###PD
regression_metrics_df_PD <- data.frame()

for (i in 1:13) {
  # Extract the predictive column name explicitly
  x_col <- names(projected_cumulativePDdflist[[i]][4])
  
  # Fit the linear model
  model <- lm(projected_rasterdflist[[i]][,3] ~ projected_cumulativePDdflist[[i]][, 4])
  
  # Extract only the p-value for the slope coefficient
  p_value <- coef(summary(model))[2, 4]
  
  # Store metrics in the dataframe, excluding the intercept row
  regression_metrics_df_PD <- rbind(
    regression_metrics_df_PD,
    data.frame(
      i = i,
      rsquared = summary(model)$r.squared,
      p_value = p_value,
      rmse = sqrt(mean(model$residuals^2)),
      slope = model$coefficients[2],
      intercept = model$coefficients[1],
      predictor = x_col  # Add column for predictor name
    )
  )

  colnamey<-colnames((projected_cumulativePDdflist[[i]])[3])
  # Create the ggscatter plot
  plot <- ggscatter(projected_cumulativePDdflist[[i]], x = "DOP_mean_mean", y = colnamey, add = "reg.line", size = 3) +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 30, label.y = 3000, size = 8
    ) +
    stat_regline_equation(label.x = 30, label.y = 2800, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
    xlab(paste0("Planting Date")) +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("Annual Cumulative Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
  
  # Save the plot with a specific filename
  filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/PDFigure/PDGPP_", i, "_", x_col, ".png")
  ggsave(filename, plot, width = 14, height = 8)
}
# Assuming 'i' is an integer column
regression_metrics_df_PD <- mutate(regression_metrics_df_PD,
                                    i = ifelse(i == 1, 2008,
                                               ifelse(i == 13, 2020,
                                                      ifelse(i >= 2 & i <= 12, i + 2007, i))))
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_df_PD, "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/regression_metrics_df_PD.csv", row.names = FALSE)

###############################################################
###############################################################
###############################################################
###############################################################
#### Plot EVI and GPP all years into one graph
# Initialize an empty DataFrame
merged_df_EVIGPP <- data.frame()
merged_df_LSWIGPP<-data.frame()
merged_df_PARGPP<-data.frame()
merged_df_TempGPP<-data.frame()

projected_cumulativeEVIdflistyear<-projected_cumulativeEVIdflist
projected_cumulativeLSWIdflistyear<-projected_cumulativeLSWIdflist
projected_cumulativepardflistyear<-projected_cumulativePARdflist
projected_cumulativeTempdflistyear<-projected_cumulativeTempdflist
for (i in 1:13) {
  projected_cumulativeEVIdflistyear[[i]]$year <- 2007 + i
  projected_cumulativeLSWIdflistyear[[i]]$year <- 2007 + i
  projected_cumulativepardflistyear[[i]]$year <- 2007 + i
  projected_cumulativeTempdflistyear[[i]]$year <- 2007 + i# Calculate year based on position in list
}


for (i in 1:13) {
  # Rename columns to remove trailing digits
  names(projected_cumulativeEVIdflistyear[[i]]) <- sub("\\d{4}$", "", names(projected_cumulativeEVIdflistyear[[i]]))
  names(projected_cumulativeLSWIdflistyear[[i]]) <- sub("\\d{4}$", "", names(projected_cumulativeLSWIdflistyear[[i]]))
  names(projected_cumulativepardflistyear[[i]]) <- sub("\\d{4}$", "", names(projected_cumulativepardflistyear[[i]]))
  names(projected_cumulativeTempdflistyear[[i]]) <- sub("\\d{4}$", "", names(projected_cumulativeTempdflistyear[[i]]))
  
  # Bind rows to the merged DataFrame
  merged_df_EVIGPP <- bind_rows(merged_df_EVIGPP, projected_cumulativeEVIdflistyear[[i]])
  merged_df_LSWIGPP <- bind_rows(merged_df_LSWIGPP, projected_cumulativeLSWIdflistyear[[i]])
  merged_df_PARGPP <- bind_rows(merged_df_PARGPP, projected_cumulativepardflistyear[[i]])
  merged_df_TempGPP <- bind_rows(merged_df_TempGPP, projected_cumulativeTempdflistyear[[i]])
}




mergedEVILSWIPARGPP<-cbind(merged_df_EVIGPP, merged_df_LSWIGPP, merged_df_PARGPP, merged_df_TempGPP)
View(mergedEVILSWIPARGPP)
columns_to_select <- c("x", "y", "AnnualGPP", "year", "EVI_SG_mean", "LSWI_SG_mean", "par_mean", "tmean_mean")
mergedEVILSWIPARGPP <- mergedEVILSWIPARGPP[, columns_to_select]
write.csv(mergedEVILSWIPARGPP, file = file.path('C:\\Users\\rbmahbub\\Documents\\RProjects\\VPMmodel\\VPMmodel\\Figures', 'mergedEVILSWIPARGPP.csv'), row.names = FALSE)
mergedEVILSWIPARGPP <- read.csv('C:\\Users\\rbmahbub\\Documents\\RProjects\\VPMmodel\\VPMmodel\\Figures\\mergedEVILSWIPARGPP.csv')

### Get rid of the values of EVI that are greater than 1 
mergedEVILSWIPARGPP <- mergedEVILSWIPARGPP[mergedEVILSWIPARGPP$EVI_SG_mean <= 1, ]
mergedEVILSWIPARGPP <- na.omit(mergedEVILSWIPARGPP)
#mergedEVILSWIPARGPP <- head(mergedEVILSWIPARGPP, 1000)


#############EVI###########################
evi<-ggscatter(mergedEVILSWIPARGPP, x = "EVI_SG_mean", y = "AnnualGPP", size =0)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001, label.x = 0.1, label.y = 3000, size = 8) +
  stat_regline_equation(label.x = 0.1, label.y = 2800, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  xlab(paste0("Annual Mean EVI")) +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_bw()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Annual Cumulative GPP (g C ", m^-2, year^-1, ")")))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))

ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/EVIFigure/EVI_SG_mean_vs_AnnualGPP_byCount.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 10,           # Adjust width and height as needed
       height = 7)

###LSWI
lswi<-ggscatter(mergedEVILSWIPARGPP, x = "LSWI_SG_mean", y = "AnnualGPP", size =0)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001, label.x = -0.1, label.y = 3400, size = 8) +
  stat_regline_equation(label.x = -0.1, label.y = 3200, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  xlab(paste0("Annual Mean LSWI")) +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_bw()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Annual Cumulative GPP (g C ", m^-2, year^-1, ")")))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))

ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/LSWIFigure/LSWI_SG_mean_vs_AnnualGPP_byCount.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 10,           # Adjust width and height as needed
       height = 7)

###PAR
par<-ggscatter(mergedEVILSWIPARGPP, x = "par_mean", y = "AnnualGPP", size =0)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001, label.x = 27, label.y = 3400, size = 8) +
  stat_regline_equation(label.x = 27, label.y = 3200, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  xlab(paste0("Annual Mean PAR")) +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_bw()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Annual Cumulative GPP (g C ", m^-2, year^-1, ")")))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))

ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/PARFigure/PAR_SG_mean_vs_AnnualGPP_by_Year.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 10,           # Adjust width and height as needed
       height = 7)

### Temperature
temp<-ggscatter(mergedEVILSWIPARGPP, x = "tmean_mean", y = "AnnualGPP", size =0)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    p.accuracy = 0.001, label.x = 15.5, label.y = 3400, size = 8) +
  stat_regline_equation(label.x = 15.5, label.y = 3200, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3500)) +
  xlab(paste0("Annual Mean Temperature")) +
  geom_bin2d(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method="lm", size = 3, se=FALSE,)+
  theme_bw()+
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Annual Cumulative GPP (g C ", m^-2, year^-1, ")")))+
  theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20)
        ,legend.key.size = unit(1.5, "cm")) +
  guides(fill = guide_colorbar(title = "Count", label.theme = element_text(size = 20)))
temp

ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/TemperatureFigure/Temp_SG_mean_vs_AnnualGPP_by_Year.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 10,           # Adjust width and height as needed
       height = 7)

ggarrange(evi, lswi, par, temp , 
          labels = c("A", "B", "C", "D"),
          font.label = list(size = 24, color = "black", face = "bold", family = NULL),
          ncol = 2, nrow = 2)
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/EVIPARTEMPGPPLSWIarrange.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 25,           # Adjust width and height as needed
       height = 20)          # Adjust width and height as needed

# Your ggarrange code
arranged_plot <- ggarrange(evi, lswi, par, temp + rremove("x.text"),
                           labels = c("A", "B", "C", "D"),
                           
                           ncol = 2, nrow = 2)
# Increase font size and place labels on the right
final_plot <- plot_grid(
  arranged_plot,
  labels = c("A", "B", "C", "D"),
  label_size = 24,
  align = "v",
  hjust = 1
)
# Display the final_plot
final_plot
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/EVIPARTEMPGPPLSWIfinalplot.png",  # Customize filename and format
       plot = last_plot(),    # Ensure we save the last generated plot
       width = 22,           # Adjust width and height as needed
       height = 16)          # Adjust width and height as needed


# Initialize an empty DataFrame
merged_df_EVIGPP <- data.frame()
merged_df_LSWIGPP <- data.frame()
merged_df_PARGPP <- data.frame()
merged_df_TempGPP <- data.frame()
#### high and low region analysis
# Initialize an empty DataFrame to store the merged results
###naming for (i in 1:length(projected_EVIdflist)) {
for (i in 1:13) {
  year <- 2008 + i - 1  # Calculate the corresponding year for each DataFrame
  names(projected_EVIdflist[[i]])[names(projected_EVIdflist[[i]]) == "EVI_SG_mean"] <- paste0("EVI_SG_mean_", year)
  names(projected_LSWIdflist[[i]])[names(projected_LSWIdflist[[i]]) == "LSWI_SG_mean"] <- paste0("LSWI_SG_mean", year)
  names(projected_PARdflist[[i]])[names(projected_PARdflist[[i]]) == "par_mean"] <- paste0("par_mean_", year)
  names(projected_Tempdflist[[i]])[names(projected_Tempdflist[[i]]) == "tmean_mean"] <- paste0("tmean_mean", year)
  names(projected_PDdflist[[i]])[names(projected_PDdflist[[i]]) == "DOP_mean_mean"] <- paste0("DOPmean_mean", year)
}



merged_df <- projected_rasterdflist[[1]]
merged_dfEVI <- projected_EVIdflist[[1]]
merged_dfLSWI <- projected_LSWIdflist[[1]]
merged_dfPAR <- projected_PARdflist[[1]]
merged_dfTemp <- projected_Tempdflist[[1]]
merged_dfPD <- projected_PDdflist[[1]]

# Iterate through the remaining DataFrames and merge them
for (i in 2:13) {
  merged_df <- merge(merged_df, projected_rasterdflist[[i]], by = c("x", "y"), all = TRUE)
  merged_dfEVI <- merge(merged_dfEVI, projected_EVIdflist[[i]], by = c("x", "y"), all = TRUE)
  merged_dfLSWI <- merge(merged_dfLSWI, projected_LSWIdflist[[i]], by = c("x", "y"), all = TRUE)
  merged_dfPAR <- merge(merged_dfPAR, projected_PARdflist[[i]], by = c("x", "y"), all = TRUE)
  merged_dfTemp <- merge(merged_dfTemp, projected_Tempdflist[[i]], by = c("x", "y"), all = TRUE)
  merged_dfPD <- merge(merged_dfPD, projected_PDdflist[[i]], by = c("x", "y"), all = TRUE)
}

# Columns to check
columns_to_check <- c("2008AnnualGPP", "2009AnnualGPP" ,"2010AnnualGPP", "2011AnnualGPP",
                      "2012AnnualGPP", "2013AnnualGPP", "2014AnnualGPP" ,"2015AnnualGPP", "2016AnnualGPP" ,"2017AnnualGPP",
                      "2018AnnualGPP", "2019AnnualGPP", "2020AnnualGPP")
columns_to_checkEVI <- c("EVI_SG_mean_2008", "EVI_SG_mean_2009" ,"EVI_SG_mean_2010" ,"EVI_SG_mean_2011",
                          "EVI_SG_mean_2012", "EVI_SG_mean_2013" ,"EVI_SG_mean_2014",
                          "EVI_SG_mean_2015" ,"EVI_SG_mean_2016", "EVI_SG_mean_2017",
                          "EVI_SG_mean_2018", "EVI_SG_mean_2019", "EVI_SG_mean_2020")
columns_to_checkLSWI <- c("LSWI_SG_mean2008", "LSWI_SG_mean2009" ,"LSWI_SG_mean2010" ,"LSWI_SG_mean2011",
                          "LSWI_SG_mean2012", "LSWI_SG_mean2013" ,"LSWI_SG_mean2014",
                          "LSWI_SG_mean2015" ,"LSWI_SG_mean2016", "LSWI_SG_mean2017",
                          "LSWI_SG_mean2018", "LSWI_SG_mean2019", "LSWI_SG_mean2020")
columns_to_checkTemp <- c("tmean_mean2008", "tmean_mean2009" ,"tmean_mean2010" ,"tmean_mean2011",
                          "tmean_mean2012", "tmean_mean2013" ,"tmean_mean2014",
                          "tmean_mean2015" ,"tmean_mean2016", "tmean_mean2017",
                          "tmean_mean2018", "tmean_mean2019", "tmean_mean2020")

columns_to_checkPAR <- c("par_mean_2008", "par_mean_2009" ,"par_mean_2010" ,"par_mean_2011",
                         "par_mean_2012", "par_mean_2013" ,"par_mean_2014",
                         "par_mean_2015" ,"par_mean_2016", "par_mean_2017",
                         "par_mean_2018", "par_mean_2019", "par_mean_2020")


columns_to_checkPD <- c("DOPmean_mean2008", "DOPmean_mean2009" ,"DOPmean_mean2010" ,"DOPmean_mean2011",
                        "DOPmean_mean2012", "DOPmean_mean2013" ,"DOPmean_mean2014",
                        "DOPmean_mean2015" ,"DOPmean_mean2016", "DOPmean_mean2017",
                        "DOPmean_mean2018", "DOPmean_mean2019", "DOPmean_mean2020")

# Remove rows where at least 11 columns have NA values
merged_df_df <- merged_df[rowSums(is.na(merged_df[, columns_to_check])) <= 5, , drop = TRUE]
merged_df_df_EVI <- merged_dfEVI[rowSums(is.na(merged_dfEVI[, columns_to_checkEVI])) <= 5, , drop = TRUE]
merged_df_df_LSWI <- merged_dfLSWI[rowSums(is.na(merged_dfLSWI[, columns_to_checkLSWI])) <= 5, , drop = TRUE]
merged_df_df_PAR <- merged_dfPAR[rowSums(is.na(merged_dfPAR[, columns_to_checkPAR])) <= 5, , drop = TRUE]
merged_df_df_Temp <- merged_dfTemp[rowSums(is.na(merged_dfTemp[, columns_to_checkTemp])) <= 5, , drop = TRUE]
merged_df_df_PD <- merged_dfPD[rowSums(is.na(merged_dfPD[, columns_to_checkPD])) <= 5, , drop = TRUE]
# Function to calculate slope and p-value for each row
calculate_slope_pvalue <- function(row) {
  model <- lm(unlist(row) ~ c(1:length(row)))  # Fit linear model
  slope <- model$coefficients[[2]]              # Extract slope
  p_value <- summary(model)$coefficients[2, 4]  # Extract p-value
  return(c(slope, p_value))
}

# Apply the function to each row and add as new columns
merged_df_df_slope <- merged_df_df %>%
  rowwise() %>%
  dplyr::mutate(slope = calculate_slope_pvalue(c_across(all_of(columns_to_check)))[1],
         p_value = calculate_slope_pvalue(c_across(all_of(columns_to_check)))[2])
merged_df_df_EVI_slope <- merged_df_df_EVI %>%
  rowwise() %>%
  dplyr::mutate(slopeEVI = calculate_slope_pvalue(c_across(all_of(columns_to_checkEVI)))[1],
         p_valueEVI = calculate_slope_pvalue(c_across(all_of(columns_to_checkEVI)))[2])

merged_df_df_LSWI_slope <- merged_df_df_LSWI %>%
  rowwise() %>%
  dplyr::mutate(slopeLSWI = calculate_slope_pvalue(c_across(all_of(columns_to_checkLSWI)))[1],
         p_valueLSWI = calculate_slope_pvalue(c_across(all_of(columns_to_checkLSWI)))[2])
merged_df_df_PAR_slope <- merged_df_df_PAR %>%
  rowwise() %>%
  dplyr::mutate(slopePAR = calculate_slope_pvalue(c_across(all_of(columns_to_checkPAR)))[1],
         p_valuePAR = calculate_slope_pvalue(c_across(all_of(columns_to_checkPAR)))[2])
merged_df_df_Temp_slope <- merged_df_df_Temp %>%
  rowwise() %>%
  dplyr::mutate(slopeTemp = calculate_slope_pvalue(c_across(all_of(columns_to_checkTemp)))[1],
         p_valueTemp = calculate_slope_pvalue(c_across(all_of(columns_to_checkTemp)))[2])
merged_df_df_PD_slope <- merged_df_df_PD %>%
  rowwise() %>%
  dplyr::mutate(slopePD = calculate_slope_pvalue(c_across(all_of(columns_to_checkPD)))[1],
         p_valuePD = calculate_slope_pvalue(c_across(all_of(columns_to_checkPD)))[2])


merged_df_df_slope_pvalue <- merged_df_df_slope[merged_df_df_slope$p_value < 0.05, ]
### The ones where GPP is low and high merge them with EVI and other parameters
merged_df_df_slope_pvalue_EVI<- merge(merged_df_df_slope_pvalue, merged_df_df_EVI_slope, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
merged_df_df_slope_pvalue_LSWI<- merge(merged_df_df_slope_pvalue, merged_df_df_LSWI_slope , by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
merged_df_df_slope_pvalue_Temp<- merge(merged_df_df_slope_pvalue, merged_df_df_Temp_slope, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
merged_df_df_slope_pvalue_PD<- merge(merged_df_df_slope_pvalue, merged_df_df_PD_slope, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
merged_df_df_slope_pvalue_PAR<- merge(merged_df_df_slope_pvalue, merged_df_df_PAR_slope, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)

plot(merged_df_df_slope_pvalue_EVI$slopeEVI, merged_df_df_slope_pvalue_EVI$slope)
plot(merged_df_df_slope_pvalue_LSWI$slopeLSWI, merged_df_df_slope_pvalue_LSWI$slope)
plot(merged_df_df_slope_pvalue_Temp$slopeTemp, merged_df_df_slope_pvalue_Temp$slope)
plot(merged_df_df_slope_pvalue_PD$slopePD, merged_df_df_slope_pvalue_PD$slope)
plot(merged_df_df_slope_pvalue_PAR$slopePAR, merged_df_df_slope_pvalue_PAR$slope)

plot(merged_df_df_slope_pvalue_Temp$slopeTemp, merged_df_df_slope_pvalue_EVI$slopeEVI) 

slopeEVIdflist<-vector("list", 13)
slopeLSWIdflist<-vector("list", 13)
slopePARdflist<-vector("list", 13)
slopeTempdflist<-vector("list", 13)
slopePDdflist<-vector("list", 13)
sloperasterdflist<-vector("list", 13)
for (i in 1:13){
  ## slope
  slopeEVIdflist[[i]]<- merge(merged_df_df_slope_pvalue, projected_EVIdflist[[i]], by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
  slopeLSWIdflist[[i]]<- merge(merged_df_df_slope_pvalue, projected_LSWIdflist[[i]], by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
  slopePARdflist[[i]]<- merge(merged_df_df_slope_pvalue, projected_PARdflist[[i]], by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
  slopeTempdflist[[i]]<- merge(merged_df_df_slope_pvalue, projected_Tempdflist[[i]], by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
  slopePDdflist[[i]]<- merge(merged_df_df_slope_pvalue, projected_PDdflist[[i]], by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
  sloperasterdflist[[i]]<- merge(merged_df_df_slope_pvalue, projected_rasterdflist[[i]], by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
  
}





# Iterate through the remaining DataFrames and merge them
for (i in 2:13) {

}
merged_dfPD

### merging slope values with EVI,LSWI,PAR, TEMP ##new variable: 
mergeEVIslope<-merge(merged_df_df_slope_pvalue, merged_dfEVI, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
mergeLSWIslope<-merge(merged_df_df_slope_pvalue, merged_dfLSWI, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
mergePARslope<-merge(merged_df_df_slope_pvalue, merged_dfPAR, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
mergeTempslope<-merge(merged_df_df_slope_pvalue, merged_dfTemp, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)
mergePDslope<-merge(merged_df_df_slope_pvalue, merged_dfPD, by.x = c("x", "y"), by.y = c("x", "y"), all.x = TRUE)

### Calculate the means of EVI, LSWI, PAR, Temp across differennt years
# Calculate mean of EVI_SG_mean columns
##EVI###
evi_sg_mean_mean <- mergeEVIslope %>%
  dplyr::select(starts_with("EVI_SG_mean_")) %>%
  rowMeans(na.rm = TRUE)
# Calculate mean of AnnualGPP columns
annual_gpp_mean <- mergeEVIslope %>%
  dplyr::select(starts_with("20")) %>%  # Select columns starting with "20" for years
  rowMeans(na.rm = TRUE)
mergeEVIslope$EVI_SG_mean_mean <- evi_sg_mean_mean
mergeEVIslope$Annual_GPP_mean <- annual_gpp_mean
mergeEVIslope$slope_sign <- ifelse(mergeEVIslope$slope > 0, "positive", "negative") ##slope signs


###LSWI###
lswi_sg_mean_mean <- mergeLSWIslope %>%
  dplyr::select(starts_with("LSWI_SG_mean")) %>%
  rowMeans(na.rm = TRUE)
# Calculate mean of AnnualGPP columns
annual_gpp_mean <- mergeLSWIslope %>%
  dplyr::select(starts_with("20")) %>%  # Select columns starting with "20" for years
  rowMeans(na.rm = TRUE)
mergeLSWIslope$LSWI_SG_mean_mean <- lswi_sg_mean_mean
mergeLSWIslope$Annual_GPP_mean <- annual_gpp_mean
mergeLSWIslope$slope_sign <- ifelse(mergeLSWIslope$slope > 0, "positive", "negative") ##slope signs


###PAR###
par_sg_mean_mean <- mergePARslope %>%
  dplyr::select(starts_with("par_mean_")) %>%
  rowMeans(na.rm = TRUE)
# Calculate mean of AnnualGPP columns
annual_gpp_mean <- mergePARslope %>%
  dplyr::select(starts_with("20")) %>%  # Select columns starting with "20" for years
  rowMeans(na.rm = TRUE)
mergePARslope$par_sg_mean_mean <- par_sg_mean_mean
mergePARslope$Annual_GPP_mean <- annual_gpp_mean
mergePARslope$slope_sign <- ifelse(mergePARslope$slope > 0, "positive", "negative") ##slope signs

###Temperature###
temp_sg_mean_mean <- mergeTempslope %>%
  dplyr::select(starts_with("tmean_mean")) %>%
  rowMeans(na.rm = TRUE)
# Calculate mean of AnnualGPP columns
annual_gpp_mean <- mergeTempslope %>%
  dplyr::select(starts_with("20")) %>%  # Select columns starting with "20" for years
  rowMeans(na.rm = TRUE)

mergeTempslope$temp_sg_mean_mean <- temp_sg_mean_mean
mergeTempslope$Annual_GPP_mean <- annual_gpp_mean
mergeTempslope$slope_sign <- ifelse(mergeTempslope$slope > 0, "positive", "negative") ##slope signs


##PD###
PD_sg_mean_mean <- mergePDslope %>%
  dplyr::select(starts_with("DOPmean_mean")) %>%
  rowMeans(na.rm = TRUE)
# Calculate mean of AnnualGPP columns
annual_gpp_mean <- mergePDslope %>%
  dplyr::select(starts_with("20")) %>%  # Select columns starting with "20" for years
  rowMeans(na.rm = TRUE)

mergePDslope$PD_sg_mean_mean <- PD_sg_mean_mean
mergePDslope$Annual_GPP_mean <- annual_gpp_mean
mergePDslope$slope_sign <- ifelse(mergePDslope$slope > 0, "positive", "negative") ##slope signs
#### Merge the means into one
# Assuming all DataFrames are in the same environment

mergeEVIslopefiltered<-mergeEVIslope%>% dplyr::select(EVI_SG_mean_mean, x, y)
mergeLSWIslopefiltered<-mergeLSWIslope%>% dplyr::select(LSWI_SG_mean_mean, x, y)
mergePARslopefiltered<-mergePARslope%>% dplyr::select(par_sg_mean_mean, x, y)
mergeTempslopefiltered<-mergeTempslope%>% dplyr::select(temp_sg_mean_mean, x, y)
mergePDslopefiltered<-mergePDslope%>% dplyr::select(PD_sg_mean_mean, x, y, slope, slope_sign, Annual_GPP_mean)

merged_PDEVILSWITEMPPAR <- merge(mergeEVIslopefiltered, mergeLSWIslopefiltered, by = c("x", "y")) %>%
  merge(., mergePARslopefiltered, by = c("x", "y")) %>%
  merge(., mergeTempslopefiltered, by = c("x", "y")) %>%
  merge(., mergePDslopefiltered, by = c("x", "y"))


p_value <- compare_means(EVI_SG_mean_mean ~ slope_sign, data = mergeEVIslope) %>%
  .$p %>%
  format(scientific = FALSE, digits = 3)

# Define color palette for classes
color_palette <- c("positive" = "#999999", "negative" = "#E69F00")

# Create the plot
ggplot(mergeEVIslope, aes(x = slope_sign, y = EVI_SG_mean_mean, fill = slope_sign)) +
  geom_boxplot(notch = TRUE, showmeans = TRUE) +
  labs(
       x = "Slope Sign", y = "EVI", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()


ggplot(merged_PDEVILSWITEMPPAR, aes(x = EVI_SG_mean_mean, y = slope, color = temp_sg_mean_mean)) +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Mean EVI", y = "Slope", color = "temp_sg_mean_mean"
  ) +
  scale_color_viridis(limits = c(min(merged_PDEVILSWITEMPPAR$temp_sg_mean_mean), max(merged_PDEVILSWITEMPPAR$temp_sg_mean_mean))) +
  theme_bw()+
  theme(legend.position = c(0.3, 0.85), legend.direction = "horizontal")

filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/EVI/BoxPlotEVIGPP_",".png")
ggsave(filename, plot, width = 14, height = 8)


ggplot(mergeLSWIslope, aes(x = slope_sign, y = LSWI_SG_mean_mean, fill = slope_sign)) +
  geom_boxplot(notch = TRUE, showmeans = TRUE) +
  labs(
       x = "Slope Sign", y = "LSWI", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()

ggplot(mergeLSWIslope, aes(x = LSWI_SG_mean_mean, y = slope, fill = slope_sign)) +
  geom_point() +
  labs(
    x = "Mean LSWI (2008-2020)", y = "slope", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()

ggplot(mergePARslope, aes(x = slope_sign, y = par_sg_mean_mean, fill = slope_sign)) +
  geom_boxplot(notch = TRUE, showmeans = TRUE) +
  labs(
       x = "Slope Sign", y = "PAR", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()

ggplot(mergePARslope, aes(x = par_sg_mean_mean, y = slope, fill = slope_sign)) +
  geom_point() +
  labs(
    x = "Mean PAR (2008-2020)", y = "slope", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()

ggplot(mergeTempslope, aes(x = slope_sign, y = temp_sg_mean_mean, fill = slope_sign)) +
  geom_boxplot(notch = FALSE, showmeans = TRUE) +
  labs(
       x = "Slope Sign", y = "Mean Air Temperature", fill = "Slope Sign") +
  scale_fill_brewer(palette="Dark2") +
  theme_classic()+
  theme(text = element_text(size = 16))+
  theme(legend.position="bottom")

ggplot(mergeTempslope, aes(x = temp_sg_mean_mean, y = slope, fill = slope_sign)) +
  geom_point() +
  labs(
    x = "Mean PAR (2008-2020)", y = "slope", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()

ggplot(mergePDslope, aes(x = slope_sign, y = PD_sg_mean_mean, fill = slope_sign)) +
  geom_boxplot(notch = FALSE, showmeans = TRUE) +
  labs(
    x = "Slope Sign", y = "PlantingDate", fill = "Slope Sign") +
  scale_fill_brewer(palette="Dark2") +
  theme_classic()+
  theme(text = element_text(size = 16))+
  theme(legend.position="bottom")

ggplot(mergePDslope, aes(x = temp_sg_mean_mean, y = slope, fill = slope_sign)) +
  geom_point() +
  labs(
    x = "Mean PAR (2008-2020)", y = "slope", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()


### Merge Four dataframes into one


### Scale the data


#### Plot EVI and GPP all years into one graph
# Initialize an empty DataFrame
merged_df_EVIGPP <- data.frame()
merged_df_LSWIGPP<-data.frame()
merged_df_parGPP<-data.frame()
merged_df_Temp<-data.frame()

