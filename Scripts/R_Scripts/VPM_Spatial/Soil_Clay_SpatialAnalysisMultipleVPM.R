library(sf)
library(dplyr)
library(sp) ## prerequisite for the raster package
library(raster) #raster(read the rasterfile)
library(terra) # The terra::project function is used in the terra package to project (resample/aggregate) one raster to the grid of another.
library(ggplot2)
library(ggpubr)
library(ggtext)

# Specify the directory
dirclay <- "C://Users//rbmahbub//Box//Research//Data//VPM//SoilClay" ## windows directory
dirSOC <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/SoilOrganicCarbon"## soilorganiccarbon
dirraster <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM"
file_name_clay <- "ClayContentArkansas.tif"
file_name_SOC <- "SoilOrganicCarbonArkansas.tif"

# List all files in the directory with a .tif extension
files <- list.files(dirraster, pattern = "\\.tif$", full.names = TRUE)

# Read the raster images
rasterlist <- vector("list", 13)
file_path<-vector("list", 13)
for (i in 1:13){
  rasterlist[i]<-rast(files[i])
  file_path[i] <- sources(rasterlist[[i]])
  year[i] <- as.numeric(sub(".*([0-9]{4})VPMcumulative.*", "\\1", file_path[i])) ### for the VPM images we are changing the name of the bands to year so that it is easy to plot it later
  rasterlist[i]<-setNames(rasterlist[[i]], paste0(year[i], "AnnualGPP"))
  
}
raster_path_clay <- file.path(dirclay, file_name_clay)
clay_raster <- rast(raster_path_clay)

raster_path_SOC <- file.path(dirSOC, file_name_SOC)
SOC_raster <- rast(raster_path_SOC)


##projected
projected_claylist<-vector("list", 13)
projected_SOC<-vector("list", 13)
for (i in 1:13){
  projected_claylist[i]<- terra::project(clay_raster, rasterlist[[i]], method = "average")
  projected_SOC[i]<- terra::project(SOC_raster, rasterlist[[i]], method = "average")
  
}

projected_dflist<-vector("list", 13)
cumulative_dflist<-vector("list", 13)
projected_SOCdflist<-vector("list", 13)
for (i in 1:13){
  projected_dflist[[i]] <- terra::as.data.frame(projected_claylist[[i]], xy=TRUE)
  cumulative_dflist[[i]] <- terra::as.data.frame(rasterlist[[i]], xy=TRUE)
  projected_SOCdflist[[i]] <- terra::as.data.frame(projected_SOC[[i]], xy=TRUE)
}

### List dataframe problem, increase the bracket size

projected_cumulativedflist<-vector("list", 13)
projected_cumulativeSOCdflist<-vector("list", 13)
for (i in 1:13){
  projected_cumulativedflist[[i]]<- left_join(cumulative_dflist[[i]], projected_dflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativedflist[[i]][,3]<- projected_cumulativedflist[[i]][,3]*8
  projected_cumulativedflist[[i]]$mean_b <- rowMeans(projected_cumulativedflist[[i]][, c("b0", "b10", "b30", "b60", "b100", "b200")])
  projected_cumulativedflist[[i]]$mean_b30 <- rowMeans(projected_cumulativedflist[[i]][, c("b0", "b10", "b30")])
  projected_cumulativedflist[[i]]$mean_b100 <- rowMeans(projected_cumulativedflist[[i]][, c("b30", "b60", "b100")])
  
  projected_cumulativeSOCdflist[[i]]<- left_join(cumulative_dflist[[i]], projected_SOCdflist[[i]], by = c("x" = "x", "y" = "y"))
  projected_cumulativeSOCdflist[[i]][,3]<- projected_cumulativeSOCdflist[[i]][,3]*8
  projected_cumulativeSOCdflist[[i]]$mean_b <- rowMeans(projected_cumulativeSOCdflist[[i]][, c("b0", "b10", "b30", "b60", "b100", "b200")])
  projected_cumulativeSOCdflist[[i]]$mean_b30 <- rowMeans(projected_cumulativeSOCdflist[[i]][, c("b0", "b10", "b30")])
  projected_cumulativeSOCdflist[[i]]$mean_b100 <- rowMeans(projected_cumulativeSOCdflist[[i]][, c("b30", "b60", "b100")])
}


# Create an empty dataframe to store regression metrics
regression_metrics_df <- data.frame()
projected_cumulativedflist_filtered<-vector("list", 13)

for (i in 1:13) { ###
  for (j in 4:11) {  ### the column index of the bands b0-b200, means are in 4 to 11
    # Select the desired columns directly
    gpp_col <- colnames(projected_cumulativedflist[[i]][3])
    colnamebq <- colnames(projected_cumulativedflist[[i]][j])
    projected_cumulativedflist_filtered[[i]] <- projected_cumulativedflist[[i]] %>%
      dplyr::select(gpp_col, colnamebq)
    # Calculate mean and standard deviation
    mean_value <- mean(as.numeric(projected_cumulativedflist_filtered[[i]][[2]]))
    sd_value <- sd(as.numeric(projected_cumulativedflist_filtered[[i]][[2]]))
    mean_value3sdp <- mean_value + (3 * sd_value)
    mean_value3sdn <- mean_value - (3 * sd_value)
    
    # Filter the DataFrame directly in a single step
    projected_cumulativedflist_filtered[[i]] <-projected_cumulativedflist_filtered[[i]][projected_cumulativedflist_filtered[[i]][, 2] <mean_value3sdp & projected_cumulativedflist_filtered[[i]][, 2] > mean_value3sdn, ]
    
    
    # Extract the predictive column name explicitly
    x_col <- names(projected_cumulativedflist_filtered[[i]][2])
    # Fit the linear model
    model <- lm(projected_cumulativedflist_filtered[[i]][,1] ~ projected_cumulativedflist_filtered[[i]][, 2]) ### gpp is the first column and clay content is the second column
    
    # Extract only the p-value for the slope coefficient
    p_value <- coef(summary(model))[2, 4]
    
    # Store metrics in the dataframe, excluding the intercept row
    regression_metrics_df <- rbind(
      regression_metrics_df,
      data.frame(
        i = i,
        j = j,
        rsquared = summary(model)$r.squared,
        p_value = p_value,
        rmse = sqrt(mean(model$residuals^2)),
        slope = model$coefficients[2],
        intercept = model$coefficients[1],
        predictor = x_col  # Add column for predictor name
      )
    )
    colnamey<-colnames((projected_cumulativedflist_filtered[[i]])[1])
    # Create the ggscatter plot
    plot <- ggscatter(projected_cumulativedflist_filtered[[i]], x = x_col, y = colnamey, add = "reg.line", size = 3) +
      stat_cor(
        aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
        label.x = 0, label.y = 3000, size = 8
      ) +
      stat_regline_equation(label.x = 0, label.y = 2800, size = 8) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
      xlab(paste0("Clay content % (kg / kg) at ", x_col)) +
      font("xy.text", size = 24) +
      font("xlab", size = 24) +
      font("ylab", size = 24) +
      ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
    
    # Save the plot with a specific filename
    filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/SoilClay/GPPMeanClay/SoilClay_", i, "_", x_col, ".png")
    ggsave(filename, plot, width = 14, height = 8)
  }
}

# Assuming 'i' is an integer column
regression_metrics_df <- mutate(regression_metrics_df,
                                i = ifelse(i == 1, 2008,
                                           ifelse(i == 13, 2020,
                                                  ifelse(i >= 2 & i <= 12, i + 2007, i))))

regression_metrics_df <- rename(regression_metrics_df,
                                year = i,
                                clay_content_band_values = predictor)
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_df, "C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Regression_metrics.csv", row.names = FALSE)

###SOC

# Create an empty dataframe to store regression metrics
regression_metrics_SOCdf <- data.frame()
projected_cumulativeSOCdflist_filtered<-vector("list", 13)

for (i in 1:13) {
  for (j in 4:11) {
    # Select the desired columns directly
    gpp_col <- colnames(projected_cumulativeSOCdflist[[i]][3])
    colnamebq <- colnames(projected_cumulativeSOCdflist[[i]][j])
    projected_cumulativeSOCdflist_filtered[[i]] <- projected_cumulativeSOCdflist[[i]] %>%
      dplyr::select(gpp_col, colnamebq)
    
    # Calculate mean and standard deviation
    mean_value <- mean(as.numeric(projected_cumulativeSOCdflist_filtered[[i]][[2]]))
    sd_value <- sd(as.numeric(projected_cumulativeSOCdflist_filtered[[i]][[2]]))
    mean_value3sdp <- mean_value + (3 * sd_value)
    mean_value3sdn <- mean_value - (3 * sd_value)
    
    # Filter the DataFrame directly in a single step
    projected_cumulativeSOCdflist_filtered[[i]] <-projected_cumulativeSOCdflist_filtered[[i]][projected_cumulativeSOCdflist_filtered[[i]][, 2] <mean_value3sdp & projected_cumulativeSOCdflist_filtered[[i]][, 2] > mean_value3sdn, ]
    # Extract the predictive column name explicitly
    x_col <- names(projected_cumulativeSOCdflist_filtered[[i]])[2]
    
    # Fit the linear model
    model <- lm(projected_cumulativeSOCdflist_filtered[[i]][,1] ~ projected_cumulativeSOCdflist_filtered[[i]][, 2])
    
    # Extract only the p-value for the slope coefficient
    p_value <- coef(summary(model))[2, 4]
    
    # Store metrics in the dataframe, excluding the intercept row
    regression_metrics_SOCdf <- rbind(
      regression_metrics_SOCdf,
      data.frame(
        i = i,
        j = j,
        rsquared = summary(model)$r.squared,
        p_value = p_value,
        rmse = sqrt(mean(model$residuals^2)),
        slope = model$coefficients[2],
        intercept = model$coefficients[1],
        predictor = x_col  # Add column for predictor name
      )
    )
    colnamey<-colnames((projected_cumulativeSOCdflist_filtered[[i]])[1])
    # Create the ggscatter plot
    plot <- ggscatter(projected_cumulativeSOCdflist_filtered[[i]], x = x_col, y = colnamey, add = "reg.line", size = 3) +
      stat_cor(
        aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
        label.x = 0, label.y = 3000, size = 8
      ) +
      stat_regline_equation(label.x = 0, label.y = 2800, size = 8) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
      xlab(paste0("Soil Organic Carbon % (g / kg) at ", x_col)) +
      font("xy.text", size = 24) +
      font("xlab", size = 24) +
      font("ylab", size = 24) +
      ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
    
    # Save the plot with a specific filename
    filename <- paste0("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Figures/SOC/SOC_", i, "_", x_col, ".png")
    ggsave(filename, plot, width = 14, height = 8)
  }
}

# Assuming 'i' is an integer column
regression_metrics_SOCdf <- mutate(regression_metrics_SOCdf,
                                i = ifelse(i == 1, 2008,
                                           ifelse(i == 13, 2020,
                                                  ifelse(i >= 2 & i <= 12, i + 2007, i))))

regression_metrics_SOCdf <- rename(regression_metrics_SOCdf,
                                year = i,
                                clay_content_band_values = predictor)
# Export the regression metrics as a table (you can choose the desired format, e.g., CSV)
write.csv(regression_metrics_SOCdf, "C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/SOC/Regression_metrics.csv", row.names = FALSE)


mean(projected_cumulativeSOCdflist[[1]]$b0)+ 3*sd(projected_cumulativeSOCdflist[[1]]$b0)
hist(projected_cumulativeSOCdflist[[1]]$b200)











ggscatterlist<-vector("list", 13)
for (i in 1: 13){
  ggscatter(projected_cumulativedflist[[i]], x = "mean_b", y = "layer_8", add = "reg.line", size = 3) +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 40, label.y = 2800, size = 8
    ) +
    stat_regline_equation(label.x = 40, label.y = 2600, size = 8) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
    xlab("Clay content % (kg / kg) ") +
    font("xy.text", size = 24) +
    font("xlab", size = 24) +
    font("ylab", size = 24) +
    ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1, ")")))
  ggsave(paste0("C:/Users/rbmahbub/DocumentsData/GeospatialData/CumulativeVPM/SoilOrganicCarbon/SoC_", i, ".png"), width = 14, height = 8)
  
}
  


# Create a scatter plot
ggscatter(results_df, x = "mean_b", y = "layer_8", add = "reg.line", size = 3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 40, label.y = 2800, size = 8
  ) +
  stat_regline_equation(label.x = 40, label.y = 2600, size = 8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3200)) +
  xlab("Clay content % (kg / kg) ") +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1, ")")))

# Save the plot
ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/SoilClay.png", width = 14, height = 8)

# Print the correlation analysis for each column
column_names <- c("b0", "b10", "b30", "b60", "b100", "b200")

for (col in column_names) {
  correlation <- cor(results_df$layer, results_df[[col]])
  model <- lm(layer ~ ., data = results_df[c("layer", col)])
  tidied_model <- tidy(model)
  r_squared <- tidied_model$r.squared
  p_value <- tidied_model$p.value
  rmse <- sqrt(mean(residuals(model)^2))
  
  cat("Correlation between layer and", col, ":", correlation, "\n")
  cat("R-squared:", r_squared, "\n")
  cat("P-value:", p_value, "\n")
  cat("RMSE:", rmse, "\n")
  cat("Equation:", capture.output(print(model$call)), "\n\n")
}


###High and Low GPP regions
#### high and low region analysis
# Initialize an empty DataFrame to store the merged results
#### compress the band information for the 
#### Merge the 13 dataframes to one dataframe 

###namingfor (i in 1:length(projected_EVIdflist)) {
for (i in 1:13) {
  year <- 2008 + i - 1  # Calculate the corresponding year for each DataFrame
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "b0"] <- paste0("b0", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "b10"] <- paste0("b10", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "b30"] <- paste0("b30", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "b60"] <- paste0("b60", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "b100"] <- paste0("b100", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "b200"] <- paste0("b200", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "mean_b"] <- paste0("mean_b", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "mean_b30"] <- paste0("mean_b30", year)
  names(projected_cumulativedflist[[i]])[names(projected_cumulativedflist[[i]]) == "mean_b100"] <- paste0("mean_b100", year)
  
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "b0"] <- paste0("b0", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "b10"] <- paste0("b10", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "b30"] <- paste0("b30", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "b60"] <- paste0("b60", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "b100"] <- paste0("b100", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "b200"] <- paste0("b200", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "mean_b"] <- paste0("mean_b", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "mean_b30"] <- paste0("mean_b30", year)
  names(projected_cumulativeSOCdflist[[i]])[names(projected_cumulativeSOCdflist[[i]]) == "mean_b100"] <- paste0("mean_b100", year)
  
}

mergedclay <- projected_cumulativedflist[[1]]
mergedSOC <- projected_cumulativeSOCdflist[[1]]
# Iterate through the remaining DataFrames and merge them
for (i in 2:13) {
  mergedclay <- merge(mergedclay, projected_cumulativedflist[[i]], by = c("x", "y"), all = TRUE)
  mergedSOC <- merge(mergedSOC, projected_cumulativeSOCdflist[[i]], by = c("x", "y"), all = TRUE)
}

##slopemerging and overall merging to check soil and EVI, Temp, LSWI relationship
##merged_dfEVI, merged_dfLSWI, merged_PDEVILSWITEMPPAR
merged_clay_slope_EVI <- inner_join(merged_df_df_slope_pvalue_EVI, mergedclay, by = c("x", "y")) ### high low regions
merged_SOC_slope_EVI <- inner_join(merged_df_df_slope_pvalue_EVI, mergedSOC, by = c("x", "y")) ### high low regions
merged_clay_slope_LSWI <- inner_join(merged_df_df_slope_pvalue_LSWI, mergedclay, by = c("x", "y")) ### high low regions
merged_SOC_slope_LSWI <- inner_join(merged_df_df_slope_pvalue_LSWI, mergedSOC, by = c("x", "y")) ### high low regions
merged_clay_overall_EVI <- inner_join(merged_dfEVI, mergedclay, by = c("x", "y")) ### Overall 
merged_SOC_overall_EVI <- inner_join(merged_dfEVI, mergedSOC, by = c("x", "y")) ### Overall 
merged_clay_overall_LSWI <- inner_join(merged_dfLSWI, mergedclay, by = c("x", "y")) ### 
merged_SOC_overall_LSWI <- inner_join(merged_dfLSWI, mergedSOC, by = c("x", "y"))

### mean of b0
merged_clay_slope_EVI <- merged_clay_slope_EVI %>%
  mutate(
    b0 = rowMeans(dplyr::select(., starts_with("b0")), na.rm = TRUE),  # Calculate mean ignoring NAs
    b10 = rowMeans(dplyr::select(., starts_with("b10")), na.rm = TRUE),
    b30 = rowMeans(dplyr::select(., starts_with("b30")), na.rm = TRUE),
    EVImean = rowMeans(dplyr::select(., starts_with("EVI_SG_mean_")), na.rm = TRUE)
  )

merged_SOC_slope_EVI <- merged_SOC_slope_EVI %>%
  mutate(
    b0 = rowMeans(dplyr::select(., starts_with("b0")), na.rm = TRUE),  # Calculate mean ignoring NAs
    b10 = rowMeans(dplyr::select(., starts_with("b10")), na.rm = TRUE),
    b30 = rowMeans(dplyr::select(., starts_with("b30")), na.rm = TRUE),
    EVImean = rowMeans(dplyr::select(., starts_with("EVI_SG_mean_")), na.rm = TRUE)
  )

merged_clay_overall_EVI <- merged_clay_overall_EVI %>%
  mutate(
    b0 = rowMeans(dplyr::select(., starts_with("b0")), na.rm = TRUE),  # Calculate mean ignoring NAs
    b10 = rowMeans(dplyr::select(., starts_with("b10")), na.rm = TRUE),
    b30 = rowMeans(dplyr::select(., starts_with("b30")), na.rm = TRUE),
    EVImean = rowMeans(dplyr::select(., starts_with("EVI_SG_mean_")), na.rm = TRUE)
  )

merged_SOC_overall_EVI <- merged_SOC_overall_EVI %>%
  mutate(
    b0 = rowMeans(dplyr::select(., starts_with("b0")), na.rm = TRUE),  # Calculate mean ignoring NAs
    b10 = rowMeans(dplyr::select(., starts_with("b10")), na.rm = TRUE),
    b30 = rowMeans(dplyr::select(., starts_with("b30")), na.rm = TRUE),
    EVImean = rowMeans(dplyr::select(., starts_with("EVI_SG_mean_")), na.rm = TRUE)
  )

merged_clay_slope_LSWI <- merged_clay_slope_LSWI %>%
  mutate(
    b0 = rowMeans(dplyr::select(., starts_with("b0")), na.rm = TRUE),  # Calculate mean ignoring NAs
    b10 = rowMeans(dplyr::select(., starts_with("b10")), na.rm = TRUE),
    b30 = rowMeans(dplyr::select(., starts_with("b30")), na.rm = TRUE),
    LSWImean = rowMeans(dplyr::select(., starts_with("LSWI_SG_mean")), na.rm = TRUE)
  )


### Remove the rows merged_clay_slope_EVI
merged_clay_slope_EVINA <- merged_clay_slope_EVI[ complete.cases(merged_clay_slope_EVI),]
merged_SOC_slope_EVINA <- merged_SOC_slope_EVI[!is.na(merged_SOC_slope_EVI$mean_b302020)]


ggplot(merged_clay_slope_EVI, aes(x = slopeEVI, y = slope, color = b0)) +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Slope of EVI", y = "Slope of GPP", color = "b0"
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +
  scale_color_viridis(direction = -1)+
  theme_bw()+
  theme(legend.position = c(0.2, 0.85), legend.direction = "horizontal")



ggplot(merged_clay_slope_EVI, aes(x = b0, y = slopeEVI, color = slope)) +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Clay Conent", y = "Slope of EVI", color = "slope"
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +
  theme_bw()+
  theme(legend.position = c(0.8, 0.85), legend.direction = "horizontal")


ggplot(merged_clay_overall_EVI, aes(x = b0, y = EVImean)) +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Mean EVI (2008-2020)", y = "Clay content % (kg / kg) at 0 cm"
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +

  theme_bw()+
  theme(legend.position = c(0.3, 0.85), legend.direction = "horizontal")

ggplot(merged_clay_overall_EVI, aes(x = slope_sign, y = mean_b302020, fill = slope_sign)) +
  geom_boxplot(notch = TRUE, showmeans = TRUE) +
  labs(
    x = "Slope Sign", y = "EVI", fill = "Slope Sign") +
  scale_fill_manual(values = color_palette) +
  theme_bw()

ggplot(merged_clay_slope_EVI, aes(x = EVI_SG_mean_mean, y = slope, color = b0)) +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Mean EVI", y = "Slope", color = "b0"
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +
  theme_bw()+
  theme(legend.position = c(0.3, 0.85), legend.direction = "horizontal")


ggplot(merged_SOC_slope_EVI, aes(x = EVI_SG_mean_mean, y = slope, color = b0)) +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Mean EVI", y = "Slope", color = "b0"
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +
  theme_bw()+
  theme(legend.position = c(0.3, 0.85), legend.direction = "horizontal")



library(ggplot2)

### Slope Plotting
ggscatter(merged_clay_slope_EVI, x = "slopeEVI", y = "slope", color = "b0", add = "reg.line", size = 4) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = -0.015, label.y = 10, size = 10
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +
  scale_color_viridis(direction = -1) +
  stat_regline_equation(label.x = -0.015, label.y = 15, size = 10) +
  xlab(paste0("slope of EVI")) +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Slope of GPP"))) +
  theme(legend.position = c(0.9, 0.2), legend.direction = "horizontal",
        legend.text = element_text(size = 20),  # Adjust legend text size
        legend.title = element_text(size = 20)) +  # Adjust legend title size
  labs(color = "Clay content at 0 cm") +  # Legend title
  guides(color = guide_legend(title.position = "top"))  # Legend title position
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Figures/GPPEVIslopeSoilClay.png", width = 14, height = 8)

### EVI vs Clay content for high and low regions
ggscatter(merged_clay_slope_EVI, x = "b0", y = "EVImean", add = "reg.line", size = 4) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 40, label.y = 0.37, size = 10
  ) +
  stat_regline_equation(label.x = 40, label.y = 0.4, size = 10) +
  xlab(paste0("Clay content % (kg / kg) at 0 cm")) +
  font("xy.text", size = 24) +
  font("xlab", size = 24) +
  font("ylab", size = 24) +
  ylab(expression(paste("Mean EVI")))
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPMmodel/VPMmodel/Figures/Figures/EVIhighlowSoilClay.png", width = 14, height = 8)

ggscatter(merged_clay_slope_EVI, x = "b0", y = "slopeEVI") +
  geom_point() +  # Change to geom_jitter() for less overlap if needed
  labs(
    x = "Clay Conent", y = "Slope of EVI", color = "slope"
  ) +
  scale_color_viridis(limits = c(min(merged_SOC_slope_EVI$mean_b302020), max(merged_SOC_slope_EVI$mean_b302020))) +
  theme_bw()+
  theme(legend.position = c(0.8, 0.85), legend.direction = "horizontal")



ggplot(merged_SOC_slope_EVI, aes(x = b102020, y = slope)) +
  geom_point() 


ggplot(merged_SOC_slope_EVI, aes(x = b02020, y = EVI_SG_mean_mean)) +
  geom_point()

### Calculate the means of EVI, LSWI, PAR, Temp across differennt years
mergedclaymean <- mergedclay %>%
  dplyr::select(starts_with("b0")) %>%
  rowMeans(na.rm = TRUE)

merged_df <- cumulative_dflist[[1]]

# Iterate through the remaining DataFrames and merge them
for (i in 2:13) {
  merged_df <- merge(merged_df, cumulative_dflist[[i]], by = c("x", "y"), all = TRUE)
}
merged_df
# Print the merged DataFrame,
columns_to_check <- c("2008AnnualGPP", "2009AnnualGPP" ,"2010AnnualGPP", "2011AnnualGPP",
                      "2012AnnualGPP", "2013AnnualGPP", "2014AnnualGPP" ,"2015AnnualGPP", "2016AnnualGPP" ,"2017AnnualGPP",
                      "2018AnnualGPP", "2019AnnualGPP", "2020AnnualGPP")

# Remove rows where at least 11 columns have NA values
merged_df_df <- merged_df[rowSums(is.na(merged_df[, columns_to_check])) <= 5, , drop = TRUE]
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
  mutate(slope = calculate_slope_pvalue(c_across(all_of(columns_to_check)))[1],
         p_value = calculate_slope_pvalue(c_across(all_of(columns_to_check)))[2])
nrow(merged_df_df_slope)
merged_df_df_slope_pvalue <- merged_df_df_slope[merged_df_df_slope$p_value < 0.05, ]
nrow(merged_df_df_slope_pvalue)



colnameb

colnameb<-colnames(projected_cumulativeSOCdflist[[1]][4])
projected_cumulativeSOCdflist_filtered[[1]] <- projected_cumulativeSOCdflist[[1]] %>% dplyr::select(layer_8, colnameb)
# Calculate mean and standard deviation
mean_value <- mean(as.numeric(projected_cumulativeSOCdflist_filtered[[1]][[2]]))
sd_value <- sd(as.numeric(projected_cumulativeSOCdflist_filtered[[1]][[2]]))
mean_value3sdp <- mean_value+(3*sd_value)
mean_value3sdn<- mean_value-(3*sd_value)

# Filter columns above and below 3 standard deviations from the mean
# Filter columns above and below 3 standard deviations from the mean
projected_cumulativeSOCdflist_filtered[[1]] <- projected_cumulativeSOCdflist_filtered[[1]] %>%
  filter(colnameb<mean_value3sdp | colnameb>mean_value3sdn)

projected_cumulativeSOCdflist_filtered[[1]] <- projected_cumulativeSOCdflist_filtered[[1]] %>%
  filter(colnameb < mean_value3sdp | colnameb > mean_value3sdn)





colnamebq <- colnames(projected_cumulativeSOCdflist[[1]][4])

colnamebnq <-noquote(colnamebq)
colnamebq
colnamebnq

# Select the desired columns directly
projected_cumulativeSOCdflist_filtered[[1]] <- projected_cumulativeSOCdflist[[1]] %>%
  dplyr::select(layer_8, colnamebq)

# Calculate mean and standard deviation
mean_value <- mean(as.numeric(projected_cumulativeSOCdflist_filtered[[1]][[2]]))
sd_value <- sd(as.numeric(projected_cumulativeSOCdflist_filtered[[1]][[2]]))==
mean_value3sdp <- mean_value + (3 * sd_value)
mean_value3sdn <- mean_value - (3 * sd_value)

# Filter the DataFrame directly in a single step
projected_cumulativeSOCdflist_filtered[[1]] <-projected_cumulativeSOCdflist_filtered[[1]][projected_cumulativeSOCdflist_filtered[[1]][, 2] 
                                                                                          <mean_value3sdp & 
                                                                                            projected_cumulativeSOCdflist_filtered[[1]][, 2] > mean_value3sdn, ]
nrow(projected_cumulativeSOCdflist_filtered[[1]])


projected_cumulativeSOCdflist_filtered[[1]] <- projected_cumulativeSOCdflist_filtered[[1]] %>%
  filter(sym(colnamebq) < (mean_value3sdp))
nrow(projected_cumulativeSOCdflist_filtered[[1]])

projected_cumulativeSOCdflist_filtered[[1]] <- projected_cumulativeSOCdflist_filtered[[1]]%>%dplyr::filter(colnamebnq< mean_value3sdp)
nrow(projected_cumulativeSOCdflist_filtered[[1]])
nrow(projected_cumulativeSOCdflist[[1]])

projected_cumulativeSOCdflist_filtered[[1]]%>%projected_cumulativeSOCdflist_filtered[df[, 2] < mean_value3sdp & df[, 2] > mean_value3sdn, ]

filter(projected_cumulativeSOCdflist_filtered[[1]], projected_cumulativeSOCdflist_filtered[[1]][,2]>80&projected_cumulativeSOCdflist_filtered[[1]]$age<10) 

gpp_col <- colnames(projected_cumulativeSOCdflist[[1]][3])
colnamebq <- colnames(projected_cumulativeSOCdflist[[1]][4])
projected_cumulativeSOCdflist[[1]] %>%
  dplyr::select(gpp_col, colnamebq)
