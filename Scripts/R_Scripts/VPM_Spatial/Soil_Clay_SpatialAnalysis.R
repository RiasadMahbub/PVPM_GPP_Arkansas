
### Export each counties
### Necessary libraries

library(sf)
library(dplyr)
library(sp) ## prerequisite for the raster package
library(raster) #raster(read the rasterfile)
library(terra) # The terra::project function is used in the terra package to project (resample/aggregate) one raster to the grid of another.
library(ggplot2)
library(ggpubr)
library(ggtext)



### upscale the 30m data to 500m
### plot gpp vs clay content
## directory of the clay content file
# Specify the directory and file name
#dir <- "/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Data/VPM/SoilClay" ## mac directory
dir <- "C://Users//rbmahbub//Box//Research//Data//VPM//SoilClay" ## windows directory
dircumulative <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM-alltogether"


# Specify the file names
file_name_clay <- "ClayContentArkansas.tif"
file_name_cumulative <- "arkansasRice20082020VPMcumulative.tif"

# Combine the directory and file names
raster_path_clay <- file.path(dir, file_name_clay)
raster_path_cumulative <- file.path(dircumulative, file_name_cumulative)

# Read the raster images
clay_raster <- rast(raster_path_clay)
cumulative_raster <- rast(raster_path_cumulative)

# Convert 'clay_raster' to a terra object
terra_clay <- rast(clay_raster)
terra_clay <-clay_raster

# Project 'clay_raster' to the grid of 'cumulative_raster'
projected_clay <- terra::project(terra_clay, cumulative_raster, method = "average")


# Convert projected raster to dataframe
#projected_df <- terra::as.data.frame(projected_clay)
projected_df <- terra::as.data.frame(projected_clay, xy=TRUE)

# Convert cumulative raster to dataframe
#cumulative_df <- terra::as.data.frame(cumulative_raster)
cumulative_df <- terra::as.data.frame(cumulative_raster, xy=TRUE)

projected_cumulativedf<- left_join(cumulative_df, projected_df, by = c("x" = "x", "y" = "y"))

#3 Multiply by 8
projected_cumulativedf$layer_8<- projected_cumulativedf$layer*8
projected_cumulativedf$mean_b <- rowMeans(projected_cumulativedf[, c("b0", "b10", "b30", "b60", "b100", "b200")])


# Create a scatter plot
ggscatter(projected_cumulativedf, x = "mean_b", y = "layer_8", add = "reg.line", size =3) +
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 40, label.y =2800, size =8
  )+
  stat_regline_equation(label.x = 40, label.y = 2600, size =8)+
  #scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3200))+
  xlab("Clay content % (kg / kg) ")+
  font("xy.text", size = 24)+
  font("xlab", size = 24)+
  font("ylab", size = 24)+
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1,")")))

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/SoilClay.png",width = 14, height = 8 )

colnames(projected_cumulativedf)


library(broom)

dataframe <- projected_cumulativedf
column_names <- c("b0", "b10", "b30", "b60", "b100", "b200")

# Iterate over each column and calculate correlation, R2, p value, rmse, and equation
for (col in column_names) {
  # Calculate correlation
  correlation <- cor(dataframe$layer, dataframe[[col]])
  
  # Fit linear model
  model <- lm(layer ~ ., data = dataframe[c("layer", col)])
  
  # Tidy up the results using broom
  tidied_model <- tidy(model)
  
  # Extract R2, p value, and rmse from the tidied results
  r_squared <- tidied_model$r.squared
  p_value <- tidied_model$p.value
  rmse <- sqrt(mean(residuals(model)^2))
  
  # Print the results
  cat("Correlation between layer and", col, ":", correlation, "\n")
  cat("R-squared:", r_squared, "\n")
  cat("P-value:", p_value, "\n")
  cat("RMSE:", rmse, "\n")
  cat("Equation:", capture.output(print(model$call)), "\n\n")
}



# Convert rasters to matrices
matrix1 <- raster::extract(projected_clay, projected_clay)
matrix2 <- raster::extract(cumulative_raster, cumulative_raster)

# Flatten the matrices
values1 <- matrix1[!is.na(matrix1)]
values2 <- matrix2[!is.na(matrix2)]

# Create a spatial weights matrix
coords <- raster::xyFromCell(raster1, 1:ncell(raster1))
nb <- dnearneigh(coords, d1 = 0, d2 = 1, longlat = FALSE)

# Calculate Moran's I for the two variables
moran1 <- moran.test(values1, listw = nb)
moran2 <- moran.test(values2, listw = nb)

# Print the results
print(moran1)
print(moran2)



