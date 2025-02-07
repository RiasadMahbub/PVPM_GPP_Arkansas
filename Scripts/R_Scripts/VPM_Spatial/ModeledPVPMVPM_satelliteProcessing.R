##Processing the Modeled data
library(readr)
library(lubridate)
library(cowplot)
library(patchwork)
library(tidyverse)
library(viridis)
library(ggpubr)
##Create RMSE and MAE metrics
library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(cowplot)
library(tidyverse)
library(terra)

GPPsiteVPMrmse<-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0_06512_31_79)
#GPPsitePVPMrmse <-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPpvpmmodel)
GPPsatelliteVPMrmse<-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpp)
#PPsatellitePVPMrmse<-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpppvpm)
GPPsiteVPMmae<-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0_06512_31_79)
#GPPsitePVPMmae<-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPpvpmmodel)
GPPsatelliteVPMmae <-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpp)
#GPPsatellitePVPMmae<-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpppvpm)
GPPsiteVPMbias<-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0_06512_31_79)
#GPPsitePVPMbias<-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPpvpmmodel)
GPPsatelliteVPMbias <-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpp)
#GPPsatellitePVPMbias<-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpppvpm)
GPPsiteVPM<-ggplot(data=sitesatellitemergedDATA, aes(x =GPP_site , y =GPP_0_06512_31_79, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(expression(paste(GPP~VPM[site],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  stat_regline_equation(label.x = -5, label.y = 22, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 20, size = 18)+
  ggplot2::annotate("text", x = -0.95, y = 28, label = sprintf("RMSE: %0.2f", GPPsiteVPMrmse), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 24, label = sprintf("MAE:  %0.2f",GPPsiteVPMmae), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 26, label = sprintf("Bias: %0.2f", GPPsiteVPMbias), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = 8.5, y = 28, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 24, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 26, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

GPPsatelliteVPM<-ggplot(data=sitesatellitemergedDATA, aes(x =GPP_site , y =gpp, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(expression(paste(GPP~VPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  stat_regline_equation(label.x = -5, label.y = 23, size = 16)+
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 21, size = 16)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  ggplot2::annotate("text", x = -0.95, y = 29, label = sprintf("RMSE: %0.2f", GPPsatelliteVPMrmse), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 25, label = sprintf("MAE:  %0.2f",GPPsatelliteVPMmae), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 27, label = sprintf("Bias: %0.2f", GPPsatelliteVPMbias), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = 8.5, y = 29, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 25, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 27, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
##VPM
combined <-  GPPsiteVPM  + GPPsatelliteVPM  & theme(legend.position = "right")
combinedvpm<-combined + plot_layout(guides = "collect")+plot_annotation(tag_levels = 'A')
combinedvpm
ggsave(combinedvpm, filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMModelsscatterplot.png", width = 40, height = 20)


# Assuming you have calculated the metrics

cat("GPPsiteVPMrmse:", GPPsiteVPMrmse, "\n")
cat("GPPsatelliteVPMrmse:", GPPsatelliteVPMrmse, "\n")
cat("GPPsiteVPMmae:", GPPsiteVPMmae, "\n")
cat("GPPsatelliteVPMmae:", GPPsatelliteVPMmae, "\n")
cat("GPPsiteVPMbias:", GPPsiteVPMbias, "\n")
cat("GPPsatelliteVPMbias:", GPPsatelliteVPMbias, "\n")


###Max minimum values of the result sections:
max(sitesatellitemergedDATA$GPPvpmsite)
max(sitesatellitemergedDATA$gpp)


###VPM thesis
separate_DF<-sitesatellitemergedDATA%>% select(GPP_site, GPP_0_06512_31_79, gpp, DAP)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP"),
                variable.name = "Model",
                value.name = "GPP")

# Use spread to convert back to the original wide format
# Filter rows where Model is "GPP_site"
filtered_df1 <- df_long %>%
  filter(Model == "GPP_site")
filtered_df2 <- df_long %>%
  filter(Model == "GPPvpmsite")
filtered_df3 <- df_long %>%
  filter(Model == "gpp")

colnames(filtered_df1)
colnames(filtered_df2)

### Change the labels name
plt<-ggplot(df_long,aes(x=DAP,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =3)+geom_smooth(alpha=0.5, size = 3)+
  scale_shape_manual(name = "Model",labels = c("EC", bquote(VPM[site]),  bquote(VPM[spatial])), values=c(16, 17, 18, 19, 20))+
  scale_color_manual(name = "Model",labels = c("EC", bquote(VPM[site]),  bquote(VPM[spatial])), values=c("#E7B800", "#CC79A7", "#1b97de","#f581ad","#b94e76"))+
  scale_x_continuous(breaks = seq(0, 150, by = 20))+
  scale_y_continuous(breaks = seq(0, 30, by = 5))+
  xlab(bquote('Days after Planting (DAP)'))+
  ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  labs(color=NULL)+
  theme_classic()+
  
  theme(legend.position = c(0.1, 0.8),
        legend.title=element_blank(),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"),
        legend.title.align = 0.5)+
  theme(text = element_text(size = 25))
plt
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/vpmtemporalgpp2.png", plot = plt, width = 16, height = 8, dpi = 300)


####RESIDUAL PLOTS
sitesatellitemergedDATA$residualsitevpm<-sitesatellitemergedDATA$GPP_site- sitesatellitemergedDATA$gpp
sitesatellitemergedDATA$residualsatellitevpm<-sitesatellitemergedDATA$GPP_site- sitesatellitemergedDATA$GPP_0_06512_31_79

sitesatellitemergedDATA$residualsitevpm
sitesatellitemergedDATA$residualsatellitevpm

# Create ggplot objects for each set of data
plot1 <- ggplot(sitesatellitemergedDATA, aes(x = DAP)) +
  geom_point(aes(y = residualsitevpm),  size = 2) +
  labs(x = NULL, y = expression("Residuals from VPM"[site]*" (g C m"^{-2}*" day"^{-1}*")")) +
  theme_bw(base_size = 19)+
  scale_x_continuous(breaks = seq(0, 150, by = 25))+
  annotate("text", x = 150, y = 14, label = "A", vjust = 1, hjust = 1, size = 17, fontface = "bold")

plot2 <- ggplot(sitesatellitemergedDATA, aes(x = DAP)) +
  geom_point(aes(y = residualsatellitevpm), size = 2) +
  labs(x = "Days after planting (day)", y = expression("Residuals from VPM"[spatial]*" (g C m"^{-2}*" day"^{-1}*")")) +
  theme_bw(base_size = 19)+
  scale_x_continuous(breaks = seq(0, 150, by = 25))+
  annotate("text", x = 150, y = 14, label = "B", vjust = 1, hjust = 1, size = 17, fontface = "bold")

# Combine the plots
combined_plot <- plot1 + plot2 + plot_layout(nrow = 2) 
# Save the combined plot as an image
ggsave(file = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/residuals_graph.png",
       plot = combined_plot,
       width = 16, height = 12, units = "in", dpi = 300)

#################################################
######YEARLY performance#########################
#################################################

# Summarize metrics by year
yearly_metrics <- sitesatellitemergedDATA %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    RMSE_VPMsite = Metrics::rmse(GPP_site, GPP_0_06512_31_79),
    RMSE_VPMsatellite = Metrics::rmse(GPP_site, gpp),
    MAE_VPMsite = Metrics::mae(GPP_site, GPP_0_06512_31_79),
    MAE_VPMsatellite = Metrics::mae(GPP_site, gpp),
    Bias_VPMsite = Metrics::bias(GPP_site, GPP_0_06512_31_79),
    Bias_VPMsatellite = Metrics::bias(GPP_site, gpp),
    R2_VPMsite = round(cor(GPP_site, GPP_0_06512_31_79, use = "complete.obs")^2, 2),  # R² rounded to 2 decimals
    R2_VPMsatellite = round(cor(GPP_site, gpp, use = "complete.obs")^2, 2)           # R² rounded to 2 decimals
  ) %>%
  dplyr::mutate(year = as.character(year))  # Ensure 'year' column is treated as a character for formatting

# Restructure the table to your desired format
formatted_table <- yearly_metrics %>%
  dplyr::select(
    year,
    RMSE_VPMsite, RMSE_VPMsatellite,
    MAE_VPMsite, MAE_VPMsatellite,
    Bias_VPMsite, Bias_VPMsatellite,
    R2_VPMsite, R2_VPMsatellite
  ) %>%
  dplyr::rename(
    "year" = year,
    "RMSE VPMsite" = RMSE_VPMsite,
    "RMSE VPMsatellite" = RMSE_VPMsatellite,
    "MAE VPMsite" = MAE_VPMsite,
    "MAE VPMsatellite" = MAE_VPMsatellite,
    "Bias VPMsite" = Bias_VPMsite,
    "Bias VPMsatellite" = Bias_VPMsatellite,
    "R2 VPMsite" = R2_VPMsite,
    "R2 VPMsatellite" = R2_VPMsatellite
  )

# Save the table as a CSV file
output_file_path <- "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/yearly_metrics_formatted.csv"
write.csv(formatted_table, file = output_file_path, row.names = FALSE)

# Print a confirmation message
cat("Formatted table saved successfully at:", output_file_path, "\n")


# View the results
View(yearly_metrics)
#################################################
######Crosssite performance##########################################
#################################################
site_metrics <- sitesatellitemergedDATA %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(
    GPPsiteVPMrmse = Metrics::rmse(GPP_site, GPP_0_06512_31_79),
    GPPsatelliteVPMrmse = Metrics::rmse(GPP_site, gpp),
    GPPsiteVPMmae = Metrics::mae(GPP_site, GPP_0_06512_31_79),
    GPPsatelliteVPMmae = Metrics::mae(GPP_site, gpp),
    GPPsiteVPMbias = Metrics::bias(GPP_site, GPP_0_06512_31_79),
    GPPsatelliteVPMbias = Metrics::bias(GPP_site, gpp),
    GPPsiteVPMR2 = round(1 - sum((GPP_site - GPP_0_06512_31_79)^2) / sum((GPP_site - mean(GPP_site))^2), 2),
    GPPsatelliteVPMR2 = round(1 - sum((GPP_site - gpp)^2) / sum((GPP_site - mean(GPP_site))^2), 2)
  )

View(site_metrics)
# Save the table as a CSV file
output_file_path <- "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/Site_metrics_formatted.csv"
write.csv(site_metrics, file = output_file_path, row.names = FALSE)

#################################################
######Mixedpixel##########################################
#################################################
# Load the raster and shapefile
raster_data <- rast("C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM_0_05_30/arkansasRice2012VPMcumulative0_530.tif")
shapefile_data <- vect("C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/ShapefileData/ArkansasAmerifluxSiteData/SHP/ArkansasAmerifluxPolygon.shp")
# Calculate the area of each raster pixel
pixel_area <- cellSize(raster_data, unit = "m")
# Intersect raster and shapefile
overlap <- terra::extract(raster_data, shapefile_data, weights = TRUE, cell = TRUE)
# Add the polygon name (using 'Name' as the identifier)
overlap$polygon_name <- shapefile_data$Name[overlap$ID]
# Calculate the area of each polygon in square meters
polygon_areas <- expanse(shapefile_data, unit = "m")
# Assuming your data is in a data frame named 'overlap' 
max_weights <- overlap %>%
  group_by(polygon_name) %>%
  summarize(
    max_weight = max(weight),
    max_weight_cell = cell[which.max(weight)],
    gpp_sum   = gpp_sum[which.max(weight)],
    .groups = "drop"
  )
max_weights

# Perform the join and save to new dataframe
site_mixedpixel_metrics <- dplyr::left_join(site_metrics, max_weights, 
                                            by = c("site" = "polygon_name"))
site_mixedpixel_metrics$mixedpixelratio <-site_mixedpixel_metrics$max_weight*100
# View the new dataframe
# Drop the specified columns
site_mixedpixel_metrics <- site_mixedpixel_metrics %>%
  dplyr::select(-max_weight, -max_weight_cell, -gpp_sum)
output_file_path <- "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/Site_metrics_formatted.csv"
write.csv(site_mixedpixel_metrics, file = output_file_path, row.names = FALSE)


#################################################
######LAI##########################################
#################################################
library(lubridate)
# Define the directory path
data_dir <- "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/MODISLAIdata"

# Get the list of CSV files
csv_files <- list.files(path = data_dir, pattern = "*.csv", full.names = TRUE)

# Read all CSV files and combine them into one dataframe
LAIcombined <- do.call(rbind, lapply(csv_files, function(file) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$site <- tools::file_path_sans_ext(basename(file))  # Add 'site' column with file name (without extension)
  return(df)
}))



# Ensure date column is in Date format
LAIcombined <- LAIcombined %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Function to calculate 8-day averages while respecting year boundaries
# Function to calculate 8-day averages while respecting year boundaries and keeping 'site'
average_8day <- function(df) {
  df %>%
    mutate(year = year(date),
           day_of_year = yday(date),
           interval = ((day_of_year - 1) %/% 8) + 1) %>%
    group_by(site, year, interval) %>%  # Group by site as well to keep the site column
    summarise(
      start_date = min(date),
      end_date = max(date),
      avg_Lai = mean(Lai, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(site, year, start_date)  # Ensure results are sorted by site, year, and start date
}

# Apply the function
LAIcombined <- average_8day(LAIcombined)
LAIcombined$sitedate <- paste(LAIcombined$site, LAIcombined$start_date, sep = "_")

sitesatellitemergedDATA$sitedate <- paste(sitesatellitemergedDATA$site, sitesatellitemergedDATA$date, sep = "_")
# Assuming both datasets have the 'sitedate' column
sitesatellitemergedDATALAI <- sitesatellitemergedDATA %>%
  left_join(LAIcombined, by = "sitedate")


# Modify the site names: replace the underscores with hyphens after the "US"
sitesatellitemergedDATALAI$site.x <- gsub("US([A-Za-z]+)(.*)", "US-\\1\\2", sitesatellitemergedDATALAI$site.x)

sitesatellitemergedDATALAI<-sitesatellitemergedDATALAI %>%
  filter(site.x != "US-HRA")

# Assuming sitesatellitemergedDATALAI contains avg_Lai, residualsitevpm, DOY, and site.x columns
ggplot(sitesatellitemergedDATALAI, aes(x = avg_Lai, y = residualsitevpm, color = DAP)) +
  geom_point() + # Scatter plot
  geom_point(size =3) +
  labs(x = expression("Average LAI (" * m^2/m^2 * ") 8-day mean"), 
       y = expression("Residual VPM (" * g~C~m^-2~day^-1 * ") 8-day mean"), 
       color = "DAP") + 
  theme_minimal() + # A minimal theme
  theme(text = element_text(size = 12))+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  facet_wrap(~ site.x) + # Facet by site.x column
  theme(
    axis.line = element_line(color = "black", size = 0.8),  # Axis lines for both x and y axes
    #panel.grid.major = element_blank(),  # Remove major grid lines
    #panel.grid.minor = element_blank()   # Remove minor grid lines
  )

# Assuming 'sitesatellitemergedDATALAI' contains avg_Lai, residualsitevpm, DAP, site.x, and group (ID or unique identifier)
sitesatellitemergedDATALAI <- sitesatellitemergedDATALAI %>%
  mutate(xend = lead(avg_Lai), yend = lead(residualsitevpm))  # Create 'xend' and 'yend' for arrows

# plot3 <- ggplot(sitesatellitemergedDATALAI, aes(x = avg_Lai, y = residualsitevpm, group = site.x)) +
#   geom_point(aes(color = DAP), size = 3) +  # Add points with DAP as color
#   geom_segment(aes(xend = xend, yend = yend, colour = (DAP)),
#                arrow = arrow(length = unit(0.08, "npc"), type = "closed")) +  # Add arrows
#   scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +  # Color scale for DAP
#   facet_wrap(~ site.x) +  # Facet by site.x column
#   labs(x = expression("Average LAI (" * m^2/m^2 * ") 8-day mean"), 
#        y = expression("Residual VPM (" * g~C~m^-2~day^-1 * ") 8-day mean"), 
#        color = "DAP") + 
#   theme_minimal() +  # A minimal theme
#   theme(
#     axis.line = element_line(color = "black", size = 0.8),  # Axis lines for both x and y axes
#     text = element_text(size = 12)  # Set the text size for the plot
#   )
# plot3

# Assuming you have your ggplot object stored in `p`
ggsave("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/residuallai.png", 
       width = 8, 
       height = 6, 
       dpi = 300)

#############################################
#############ECGPP-yield relationship########
#############################################
yielddata <- read.csv("C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/16siteyearYield.csv", header = TRUE)
sitesatellitemergedDATAfiltered<- sitesatellitemergedDATA[, c("GPP_site", "gpp", "GPP_0_06512_31_79", "siteyear.x")]
# Calculate the sum of each variable for each unique siteyear.x
sitesatellitemergedDATAfiltered <- sitesatellitemergedDATA %>%
  group_by(siteyear.x) %>%
  summarise(
    sum_GPP_site = sum(GPP_site, na.rm = TRUE),
    sum_gpp = sum(gpp, na.rm = TRUE),
    sum_GPP_0_06512_31_79 = sum(GPP_0_06512_31_79, na.rm = TRUE)
  )

# Perform the join (inner join by default)
sitesatellitemergedDATAfiltered <- sitesatellitemergedDATAfiltered %>%
  inner_join(yielddata, by = "siteyear.x")

# Multiply the specified columns by 8
sitesatellitemergedDATAfiltered <- sitesatellitemergedDATAfiltered %>%
  mutate(
    sum_GPP_site = sum_GPP_site * 8,
    sum_gpp = sum_gpp * 8,
    sum_GPP_0_06512_31_79 = sum_GPP_0_06512_31_79 * 8,
    Yieldtha_1_gm2 = Yieldtha_1 * 100
  )


library(ggplot2)
library(Metrics)
library(patchwork)

# Calculate RMSE, MAE, and Bias for each comparison
rmse_sum_GPP_site <- rmse(sitesatellitemergedDATAfiltered$sum_GPP_site, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
mae_sum_GPP_site <- mae(sitesatellitemergedDATAfiltered$sum_GPP_site, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
bias_sum_GPP_site <- bias(sitesatellitemergedDATAfiltered$sum_GPP_site, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)

rmse_sum_gpp <- rmse(sitesatellitemergedDATAfiltered$sum_gpp, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
mae_sum_gpp <- mae(sitesatellitemergedDATAfiltered$sum_gpp, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
bias_sum_gpp <- bias(sitesatellitemergedDATAfiltered$sum_gpp, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)

rmse_sum_GPP_0_06512_31_79 <- rmse(sitesatellitemergedDATAfiltered$sum_GPP_0_06512_31_79, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
mae_sum_GPP_0_06512_31_79 <- mae(sitesatellitemergedDATAfiltered$sum_GPP_0_06512_31_79, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
bias_sum_GPP_0_06512_31_79 <- bias(sitesatellitemergedDATAfiltered$sum_GPP_0_06512_31_79, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)
library(dplyr)

# Calculate R² for each model
r2_sum_GPP_site <- cor(sitesatellitemergedDATAfiltered$sum_GPP_site, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)^2
r2_sum_gpp <- cor(sitesatellitemergedDATAfiltered$sum_gpp, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)^2
r2_sum_GPP_0_06512_31_79 <- cor(sitesatellitemergedDATAfiltered$sum_GPP_0_06512_31_79, sitesatellitemergedDATAfiltered$Yieldtha_1_gm2)^2

# Create a data frame for the table
summary_table <- data.frame(
  Model = c("VPMsite", "GPPEC", "VPMspatial"),
  R2 = c(r2_sum_GPP_site, r2_sum_gpp, r2_sum_GPP_0_06512_31_79),
  RMSE = c(rmse_sum_GPP_site, rmse_sum_gpp, rmse_sum_GPP_0_06512_31_79),
  MAE = c(mae_sum_GPP_site, mae_sum_gpp, mae_sum_GPP_0_06512_31_79),
  Bias = c(bias_sum_GPP_site, bias_sum_gpp, bias_sum_GPP_0_06512_31_79)
)

# Round the values for better readability
summary_table <- summary_table %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

# Display the table
print(summary_table)

# Plot 1: sum_GPP_site vs Yieldtha_1_gm2
plot_sum_GPP_site <- ggplot(data = sitesatellitemergedDATAfiltered, aes(x = sum_GPP_site, y = Yieldtha_1_gm2, col = siteyear.x)) +
  geom_abline(intercept = 0, slope = 1, size = 1.5, col = "red", linetype = "dashed") +
  geom_point(size = 5) +
  xlim(0, max(sitesatellitemergedDATAfiltered$sum_GPP_site, na.rm = TRUE)) +
  ylim(0, max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE)) +
  xlab(bquote('sum_GPP_site ('*g~ 'C'~ m^-2~day^-1*')')) +
  ylab(bquote('Yieldtha_1_gm2 ('*g~ 'C'~ m^-2~day^-1*')')) +
  geom_smooth(method = lm, se = FALSE, size = 1.5) +
  scale_color_viridis_d(option = "D", direction = -1) +
  stat_regline_equation(label.x = 0, label.y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.9, size = 6) +
  stat_cor(aes(label = ..rr.label..), label.x = 0, label.y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.8, size = 6) +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.7, label = sprintf("RMSE: %0.2f", rmse_sum_GPP_site), size = 6, fontface = 'italic') +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.6, label = sprintf("MAE: %0.2f", mae_sum_GPP_site), size = 6, fontface = 'italic') +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.5, label = sprintf("Bias: %0.2f", bias_sum_GPP_site), size = 6, fontface = 'italic') +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.line = element_line(size = 1.2))

# Plot 2: sum_gpp vs Yieldtha_1_gm2
plot_sum_gpp <- ggplot(data = sitesatellitemergedDATAfiltered, aes(x = sum_gpp, y = Yieldtha_1_gm2, col = siteyear.x)) +
  geom_abline(intercept = 0, slope = 1, size = 1.5, col = "red", linetype = "dashed") +
  geom_point(size = 5) +
  xlim(0, max(sitesatellitemergedDATAfiltered$sum_gpp, na.rm = TRUE)) +
  ylim(0, max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE)) +
  xlab(bquote('sum_gpp ('*g~ 'C'~ m^-2~day^-1*')')) +
  ylab(bquote('Yieldtha_1_gm2 ('*g~ 'C'~ m^-2~day^-1*')')) +
  geom_smooth(method = lm, se = FALSE, size = 1.5) +
  scale_color_viridis_d(option = "D", direction = -1) +
  stat_regline_equation(label.x = 0, label.y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.9, size = 6) +
  stat_cor(aes(label = ..rr.label..), label.x = 0, label.y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.8, size = 6) +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.7, label = sprintf("RMSE: %0.2f", rmse_sum_gpp), size = 6, fontface = 'italic') +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.6, label = sprintf("MAE: %0.2f", mae_sum_gpp), size = 6, fontface = 'italic') +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.5, label = sprintf("Bias: %0.2f", bias_sum_gpp), size = 6, fontface = 'italic') +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.line = element_line(size = 1.2))
# Plot 3: sum_GPP_0_06512_31_79 vs Yieldtha_1_gm2
plot_sum_GPP_0_06512_31_79 <- ggplot(data = sitesatellitemergedDATAfiltered, aes(x = sum_GPP_0_06512_31_79, y = Yieldtha_1_gm2, col = siteyear.x)) +
  geom_abline(intercept = 0, slope = 1, size = 1.5, col = "red", linetype = "dashed") +
  geom_point(size = 5) +
  xlim(0, max(sitesatellitemergedDATAfiltered$sum_GPP_0_06512_31_79, na.rm = TRUE)) +
  ylim(0, max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE)) +
  xlab(bquote('sum_GPP_0_06512_31_79 ('*g~ 'C'~ m^-2~day^-1*')')) +
  ylab(bquote('Yieldtha_1_gm2 ('*g~ 'C'~ m^-2~day^-1*')')) +
  geom_smooth(method = lm, se = FALSE, size = 1.5) +
  scale_color_viridis_d(option = "D", direction = -1) +
  stat_regline_equation(label.x = 0, label.y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.9, size = 6) +
  stat_cor(aes(label = ..rr.label..), label.x = 0, label.y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.8, size = 6) +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.7, label = sprintf("RMSE: %0.2f", rmse_sum_GPP_0_06512_31_79), size = 6, fontface = 'italic') +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.6, label = sprintf("MAE: %0.2f", mae_sum_GPP_0_06512_31_79), size = 6, fontface = 'italic') +
  #annotate("text", x = 0, y = max(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE) * 0.5, label = sprintf("Bias: %0.2f", bias_sum_GPP_0_06512_31_79), size = 6, fontface = 'italic') +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  theme(legend.key.size = unit(1, 'cm')) +
  theme(axis.line = element_line(size = 1.2))

# Combine the plots vertically
combined_plots_vertical <- plot_sum_GPP_site / plot_sum_gpp / plot_sum_GPP_0_06512_31_79 +
  plot_layout(guides = "collect", ncol = 1) +
  plot_annotation(tag_levels = 'A')

# Display the combined plots
combined_plots_vertical
ggsave(combined_plots_vertical, filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/YieldECgpp.png", width = 10, height = 22)
write.csv(summary_table, "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/YieldECgpp_summary.csv", row.names = FALSE)

sd(sitesatellitemergedDATAfiltered$Yieldtha_1_gm2, na.rm = TRUE)
sd(sitesatellitemergedDATAfiltered$sum_GPP_0_06512_31_79, na.rm = TRUE)
sd(sitesatellitemergedDATAfiltered$sum_gpp, na.rm = TRUE)
sd(sitesatellitemergedDATAfiltered$sum_GPP_site, na.rm = TRUE)



#############################################
#############UNNECESSARY######################
#################################################

# If necessary, convert data to numeric
library(openair)
library(tidyverse)
df_long
GPP_site <- as.numeric(GPP_site)
VPMsite <- as.numeric(VPMsite)
VPMspatial <- as.numeric(VPMspatial)

# Create tailor diagram
TaylorDiagram(df_long, 
              obs = "GPP_site", 
              mod = "Model", 
              group = "GPP")

# Add legend after creating the plot
legend("bottomright", legend = c("VPMsite", "VPMspatial"),
       col = c("black", "blue"), pch = c(16, 17), bty = "n")


library(plotrix)


# Subset the data for GPP_site and GPPpvpmmodel
observed <- subset(df_long, Model == "GPP_site")$GPP
modeled <- subset(df_long, Model == "GPPpvpmmodel")$GPP

# Create Taylor Diagram
taylor.diagram(observed, modeled, col = "black", pch = 16, main = "Taylor Diagram of Model Performances")



### This data is from computer VPM spatial project YieldPP_COuntymaps_yearwise data
gppcounty<-read.csv("gppricecounty.csv")

View(gppcounty)
gppcounty$Year<- as.integer(gppcounty$Year)
##Linearregression predicted yield 
gppcounty$predictedyield<- 570+ 0.18*gppcounty$gpp_sum_8day
rmseyield<-rmse(gppcounty$yieldgm, gppcounty$predictedyield, na.rm=TRUE)
rmseyield

lb1 <- paste("RMSE == rmseyield", round(runif(1),4))
ggplot(data=gppcounty, aes(x =gpp_sum_8day , y =yieldgm, col = Year))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =6) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean Cumulative GPP ('*g~ 'C'~ m^-2~season^-1*')'))+
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_y_continuous(limits = c(500,1000))+
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2008, 2012, 2016, 2020), labels = c("2008","2012", "2016", "2020"))+
  
  stat_regline_equation(label.x = 1450, label.y = 580, size = 10)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = 1450, label.y = 625, size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 30))+ annotate(
    geom = "text",
    x = 1575,
    y = 520,
    label = bquote("RMSE == " * 56 * " g  m"^"-2" ~ season^-1),
    parse = TRUE,
    size = 10
  ) +
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(axis.ticks.length = unit(.25, "cm"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

gppcountyyieldplot
ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/gppcountyyieldplot.png", width = 40, height = 20, units = "cm")



gppcountyyieldplot
rmse(gppcounty$gpp_sum_8day, gppcounty$yieldgm, na.rm=TRUE)

ggplot(data=gppcounty, aes(x =gpp_sum_8day , y =yieldgm, col = Year))+
  geom_point()+
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2010, 2015, 2020), labels = c("2010", "2015", "2020"))+

  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+

  stat_regline_equation(label.x = 1400, label.y = 600, size = 15)+
  stat_cor(aes(label = ..rr.label..), label.x = 1500, label.y = 650, size = 15)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#net change
ggplot(data = gppcounty, aes(x = gpp_sum_8day, y = yieldgm, col = Year)) +
  geom_point(size = 6) +
  xlab(bquote('Mean Cumulative GPP ('*g~ C~ m^-2~season^-1*')')) +
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')')) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  scale_y_continuous(limits = c(500, 1000)) +
  scale_color_continuous(high = "#132B43", low = "#56B1F7",
                         breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020")) +
  stat_regline_equation(label.x = 1450, label.y = 580, size = 10) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001,
           label.x = 1450, label.y = 625, size = 10) +
  theme_classic() +
  theme(text = element_text(size = 30)) +
  # Corrected line: use bquote for consistent formatting and units
  annotate(geom = "text", x = 1575, y = 520,
           label = bquote('RMSE = ', 56.88~g~m^-2~season^-1), size = 10, parse = TRUE) +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(axis.line = element_text(size = 1.7)) +
  theme(axis.ticks.length = unit(.25, "cm")) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


df_luemaxgdd_modeled$sitenew<-df_luemaxgdd_modeled$site.x

### FACET wrap function
### FACET wrap r2 and rmse

##rename usbda to us-bda
View(LUEmaxGDD)
df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USBDA = "US-BDA")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USBDC = "US-BDC")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USHRC = "US-HRC")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USHRA = "US-HRA")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USOF1 = "US-OF1")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USOF2 = "US-OF2")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USOF3 = "US-OF3")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USOF4 = "US-OF4")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USOF5 = "US-OF5")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                         USOF6 = "US-OF6")




library(caret)
library(tidyverse)
gpppvpmsitermser2 <- df_luemaxgdd_modeled %>% 
  group_by(sitenew) %>% 
  summarise(Rsq = R2(GPP_site, GPPpvpmmodel),
            RMSE = RMSE(GPP_site, GPPpvpmmodel)) %>% 
  mutate_if(is.numeric, round, digits=2)
# Here we create our annotations data frame.
df.annotationssitePVPM <- data.frame()
# Rsq
df.annotationssitePVPM <- rbind(df.annotationssitePVPM ,
                        cbind(as.character(gpppvpmsitermser2$sitenew),
                              paste("Rsq", gpppvpmsitermser2$Rsq,
                                    sep = " = ")))
# RMSE
df.annotationssitePVPM <- rbind(df.annotationssitePVPM ,
                        cbind(as.character(gpppvpmsitermser2$sitenew),
                              paste("RMSE", gpppvpmsitermser2$RMSE,
                                    sep = " = ")))

# This here is important, especially naming the first column
colnames(df.annotationssitePVPM ) <- c("sitenew", "label")
vertical_adjustment = ifelse(grepl("Rsq",df.annotationssitePVPM$label),1.5,3)







