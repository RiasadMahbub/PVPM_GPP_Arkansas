####This script is for visualization 
library(tidyverse)
library(ggplot2)
library(viridis)
library(dplyr)
library(scales)
library(tidyr)
library(scales)
library(extrafont)
library(ggpattern)
library(patchwork)
library(mgcv)      # for gam()
library(Metrics)   # for rmse()
library(patternplot)
library(png)

library("scales")
library(ggsci)
show_col(pal_npg("nrc")(10))
pal_npg(palette = c("nrc"), alpha = 1)

library(ggplot2)
library(viridis)

ggplot(rf_data, aes(x = DAP, y = LUE, color = fAPAR, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  scale_color_viridis(option = "D", direction = -1) +
  theme_minimal() +
  labs(
    color = "fAPAR",
    size = "GPP_site",
    x = "Days After Planting (DAP)",
    y = "Light Use Efficiency (LUE)"
  )

val_siteyears <- c("USHRC2015", "USHRC2016")
mae <- mean(abs(rf_data$GPP_site - rf_data$GPP_predicted))
print(mae)
ggplot(data=rf_data, aes(x =GPP_site , y =GPP_predicted, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('Daily GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(Daily~GPP~LUE[randomforest],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  stat_regline_equation(label.x = -5, label.y = 23, size = 16)+
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 21, size = 16)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave(filename = "RFGPPpredictalldata.png",
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300,
       width = 24, height = 15, units = "in")

train_siteyears <- c("USHRA2016", "USOF22017", "USOF12017", "USHRA2015", 
                     "USBDA2016", "USBDC2015", "USBDC2016", "USHRA2017",
                     "USHRC2015", "USHRC2016", "USOF62018" ,"USOF52018")
test_siteyears  <- c("USHRC2017", "USBDA2015", "USOF42018")


maetrain <- rf_data %>%
  dplyr::filter(siteyear %in% train_siteyears) %>%
  dplyr::summarise(mae = mean(abs(GPP_site - GPP_predicted))) %>%
  dplyr::pull(mae)

maetest <- rf_data %>%
  dplyr::filter(siteyear %in% test_siteyears) %>%
  dplyr::summarise(mae = mean(abs(GPP_site - GPP_predicted))) %>%
  dplyr::pull(mae)

ggplot(data = dplyr::filter(rf_data, siteyear %in% train_siteyears), 
       aes(x = GPP_site, y = GPP_predicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  xlim(-5, 30) +
  ylim(-5, 30) +
  xlab(bquote('Daily GPP EC ('*g~ 'C'~ m^-2~day^-1*')')) +
  ylab(expression(paste(Daily~GPP~LUE[randomforest],~'('*g~ 'C'~ m^-2~day^-1*')' ))) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = -5, label.y = 23, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 21, size = 16) +
  theme_classic() +
  theme(text = element_text(size = 48),
        legend.key.size = unit(2, 'cm'),
        axis.line = element_line(size = 1.7),
        plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))

ggsave(filename = "RFGPPpredicttrain.png",
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300,
       width = 24, height = 15, units = "in")



# Sample ggplot2 code assuming your data frame is called `df`
# and has columns 'Variety', 'LUE', and 'Treatment'

ggplot(rf_data, aes(x = Variety, y = LUE, fill = Variety)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = c(
    "CL XL745" = "#4B8BBE",            # Blue
    "XL745" = "#E69F00",            # Orange
    "Future Climate" = "#F0E442",     # Yellow
    "XL753" = "#A65628"  # Brown
  )) +
  theme_minimal(base_size = 14) +
  labs(
    y = "LUE",
    x = "Variety",
    fill = "Variety"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

######################################
## LUE box plot for different cultivars and lon Adding a growth parameter
######################################
# Create DAP bins (every 15 days)
rf_data <- rf_data %>%
  mutate(DAP_bin = cut(DAP, breaks = seq(min(DAP), max(DAP), by = 15), include.lowest = TRUE))

# Box plot grouped by DAP bins with dynamic y-axis per facet
ggplot(rf_data, aes(x = Variety, y = LUE, fill = Variety)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = c(
    "CL XL745" = "#A65628",      # Blue
    "XL745" = "#E69F00",         # Orange
    "Future Climate" = "#F0E442", # Yellow
    "XL753" = "#4B8BBE"          # Brown
  )) +
  facet_wrap(~ DAP_bin, scales = "free_y") +  # Y-axis free per DAP_bin
  theme_minimal(base_size = 14) +
  labs(
    y = "LUE",
    x = "Variety",
    fill = "Variety",
    title = "Box Plot of LUE for Different Varieties Across 15-Day DAP Bins"
  ) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# First, reshape data to long format for vegetation indices
vi_data <- rf_data %>%
  select(DAP, kNDVI, NDVI, NIRv) %>%
  pivot_longer(cols = c(kNDVI, NDVI, NIRv), names_to = "VI_type", values_to = "VI_value")

# Make sure fonts are loaded
# font_import(pattern = "DejaVu", prompt = FALSE)  # Run once
loadfonts(device = "win")  # or "pdf", "all", depending on your output
# Set your preferred font
myFont <- "DejaVu Sans Mono"  # Or "DejaVu Sans" if that's your import
windowsFonts(sans = myFont)   # Optionally register for use as default

rf_data$GPP_predicted<-as.numeric(rf_data$GPP_predicted)
rf_data$GPP_site<-as.numeric(rf_data$GPP_site)
# First reshape the data for plotting
plot_data <- rf_data %>%
  select(DAP, siteyear, GPP_predicted, GPP_site, kNDVI, NDVI, NIRv) %>%
  pivot_longer(cols = c(GPP_predicted, GPP_site, kNDVI, NDVI, NIRv), names_to = "variable", values_to = "value")

# Define color palette
variable_colors <- c(
  "GPP_site" = "grey40",
  "GPP_predicted" = "black",
  "NDVI" = "green",
  "kNDVI" = "purple",
  "NIRv" = "orange"
)



# Reshape data
plot_data <- rf_data %>%
  select(DAP, siteyear, GPP_predicted, GPP_site, kNDVI, NDVI, NIRv, TDVI, dayl, Es) %>%
  pivot_longer(cols = c(GPP_predicted, GPP_site, kNDVI, NDVI, NIRv, TDVI, dayl, Es),
               names_to = "variable", values_to = "value")

# Color palette
variable_colors <- c(
  "GPP_site" = "#B09C85FF",
  "GPP_predicted" = "#7E6148FF",
  "NDVI" = "#91D1C2FF",
  "kNDVI" = "#8491B4FF",
  "NIRv" = "#00A087FF",
  "TDVI" = "#3C5488FF",
  "Es" = "#F39B7FFF"
)


# Define the scaling factors
gpp_range <- range(filter(plot_data, variable == "GPP_predicted")$value, na.rm = TRUE)
ndvi_range <- c(0, 1) # Desired range for NDVI, kNDVI
nirv_range <- c(0, 0.4) # Desired range for NIRv
Es_range <-c(0.02,4.50)

# Function to scale values from one range to another
scale_to <- function(x, from, to) {
  return((x - from[1]) / (from[2] - from[1]) * (to[2] - to[1]) + to[1])
}

ggplot(plot_data, aes(x = DAP)) +
  
  # Plot GPP predicted (primary axis)
  geom_line(aes(y = value, color = variable),
            data = filter(plot_data, variable == "GPP_predicted"), size = 1) +
  
  # Plot GPP site (primary axis) – plotted after so it appears on top
  geom_point(aes(y = value, color = variable),
             data = filter(plot_data, variable == "GPP_site"), size = 1.5, shape = 16) +
  
  # Plot NDVI (secondary axis)
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = filter(plot_data, variable == "NDVI"), size = 1) +
  # kNDVI
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = filter(plot_data, variable == "kNDVI"), size = 1) +
  # NIRv
  geom_line(aes(y = scale_to(value, nirv_range, gpp_range), color = variable),
            data = filter(plot_data, variable == "NIRv"), size = 1) +
  # TDVI
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = filter(plot_data, variable == "TDVI"), size = 1) +
  # Ec
  geom_line(aes(y = scale_to(value, Es_range, gpp_range), color = variable),
            data = filter(plot_data, variable == "Es"), size = 1) +
  
  facet_wrap(~ siteyear, scales = "fixed", ncol = 4) +
  scale_color_manual(values = variable_colors) +
  scale_x_continuous(limits = c(25, max(plot_data$DAP, na.rm = TRUE))) +
  scale_y_continuous(
    name = "GPP (gC m⁻² day⁻¹)",
    sec.axis = sec_axis(
      trans = ~ scale_to(., gpp_range, ndvi_range),
      name = "Vegetation Indices scaled to 0-1",
      breaks = seq(0, 1, by = 0.2)
    )
  ) +
  labs(x = "Days After Planting (DAP)", color = "Variable") +
  theme_minimal(base_family = myFont) +
  theme(
    strip.text = element_text( family = myFont),
    legend.position = "bottom",
    axis.title.y.right = element_text(angle = 90, vjust = 0.5, family = myFont)
  )

rf_data$dayl_hour <- rf_data$dayl / 3600
plot(rf_data$dayl_hour, rf_data$GPP_site)


ggplot(rf_data, aes(x = dayl_hour, y = GPP_site, color = DAP)) +
  geom_point() +
  scale_color_viridis_c(option = "D", direction = -1) +
  labs(x = "Daylength (hours)", color = "Days after Planting") +
  ylab(bquote('Daily GPP EC ('*g~ 'C'~ m^-2~day^-1*')')) +
  theme_minimal()
# You may need to define a variable for this based on max GPP or a DAP threshold.
rf_data <- rf_data %>%
  group_by(siteyear) %>%
  mutate(
    max_dap = DAP[which.max(GPP_site)],
    phase = ifelse(DAP <= max_dap, "increasing", "decreasing")
  ) %>%
  ungroup()
# Fit loess models per phase
loess_results <- rf_data %>%
  group_by(phase) %>%
  summarise(
    model = list(loess(GPP_site ~ dayl_hour, data = cur_data())),
    .groups = "drop"
  )
# Function to extract R² from a loess model
get_r2 <- function(model) {
  pred <- predict(model)
  obs <- model$y
  1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)
}
# Add R² to results
loess_results <- loess_results %>%
  rowwise() %>%
  mutate(R2 = get_r2(model)) %>%
  mutate(eq = paste0("LOESS (span=", model$pars$span, ")"))

# View results
loess_results %>%
  select(phase, eq, R2)
ggplot(rf_data, aes(x = dayl_hour, y = GPP_site, color = DAP)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(aes(group = phase, linetype = phase), method = "loess", se = FALSE, size = 2) +
  scale_color_viridis_c(option = "D", direction = -1) +
  labs(
    x = "Daylength (hours)",
    y = bquote('Daily GPP EC ('*g~ 'C'~ m^-2~day^-1*')'),
    color = "Days after Planting",
    linetype = "Trend Phase"
  ) +
  theme_minimal(base_size = 16)

rf_data <- rf_data %>%
  mutate(ET = Es + Ei + Ec)  # Sum across Es, Ei, and Ec for each row

rf_summary <- rf_data %>%
  group_by(siteyear) %>%
  summarise(
    Total_kndvi = sum(kNDVI, na.rm = TRUE),
    Total_temperature = sum(Tair_site, na.rm = TRUE),
    total_GPP_site = sum(GPP_site, na.rm = TRUE),
    total_dayl_hour = sum(dayl_hour, na.rm = TRUE),
    total_ET = sum(ET, na.rm = TRUE),  # Total of Es + Ei + Ec
    DOP = mean(DOP, na.rm = TRUE),
    Variety = first(Variety)
  )

rf_summary <- rf_summary %>%
  mutate(ET_level = cut(
    total_ET,
    breaks = quantile(total_ET, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
    labels = c("Low ET", "Medium ET", "High ET"),
    include.lowest = TRUE
  ))

rf_summary <- rf_summary %>%
  mutate(Temperature_level = cut(
    Total_temperature,
    breaks = quantile(Total_temperature, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
    labels = c("Low T", "Medium T", "High T"),
    include.lowest = TRUE
  ))





# 1. Fit the GAM model
gam_model <- gam(total_GPP_site ~ s(total_dayl_hour), data = rf_summary)
# 2. Predict GPP values using the model
rf_summary$GPP_predicted <- predict(gam_model, newdata = rf_summary)
# Create annotation label without italicizing R²
annotation_label <- paste("R² = ", round(r2_value, 3), "\nRMSE = ", round(rmse_value, 2), " g C m⁻² season⁻¹")
# Full plot code
ggplot(rf_summary, aes(x = total_dayl_hour, y = total_GPP_site, 
                       label = siteyear, color = DOP, shape = Variety, size = Total_kndvi)) +
  geom_point() +
  geom_text(vjust = -1, size = 3.5, fontface = "bold") +
  geom_smooth(
    method = "gam", 
    formula = y ~ s(x), 
    se = TRUE, 
    color = "#7E6148FF", 
    fill = "gray70", 
    alpha = 0.3, 
    linetype = "dashed",
    inherit.aes = FALSE,
    mapping = aes(x = total_dayl_hour, y = total_GPP_site)
  ) +
  annotate("text", 
           x = max(rf_summary$total_dayl_hour) * 0.95, 
           y = max(rf_summary$total_GPP_site) * 0.95,
           label = annotation_label, 
           hjust =0.5, vjust = 0.5,
           size = 5) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  scale_size_continuous(range = c(2, 6)) +
  scale_x_continuous(
    breaks = pretty(rf_summary$total_dayl_hour, n = 5),
    labels = pretty(rf_summary$total_dayl_hour, n = 5)
  ) +
  scale_y_continuous(
    breaks = pretty(rf_summary$total_GPP_site, n = 5),
    labels = pretty(rf_summary$total_GPP_site, n = 5)
  ) +
  labs(
    x = "Cumulative Seasonal Daylength (hours)",
    y = "Cumulative Seasonal GPP (g C m⁻² season⁻¹)",
    title = "GAM Fit: Cumulative GPP vs. Cumulative Daylength by Site-Year",
    color = "DOP",
    shape = "Variety",
    size = "Total_kndvi"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )


# Load necessary package for pattern
if (!require(ggpattern)) install.packages("ggpattern", repos = "http://cran.us.r-project.org")
# Full plot code with alpha for categorical Temperature_level
ggplot(rf_summary, aes(x = total_dayl_hour, y = total_GPP_site, 
                       label = siteyear, color = DOP, shape = Variety, size = Total_kndvi)) +
  geom_point(aes(alpha = factor(Temperature_level))) +  # Treat Temperature_level as a factor
  geom_text(vjust = -1, size = 3.5, fontface = "bold") +
  geom_smooth(
    method = "gam", 
    formula = y ~ s(x), 
    se = TRUE, 
    color = "#7E6148FF", 
    fill = "gray70", 
    alpha = 0.3, 
    linetype = "dashed",
    inherit.aes = FALSE,
    mapping = aes(x = total_dayl_hour, y = total_GPP_site)
  ) +
  annotate("text", 
           x = max(rf_summary$total_dayl_hour) * 0.95, 
           y = max(rf_summary$total_GPP_site) * 0.95,
           label = annotation_label, 
           hjust = 0.5, vjust = 0.5,
           size = 5) +
  scale_color_viridis_c(option = "plasma", direction = -1) +
  scale_size_continuous(range = c(2, 6)) +
  scale_alpha_manual(values = c("High T" = 1, "Medium T" = 0.6, "Low T" = 0.3)) +  # Set custom alpha values for categories
  scale_x_continuous(
    breaks = pretty(rf_summary$total_dayl_hour, n = 5),
    labels = pretty(rf_summary$total_dayl_hour, n = 5)
  ) +
  scale_y_continuous(
    breaks = pretty(rf_summary$total_GPP_site, n = 5),
    labels = pretty(rf_summary$total_GPP_site, n = 5)
  ) +
  labs(
    x = "Cumulative Seasonal Daylength (hours)",
    y = "Cumulative Seasonal GPP (g C m⁻² season⁻¹)",
    title = "GAM Fit: Cumulative GPP vs. Cumulative Daylength by Site-Year",
    color = "Day of Planting \n (color)",
    shape = "Variety \n (Shape)",
    size = "Total kNDVI \n (Size)",
    alpha = "Temperature Level \n  (Transperancy)"  # Label for alpha
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )
