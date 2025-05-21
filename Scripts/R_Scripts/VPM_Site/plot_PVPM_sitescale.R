###############################################################################
# VISUALIZATION SCRIPT FOR GPP AND VEGETATION INDEX ANALYSIS
#
# This script contains:
# 1. Package loading and setup
# 2. GPP prediction visualizations
# 3. LUE (Light Use Efficiency) analysis
# 4. Vegetation index time series
# 5. Daylength-GPP relationships
# 6. Correlation plots
# 7. LAI visualizations
###############################################################################

# =============================================================================
# 1. LOAD REQUIRED PACKAGES AND SETUP =========================================
# =============================================================================

# Core data manipulation and visualization
library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

# Color scales and themes
library(viridis)
library(scales)
library(ggsci)
library(wesanderson)

# Modeling and evaluation
library(mgcv)
library(Metrics)

# Data handling
library(readxl)
library(tidyr)
library(dplyr)
library(png)
library(ggpubr)

# Font management
library(extrafont)
loadfonts(device = "win")

# Check available color palettes
show_col(pal_npg("nrc")(10))
npg_pal <- pal_npg(palette = c("nrc"), alpha = 1)

# Common base size
base_font_size <- 18

# Strip 'formattable' class and convert to numeric
rf_data$GPP_site <- as.numeric(rf_data$GPP_site)
rf_data$GPP_predicted <- as.numeric(rf_data$GPP_predicted)


# 2. BASIC VISUALIZATIONS ====================================================
library(ggplot2)
library(viridis)

FAPARLai <- ggplot(rf_data, aes(x = Lai, y = fAPAR, color = DAP, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, 
              color = "black", linetype = "dashed", size = 1) +
  scale_color_viridis(option = "D", direction = -1) +
  theme_minimal(base_size = base_font_size) +
  labs(
    color = "Days after Planting",  
    size = expression(atop("GPP EC", paste("(gC m"^-2, " day"^-1, ")"))),
    x = expression("Leaf Area Index (m"^2 * "/m"^2 * ")"),
    y = "fAPAR"
  ) +
  annotate("text",
           x = max(rf_data$Lai) * 0.95,
           y = min(rf_data$fAPAR) * 1.05,
           label = "italic(fAPAR) == 1 - exp^('-K × LAI')",
           parse = TRUE,
           size = 5,
           hjust = 1)


ggsave(
  filename = "FaparLAI.png",
  plot = FAPARLai,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 7,      # adjust as needed
  height = 5,     # adjust as needed
  dpi = 300
)



LUEtimeseries<- ggplot(rf_data, aes(x = cumulative_gdd, y = LUE, color = fAPAR, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, 
              color = "black", linetype = "dashed", size = 1) +
  scale_color_viridis(option = "D", direction = -1) +  # Viridis color scale
  theme_minimal(base_size = base_font_size) +
  labs(
    color = "fAPAR",  
    size = "GPP_site",  
    x = "Cumulative Growing Degree Days",  
    y = expression("Light Use Efficiency (gC mol"^{-1}~"photon)")
  )

LUEtimeseries <- ggplot(rf_data, aes(x = cumulative_gdd, y = LUE, color = fAPAR, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(
    aes(group = 1), 
    method = "gam", 
    formula = y ~ s(x, bs = "cs"),  # Cubic regression spline
    se = TRUE,                      # Show confidence interval
    color = "black", 
    fill = "gray70",                # Shading color for CI
    alpha = 0.2,                    # Transparency for CI
    linetype = "solid", 
    size = 1
  ) +
  scale_color_viridis(
    option = "D", 
    direction = -1,
    guide = guide_colorbar(barheight = unit(3, "cm"))  # Adjust colorbar size
  ) +
  scale_size_continuous(
    range = c(1, 5),                # Adjust point size range
    guide = guide_legend(override.aes = list(alpha = 1))  # Make legend points opaque
  ) +
  theme_minimal(base_size = base_font_size) +
  labs(
    color = "fAPAR",  
    size = expression("GPP"~(gC~m^{-2}~d^{-1})),  # Formatted GPP label
    x = "Cumulative Growing Degree Days",  
    y = expression("Light Use Efficiency (gC mol"^{-1}~"photon)"),
    caption = "Smoothing line: GAM with 95% confidence interval"
  ) +
  theme(
    legend.position = "right",
    legend.box = "vertical",        # Stack legends vertically
    legend.spacing.y = unit(0.5, "cm")  # Add space between legends
  )
# Save the plot
ggsave(filename = "LUE.png",
       plot = LUEtimeseries, 
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       width = 8, height = 6, dpi = 300)

# ==============================================================================
# 2. VARIETY COMPARISONS =====================================================
# ==============================================================================

# 4.2 LUE by variety and growth stage
rf_data <- rf_data %>%
  mutate(DAP_bin = cut(DAP, breaks = seq(min(DAP), max(DAP), by = 15), include.lowest = TRUE))  # 15-day bins

LUEvariety <- ggplot(rf_data, aes(x = Variety, y = LUE, fill = Variety)) +
  geom_violin(trim = TRUE, scale = "width", width = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = 21, outlier.size = 1.5) + # Add small boxplot inside
  scale_fill_manual(values = c(
    "CL XL745" = "#A65628",  # Brown
    "XL745" = "#E69F00",     # Orange
    "Future Climate" = "#F0E442",  # Yellow
    "XL753" = "#4B8BBE"      # Blue
  )) +
  facet_wrap(~ DAP_bin, scales = "free_y") +  # Separate panels per DAP bin
  labs(title = "LUE by Variety Across Growth Stages",
       y = expression("Light Use Efficiency (gC mol"^{-1}~"photon)"), 
       x = "Variety") +
  theme_minimal(base_size = base_font_size) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" # Remove legend if colors are redundant with x-axis
  )

# Combine with labels A and B
combined_plot <- LUEtimeseries + LUEvariety + 
  plot_annotation(tag_levels = 'A')

# Save the combined plot
ggsave(
  filename = "LUEtimeseriesVariety.png",
  plot = combined_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 16, height = 6, dpi = 300
)

# 4.1 LUE by variety boxplot
ggplot(rf_data, aes(x = Variety, y = LUE, fill = Variety)) +
  geom_boxplot(outlier.shape = 21) +  # Boxplot with outlier points
  scale_fill_manual(values = c(
    "CL XL745" = "#4B8BBE",  # Blue
    "XL745" = "#E69F00",     # Orange
    "Future Climate" = "#F0E442",  # Yellow
    "XL753" = "#A65628"      # Brown
  )) +
  theme_minimal(base_size = 14) +
  labs(y = "LUE", x = "Variety", fill = "Variety") +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 0, hjust = 1))

# ==============================================================================
# SECTION 3: GPP PREDICTION VALIDATION PLOTS
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.1: Overall Predicted vs Observed GPP
# ------------------------------------------------------------------------------

# Calculate overall MAE
mae_all <- mean(abs(rf_data$GPP_site - rf_data$GPP_predicted))
print(paste("Overall MAE:", mae_all))

get_metrics_text <- function(obs, pred, x, y, size = 16) {
  rmse_val <- rmse(obs, pred)
  mae_val <- mae(obs, pred)
  bias_val <- mean(pred - obs)
  
  txt <- paste0("RMSE = ", round(rmse_val, 2),
                "\nMAE = ", round(mae_val, 2),
                "\nBias = ", round(bias_val, 2))
  
  annotate("text", x = x, y = y, label = txt, size = size, hjust = 0)
}
# Overall scatter plot with 1:1 line and regression
p_all <- ggplot(data = rf_data, aes(x = GPP_site, y = GPP_predicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  xlim(-5, 30) + ylim(-5, 30) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +  
  scale_y_continuous(breaks = seq(0, 30, by = 5)) + # New: ticks every 10 units
  labs(x = bquote('Observed GPP ('*g~ 'C'~ m^-2~day^-1*')'),
       y = bquote('Predicted GPP ('*g~ 'C'~ m^-2~day^-1*')')) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = -5, label.y = 24, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 27, size = 16) +
  get_metrics_text(rf_data$GPP_site, rf_data$GPP_predicted, x = -5, y = 19, size = 16) +
  theme_classic() +
  theme(text = element_text(size = 48),
        legend.key.size = unit(2, 'cm'),
        axis.line = element_line(size = 1.7))


ggsave(filename = "RFGPPpredictalldata.png",
       plot = p_all,
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300, width = 24, height = 15, units = "in")

# ------------------------------------------------------------------------------
# 3.2: Train/Test Split Evaluation
# ------------------------------------------------------------------------------

# Define training and testing site-years
train_siteyears <- c("USOF22017", "USOF12017", "USBDA2016", "USBDC2016", 
                     "USHRC2016", "USOF62018", "USOF52018", "USHRC2015", 
                     "USHRA2015", "USBDC2015", "USOF32017")

test_siteyears  <- c("USHRC2017", "USBDA2015", "USOF42018", 
                     "USHRA2016", "USHRA2017")


# ---- Plot: Training Set ----
train_data <- rf_data %>% dplyr::filter(siteyear %in% train_siteyears)

p_train <- ggplot(data = train_data, aes(x = GPP_site, y = GPP_predicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  xlim(-5, 30) + ylim(-5, 30) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +  # New: ticks every 10 units
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +  # New: ticks every 10 units
  labs(x = bquote('Observed GPP ('*g~ 'C'~ m^-2~day^-1*')'),
       y = bquote('Predicted GPP ('*g~ 'C'~ m^-2~day^-1*')')) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = -5, label.y = 24, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 27, size = 16) +
  get_metrics_text(train_data$GPP_site, train_data$GPP_predicted, x = -5, y = 19, size = 16) +
  theme_classic() +
  theme(text = element_text(size = 48),
        legend.key.size = unit(2, 'cm'),
        axis.line = element_line(size = 1.7))

ggsave(filename = "RFGPPpredicttrain.png",
       plot = p_train,
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300, width = 24, height = 15, units = "in")

# ---- Plot: Testing Set ----
test_data <- rf_data %>% dplyr::filter(siteyear %in% test_siteyears)

p_test <- ggplot(data = test_data, aes(x = GPP_site, y = GPP_predicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  xlim(-5, 30) + ylim(-5, 30) +
  scale_x_continuous(breaks = seq(0, 30, by = 5)) +  # New: ticks every 10 units
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +  # New: ticks every 10 units
  labs(x = bquote('Observed GPP ('*g~ 'C'~ m^-2~day^-1*')'),
       y = bquote('Predicted GPP ('*g~ 'C'~ m^-2~day^-1*')')) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = -5, label.y = 24, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 27, size = 16) +
  get_metrics_text(test_data$GPP_site, test_data$GPP_predicted, x = -5, y = 19, size = 16) +
  theme_classic() +
  theme(text = element_text(size = 48),
        legend.key.size = unit(2, 'cm'),
        axis.line = element_line(size = 1.7))

ggsave(filename = "RFGPPpredicttest.png",
       plot = p_test,
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300, width = 24, height = 15, units = "in")

plot_annotation(
  theme = theme(plot.margin = grid::unit(c(1, 1, 1, 1), "cm"))
)

# Label each plot
p_all_lab <- p_all + 
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 48, face = "bold"),
        plot.tag.position = c(0.05, 0.95))

p_train_lab <- p_train + 
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 48, face = "bold"),
        plot.tag.position = c(0.05, 0.95))

p_test_lab <- p_test + 
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 48, face = "bold"),
        plot.tag.position = c(0.05, 0.95))

# Create a blank plot for the 4th cell
blank_plot <- ggplot() + theme_void()

# Now combine into a 2x2 layout
combined_layout <- (p_all_lab + p_train_lab) / 
  (p_test_lab + blank_plot) +
  plot_layout(widths = c(1, 1), heights = c(1, 1), guides = "collect") +
  plot_annotation(
    theme = ggplot2::theme(
      plot.margin = ggplot2::margin(1, 1, 1, 1, unit = "cm")
    )
  )

# Save the combined plot
ggsave(
  filename = "testrainall_2x2.png",
  plot = combined_layout,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  dpi = 300,
  width = 30,
  height = 30,
  units = "in",
  limitsize = FALSE
)

#--------------------------------------------      
# LUE with different features - FIXED VERSION --------------------------------------------      
#--------------------------------------------      
# Color palette
scatter_colors <- c("LUE" = wes_palette("Zissou1")[3],
                    "LUEpredicted" = wes_palette("Darjeeling1")[2])
# Common y-axis label
lue_lab <- expression("Light Use Efficiency (gC mol"^{-1}~"photon)")
# Base theme
my_theme <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  )
# Custom theme for plots without y-axis
theme_no_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)
plot_dual <- function(xvar, xlab, show_y = FALSE, show_legend = FALSE) {
  p <- ggplot(rf_data, aes(x = .data[[xvar]])) +
    geom_point(aes(y = LUE, color = "LUE"), alpha = 0.8) +
    geom_point(aes(y = LUEpredicted, color = "LUEpredicted"), alpha = 0.3) +
    geom_smooth(aes(y = LUE, color = "LUE"), method = "loess", size = 2, se = TRUE) +
    geom_smooth(aes(y = LUEpredicted, color = "LUEpredicted"), method = "loess", se = TRUE) +
    scale_color_manual(values = scatter_colors, name = NULL,
                       labels = c("LUE", "LUE predicted")) +
    labs(x = xlab) +
    my_theme
  
  if (show_y) {
    p <- p + ylab(lue_lab)
  } else {
    p <- p + theme_no_y
  }
  
  if (show_legend) {
    p <- p + theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.box.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = ggplot2::margin(3, 3, 3, 3, unit = "pt"),  # Explicitly call margin from ggplot2
      legend.text = element_text(size = 10)
    )
  }
  
  return(p)
}

# Create plots - NOW WILL WORK
p1 <- plot_dual("VPD_site", "VPD (kPa)",  show_y = TRUE, show_legend = TRUE)
p2 <- plot_dual("rH_site", "Relative Humidity (%)")
p3 <- plot_dual("nir", "NIR")
p4 <- plot_dual("MLSWI26", "MLSWI26")
p5 <- plot_dual("GDVI", "GDVI",show_y = TRUE)
p6 <- plot_dual("Tair_site", "Air Temperature (°C)")
p7 <- plot_dual("Es", "Es")
p8 <- plot_dual("cumulative_gdd", "Cumulative GDD")

# Combine plots
LUEbiophysical <- (p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)

# Display plot
LUEbiophysical
# Save the plot
ggsave(
  filename = "LUEbiophysical.png",
  plot = LUEbiophysical,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 12,       # Adjust width as needed
  height = 8,       # Adjust height as needed
  dpi = 300,        # High resolution for publications
  units = "in"      # Units for width/height (inches)
)


# ==============================================================================
# 3. GPP VI GRAPHS=========================================
# ==============================================================================
# print(top_20_lowest_train_mae)
# List of vegetation indices
vi_list <- c("IAVI", "VARI",  "NDVI", "TSAVI", "RNDVI", "kNDVI", "EVI","ATSAVI")

# Ensure numeric (as in your original code)
# joined_df <- joined_df %>%
#   dplyr::filter(!str_detect(siteyear, "2015$|2016$|2017$"))
joined_df[vi_list] <- lapply(joined_df[vi_list], as.numeric)
joined_df$GPP_site <- as.numeric(joined_df$GPP_site)

# Get GPP range
gpp_min <- min(joined_df$GPP_site, na.rm = TRUE)
gpp_max <- max(joined_df$GPP_site, na.rm = TRUE)

# Scale VIs to GPP range
scale_to_gpp_range <- function(x, vi_min, vi_max, gpp_min, gpp_max) {
  (x - vi_min) / (vi_max - vi_min) * (gpp_max - gpp_min) + gpp_min
}

# Add scaled VI columns
for (vi in vi_list) {
  vi_min <- min(joined_df[[vi]], na.rm = TRUE)
  vi_max <- max(joined_df[[vi]], na.rm = TRUE)
  joined_df[[paste0(vi, "_scaled")]] <- scale_to_gpp_range(joined_df[[vi]], vi_min, vi_max, gpp_min, gpp_max)
}

plot_dual_axis_vi_gpp <- function(data, vi_col, gpp_col = "GPP_site", 
                                  color_vi = NULL, color_gpp = NULL) {
  # Load wesanderson palette
  palette_colors <- wesanderson::wes_palette("GrandBudapest2")
  
  # Assign default colors from the palette if not specified
  if (is.null(color_vi)) color_vi <- palette_colors[1]
  if (is.null(color_gpp)) color_gpp <- palette_colors[4]
  
  # Calculate R-squared and p-value
  cor_test <- cor.test(data[[vi_col]], data[[gpp_col]], use = "complete.obs")
  r_squared <- round(cor_test$estimate^2, 2)
  p_value <- ifelse(cor_test$p.value < 0.001, "< 0.001", round(cor_test$p.value, 3))
  
  # Ranges and scaling
  vi_range <- range(data[[vi_col]], na.rm = TRUE)
  gpp_range <- range(data[[gpp_col]], na.rm = TRUE)
  scale_factor <- diff(gpp_range) / diff(vi_range)
  shift_factor <- gpp_range[1] - vi_range[1] * scale_factor
  vi_to_gpp <- function(x) { x * scale_factor + shift_factor }
  gpp_to_vi <- function(x) { (x - shift_factor) / scale_factor }
  vi_breaks <- pretty(vi_range, n = 5)
  
  # Plot
  ggplot(data, aes(x = DAP)) +
    geom_point(aes(y = !!sym(gpp_col)), color = color_gpp, size = 1.2, alpha = 0.7) +
    geom_point(aes(y = vi_to_gpp(!!sym(vi_col))), color = color_vi, size = 1.2, alpha = 0.7) +
    scale_y_continuous(
      name = "GPP (gC m⁻² day⁻¹)",
      limits = gpp_range,
      breaks = pretty(gpp_range, n = 5),
      sec.axis = sec_axis(
        trans = gpp_to_vi,
        name = "Vegetation Index scaled to 0–1",
        breaks = vi_breaks
      )
    ) +
    labs(
      title = bquote(.(vi_col) ~ "(" * italic(R)^2 ~ "=" ~  .(r_squared)*"," ~ italic("p") ~ "=" ~ .(p_value)*")"),
      x = "Days after Planting"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title.y.left = element_text(color = color_gpp, face = "bold",margin = ggplot2::margin(r = 10)),
      axis.text.y.left = element_text(color = color_gpp),
      axis.title.y.right = element_text(color = color_vi, face = "bold",margin = ggplot2::margin(l = 10)),
      axis.text.y.right = element_text(color = color_vi),
      plot.title = element_text(hjust = 0.5, size = 12)
    )
}


# Generate all plots
plot_list <- lapply(vi_list, function(vi) {
  plot_dual_axis_vi_gpp(joined_df, vi)
})

# Adjust axis labels for grid layout
for (i in seq_along(plot_list)) {
  if (i %in% 1:4) {
    plot_list[[i]] <- plot_list[[i]] + theme(axis.title.x = element_blank())
  }
  if (!i %in% c(1, 5)) {
    plot_list[[i]] <- plot_list[[i]] + theme(
      axis.title.y.left = element_blank(),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank()
    )
  }
  if (!i %in% c(4, 8)) {
    plot_list[[i]] <- plot_list[[i]] + theme(
      axis.title.y.right = element_blank(),
      axis.text.y.right = element_blank(),
      axis.ticks.y.right = element_blank()
    )
  }
}

# Create the final plot with global axis titles
final_plot <- grid.arrange(
  grobs = plot_list,
  ncol = 4,
  # left = textGrob("GPP (gC m⁻² day⁻¹)", rot = 90, 
  #                 gp = gpar(fontsize = 14, col = "gray40", fontface = "bold")),
  # right = textGrob("Vegetation Index (0–1)", rot = 270, 
  #                  gp = gpar(fontsize = 14, col = "magenta1", fontface = "bold"))
)
final_plot
# Save the plot
ggsave(
  filename = "GPPVI_dual_axis.png",
  plot = final_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 16,
  height = 8,
  dpi = 300,
  units = "in"
)


# ==============================================================================
# 3. GPP PREDICTION VI GRAPHS=========================================
# ==============================================================================
# Extract values properly from the named vectors
# Extract model coefficients
# Define the function to display metrics
get_metrics_text <- function(obs, pred, x, y, size = 5) {
  rmse_val <- rmse(obs, pred)
  mae_val <- mae(obs, pred)
  bias_val <- mean(pred - obs)
  
  txt <- paste0("RMSE = ", round(rmse_val, 2),
                "\nMAE = ", round(mae_val, 2),
                "\nBias = ", round(bias_val, 2))
  
  annotate("text", x = x, y = y, label = txt, size = size, hjust = 0)
}
intercept <- as.numeric(summary_results_df[1, ]$Final_Intercept)
slope <- as.numeric(summary_results_df[1, ]$Final_VI_PAR_Slope)

# Predict GPP using IAVI
rf_data$GPP_predicted_VI <- intercept + slope * (rf_data$IAVI * rf_data$PAR_site)

# Create the plot
p_all_VI <- ggplot(data = rf_data, aes(x = GPP_site, y = GPP_predicted_VI, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 1.5, col = "red", linetype = "dashed") +
  geom_point(size = 3) +
  geom_smooth(method = lm, se = FALSE, size = 1.5, col = "black") +
  scale_x_continuous(breaks = seq(0, 30, by = 5), limits = c(-5, 30)) +  
  scale_y_continuous(breaks = seq(0, 30, by = 5), limits = c(-5, 30)) +
  labs(
    title = "GPP Prediction using IAVI × PAR",
    subtitle = "Model: GPP = Intercept + Slope × (IAVI × PAR)",
    x = bquote('Observed GPP ('*g~ 'C'~ m^-2~day^-1*')'),
    y = bquote('Predicted GPP ('*g~ 'C'~ m^-2~day^-1*')')
  ) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = -5, label.y = 25, size = 5) +
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 27, size = 5) +
  get_metrics_text(rf_data$GPP_site, rf_data$GPP_predicted_VI, x = -5, y = 19, size = 5) +
  theme_classic() +
  theme(
    text = element_text(size = 18),
    legend.key.size = unit(1, 'cm'),
    axis.line = element_line(size = 1)
  )

# Save the plot
ggsave(
  filename = "RFGPPpredictalldataVI_IAVI.png",
  plot = p_all_VI,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  dpi = 300, width = 12, height = 8, units = "in"
)

############################################################
##############CorrelationPlot###############################
############################################################






#===================================================
#===================================================
#===================================================
#===================================================

ggplot(joined_df, aes(x = dayl, y = LUE, color = PAR_site)) +
  geom_point(shape = 19, size = 3, alpha = 0.7) +  # Match pch=19, add transparency
  scale_color_gradientn(
    colors = colorRampPalette(c("blue", "red"))(100),
    name = "Solar Radiation"
  ) +
  labs(
    x = "Daylength (hours)",
    y = "Light Use Efficiency (LUE)",
    title = "LUE vs. Daylength Colored by Solar Radiation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )


ggplot(joined_df, aes(x = cumulative_gdd, y = Lai, color = site, size = LUE)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Cumulative GDD",
    y = "LAI",
    size = "LUE",
    color = "Site",
    title = "LAI vs. Cumulative GDD Faceted by Site-Year"
  ) +
  facet_wrap(~ siteyear) +
  theme_minimal()

#################################################
##############################################
##############################################
# Load required libraries

#### ============================ ####
#### 1. Ground LAI from Excel File ####
#### ============================ ####

# File path for ground LAI data
file_path <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SiteLAICHdata/Wy3Wy4LAI2015_2017.xlsx"

# Output path for ground LAI plot
output_path_ground <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LAI_data/LAI_Way3_Way4_2015_2017.png"

# Years (sheet names) to read
sheet_years <- c("2015", "2016", "2017")

# Read sheets into a list
lai_data_list <- lapply(sheet_years, function(year) {
  read_excel(file_path, sheet = year)
})
names(lai_data_list) <- sheet_years

# Function to generate plot for a given year
plot_lai_year <- function(df, year) {
  df_long <- df %>%
    pivot_longer(cols = c("Way 3", "Way 4"), names_to = "Site", values_to = "LAI")
  
  ggplot(df_long, aes(x = Date, y = LAI, color = Site)) +
    geom_line(size = 1) +
    labs(
      title = paste("LAI over Time for", year),
      x = "Date",
      y = "LAI",
      color = "Site"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Way 3" = "blue", "Way 4" = "green")) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
}

# Generate plots for each year
plot_2015 <- plot_lai_year(lai_data_list[["2015"]], "2015")
plot_2016 <- plot_lai_year(lai_data_list[["2016"]], "2016")
plot_2017 <- plot_lai_year(lai_data_list[["2017"]], "2017")

# Combine and save
combined_ground_plot <- grid.arrange(plot_2015, plot_2016, plot_2017, ncol = 1)
ggsave(output_path_ground, combined_ground_plot, width = 8, height = 12, dpi = 300)


#### ===================================== ####
#### 2. Ungapfilled Satellite LAI (2015–2018) ####
#### ===================================== ####

# Extract site names from list (first 10 characters of each name)
site_names_ungap <- substr(names(meteo_df_2015_2018ni), 1, 10)

# Generate LAI plots for ungapfilled data
plot_list_ungap <- lapply(1:16, function(i) {
  df <- meteo_df_2015_2018ni[[i]]
  site_name <- site_names_ungap[i]
  
  ggplot(df, aes(x = Date, y = Lai)) +
    geom_point(color = "darkgreen", size = 1) +
    labs(
      title = paste("Site:", site_name),
      x = "Date",
      y = "LAI"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
})

# Combine and save ungapfilled plots
combined_ungap_plot <- wrap_plots(plotlist = plot_list_ungap, ncol = 4)
output_path_ungap <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LAI_data/LAI_16sites_grid_ngapfilled.png"
ggsave(filename = output_path_ungap, plot = combined_ungap_plot, width = 16, height = 12, dpi = 300)


#### =================================== ####
#### 3. Gapfilled Satellite LAI (2015–2018) ####
#### =================================== ####

# Extract site names from gapfilled data
site_names_gap <- substr(names(meteo_df_2015_2018), 1, 10)

# Generate LAI plots for gapfilled data
plot_list_gap <- lapply(1:16, function(i) {
  df <- meteo_df_2015_2018[[i]]
  site_name <- site_names_gap[i]
  
  ggplot(df, aes(x = Date, y = Lai)) +
    geom_point(color = "darkgreen", size = 1) +
    labs(
      title = paste("Site:", site_name),
      x = "Date",
      y = "LAI"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
})

# Combine and save gapfilled plots
combined_gap_plot <- wrap_plots(plotlist = plot_list_gap, ncol = 4)
output_path_gap <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LAI_data/LAI_16sites_gridgapfilled.png"
ggsave(filename = output_path_gap, plot = combined_gap_plot, width = 16, height = 12, dpi = 300)



#### =================================== ####
#### 3. SG fixed Satellite LAI (2015–2018) ####
#### =================================== ####

# Extract site names from gapfilled data
site_names_gap <- substr(names(meteo_df_2015_2018), 1, 10)

# Generate LAI plots for gapfilled data
plot_list_gap <- lapply(1:16, function(i) {
  df <- meteo_df_2015_2018[[i]]
  site_name <- site_names_gap[i]
  
  ggplot(df, aes(x = Date, y = Lai)) +
    geom_point(color = "darkgreen", size = 1) +
    labs(
      title = paste("Site:", site_name),
      x = "Date",
      y = "LAI"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
})

# Combine and save gapfilled plots
combined_gap_plot <- wrap_plots(plotlist = plot_list_gap, ncol = 4)
output_path_gap <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LAI_data/LAI_16sites_gridgapfilled.png"
ggsave(filename = output_path_gap, plot = combined_gap_plot, width = 16, height = 12, dpi = 300)



library(dplyr)
library(lubridate)

# Planting DOY values
DOP_values <- c(
  "USBDA_2015" = 92, "USBDA_2016" = 82, "USBDC_2015" = 92, "USBDC_2016" = 82,
  "USHRA_2015" = 97, "USHRA_2016" = 114, "USHRA_2017" = 99, "USHRC_2015" = 98,
  "USHRC_2016" = 114, "USHRC_2017" = 100, "USOF1_2017" = 91, "USOF2_2017" = 91,
  "USOF3_2017" = 91, "USOF4_2018" = 99, "USOF5_2018" = 99, "USOF6_2018" = 99
)

# Fixed harvest DOY (September 1)
harvest_doy <- yday(ymd("2020-09-01"))  # = 245

# Apply the condition to each site
meteo_df_2015_2018ni <- lapply(names(meteo_df_2015_2018ni), function(site_name) {
  df <- meteo_df_2015_2018ni[[site_name]]
  # Extract site key (first 10 characters to match DOP_values)
  site_key <- substr(site_name, 1, 10)
  dop <- DOP_values[site_key]
  # Skip if DOP not found
  if (is.na(dop)) return(df)
  df <- df %>%
    mutate(
      DOY = yday(Date),
      Lai = ifelse(DOY > (dop + 20) & DOY < harvest_doy & Lai < 2.5, NA, Lai)
    )
  return(df)
})
# Rename list to original names
names(meteo_df_2015_2018ni) <- names(DOP_values)

meteo_df_2015_2018ni



# ==============================================================================
# 6. PLOT VARIABLE IMPORTANCE ==================================================
# ==============================================================================
# Plot
importance_plot <- ggplot(combined_df, aes(x = `%IncMSE`, y = Variable, fill = Seed)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "% Increase in MSE",
    y = NULL,
    title = "Variable Importance across Seeds 100–400"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  theme(legend.position = "bottom")

# Save the plot
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/best predictor seed.png",
  plot = importance_plot,
  width = 10,
  height = 8,
  dpi = 300
)

gini_plot <- ggplot(gini_combined, aes(x = MeanDecreaseGini, y = Variable, fill = Seed)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "Mean Decrease in Gini (Node Impurity)",
    y = NULL,
    title = "Variable Importance (Node Impurity) across Seeds 100–400"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  theme(legend.position = "bottom")

# Save the plot
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/best predictor seed node impurity.png",
  plot = gini_plot,
  width = 10,
  height = 8,
  dpi = 300
)
# =============================================================================
# 4. VEGETATION INDEX ANALYSIS ===============================================
# =============================================================================
# First, reshape data to long format for vegetation indices
vi_data <- rf_data %>%
  select(DAP, IAVI, VARI, NIRv) %>%
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
  select(DAP, siteyear, GPP_predicted, GPP_site, IAVI, VARI, GDVI) %>%
  pivot_longer(cols = c(GPP_predicted, GPP_site, IAVI, VARI, GDVI), names_to = "variable", values_to = "value")

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
  select(DAP, siteyear, GPP_predicted, GPP_site, IAVI, VARI, GDVI, ExGR, dayl, Es) %>%
  pivot_longer(cols = c(GPP_predicted, GPP_site, IAVI, VARI, GDVI, ExGR, dayl, Es),
               names_to = "variable", values_to = "value")

# Color palette
variable_colors <- c(
  "GPP_site" = "#B09C85FF",
  "GPP_predicted" = "#7E6148FF",
  "IAVI" = "#91D1C2FF",
  "VARI" = "#8491B4FF",
  "GDVI" = "#00A087FF",
  "ExGR" = "#3C5488FF",
  "Es" = "#F39B7FFF"
)


# Define the scaling factors
gpp_range <- range(dplyr::filter(plot_data, variable == "GPP_predicted")$value, na.rm = TRUE)
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
            data = dplyr::filter(plot_data, variable == "GPP_predicted"), size = 1) +
  
  # Plot GPP site (primary axis) – plotted after so it appears on top
  geom_point(aes(y = value, color = variable),
             data = dplyr::filter(plot_data, variable == "GPP_site"), size = 1.5, shape = 16) +
  
  # Plot NDVI (secondary axis)
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = dplyr::filter(plot_data, variable == "IAVI"), size = 1) +
  # kNDVI
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = dplyr::filter(plot_data, variable == "VARI"), size = 1) +
  # NIRv
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = dplyr::filter(plot_data, variable == "GDVI"), size = 1) +
  # TDVI
  geom_line(aes(y = scale_to(value, ndvi_range, gpp_range), color = variable),
            data = dplyr::filter(plot_data, variable == "ExGR"), size = 1) +
  # Ec
  geom_line(aes(y = scale_to(value, Es_range, gpp_range), color = variable),
            data = dplyr::filter(plot_data, variable == "Es"), size = 1) +
  
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
    Total_IAVI = sum(IAVI, na.rm = TRUE),
    Total_temperature = sum(Tair_site, na.rm = TRUE),
    total_GPP_site = sum(GPP_site, na.rm = TRUE),
    total_GPP_predicted = sum(GPP_predicted, na.rm = TRUE),
    total_GPP_predicted_VI = sum(GPP_predicted_VI, na.rm = TRUE),
    total_dayl_hour = sum(dayl_hour, na.rm = TRUE),
    total_Es = sum(Es, na.rm = TRUE),  # Total of Es + Ei + Ec
    DOP = mean(DOP, na.rm = TRUE),
    Variety = first(Variety)
  )

rf_summary <- rf_summary %>%
  mutate(ET_level = cut(
    total_Es,
    breaks = quantile(total_Es, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
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
# 3. Calculate R² and RMSE
r2_value <- summary(gam_model)$r.sq
rmse_value <- rmse(rf_summary$total_GPP_site, rf_summary$GPP_predicted)
# Create annotation label without italicizing R²
annotation_label <- paste("R² = ", round(r2_value, 3), "\nRMSE = ", round(rmse_value, 2), " g C m⁻² season⁻¹")
# Full plot code
ggplot(rf_summary, aes(x = total_dayl_hour, y = total_GPP_site, 
                       label = siteyear, color = DOP, shape = Variety, size = Total_IAVI)) +
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
    size = "Total_IAVI"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(size = 12)
  )

library(ggplot2)
library(mgcv)
library(dplyr)
library(tidyr)
library(broom)  # Needed for 'augment'

# 1. Pivot data to long format
rf_long <- rf_summary %>%
  select(siteyear, total_dayl_hour, total_GPP_site, total_GPP_predicted, total_GPP_predicted_VI) %>%
  pivot_longer(
    cols = starts_with("total_GPP"),
    names_to = "GPP_type",
    values_to = "GPP_value"
  )

plot(rf_long$total_dayl_hour, rf_long$GPP_value)
library(ggplot2)

ggplot(rf_long, aes(x = total_dayl_hour, y = GPP_value, color = GPP_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +   # linear regression lines without confidence intervals
  labs(
    x = "Total Daylight Hours",
    y = "GPP Value",
    color = "GPP Type"
  ) +
  theme_minimal()


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


#### Plot the Vegetation Indices #######
# Scatter plot: LUE vs all predictors from rf_data
ggplot(rf_data, aes(x = DAP, y = LUE)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "DAP vs LUE", x = "DAP", y = "LUE")

ggplot(joined_df, aes(x = kNDVI, y = GPP_site)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "kNDVI vs GPP_site", x = "kNDVI", y = "GPP_site")

ggplot(joined_df, aes(x = SAVI2, y = GPP_site)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "SAVI2 vs GPP_site", x = "SAVI2", y = "GPP_site")

ggplot(joined_df, aes(x = GNDVI, y = GPP_site)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "GNDVI vs GPP_site", x = "GNDVI", y = "GPP_site")

ggplot(joined_df, aes(x = LSWI, y = GPP_site)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "LSWI vs GPP_site", x = "LSWI", y = "GPP_site")

ggplot(joined_df, aes(x = DAP, y = LUE)) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(title = "DAP vs LUE (joined_df)", x = "DAP", y = "LUE")



# Prepare %IncMSE data: rename column to a common name 'Importance'
df_imp <- combined_df %>%
  rename(Importance = `%IncMSE`) %>%
  mutate(Metric = "% Increase in MSE")

# Prepare Gini data: rename column to 'Importance'
df_gini <- gini_combined %>%
  rename(Importance = MeanDecreaseGini) %>%
  mutate(Metric = "Mean Decrease in Gini")

# Combine both datasets
plot_df <- bind_rows(df_imp, df_gini)

# Order variables by average importance across all metrics and seeds
ordered_vars <- plot_df %>%
  group_by(Variable) %>%
  summarise(meanImp = mean(Importance), .groups = "drop") %>%
  arrange(meanImp) %>%
  pull(Variable)

plot_df$Variable <- factor(plot_df$Variable, levels = ordered_vars)

# Plot both metrics in one plot
combined_plot <- ggplot(plot_df, aes(x = Importance, y = Variable, fill = Seed)) +
  geom_bar(
    aes(
      group = interaction(Seed, Metric),
      # Set linetype only for Gini bars to dashed, solid otherwise
      linetype = Metric
    ),
    stat = "identity", 
    position = position_dodge(width = 0.8),
    color = "black",
    size = 0.5,
    show.legend = TRUE
  ) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  scale_linetype_manual(values = c("% Increase in MSE" = "solid", "Mean Decrease in Gini" = "dashed")) +
  labs(
    x = "Variable Importance",
    y = NULL,
    fill = "Seed",
    linetype = "Metric",
    title = "Variable Importance across Seeds and Metrics"
  ) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom")

# Save plot
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/combined_importance_singleplot.jpeg",
  plot = combined_plot,
  width = 18,
  height = 14,
  dpi = 300
)

library(ggpattern)
library(ggplot2)
library(dplyr)
library(wesanderson)  # for wes_palette()

# Example adapting your plot_df data structure and aesthetics
combined_plot <- ggplot(plot_df, aes(x = Importance, y = Variable, fill = Seed)) +
  geom_col_pattern(
    aes(
      pattern = Metric,            # pattern depends on metric (%IncMSE or Gini)
      pattern_angle = 45,          # fixed pattern angle (can customize per Metric if you want)
      pattern_spacing = 0.02,      # spacing between pattern elements
      pattern_density = 0.4,       # density of pattern
      pattern_fill = Seed,         # pattern fill color matches seed fill
      pattern_colour = "black"     # pattern outline color
    ),
    color = "black",               # bar outline color
    position = position_dodge(width = 0.8),
    stat = "identity",
    size = 0.3,
    show.legend = TRUE
  ) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  scale_pattern_manual(values = c("% Increase in MSE" = "none", "Mean Decrease in Gini" = "stripe")) +
  scale_pattern_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  scale_pattern_colour_manual(values = rep("black", 4)) +
  labs(
    x = "Variable Importance",
    y = NULL,
    fill = "Seed",
    pattern = "Metric",
    title = "Variable Importance across Seeds and Metrics"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")

# Save
ggsave("combined_importance_pattern_plot.jpeg", combined_plot, width = 18, height = 14, dpi = 300)

library(ggpattern)
library(ggplot2)
library(dplyr)
library(wesanderson)  # for wes_palette()

# Example adapting your plot_df data structure and aesthetics
combined_plot <- ggplot(plot_df, aes(x = Importance, y = Variable, fill = Seed)) +
  geom_col_pattern(
    aes(
      pattern = Metric,            # pattern depends on metric (%IncMSE or Gini)
      pattern_angle = 45,          # fixed pattern angle (can customize per Metric if you want)
      pattern_spacing = 0.02,      # spacing between pattern elements
      pattern_density = 0.4,       # density of pattern
      pattern_fill = Seed,         # pattern fill color matches seed fill
      pattern_colour = "black"     # pattern outline color
    ),
    color = "black",               # bar outline color
    position = position_dodge(width = 0.8),
    stat = "identity",
    size = 0.3,
    show.legend = TRUE
  ) +
  scale_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  scale_pattern_manual(values = c("% Increase in MSE" = "none", "Mean Decrease in Gini" = "stripe")) +
  scale_pattern_fill_manual(values = wes_palette("Cavalcanti1", n = 4)) +
  scale_pattern_colour_manual(values = rep("black", 4)) +
  labs(
    x = "Variable Importance",
    y = NULL,
    fill = "Seed",
    pattern = "Metric",
    title = "Variable Importance across Seeds and Metrics"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")

# Save
ggsave("combined_importance_pattern_plot.jpeg", combined_plot, width = 18, height = 14, dpi = 300)

combined_plot
