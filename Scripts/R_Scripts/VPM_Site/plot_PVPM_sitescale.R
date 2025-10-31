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
# Core Tidyverse packages (automatically includes ggplot2, dplyr, tidyr, readr, stringr, etc.)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(patchwork)
library(cowplot)
library(ggsci)
library(scales)
library(viridis)
library(wesanderson)
library(ggpattern)
library(ggplot2)

# Statistical and modeling
library(mgcv)        # Generalized Additive Models
library(Metrics)     # mae(), rmse(), etc.
library(broom)       # Needed for 'augment'

# File and data handling
library(readxl)
library(png)
library(ggpubfigs)

# Fonts (for Windows devices)
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
rf_data$VPD_site  <- as.numeric(rf_data$VPD_site)
rf_data$rH_site   <- as.numeric(rf_data$rH_site)
rf_data$Tair_site <- as.numeric(rf_data$Tair_site)



# 1. FAPARLAI====================================================
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

# 2. LUEtimeseries====================================================
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
    x = "Cumulative Growing Degree Days (°C)",  
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
       width = 12, height = 6, dpi = 300)

# ==============================================================================
# 3. VARIETY COMPARISONS =====================================================
# ==============================================================================
# Add a row index for facet grid
# Define fixed GDD breaks and labels
# Define GDD breaks and labels for 8 bins (0–2400)
breaks <- seq(0, 2400, by = 300)
labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])  # 8 labels

# Assign CGDD bins
rf_data <- rf_data %>%
  mutate(
    CGDD_bin = cut(cumulative_gdd, breaks = breaks, labels = labels, include.lowest = TRUE),
    CGDD_bin = factor(CGDD_bin, levels = labels)  # preserve order
  )

# Plot using facet_wrap with 3 rows and 3 columns (1 panel will be blank)
LUEvariety <- ggplot(rf_data, aes(x = Variety, y = LUE, fill = Variety)) +
  geom_violin(trim = TRUE, scale = "width", width = 0.8) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = 21, outlier.size = 1.5) +
  scale_fill_manual(values = c(
    "CL XL745" = "#A65628",
    "XL745" = "#E69F00",
    "Future Climate" = "#F0E442",
    "XL753" = "#4B8BBE"
  )) +
  facet_wrap(~CGDD_bin, nrow = 3, ncol = 3) +
  labs(
    title = "LUE by Variety across Cumulative Growing Degree Days",
    y = expression("Light Use Efficiency (gC mol"^{-1}~"photon)"),
    x = "Variety"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.position = "none",
    strip.background = element_rect(fill = "gray90", color = NA),
    strip.text = element_text(size = 12)
  )

# Combine with LUEtimeseries plot
combined_plot <- LUEtimeseries + LUEvariety +
  plot_annotation(tag_levels = 'A')
combined_plot
# Save the plot
ggsave(
  filename = "LUEtimeseriesVariety.png",
  plot = combined_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 16, height = 8, dpi = 300
)


# 4.1 LUE by variety boxplots====================================================
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
# SECTION 2: GPP PREDICTION VALIDATION PLOTS
# ==============================================================================
# ------------------------------------------------------------------------------
# 2.1: Overall Predicted vs Observed GPP
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
# 2.2: Train/Test Split Evaluation
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
combined_layout
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

# ==============================================================================
# SECTION 3: LUE PREDICTION VALIDATION PLOTS
# ==============================================================================
# ------------------------------------------------------------------------------
# 3.1: Overall Predicted vs Observed LUE
# ----------------------------------------------------------------------------
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
p_all_lue <- ggplot(data = rf_data, aes(x = LUE, y = LUEpredicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 1.2, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 1.5, color = "black") +
  xlim(0, 1) + ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(
    x = expression("Observed Light Use Efficiency (gC mol"^{-1}~"photon)"),
    y = expression("Predicted Light Use Efficiency (gC mol"^{-1}~"photon)")
  ) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = 0.05, label.y = 0.95, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = 0.05, label.y = 0.90, size = 16) +
  get_metrics_text(rf_data$LUE, rf_data$LUEpredicted, x = 0.05, y = 0.75, size = 16) +
  theme_classic() +
  theme(
    text = element_text(size = 36),
    legend.key.size = unit(1.5, 'cm'),
    axis.line = element_line(size = 1.2)
  )
ggsave(filename = "RFLUEpredictalldata.png",
       plot = p_alllue,
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

p_train_lue <- ggplot(data = train_data, aes(x = LUE, y = LUEpredicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 1.5, color = "black") +
  xlim(0, 1) + ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(
    x = expression("Observed Light Use Efficiency (gC mol"^{-1}~"photon)"),
    y = expression("Predicted Light Use Efficiency (gC mol"^{-1}~"photon)")
  ) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = 0.05, label.y = 0.95, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = 0.05, label.y = 0.90, size = 16) +
  get_metrics_text(train_data$LUE, train_data$LUEpredicted, x = 0.05, y = 0.75, size = 16) +
  theme_classic() +
  theme(
    text = element_text(size = 36),
    legend.key.size = unit(1.5, 'cm'),
    axis.line = element_line(size = 1.2)
  )

ggsave(filename = "RFLUEpredicttrain.png",
       plot = p_train_lue,
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300, width = 24, height = 15, units = "in")

# ---- Plot: Testing Set ----
test_data <- rf_data %>% dplyr::filter(siteyear %in% test_siteyears)
p_test_lue <- ggplot(data = test_data, aes(x = LUE, y = LUEpredicted, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 1.5, color = "black") +
  xlim(0, 1) + ylim(0, 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(
    x = expression("Observed Light Use Efficiency (gC mol"^{-1}~"photon)"),
    y = expression("Predicted Light Use Efficiency (gC mol"^{-1}~"photon)")
  ) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = 0.05, label.y = 0.95, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = 0.05, label.y = 0.90, size = 16) +
  get_metrics_text(test_data$LUE, test_data$LUEpredicted, x = 0.05, y = 0.75, size = 16) +
  theme_classic() +
  theme(
    text = element_text(size = 36),
    legend.key.size = unit(1.5, 'cm'),
    axis.line = element_line(size = 1.2)
  )

ggsave(filename = "RFLUEpredicttest.png",
       plot = p_test_lue,
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300, width = 24, height = 15, units = "in")

plot_annotation(
  theme = theme(plot.margin = grid::unit(c(1, 1, 1, 1), "cm"))
)

# Label each plot
p_all_lab_lue <- p_all_lue + 
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 48, face = "bold"),
        plot.tag.position = c(0.05, 0.95))

p_train_lab_lue <- p_train_lue + 
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 48, face = "bold"),
        plot.tag.position = c(0.05, 0.95))

p_test_lab_lue <- p_test_lue + 
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 48, face = "bold"),
        plot.tag.position = c(0.05, 0.95))

# Create a blank plot for the 4th cell
blank_plot <- ggplot() + theme_void()

# Now combine into a 2x2 layout
combined_layout <- (p_all_lab_lue + p_train_lab_lue) / 
  (p_test_lab_lue + blank_plot) +
  plot_layout(widths = c(1, 1), heights = c(1, 1), guides = "collect") +
  plot_annotation(
    theme = ggplot2::theme(
      plot.margin = ggplot2::margin(1.2, 1, 1, 1, unit = "cm")
    )
  )

# Save the combined plot
ggsave(
  filename = "LUEtestrainall_2x2.png",
  plot = combined_layout,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  dpi = 300,
  width = 30,
  height = 30,
  units = "in",
  limitsize = FALSE
)

# ==============================================================================
# 3. GPP VPM GRAPHS=========================================
# ==============================================================================
# Calculate overall MAE
mae_all_vpm <- mean(abs(rf_data$GPP_site - rf_data$GPPpredictedVPM_EVI), na.rm = TRUE)
print(paste("Overall MAE (VPM):", mae_all_vpm))

# Helper annotation function
get_metrics_text <- function(obs, pred, x, y, size = 16) {
  rmse_val <- rmse(obs, pred)
  mae_val <- mae(obs, pred)
  bias_val <- mean(pred - obs)
  
  txt <- paste0("RMSE = ", round(rmse_val, 2),
                "\nMAE = ", round(mae_val, 2),
                "\nBias = ", round(bias_val, 2))
  
  annotate("text", x = x, y = y, label = txt, size = size, hjust = 0)
}
rf_data$GPPpredictedVPM_EVI<-as.numeric(rf_data$GPPpredictedVPM_EVI)
rf_data$GPP_site <- as.numeric(rf_data$GPP_site)
# GPP VPM Predicted scatter plot
p_vpm <- ggplot(data = rf_data, aes(x = GPP_site, y = GPPpredictedVPM_EVI, col = DAP)) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 10) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  coord_cartesian(xlim = c(-5, 45), ylim = c(-5, 45))+
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +  
  scale_y_continuous(breaks = seq(0, 45, by = 5)) +
  labs(x = bquote('Observed GPP ('*g~ 'C'~ m^-2~day^-1*')'),
       y = bquote('Predicted GPP - VPM ('*g~ 'C'~ m^-2~day^-1*')')) +
  scale_color_viridis(option = "D", direction = -1, limits = c(0, 170)) +
  stat_regline_equation(label.x = 30, label.y = 0, size = 16) +
  stat_cor(aes(label = ..rr.label..), label.x = 30, label.y = 5, size = 16) +
  get_metrics_text(rf_data$GPP_site, rf_data$GPPpredictedVPM_EVI, x = 30, y = 12, size = 16) +
  theme_classic() +
  theme(text = element_text(size = 48),
        legend.key.size = unit(2, 'cm'),
        axis.line = element_line(size = 1.7))
range(rf_data$GPP_site)
range(rf_data$GPPpredictedVPM_EVI)
head(rf_data[order(-rf_data$GPPpredictedVPM_EVI), ], 5)

# Save the plot
ggsave(filename = "GPPVPMpredicted.png",
       plot = p_vpm,
       path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
       dpi = 300, width = 24, height = 15, units = "in")

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

rf_data$GPP_predicted_VI<-as.numeric(rf_data$GPP_predicted_VI)
rf_data$GPP_site <- as.numeric(rf_data$GPP_site)
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


# ==============================================================================
# SECTION 3: BIOPHYSICAL LUE
# ==============================================================================     
# Function to calculate Pearson's R
# Pearson's R
calc_pearson <- function(x, y) {
  cor(x, y, method = "pearson", use = "complete.obs")
}
# R²
calc_r2 <- function(x, y) {
  model <- lm(y ~ x)
  summary(model)$r.squared
}
# Kendall's tau
calc_kendall <- function(x, y) {
  cor(x, y, method = "kendall", use = "complete.obs")
}
# Colors
scatter_colors <- c("LUE" = wes_palette("Darjeeling2")[2],
                    "LUEpredicted" = wes_palette("Chevalier1")[1])

# Axis label
lue_lab <- expression("Light Use Efficiency (gC mol"^{-1}~"photon)")
# Base theme
my_theme <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  )
# Theme for plots without y-axis
theme_no_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

plot_dual <- function(xvar, xlab, show_y = FALSE, show_legend = FALSE) {
  # Verify required columns exist
  required_cols <- c(xvar, "LUE", "LUEpredicted")
  missing_cols <- setdiff(required_cols, names(rf_data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in rf_data:", paste(missing_cols, collapse=", ")))
  }
  
  # Create clean data frame without NAs
  plot_data <- rf_data[complete.cases(rf_data[, required_cols]), ]
  
  # Safe calculation function with error handling
  safe_calc <- function(fun, x, y) {
    tryCatch({
      if (length(x) > 0 && length(y) > 0) {
        round(fun(x, y), 2)
      } else {
        NA
      }
    }, error = function(e) NA)
  }
  
  # Calculate statistics
  tau_lue <- safe_calc(calc_kendall, plot_data[[xvar]], plot_data$LUE)
  r_lue <- safe_calc(calc_pearson, plot_data[[xvar]], plot_data$LUE)
  tau_pred <- safe_calc(calc_kendall, plot_data[[xvar]], plot_data$LUEpredicted)
  r_pred <- safe_calc(calc_pearson, plot_data[[xvar]], plot_data$LUEpredicted)
  
  # Default position parameters
  xpos <- Inf
  ypos_lue <- Inf
  ypos_pred <- Inf
  hjust_val <- 1.1
  vjust_lue <- 3
  vjust_pred <- 1.5
  
  # Custom position logic
  if (xvar %in% c("rH_site", "DBSI", "MLSWI26", "IAVI", "Tair_site")) {
    xpos <- -Inf
    ypos_lue <- Inf
    ypos_pred <- Inf
    hjust_val <- -0.1
  } else if (xvar == "VPD_site") {
    xpos <- 11  # adjust if needed
    ypos_lue <- 0.85
    ypos_pred <- 0.80
    hjust_val <- 0
    vjust_lue <- 0
    vjust_pred <- 0
  } else if (xvar == "Es") {
    xpos <- Inf
    ypos_lue <- Inf
    ypos_pred <- Inf
    hjust_val <- 1.1
  } else if (xvar == "cumulative_gdd") {
    xpos <- Inf
    ypos_lue <- -Inf
    ypos_pred <- -Inf
    hjust_val <- 1.1
    vjust_lue <- -1
    vjust_pred <- -2.5
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = .data[[xvar]])) +
    geom_point(aes(y = LUE, color = "LUE"), alpha = 0.8, na.rm = TRUE) +
    geom_point(aes(y = LUEpredicted, color = "LUEpredicted"), alpha = 0.3, na.rm = TRUE) +
    geom_smooth(aes(y = LUE, color = "LUE"), method = "loess", size = 2, se = TRUE, na.rm = TRUE) +
    geom_smooth(aes(y = LUEpredicted, color = "LUEpredicted"), method = "loess", se = TRUE, na.rm = TRUE) +
    scale_color_manual(values = scatter_colors, 
                       name = NULL,
                       labels = c("Observed LUE", "Predicted LUE")) +
    labs(x = xlab) +
    annotate("text", 
             x = xpos, 
             y = ypos_lue,
             label = paste0("τ = ", tau_lue, ", R = ", r_lue),
             color = scatter_colors["LUE"],
             hjust = hjust_val, 
             vjust = vjust_lue, 
             size = 5,                 # increase size
             fontface = "bold") +      # make bold
    annotate("text", 
             x = xpos, 
             y = ypos_pred,
             label = paste0("τ = ", tau_pred, ", R = ", r_pred),
             color = scatter_colors["LUEpredicted"],
             hjust = hjust_val, 
             vjust = vjust_pred, 
             size = 5, 
             fontface = "bold")
    my_theme
  
  # Add y-axis if requested
  if (show_y) {
    p <- p + ylab(lue_lab)
  } else {
    p <- p + theme_no_y
  }
  
  # Add legend if requested
  if (show_legend) {
    p <- p + theme(
      legend.position = c(0.98, 0.98),
      legend.justification = c(1, 1),
      legend.direction = "vertical",
      legend.box.background = element_rect(fill = "white", color = "gray80", size = 0.3),
      legend.margin = ggplot2::margin(3, 3, 3, 3, unit = "pt"),
      legend.text = element_text(size = 10)
    )
  }
  
  return(p)
}

#----------------------------
# Create & Combine Plots
#----------------------------

class(rf_data$VPD_site)
class(rf_data$rH_site)
class(rf_data$Tair_site)
class(rf_data$LUE_predicted)
rf_data$LUEpredicted <- rf_data$LUE_predicted

p1 <- plot_dual("VPD_site", "VPD (kPa)", show_y = TRUE, show_legend = TRUE) +
  theme_classic() +   # classic clean theme
  theme(
    legend.position = c(0.98, 0.98),         # keep legend in top-right corner
    legend.justification = c(1, 1),
    legend.direction = "vertical",
    legend.box.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    legend.margin = ggplot2::margin(3, 3, 3, 3, unit = "pt"),
    legend.text = element_text(size = 10),
    
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # tick labels
    axis.title = element_text(size = 16)      # bold axis titles
  )
p2 <- plot_dual("rH_site", "Relative Humidity (%)")  +theme_classic() +  
  labs(y = NULL) +    # remove y-axis title # classic theme
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )
p3 <- plot_dual("DBSI", "DBSI") + theme_classic() +
  labs(y = NULL) +    # remove y-axis title# classic theme
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )
p4 <- plot_dual("AWEInsh", "AWEInsh")+  theme_classic() +  
  labs(y = NULL) +    # remove y-axis title# classic theme
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )
p5 <- plot_dual("IAVI", "IAVI", show_y = TRUE) + theme_classic() + 
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )
p6 <- plot_dual("Tair_site", "Air Temperature (°C)")+  
  theme_classic() +   # classic theme
  labs(y = NULL) +    # remove y-axis title
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )
p7 <- plot_dual("Es", "Es") +  theme_classic() + 
  # classic theme
  labs(y = NULL) +
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )
p8 <- plot_dual("cumulative_gdd", "Cumulative GDD (°C)")+  theme_classic() +   # classic theme
  labs(y = NULL) +
  theme(
    legend.position = "none",          # remove legend
    axis.line = element_line(size = 1.5, color = "black"),   # thick black axes
    axis.ticks = element_line(size = 1),                     # thicker ticks
    axis.text = element_text(size = 14, color = "black"),    # axis labels
    axis.title = element_text(size = 16)      # axis titles bold
  )

LUEbiophysical <- ((p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)) +
  plot_annotation(
    tag_levels = 'A',
    theme = theme(
      plot.tag = element_text(size = 12, face = "bold", hjust = 1, vjust = 1)
    )
  )

LUEbiophysical <- ((p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)) +
  plot_annotation(
    tag_levels = 'A',
    tag_prefix = "",
    theme = theme(
      plot.tag = element_text(size = 12, face = "bold"),
      plot.tag.position = c(1, 1)   # top-right corner
    )
  )


#----------------------------
# Save the Plot
#----------------------------
LUEbiophysical
ggsave(
  filename = "LUEbiophysical.png",
  plot = LUEbiophysical,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 17,
  height = 10,
  dpi = 300,
  units = "in"
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

# GPP range
gpp_min <- min(joined_df$GPP_site, na.rm = TRUE)
gpp_max <- max(joined_df$GPP_site, na.rm = TRUE)

# Scale function
scale_to_gpp_range <- function(x, vi_min, vi_max, gpp_min, gpp_max) {
  (x - vi_min) / (vi_max - vi_min) * (gpp_max - gpp_min) + gpp_min
}

# Scale VIs
for (vi in vi_list) {
  vi_min <- min(joined_df[[vi]], na.rm = TRUE)
  vi_max <- max(joined_df[[vi]], na.rm = TRUE)
  joined_df[[paste0(vi, "_scaled")]] <- scale_to_gpp_range(joined_df[[vi]], vi_min, vi_max, gpp_min, gpp_max)
}

# Equation annotations for each VI
vi_equations <- c(
  IAVI   = "IAVI == frac(NIR - (Red - gamma %.% (Blue - Red)), NIR + (Red - gamma %.% (Blue - Red)))",
  VARI   = "VARI == frac(Green - Red, Green + Red - Blue)",
  NDVI   = "NDVI == frac(NIR - Red, NIR + Red)",
  TSAVI  = "TSAVI == frac(sla %.% (NIR - sla %.% Red - slb), sla %.% NIR + Red - sla %.% slb)",
  RNDVI  = "RNDVI == frac(Red - NIR, Red + NIR)",
  kNDVI  = "kNDVI == frac(kNN - kNR, kNN + kNR)",
  EVI    = "EVI == g %.% frac(NIR - Red, NIR + C[1] %.% R - C[2] %.% B + L)",
  ATSAVI = "ATSAVI == frac(1 - NIR - SWIR[1], 1 - NIR + SWIR[1])"
)

# Plot function
plot_dual_axis_vi_gpp <- function(data, vi_col, gpp_col = "GPP_site", 
                                  color_vi = NULL, color_gpp = NULL,
                                  equation_label = NULL) {
  palette_colors <- wesanderson::wes_palette("Cavalcanti1")
  if (is.null(color_vi)) color_vi <- palette_colors[1]
  if (is.null(color_gpp)) color_gpp <- palette_colors[2]
  
  cor_val <- cor(data[[vi_col]], data[[gpp_col]], use = "complete.obs")
  r_squared <- round(cor_val^2, 2)
  
  vi_range <- range(data[[vi_col]], na.rm = TRUE)
  gpp_range <- range(data[[gpp_col]], na.rm = TRUE)
  
  # Handle cases where vi_range is collapsed (e.g., all NA or all same value)
  # or if it's not finite (e.g., if a VI column is all NAs)
  if (!is.finite(diff(vi_range)) || diff(vi_range) == 0) {
    warning(paste("VI range is non-finite or zero for", vi_col, ". Cannot scale and plot."))
    return(ggplot() + labs(title = paste("Cannot plot for", vi_col, ": VI data invalid.")))
  }
  
  scale_factor <- diff(gpp_range) / diff(vi_range)
  shift_factor <- gpp_range[1] - vi_range[1] * scale_factor
  vi_to_gpp <- function(x) { x * scale_factor + shift_factor }
  gpp_to_vi <- function(x) { (x - shift_factor) / scale_factor }
  
  # Ensure vi_breaks are valid even if vi_range is very small or all NA
  vi_breaks <- pretty(vi_range, n = 5)
  vi_breaks <- vi_breaks[vi_breaks >= vi_range[1] & vi_breaks <= vi_range[2]] # Filter to actual range
  
  p <- ggplot(data, aes(x = DAP)) +
    geom_point(aes(y = !!sym(gpp_col)), color = color_gpp, size = 1.2, alpha = 0.7) +
    geom_point(aes(y = vi_to_gpp(!!sym(vi_col))), color = color_vi, size = 1.2, alpha = 0.7) +
    scale_y_continuous(
      name = "GPP (gC m⁻² day⁻¹)",
      limits = gpp_range,
      sec.axis = sec_axis(
        trans = gpp_to_vi,
        name = "Vegetation Index scaled to 0–1",
        breaks = vi_breaks
      )
    ) +
    labs(
      title = bquote(.(vi_col) ~ "(" * italic(R)^2 ~ "=" ~  .(r_squared) * ")"),
      x = "Days after Planting"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      # Explicitly calling ggplot2::margin to avoid potential conflicts
      axis.title.y.left = element_text(color = color_gpp, face = "bold", margin = ggplot2::margin(r = 10, unit = "pt")),
      axis.text.y.left = element_text(color = color_gpp),
      axis.title.y.right = element_text(color = color_vi, face = "bold", margin = ggplot2::margin(l = 10, unit = "pt")),
      axis.text.y.right = element_text(color = color_vi),
      plot.title = element_text(hjust = 0.5, size = 12)
    )
  
  if (!is.null(equation_label)) {
    # Set x-coordinate based on vi_col
    eq_x_pos <- if (vi_col == "RNDVI") 125 else 5 
    
    p <- p + annotate("text", x = eq_x_pos, y = 22, # Fixed y-position at 25
                      label = equation_label,
                      parse = TRUE,
                      hjust = 0, # Left-align text at the x-coordinate
                      vjust = 0, # Adjust vjust based on desired y-coordinate behavior
                      size = 2, # Adjusted size
                      color = "gray30")
  }
  
  return(p)
}

# Generate plot list
plot_list <- lapply(vi_list, function(vi) {
  plot_dual_axis_vi_gpp(
    joined_df,
    vi,
    equation_label = vi_equations[[vi]]
  )
})

# Filter out any NULL plots if a VI had a collapsed or invalid range
plot_list <- plot_list[!sapply(plot_list, is.null)]


# Clean axes for grid layout
# Only proceed if plot_list is not empty
if (length(plot_list) > 0) {
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
}


# Final combined grid layout
# Check if plot_list is not empty before arranging
if (length(plot_list) > 0) {
  final_plot <- grid.arrange(
    grobs = plot_list,
    ncol = 4
  )
  print(final_plot) # Display the final plot
} else {
  message("No plots were generated. Check your data and VI ranges.")
}

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
# 3. GPP VI GRAPHS (MODIFIED: Equation in Title)
# ==============================================================================
# print(top_20_lowest_train_mae)
# List of vegetation indices
vi_list <- c("IAVI", "VARI", "NDVI", "TSAVI", "RNDVI", "kNDVI", "EVI","ATSAVI")

# Assuming 'joined_df' exists and packages are loaded (e.g., library(ggplot2), 
# library(dplyr), library(gridExtra), library(wesanderson), library(stringr))

# Ensure numeric (as in your original code)
# joined_df <- joined_df %>%
#    dplyr::filter(!str_detect(siteyear, "2015$|2016$|2017$"))
joined_df[vi_list] <- lapply(joined_df[vi_list], as.numeric)
joined_df$GPP_site <- as.numeric(joined_df$GPP_site)

# GPP range
gpp_min <- min(joined_df$GPP_site, na.rm = TRUE)
gpp_max <- max(joined_df$GPP_site, na.rm = TRUE)

# Scale function
scale_to_gpp_range <- function(x, vi_min, vi_max, gpp_min, gpp_max) {
  (x - vi_min) / (vi_max - vi_min) * (gpp_max - gpp_min) + gpp_min
}

# Scale VIs (as in original code, though the plot function does dynamic scaling too)
for (vi in vi_list) {
  vi_min <- min(joined_df[[vi]], na.rm = TRUE)
  vi_max <- max(joined_df[[vi]], na.rm = TRUE)
  joined_df[[paste0(vi, "_scaled")]] <- scale_to_gpp_range(joined_df[[vi]], vi_min, vi_max, gpp_min, gpp_max)
}

# Equation annotations for each VI - These strings will now be used in the title
vi_equations <- c(
  IAVI   = "IAVI == frac(NIR - (Red - gamma %.% (Blue - Red)), NIR + (Red - gamma %.% (Blue - Red)))",
  VARI   = "VARI == frac(Green - Red, Green + Red - Blue)",
  NDVI   = "NDVI == frac(NIR - Red, NIR + Red)",
  TSAVI  = "TSAVI == frac(sla %.% (NIR - sla %.% Red - slb), sla %.% NIR + Red - sla %.% slb)",
  RNDVI  = "RNDVI == frac(Red - NIR, Red + NIR)",
  kNDVI  = "kNDVI == frac(kNN - kNR, kNN + kNR)",
  EVI    = "EVI == g %.% frac(NIR - Red, NIR + C[1] %.% R - C[2] %.% B + L)",
  ATSAVI = "ATSAVI == frac(1 - NIR - SWIR[1], 1 - NIR + SWIR[1])"
)

# Plot function - MODIFIED to place equation in the title and use string parsing
plot_dual_axis_vi_gpp <- function(data, vi_col, gpp_col = "GPP_site", 
                                  color_vi = NULL, color_gpp = NULL) { 
  # Note: equation_label parameter removed. Accessing vi_equations globally.
  
  palette_colors <- wesanderson::wes_palette("Cavalcanti1")
  if (is.null(color_vi)) color_vi <- palette_colors[1]
  if (is.null(color_gpp)) color_gpp <- palette_colors[2]
  
  cor_val <- cor(data[[vi_col]], data[[gpp_col]], use = "complete.obs")
  r_squared <- round(cor_val^2, 2)
  
  vi_range <- range(data[[vi_col]], na.rm = TRUE)
  gpp_range <- range(data[[gpp_col]], na.rm = TRUE)
  
  # Handle cases where vi_range is collapsed
  if (!is.finite(diff(vi_range)) || diff(vi_range) == 0) {
    warning(paste("VI range is non-finite or zero for", vi_col, ". Cannot scale and plot."))
    return(ggplot() + labs(title = paste("Cannot plot for", vi_col, ": VI data invalid.")))
  }
  
  scale_factor <- diff(gpp_range) / diff(vi_range)
  shift_factor <- gpp_range[1] - vi_range[1] * scale_factor
  vi_to_gpp <- function(x) { x * scale_factor + shift_factor }
  gpp_to_vi <- function(x) { (x - shift_factor) / scale_factor }
  
  vi_breaks <- pretty(vi_range, n = 5)
  vi_breaks <- vi_breaks[vi_breaks >= vi_range[1] & vi_breaks <= vi_range[2]] # Filter to actual range
  
  # --- START MODIFICATION FOR TITLE ---
  # 1. Get the full equation string
  full_equation <- vi_equations[[vi_col]]
  
  # 2. Extract only the formula part (the right side of '==')
  # This prevents redundancy (e.g., IAVI (R^2) IAVI == ...), but keeps the plotmath
  # expressions like C[1] intact.
  formula_only <- gsub(paste0("^", vi_col, "\\s*==\\s*"), "", full_equation)
  
  # 3. Create the combined title string for plotmath parsing: VI Name (R^2 = value) (Formula)
  # FIX: Replaced outer square brackets '[' and ']' with parentheses '(' and ')'
  # to avoid plotmath parsing conflicts.
  title_string <- paste0(
    vi_col, 
    "~~(", "italic(R)^2", "==", r_squared, ")~(", 
    formula_only, 
    ")"
  )
  # --- END MODIFICATION FOR TITLE ---
  
  p <- ggplot(data, aes(x = DAP)) +
    geom_point(aes(y = !!sym(gpp_col)), color = color_gpp, size = 1.2, alpha = 0.7) +
    geom_point(aes(y = vi_to_gpp(!!sym(vi_col))), color = color_vi, size = 1.2, alpha = 0.7) +
    scale_y_continuous(
      name = "GPP (gC m⁻² day⁻¹)",
      limits = gpp_range,
      sec.axis = sec_axis(
        trans = gpp_to_vi,
        name = "Vegetation Index", # Removed "scaled to 0-1" as it's implied by the axis logic
        breaks = vi_breaks
      )
    ) +
    labs(
      # FIX: Use parse(text = title_string) to force plotmath interpretation
      title = parse(text = title_string), 
      x = "Days after Planting"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      # Explicitly calling ggplot2::margin to avoid potential conflicts
      axis.title.y.left = element_text(color = color_gpp, face = "bold", margin = ggplot2::margin(r = 10, unit = "pt")),
      axis.text.y.left = element_text(color = color_gpp),
      axis.title.y.right = element_text(color = color_vi, face = "bold", margin = ggplot2::margin(l = 10, unit = "pt")),
      axis.text.y.right = element_text(color = color_vi),
      # Reduced size for the title so the equation fits in the grid panel
      plot.title = element_text(
        hjust = 0.5, 
        size = 12, 
        face = "bold"
        # FIX: Removed the invalid 'parse = TRUE' argument from element_text()
      ) 
    )
  
  # REMOVED: The old annotation block since the equation is now in the title
  
  return(p)
}

# Generate plot list
# The lapply call is simplified as it no longer needs to pass equation_label
plot_list <- lapply(vi_list, function(vi) {
  plot_dual_axis_vi_gpp(joined_df, vi)
})

# Filter out any NULL plots if a VI had a collapsed or invalid range
plot_list <- plot_list[!sapply(plot_list, is.null)]


# Clean axes for grid layout
# Only proceed if plot_list is not empty
if (length(plot_list) > 0) {
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
}


# Final combined grid layout
# Check if plot_list is not empty before arranging
if (length(plot_list) > 0) {
  final_plot <- grid.arrange(
    grobs = plot_list,
    ncol = 4
  )
  print(final_plot) # Display the final plot
} else {
  message("No plots were generated. Check your data and VI ranges.")
}

final_plot
# Save the plot
ggsave(
  filename = "GPPVI_dual_axis_title_eq.png",
  plot = final_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 18,
  height = 9,
  dpi = 200,
  units = "in"
)

#===================================================
#LAI
#===================================================
#### ============================ ####
#### 1. Ground LAI from Excel File ####
#### ============================ ####
dev.off()
graphics.off()
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
plot_lai_year <- function(df, year, show_x_axis = TRUE) {
  df_long <- df %>%
    pivot_longer(cols = c("Way 3", "Way 4"), names_to = "Site", values_to = "LAI") %>%
    mutate(Site = recode(Site, "Way 3" = "US-HRC", "Way 4" = "US-HRA"))
  
  start_datetime <- as.POSIXct(paste0(year, "-04-01 00:00:00"), tz = "UTC")
  end_datetime <- as.POSIXct(paste0(year, "-09-30 23:59:59"), tz = "UTC")
  
  ggplot(df_long, aes(x = Date, y = LAI, color = Site)) +
    geom_line(size = 1) +
    scale_x_datetime(
      limits = c(start_datetime, end_datetime),
      date_labels = "%b",
      date_breaks = "1 month",
      name = "Time"  # ensures x-axis title
    ) +
    scale_y_continuous(limits = c(0, 6), breaks = 0:6) +
    scale_color_manual(values = c("US-HRC" = "blue", "US-HRA" = "purple")) +
    labs(
      title = paste("Ground-collected LAI over Time for", year),
      y = expression("LAI (m"^2~"m"^-2*")"),
      color = "Site"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(color = "black", size = ifelse(show_x_axis, 12, 0)),
      axis.title.x = element_text(color = "black", size = ifelse(show_x_axis, 14, 0)),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title.y = element_text(color = "black", size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12)
    )
}
plot_lai_year <- function(df, year, show_x_axis = TRUE) {
  # Data preparation: pivot and clean up site names/dates
  df_long <- df %>%
    pivot_longer(cols = c("Way 3", "Way 4"), names_to = "Site", values_to = "LAI") %>%
    mutate(
      Site = recode(Site, "Way 3" = "US-HRC", "Way 4" = "US-HRA"),
      # Ensure Date is POSIXct for scale_x_datetime
      Time = as.POSIXct(Date, tz = "UTC")
    )
  
  # Define the plotting period (April 1st to September 30th)
  start_datetime <- as.POSIXct(paste0(year, "-04-01 00:00:00"), tz = "UTC")
  end_datetime <- as.POSIXct(paste0(year, "-09-30 23:59:59"), tz = "UTC")
  
  ggplot(df_long, aes(x = Time, y = LAI, color = Site)) +
    geom_line(size = 1) +
    scale_x_datetime(
      limits = c(start_datetime, end_datetime),
      date_labels = "%b", # Display month abbreviations
      date_breaks = "1 month"
    ) +
    scale_y_continuous(limits = c(0, 6), breaks = 0:6) +
    scale_color_manual(values = c("US-HRC" = "blue", "US-HRA" = "purple")) +
    labs(
      title = paste("Ground-collected LAI over Time for", year),
      # **CORRECTION HERE: The label is now unconditionally set to "Time"**
      x = "Time",
      y = expression("LAI (m"^2~"m"^-2*")"),
      color = "Site"
    ) +
    theme_minimal() +
    theme(
      # The visibility of the label is now solely controlled by the size here:
      axis.text.x = element_text(color = "black", size = ifelse(show_x_axis, 12, 0)),
      axis.title.x = element_text(color = "black", size = ifelse(show_x_axis, 14, 0)),
      axis.text.y = element_text(color = "black", size = 12),
      axis.title.y = element_text(color = "black", size = 14),
      plot.title = element_text(size = 16, face = "bold"),
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 20)
    )
}


# Generate plots with shared x-axis only at the bottom
# Extract legend from one plot
shared_legend <- get_legend(plot_2017)  # Use the plot that has x-axis shown

# Remove legends from all plots
plot_2015_nolegend <- plot_2015 + theme(legend.position = "none")
plot_2016_nolegend <- plot_2016 + theme(legend.position = "none")
plot_2017_nolegend <- plot_2017 + theme(legend.position = "none")

# Extract legend from one plot
shared_legend <- get_legend(
  plot_2017 + theme(
    legend.position = "right",
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20)
  )
)

# Combine plots vertically
combined_plots <- plot_grid(
  plot_2015_nolegend,
  plot_2016_nolegend,
  plot_2017_nolegend,
  ncol = 1,
  align = 'v'
)

# Add shared legend to the right
final_plot <- plot_grid(
  combined_plots, shared_legend,
  ncol = 2,
  rel_widths = c(3, 0.4)  # adjust width ratio for legend
)

# Save figure
ggsave(
  filename = "combined_ground_plot.png",
  plot = final_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 12,
  height = 10,
  dpi = 300
)

#### ===================================== ####
#### 2. Ungapfilled Satellite LAI (2015–2018) ####
#### ===================================== ####
# Extract site names from list (first 10 characters of each name)
site_names_ungap <- substr(names(meteo_df_2015_2018ni), 1, 10)
site_names_gap <- substr(names(meteo_df_2015_2018), 1, 10)

# Function to create a combined LAI plot for one site
create_combined_lai_plot <- function(df_ungap, df_gap, site_name) {
  # Filter both datasets to April–September
  df_ungap <- df_ungap[format(df_ungap$Date, "%m") >= "04" & format(df_ungap$Date, "%m") <= "09", ]
  df_gap   <- df_gap[format(df_gap$Date, "%m") >= "04" & format(df_gap$Date, "%m") <= "09", ]
  # Add source identifiers
  df_ungap$Source <- "Ungapfilled"
  df_gap$Source   <- "Gapfilled"
  # Plot with gapfilled first, then ungapfilled (so it appears on top)
  ggplot() +
    geom_point(data = df_gap, aes(x = Date, y = Lai, color = Source), 
               size = 1.2, alpha = 0.7, shape = 16) +  # Gapfilled: circle
    geom_point(data = df_ungap, aes(x = Date, y = Lai, color = Source), 
               size = 2.5, alpha = 0.9, shape = 17) +  # Ungapfilled: triangle, larger
    scale_color_manual(values = c("Ungapfilled" = "darkgreen", "Gapfilled" = "orange")) +
    scale_x_datetime(date_labels = "%b", breaks = "1 month") +
    labs(
      title = paste("Site:", site_name),
      x = "Date",
      y = expression("LAI (m"^2~"m"^-2*")")
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, face = "bold", color = "black"),
      axis.title.x = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 12, color = "black"),
      axis.text = element_text(size = 10, color = "black"),
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

# Site names
site_names <- substr(names(meteo_df_2015_2018ni), 1, 10)

# Generate combined plots for all sites
plot_list_combined <- lapply(1:16, function(i) {
  create_combined_lai_plot(
    meteo_df_2015_2018ni[[i]],     # Ungapfilled
    meteo_df_2015_2018nisg[[i]],   # Gapfilled
    site_names[i]
  )
})

# Combine all plots into a grid layout
combined_plot <- wrap_plots(plotlist = plot_list_combined, ncol = 4)

# Save the final figure
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/LAI_combined_16sites_overlayed_gap_ungap.png",
  plot = combined_plot,
  width = 16, height = 12, dpi = 300
)

#### =================================== ####
#### 3. SG fixed Satellite LAI (2015–2018) ####
#### =================================== ####
# Extract site names from gapfilled data
site_names_gap <- substr(names(meteo_df_2015_2018), 1, 10)

# Generate LAI plots for gapfilled data
plot_list_gap <- lapply(1:16, function(i) {
  df <- meteo_df_2015_2018[[i]]
  site_name <- site_names_gap[i]
  
  # Filter dates between April and September (all years)
  df <- df[format(df$Date, "%m") >= "04" & format(df$Date, "%m") <= "09", ]
  
  ggplot(df, aes(x = Date, y = Lai)) +
    geom_point(color = "darkgreen", size = 1) +
    scale_x_datetime(
      date_labels = "%b",            # Show abbreviated month
      date_breaks = "1 month",
      limits = as.POSIXct(c(paste0(format(min(df$Date), "%Y"), "-04-01"),
                            paste0(format(min(df$Date), "%Y"), "-09-30")))
    ) +
    labs(
      title = paste("Site:", site_name),
      x = "Time",
      y = expression("LAI (m"^2~"m"^-2*")")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold", color = "black"),
      axis.title.x = element_text(size = 10, color = "black"),
      axis.title.y = element_text(size = 10, color = "black"),
      axis.text = element_text(size = 8, color = "black")
    )
})

# Combine and save gapfilled plots
combined_gap_plot <- wrap_plots(plotlist = plot_list_gap, ncol = 4)
output_path_gap <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LAI_data/LAI_16sites_gridgapfilled.png"
ggsave(filename = output_path_gap, plot = combined_gap_plot, width = 16, height = 12, dpi = 300)
combined_gap_plot


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

library(ggplot2)
library(patchwork)
library(gtable)

# Function for one site without legend
create_combined_lai_plot3_nolegend <- function(df_ungap, df_gap, df_corr, site_name) {
  df_ungap <- df_ungap[format(df_ungap$Date, "%m") >= "04" & format(df_ungap$Date, "%m") <= "09", c("Date", "Lai")]
  df_gap   <- df_gap[format(df_gap$Date, "%m") >= "04" & format(df_gap$Date, "%m") <= "09", c("Date", "Lai")]
  df_corr  <- df_corr[format(df_corr$Date, "%m") >= "04" & format(df_corr$Date, "%m") <= "09", c("Date", "Lai")]
  
  df_ungap$Source <- "Ungapfilled"
  df_gap$Source   <- "Gapfilled"
  df_corr$Source  <- "Gapfilled+Corrected"
  
  df_all <- rbind(df_ungap, df_gap, df_corr)
  
  ggplot(df_all, aes(x = Date, y = Lai, color = Source, shape = Source, size = Source)) +
    geom_point(alpha = 0.8) +
    scale_color_manual(values = c("Ungapfilled" = "darkgreen",
                                  "Gapfilled" = "orange",
                                  "Gapfilled+Corrected" = "blue")) +
    scale_shape_manual(values = c("Ungapfilled" = 17, "Gapfilled" = 16, "Gapfilled+Corrected" = 15)) +
    scale_size_manual(values = c("Ungapfilled" = 2.5, "Gapfilled" = 1.5, "Gapfilled+Corrected" = 2)) +
    scale_x_datetime(date_labels = "%b", breaks = "1 month") +
    labs(title = paste("Site:", site_name),
         x = "Date", y = expression("LAI (m"^2~"m"^-2*")")) +
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "none"
    )
}

# Create all 16 plots without legends
plot_list_combined3 <- lapply(1:16, function(i) {
  create_combined_lai_plot3_nolegend(
    meteo_df_2015_2018ni[[i]],
    meteo_df_2015_2018nisg[[i]],
    meteo_df_2015_2018[[i]],
    site_names[i]
  )
})

# Dummy plot to create legend
dummy_df <- rbind(
  data.frame(Date = Sys.Date(), Lai = 1, Source = "Ungapfilled"),
  data.frame(Date = Sys.Date(), Lai = 1, Source = "Gapfilled"),
  data.frame(Date = Sys.Date(), Lai = 1, Source = "Gapfilled+Corrected")
)

legend_plot <- ggplot(dummy_df, aes(x = Date, y = Lai, color = Source, shape = Source, size = Source)) +
  geom_point() +
  scale_color_manual(values = c("Ungapfilled" = "darkgreen",
                                "Gapfilled" = "orange",
                                "Gapfilled+Corrected" = "blue")) +
  scale_shape_manual(values = c("Ungapfilled" = 17, "Gapfilled" = 16, "Gapfilled+Corrected" = 15)) +
  scale_size_manual(values = c("Ungapfilled" = 2.5, "Gapfilled" = 1.5, "Gapfilled+Corrected" = 2)) +
  guides(
    color = guide_legend(override.aes = list(size = 12)),   # increase symbol size in legend
    shape = guide_legend(override.aes = list(size = 12)),
    size = "none"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 13)  # legend text size
  )

# Extract legend
get_legend <- function(mygg) {
  tmp <- ggplotGrob(mygg)
  leg <- gtable::gtable_filter(tmp, "guide-box", trim=TRUE)
  return(leg)
}
shared_legend <- get_legend(legend_plot)

# Combine 16 plots + shared legend
combined_plot3 <- wrap_plots(plotlist = plot_list_combined3, ncol = 4) /
  shared_legend + plot_layout(heights = c(10, 1))

# Save figure
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/LAI_16sites_onelegend_big.png",
  plot = combined_plot3,
  width = 16, height = 12, dpi = 300
)

# ==============================================================================
# 6. PLOT VARIABLE IMPORTANCE ==================================================
# ==============================================================================
# This can be derived from the 78 runs
# Start from your mean_importance table
importance_combined <- mean_importance %>%
  rename(
    `%IncMSE` = Mean_IncMSE,
    Gini = Mean_IncNodePurity
  ) %>%
  mutate(Total = `%IncMSE` + Gini)

# Convert to long format
importance_long <- importance_combined %>%
  select(Variable, `%IncMSE`, Gini, Total) %>%
  pivot_longer(cols = c(`%IncMSE`, Gini), names_to = "Metric", values_to = "Value") %>%
  mutate(Variable = reorder(Variable, Total))

# Plot: absolute stacked values
p_total <- ggplot(importance_long, aes(x = Value, y = Variable, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(
    values = c("%IncMSE" = "#3C5488FF", "Gini" = "#00A087FF"),
    name = "Importance Metric"
  ) +
  labs(
    title = "Variable Importance (Combined %IncMSE and Gini)",
    x = "Total Importance",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

# Save the plot
ggsave(
  filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/VariableImportance_TotalStacked.png",
  plot = p_total,
  width = 10,
  height = 8,
  dpi = 300
)




# ==============================================================================
# 6. Daylength ==================================================
# ==============================================================================
rf_data$dayl_hour <- rf_data$dayl / 3600
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


# ==============================================================================
# 6. Compute cumulative summaries ==================================================
# ==============================================================================
rf_data <- rf_data %>%
  mutate(ET = Es + Ei + Ec)  # Sum across Es, Ei, and Ec for each row

# Compute ET and summarize by site-year
rf_summary <- rf_data %>%
  mutate(Es= Es) %>%
  group_by(siteyear) %>%
  summarise(
    Mean_VPD = mean(VPD_site, na.rm = TRUE),
    Total_temperature = sum(Tair_site, na.rm = TRUE),
    total_GPP_site = sum(GPP_site, na.rm = TRUE),
    total_GPP_predicted = sum(GPP_predicted, na.rm = TRUE),
    total_GPP_predicted_VI = sum(GPP_predicted_VI, na.rm = TRUE),
    total_GPPpredictedVPM_EVI = sum(GPPpredictedVPM_EVI, na.rm = TRUE),
    total_dayl_hour = sum(dayl_hour, na.rm = TRUE),
    total_Es = sum(Es, na.rm = TRUE),
    DOP = mean(DOP, na.rm = TRUE),
    Variety = first(Variety)
  )

# Create ET and temperature levels based on quantiles
rf_summary <- rf_summary %>%
  mutate(
    ET_level = cut(total_Es,
                   breaks = quantile(total_Es, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   labels = c("Low ET", "Medium ET", "High ET"),
                   include.lowest = TRUE),
    Temperature_level = cut(Total_temperature,
                            breaks = quantile(Total_temperature, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                            labels = c("Low T", "Medium T", "High T"),
                            include.lowest = TRUE)
  )


# Convert wide GPP data into long format for unified plotting
rf_long <- rf_summary %>%
  select(siteyear, total_dayl_hour, DOP, Variety, Mean_VPD,
         total_GPP_site, total_GPP_predicted, total_GPP_predicted_VI, total_GPPpredictedVPM_EVI) %>%
  pivot_longer(
    cols = starts_with("total_GPP"),
    names_to = "GPP_type",
    values_to = "GPP_value"
  )

# Filter and recategorize
rf_long_filtered <- rf_long %>%
  filter(GPP_type %in% c("total_GPP_site", "total_GPP_predicted", "total_GPP_predicted_VI")) %>%
  mutate(
    DOP_category = case_when(
      DOP <= quantile(DOP, 1/3, na.rm = TRUE) ~ "Early",
      DOP <= quantile(DOP, 2/3, na.rm = TRUE) ~ "Mid",
      TRUE ~ "Late"
    ),
    DOP_category = factor(DOP_category, levels = c("Early", "Mid", "Late"))
  )

# Compute R values and assign manual label positions
r_stats <- rf_long_filtered %>%
  group_by(GPP_type) %>%
  summarise(
    R = cor(GPP_value, predict(gam(GPP_value ~ s(total_dayl_hour))), use = "complete.obs")
  ) %>%
  mutate(
    label = case_when(
      GPP_type == "total_GPP_site" ~ paste0("GPP[EC]~(R==", round(R, 2), ")"),
      GPP_type == "total_GPP_predicted" ~ paste0("GPP[LUERF]~(R==", round(R, 2), ")"),
      GPP_type == "total_GPP_predicted_VI" ~ paste0("GPP[VI]~(R==", round(R, 2), ")")
    ),
    x = c(1550, 1550, 1550),   # manual placement for labels (adjust if needed)
    y = c(1750, 1650, 1550)
  )

# Final plot
ggplot(rf_long_filtered, aes(x = total_dayl_hour, y = GPP_value)) +
  geom_point(
    aes(color = GPP_type, shape = DOP_category, size = Mean_VPD),
    alpha = 0.9
  ) +
  geom_smooth(
    aes(color = GPP_type),
    method = "gam",
    formula = y ~ s(x),
    se = FALSE,
    size = 1.1
  ) +
  geom_text(
    data = r_stats,
    aes(x = x, y = y, label = label, color = GPP_type),
    parse = TRUE,
    hjust = 0,
    vjust = 1,
    inherit.aes = FALSE,
    size = 5,
    show.legend = FALSE  # <-- This prevents the "a" from appearing
  ) +
  scale_color_manual(
    values = c(
      "total_GPP_site" = "#1b9e77",
      "total_GPP_predicted" = "#d95f02",
      "total_GPP_predicted_VI" = "#7570b3"
    ),
    labels = c(
      "total_GPP_site" = expression(GPP[EC]),
      "total_GPP_predicted" = expression(GPP[LUERF]),
      "total_GPP_predicted_VI" = expression(GPP[VI])
    ),
    name = "GPP Type"
  ) +
  scale_shape_manual(
    values = c("Early" = 16, "Mid" = 17, "Late" = 15),
    name = "Planting Group"
  ) +
  scale_size_continuous(
    range = c(2, 12),
    name = "Mean VPD"
  ) +
  guides(
    shape = guide_legend(override.aes = list(size = 6)),
    size = guide_legend(override.aes = list(shape = 16))
  ) +
  labs(
    x = "Cumulative Seasonal Daylength (hours)",
    y = expression("Cumulative Seasonal GPP (g C"~m^{-2}~"season"^{-1}*")")
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(2, "lines")
  )


# Save the plot
ggsave(
  filename = "GPP_Variety_Plot.png",
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  dpi = 300,
  width = 12,    # width in inches (adjust as needed)
  height = 6,    # height in inches (adjust as needed)
  units = "in"
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




#======================================================
# Temporal patterns of the model
#======================================================
# Step 1: Prepare long-format data for plotting
plot_data <- rf_data %>%
  dplyr::select(DAP, siteyear, GPP_predicted, GPP_site, IAVI, VARI, GDVI,
                GPPpredictedVPM_EVI, GPP_predicted_VI, cumulative_gdd) %>%
  pivot_longer(cols = -c(DAP, siteyear, cumulative_gdd), 
               names_to = "variable", 
               values_to = "value") %>%
  dplyr::mutate(alpha_val = ifelse(variable == "GPP_site", 1, 0.9))

# print(plot_data, n = 10) # For checking

# Step 2: Prepare data for metrics calculation
metrics_input_data <- plot_data %>%
  dplyr::filter(variable %in% c("GPP_site", "GPP_predicted", "GPPpredictedVPM_EVI", "GPP_predicted_VI")) %>%
  dplyr::mutate(value = as.numeric(value)) %>% 
  pivot_wider(
    id_cols = c(DAP, siteyear,cumulative_gdd), 
    names_from = variable,
    values_from = value,
    values_fn = list(value = function(x) if(all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)) 
  )

# Calculate metrics per siteyear (R2, MAE, Bias)
metrics_data <- metrics_input_data %>%
  dplyr::group_by(siteyear) %>%
  dplyr::summarise(
    R2_GPP_predicted = if (sum(!is.na(GPP_site) & !is.na(GPP_predicted)) > 1) {
      tryCatch(cor(GPP_site, GPP_predicted, use = "complete.obs")^2, error = function(e) NA_real_)
    } else { NA_real_ },
    MAE_GPP_predicted = if (sum(!is.na(GPP_site) & !is.na(GPP_predicted)) > 1) {
      tryCatch(mae(GPP_site[!is.na(GPP_site) & !is.na(GPP_predicted)], GPP_predicted[!is.na(GPP_site) & !is.na(GPP_predicted)]), error = function(e) NA_real_)
    } else { NA_real_ },
    Bias_GPP_predicted = if (sum(!is.na(GPP_site) & !is.na(GPP_predicted)) > 1) {
      tryCatch(bias(actual = GPP_site[!is.na(GPP_site) & !is.na(GPP_predicted)], predicted = GPP_predicted[!is.na(GPP_site) & !is.na(GPP_predicted)]), error = function(e) NA_real_)
    } else { NA_real_ },
    
    R2_GPPpredictedVPM_EVI = if (sum(!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)) > 1) {
      tryCatch(cor(GPP_site, GPPpredictedVPM_EVI, use = "complete.obs")^2, error = function(e) NA_real_)
    } else { NA_real_ },
    MAE_GPPpredictedVPM_EVI = if (sum(!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)) > 1) {
      tryCatch(mae(GPP_site[!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)], GPPpredictedVPM_EVI[!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)]), error = function(e) NA_real_)
    } else { NA_real_ },
    Bias_GPPpredictedVPM_EVI = if (sum(!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)) > 1) {
      tryCatch(bias(actual = GPP_site[!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)], predicted = GPPpredictedVPM_EVI[!is.na(GPP_site) & !is.na(GPPpredictedVPM_EVI)]), error = function(e) NA_real_)
    } else { NA_real_ },
    
    R2_GPP_predicted_VI = if (sum(!is.na(GPP_site) & !is.na(GPP_predicted_VI)) > 1) {
      tryCatch(cor(GPP_site, GPP_predicted_VI, use = "complete.obs")^2, error = function(e) NA_real_)
    } else { NA_real_ },
    MAE_GPP_predicted_VI = if (sum(!is.na(GPP_site) & !is.na(GPP_predicted_VI)) > 1) {
      tryCatch(mae(GPP_site[!is.na(GPP_site) & !is.na(GPP_predicted_VI)], GPP_predicted_VI[!is.na(GPP_site) & !is.na(GPP_predicted_VI)]), error = function(e) NA_real_)
    } else { NA_real_ },
    Bias_GPP_predicted_VI = if (sum(!is.na(GPP_site) & !is.na(GPP_predicted_VI)) > 1) {
      tryCatch(bias(actual = GPP_site[!is.na(GPP_site) & !is.na(GPP_predicted_VI)], predicted = GPP_predicted_VI[!is.na(GPP_site) & !is.na(GPP_predicted_VI)]), error = function(e) NA_real_)
    } else { NA_real_ },
    .groups = "drop"
  )

# Step 3: Prepare metrics for plotting and define plot boundaries

# Define fixed plot parameters
min_cumulative_gdd_limit <- 0 # Lower x-axis limit for the plot

# Define model names for ordering and iteration
model_identifiers <- c("GPP_predicted", "GPPpredictedVPM_EVI", "GPP_predicted_VI")

# Determine overall data ranges from the original plot_data (for y-axis scaling of text)
overall_max_gpp <- if (any(!is.na(plot_data$value))) max(plot_data$value, na.rm = TRUE) else 10
overall_min_gpp <- if (any(!is.na(plot_data$value))) min(plot_data$value, na.rm = TRUE) else 0
overall_gpp_range <- if (overall_max_gpp != overall_min_gpp) overall_max_gpp - overall_min_gpp else 1

# Define colors (used for filtering data for plot range and for plot aesthetics)
variable_colors <- c(
  "GPP_site" =   wesanderson::wes_palette("Zissou1")[3], 
  "GPP_predicted" = wesanderson::wes_palette("Zissou1")[1],                             
  "GPPpredictedVPM_EVI" =wesanderson::wes_palette("IsleofDogs1")[1],
  "GPP_predicted_VI" = wesanderson::wes_palette("Zissou1")[5]                       
)

# Filter data that will actually be plotted to determine data-driven x-axis max
plot_data_for_main_geoms <- plot_data %>%
  dplyr::filter(variable %in% names(variable_colors))

# Determine the maximum cumulative_gdd value that will be shown on the plot from the filtered data
data_driven_max_cumulative_gdd <- if (any(!is.na(plot_data_for_main_geoms$cumulative_gdd))) {
  max(plot_data_for_main_geoms$cumulative_gdd, na.rm = TRUE)
} else {
  # Fallback if filtered data is empty for cumulative_gdd
  if (any(!is.na(plot_data$cumulative_gdd))) max(plot_data$cumulative_gdd, na.rm = TRUE) else 100
}
# The actual upper limit for the x-axis scale, ensuring it's at least min_cumulative_gdd_limit
plot_actual_max_cumulative_gdd <- max(min_cumulative_gdd_limit, data_driven_max_cumulative_gdd, na.rm = TRUE)


# Position for text (top-left corner of the text block within the plot area)
# X-position is 5% from the left edge of the actual plotted cumulative_gdd range
metrics_xpos <- min_cumulative_gdd_limit + (plot_actual_max_cumulative_gdd - min_cumulative_gdd_limit) * 0.02 
# Y-position base (near top of plot) - based on overall GPP range
metrics_ypos_base <- overall_max_gpp * 0.98 

metrics_plot_labels <- metrics_data %>%
  pivot_longer(
    cols = -siteyear,
    names_to = "metric_model_raw", 
    values_to = "value"
  ) %>%
  extract(metric_model_raw, into = c("metric", "model"), regex = "^(R2|MAE|Bias)_(.*)$", remove = FALSE) %>%
  pivot_wider(
    id_cols = c(siteyear, model),
    names_from = metric,
    values_from = value
  ) %>%
  # Create the combined label string for each model
  dplyr::mutate(
    label = case_when(
      # For USHRA2015 and USHRC2015 - spaces before MAE
      siteyear %in% c("USHRA2015", "USHRC2015") ~ 
        paste0(
          "R²=", ifelse(is.na(R2), "NA", round(R2, 2)),  ", ", 
          strrep(" ", 20),  # 8 spaces before MAE
          "MAE=", ifelse(is.na(MAE), "NA", round(MAE, 2)), ", ", 
          "Bias=", ifelse(is.na(Bias), "NA", round(Bias, 2))
        ),
      
      # For USHRA2016, USHRC2016, USOF12017, USOF32017 - spaces before Bias
      siteyear %in% c("USHRA2016", "USHRC2016", "USOF12017", "USOF32017") ~ 
        paste0(
          "R²=", ifelse(is.na(R2), "NA", round(R2, 2)),  ", ", 
          "MAE=", ifelse(is.na(MAE), "NA", round(MAE, 2)),  ", ", 
          strrep(" ", 20),  # 8 spaces before Bias
          "Bias=", ifelse(is.na(Bias), "NA", round(Bias, 2))
        ),
      
      # Default format for all other sites
      TRUE ~ paste0(
        "R²=", ifelse(is.na(R2), "NA", round(R2, 2)),  ", ", 
        "MAE=", ifelse(is.na(MAE), "NA", round(MAE, 2)),  ", ", 
        "Bias=", ifelse(is.na(Bias), "NA", round(Bias, 2))
      )
    ),
    model = factor(model, levels = model_identifiers)
  ) %>%
  dplyr::filter(!is.na(model)) %>% 
  dplyr::arrange(siteyear, model) %>%
  dplyr::group_by(siteyear) %>%
  dplyr::mutate(
    xpos = metrics_xpos,
    ypos_base_val = metrics_ypos_base, 
    y_step = max(overall_gpp_range * 0.07, 0.6),
    ypos_offset = (row_number() - 1) * y_step,
    ypos = ypos_base_val - ypos_offset,
    siteyear_formatted = siteyear_formatted[siteyear]
  ) %>%
  dplyr::ungroup()
# print(metrics_plot_labels, n = 30) # For checking labels and positions
# Step 5: Generate the plot (UPDATED VERSION with formatted siteyear labels)

# First create a lookup table for formatted site names
siteyear_formatted <- c(
  "USBDA2015" = "US-BDA 2015",
  "USBDA2016" = "US-BDA 2016",
  "USBDC2015" = "US-BDC 2015",
  "USBDC2016" = "US-BDC 2016",
  "USHRA2015" = "US-HRA 2015",
  "USHRA2016" = "US-HRA 2016",
  "USHRA2017" = "US-HRA 2017",
  "USHRC2015" = "US-HRC 2015",
  "USHRC2016" = "US-HRC 2016",
  "USHRC2017" = "US-HRC 2017",
  "USOF12017" = "US-OF1 2017",
  "USOF22017" = "US-OF2 2017",
  "USOF32017" = "US-OF3 2017",
  "USOF42018" = "US-OF4 2018",
  "USOF52018" = "US-OF5 2018",
  "USOF62018" = "US-OF6 2018"
)

# Apply the formatting to your plot data
plot_data_for_main_geoms <- plot_data_for_main_geoms %>%
  mutate(siteyear_formatted = siteyear_formatted[siteyear])

metrics_plot_labels <- metrics_plot_labels %>%
  mutate(siteyear_formatted = siteyear_formatted[siteyear])

# Generate the plot with formatted labels
p <- ggplot(plot_data_for_main_geoms, aes(x = cumulative_gdd)) +
  # For GPP_predicted (line)
  geom_point(aes(y = value, color = variable),
             data = . %>% dplyr::filter(variable == "GPP_predicted"), size  = 1) + 
  
  # For other variables (points with alpha)
  geom_point(aes(y = value, color = variable),
             data = . %>% dplyr::filter(variable %in% c("GPP_site", "GPPpredictedVPM_EVI", "GPP_predicted_VI")),
             size = 1.5, shape = 16, alpha = 0.9) +
  
  # Use the formatted siteyear labels
  facet_wrap(~ siteyear_formatted, scales = "fixed", ncol = 4) +
  
  # Add metrics text if available
  {
    if(nrow(metrics_plot_labels) > 0 && all(c("xpos", "ypos", "label", "model") %in% names(metrics_plot_labels))) {
      geom_text(data = metrics_plot_labels,
                aes(x = xpos, y = ypos, label = label, color = model), 
                inherit.aes = FALSE, 
                hjust = 0,
                vjust = 1,
                size = 2.5, 
                family = myFont,
                show.legend = FALSE)
    }
  } +
  
  # Updated color scale with subscripted labels
  scale_color_manual(
    values = variable_colors,
    name = "Model",
    labels = c(
      "GPP_site" = expression(GPP[EC]),
      "GPPpredictedVPM_EVI" = expression(GPP[VPM]),
      "GPP_predicted_VI" = expression(GPP[VI]),
      "GPP_predicted" = expression(GPP[LUERF])
    )
  ) +
  
  # Adjust x and y scales
  scale_x_continuous(limits = c(min_cumulative_gdd_limit, plot_actual_max_cumulative_gdd)) +
  scale_y_continuous(name = expression(GPP~(gC~m^{-2}~day^{-1}))) + 
  
  # Customize legend appearance
  guides(color = guide_legend(override.aes = list(
    shape = c(16, 16, 16, 16),  # All circles
    size = 4,                    # Larger size in legend
    alpha = c(1, 1, 1, 1)       # Full opacity in legend
  ))) +
  
  labs(x = "Cumulative Growing Degree Days (°C)") + 
  theme_minimal(base_family = "") +
  theme(
    strip.text = element_text(family = "", margin = ggplot2::margin(t = 5, b = 5)), 
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, 'cm'),
    legend.text.align = 0,  # Helps with expression alignment
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )

p
ggsave(
  filename = "GPPmultiple.png",
  plot = p,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 10,
  height = 10,
  dpi = 300
)


#======================================================
# VIOLIN PLOT
#======================================================
# ---- STEP 1: Reshape metrics_data to long format ----
violin_plot_data <- metrics_data %>%
  pivot_longer(
    cols = -siteyear,
    names_to = "metric_model",
    values_to = "value"
  ) %>%
  extract(metric_model, into = c("metric", "model"), regex = "^(R2|MAE|Bias)_(.*)$")

# ---- STEP 2: Define consistent fill colors for the models ----
variable_colors_violin <- c(
  "GPPpredictedVPM_EVI" = wesanderson::wes_palette("IsleofDogs1")[1],
  "GPP_predicted_VI" = wesanderson::wes_palette("Zissou1")[5],
  "GPP_predicted" = wesanderson::wes_palette("Zissou1")[1]
)

# ---- STEP 3: Define expression labels for x-axis ----
model_expression_labels <- c(
  "GPPpredictedVPM_EVI" = expression(GPP[VPM]),
  "GPP_predicted_VI" = expression(GPP[VI]),
  "GPP_predicted" = expression(GPP[LUERF])
)

# ---- STEP 4: Define a reusable plotting function ----
plot_violin_metric <- function(metric_name, ylabel) {
  violin_plot_data %>%
    filter(metric == metric_name) %>%
    ggplot(aes(x = model, y = value, fill = model)) +
    geom_violin(trim = FALSE, alpha = 0.8, color = NA) +
    geom_boxplot(width = 0.1, outlier.shape = NA, fill = "white") +
    scale_fill_manual(values = variable_colors_violin) +
    scale_x_discrete(labels = model_expression_labels) +
    labs(
      x = NULL,
      y = ylabel,
      title = paste(metric_name, "across models")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
}

# ---- STEP 5: Generate individual plots ----
p_r2 <- plot_violin_metric("R2", expression(R^2))
p_mae <- plot_violin_metric("MAE", "Mean Absolute Error")
p_bias <- plot_violin_metric("Bias", "Bias")

# ---- STEP 6: Combine plots horizontally ----
combined_plot <- p_r2 + p_mae + p_bias + plot_layout(ncol = 3)

# ---- STEP 7: Save the plot to your directory ----
ggsave(
  filename = "boxplot_model_performance.png",
  plot = combined_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 12,
  height = 5,
  dpi = 300
)


#======================================================
# TABLE FOR THE CSV FILE
#======================================================
# Load required packages
# Step 1: Extract GPPLUERF row from mean_sd_metrics
extract_value <- function(x) {
  as.numeric(stringr::str_extract(x, "^[0-9\\.]+"))
}
# === 1. GPPLUERF row ===
gppluerf_row <- tibble::tibble(
  Model = "GPPLUERF",
  Train_MAE = extract_value(mean_sd_metrics$Train[mean_sd_metrics$Metric == "MAE"]),
  Train_NSE = extract_value(mean_sd_metrics$Train[mean_sd_metrics$Metric == "NSE"]),
  Train_R2 = extract_value(mean_sd_metrics$Train[mean_sd_metrics$Metric == "R2"]),
  Train_Bias = extract_value(mean_sd_metrics$Train[mean_sd_metrics$Metric == "Bias"]),
  
  Val_MAE = extract_value(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "MAE"]),
  Val_NSE = extract_value(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "NSE"]),
  Val_R2 = extract_value(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "R2"]),
  Val_Bias = extract_value(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "Bias"]),
  
  Test_MAE = extract_value(mean_sd_metrics$Test[mean_sd_metrics$Metric == "MAE"]),
  Test_NSE = extract_value(mean_sd_metrics$Test[mean_sd_metrics$Metric == "NSE"]),
  Test_R2 = extract_value(mean_sd_metrics$Test[mean_sd_metrics$Metric == "R2"]),
  Test_Bias = extract_value(mean_sd_metrics$Test[mean_sd_metrics$Metric == "Bias"])
)

# === 2. GPPVI row (from IAVI) ===
gppvi_row <- tibble(
  Model = "GPPVI",
  Train_MAE = avg_train_metrics_df %>% dplyr::filter(VI == "IAVI") %>% pull(Avg_MAE),
  Train_NSE = avg_train_metrics_df %>% dplyr::filter(VI == "IAVI") %>% pull(Avg_NSE),
  Train_R2 = avg_train_metrics_df %>%dplyr::filter(VI == "IAVI") %>% pull(Avg_R2),
  Train_Bias = avg_train_metrics_df %>%dplyr::filter(VI == "IAVI") %>% pull(Avg_Bias),
  
  Val_MAE = avg_val_metrics_df %>%dplyr::filter(VI == "IAVI") %>% pull(Avg_MAE),
  Val_NSE = avg_val_metrics_df %>%dplyr::filter(VI == "IAVI") %>% pull(Avg_NSE),
  Val_R2 = avg_val_metrics_df %>%dplyr::filter(VI == "IAVI") %>% pull(Avg_R2),
  Val_Bias = avg_val_metrics_df %>%dplyr::filter(VI == "IAVI") %>% pull(Avg_Bias),
  
  Test_MAE = summary_df %>%dplyr::filter(VI == "IAVI") %>% pull(MAE),
  Test_NSE = summary_df %>%dplyr::filter(VI == "IAVI") %>% pull(NSE),
  Test_R2 = summary_df %>%dplyr::filter(VI == "IAVI") %>% pull(R2),
  Test_Bias = summary_df %>%dplyr::filter(VI == "IAVI") %>% pull(Bias)
)

# === 3. GPPVPM row (only Testing metrics from full dataset) ===
# Calculate overall metrics
# Helper function to format "value ± sd"
combine_value_sd <- function(x) {
  if (stringr::str_detect(x, "±")) {
    parts <- stringr::str_split(x, "±", simplify = TRUE)
    value <- format(round(as.numeric(parts[1]), 2), nsmall = 2)
    sd <- format(round(as.numeric(parts[2]), 2), nsmall = 2)
    return(paste0(value, "±", sd))
  } else {
    value <- format(round(as.numeric(x), 2), nsmall = 2)
    return(value)
  }
}

# === 1. GPPLUERF row from mean_sd_metrics ===
gppluerf_row <- tibble::tibble(
  Model = "GPPLUERF",
  Train_MAE   = combine_value_sd(mean_sd_metrics$Train[mean_sd_metrics$Metric == "MAE"]),
  Train_R2    = combine_value_sd(mean_sd_metrics$Train[mean_sd_metrics$Metric == "R2"]),
  Train_Bias  = combine_value_sd(mean_sd_metrics$Train[mean_sd_metrics$Metric == "Bias"]),
  Val_MAE     = combine_value_sd(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "MAE"]),
  Val_R2      = combine_value_sd(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "R2"]),
  Val_Bias    = combine_value_sd(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "Bias"]),
  Test_MAE    = combine_value_sd(mean_sd_metrics$Test[mean_sd_metrics$Metric == "MAE"]),
  Test_R2     = combine_value_sd(mean_sd_metrics$Test[mean_sd_metrics$Metric == "R2"]),
  Test_Bias   = combine_value_sd(mean_sd_metrics$Test[mean_sd_metrics$Metric == "Bias"]),
  Train_NSE = combine_value_sd(mean_sd_metrics$Train[mean_sd_metrics$Metric == "NSE"]), 
  Val_NSE = combine_value_sd(mean_sd_metrics$Validation[mean_sd_metrics$Metric == "NSE"]), 
  Test_NSE = combine_value_sd(mean_sd_metrics$Test[mean_sd_metrics$Metric == "NSE"])
) %>%
  dplyr::relocate(Model, Train_MAE, Train_NSE, Train_R2, Train_Bias,
                  Val_MAE, Val_NSE, Val_R2, Val_Bias,
                  Test_MAE, Test_NSE, Test_R2, Test_Bias)

# === 2. GPPVI row from full_metrics_summary ===
gppvi_data <- dplyr::filter(full_metrics_summary, VI == "IAVI")

gppvi_row <- tibble::tibble(
  Model = "GPPVI",
  Train_MAE   = gppvi_data$Train_MAE,
  Train_R2    = gppvi_data$Train_R2,
  Train_Bias  = gppvi_data$Train_Bias,
  Val_MAE     = gppvi_data$Val_MAE,
  Val_R2      = gppvi_data$Val_R2,
  Val_Bias    = gppvi_data$Val_Bias,
  Test_MAE    = gppvi_data$Test_MAE,
  Test_R2     = gppvi_data$Test_R2,
  Test_Bias   = gppvi_data$Test_Bias,
  Train_NSE = gppvi_data$Train_NSE, 
  Val_NSE = gppvi_data$Val_NSE, 
  Test_NSE = gppvi_data$Test_NSE
)

# === 3. GPPVPM row from gpp_site and gpp_vpm vectors ===
gpp_site <- rf_data$GPP_site
gpp_vpm <- rf_data$GPPpredictedVPM
gppvpm_row <- tibble::tibble(
  Model = "GPPVPM",
  Train_MAE = NA, Train_NSE = NA, Train_R2 = NA, Train_Bias = NA,
  Val_MAE = NA, Val_NSE = NA, Val_R2 = NA, Val_Bias = NA,
  Test_MAE = format(round(Metrics::mae(gpp_site, gpp_vpm), 3), nsmall = 3),
  Test_NSE = format(round(1 - (sum((gpp_site - gpp_vpm)^2) / sum((gpp_site - mean(gpp_site))^2)), 3), nsmall = 3),
  Test_R2  = format(round(cor(gpp_site, gpp_vpm)^2, 3), nsmall = 3),
  Test_Bias = format(round(mean(gpp_vpm - gpp_site), 3), nsmall = 3)
)

# === 4. Convert all metrics columns to character before binding ===
gppluerf_row <- dplyr::mutate(gppluerf_row, dplyr::across(-Model, as.character))
gppvi_row    <- dplyr::mutate(gppvi_row, dplyr::across(-Model, as.character))
gppvpm_row   <- dplyr::mutate(gppvpm_row, dplyr::across(-Model, as.character))

# === 6. Add GPPLUE row from mean_sd_metrics_lue ===
gpplue_row <- tibble::tibble(
  Model = "LUERF",
  Train_MAE   = combine_value_sd(mean_sd_metrics_lue$Train[mean_sd_metrics_lue$Metric == "MAE"]),
  Train_R2    = combine_value_sd(mean_sd_metrics_lue$Train[mean_sd_metrics_lue$Metric == "R2"]),
  Train_Bias  = combine_value_sd(mean_sd_metrics_lue$Train[mean_sd_metrics_lue$Metric == "Bias"]),
  Val_MAE     = combine_value_sd(mean_sd_metrics_lue$Validation[mean_sd_metrics_lue$Metric == "MAE"]),
  Val_R2      = combine_value_sd(mean_sd_metrics_lue$Validation[mean_sd_metrics_lue$Metric == "R2"]),
  Val_Bias    = combine_value_sd(mean_sd_metrics_lue$Validation[mean_sd_metrics_lue$Metric == "Bias"]),
  Test_MAE    = combine_value_sd(mean_sd_metrics_lue$Test[mean_sd_metrics_lue$Metric == "MAE"]),
  Test_R2     = combine_value_sd(mean_sd_metrics_lue$Test[mean_sd_metrics_lue$Metric == "R2"]),
  Test_Bias   = combine_value_sd(mean_sd_metrics_lue$Test[mean_sd_metrics_lue$Metric == "Bias"]),
  Train_NSE = combine_value_sd(mean_sd_metrics_lue$Train[mean_sd_metrics_lue$Metric == "NSE"]), 
  Val_NSE = combine_value_sd(mean_sd_metrics_lue$Validation[mean_sd_metrics_lue$Metric == "NSE"]), 
  Test_NSE = combine_value_sd(mean_sd_metrics_lue$Test[mean_sd_metrics_lue$Metric == "NSE"])
) %>%
  dplyr::relocate(Model, Train_MAE, Train_NSE, Train_R2, Train_Bias,
                  Val_MAE, Val_NSE, Val_R2, Val_Bias,
                  Test_MAE, Test_NSE, Test_R2, Test_Bias)

# Convert metrics to character
gpplue_row <- dplyr::mutate(gpplue_row, dplyr::across(-Model, as.character))


# === 7. Combine all rows (now including GPPLUE) ===
final_table <- dplyr::bind_rows(gppluerf_row, gppvi_row, gppvpm_row, gpplue_row)

output_path <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/final_model_comparison_with_sd.csv"
readr::write_csv(final_table, output_path)
# === 7. Print final table ===
print(final_table)




############################################################
##############CorrelationPlot###############################
############################################################
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


# =============================================================================
# 4. Temporal correlation of GPP INDEX ANALYSIS ===============================================
# =============================================================================
# First, reshape data to long format for vegetation indices
vi_data <- rf_data %>%
  select(DAP, IAVI, VARI, NIR) %>%
  pivot_longer(cols = c(kNDVI, NDVI, NIRv), names_to = "VI_type", values_to = "VI_value")

# Make sure fonts are loaded
# font_import(pattern = "DejaVu", prompt = FALSE)  # Run once
loadfonts(device = "win")  # or "pdf", "all", depending on your output
# Set your preferred font
myFont <- "DejaVu Sans Mono"  # Or "DejaVu Sans" if that's your import
windowsFonts(sans = myFont)   # Optionally register for use as default

rf_data$GPP_predicted<-as.numeric(rf_data$GPP_predicted)
rf_data$GPP_site<-as.numeric(rf_data$GPP_site)
rf_data$GPPpredictedVPM_EVI<-as.numeric(rf_data$GPPpredictedVPM_EVI)
rf_data$GPP_predicted_VI<-as.numeric(rf_data$GPP_predicted_VI)
# First reshape the data for plotting
plot_data <- rf_data %>%
  select(DAP, siteyear, GPP_predicted, GPP_site, IAVI, VARI, GDVI) %>%
  pivot_longer(cols = c(GPP_predicted, GPP_site, IAVI, VARI, GDVI), names_to = "variable", values_to = "value")

# Define color palette using Wes Anderson colors
variable_colors <- c(
  "GPP_site" = wes_palette("Rushmore1")[3],        # Rushmore1[4] - greyish tone
  "GPP_predicted" = wes_palette("FantasticFox1")[5], # FantasticFox1[6] - dark color
  "GPPpredictedVPM_EVI" = wes_palette("Cavalcanti1")[3], # Cavalcanti1[3] - orange tone
  "GPP_predicted_VI" =wes_palette("Zissou1")[3]                  # Keeping your original teal color
)



# Filter to include only prediction vs site data
metrics_data <- plot_data %>%
  dplyr::filter(variable %in% c("GPP_site", "GPP_predicted", "GPPpredictedVPM_EVI", "GPP_predicted_VI")) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::group_by(siteyear) %>%
  summarise(
    R2_GPP_predicted = cor(GPP_site, GPP_predicted, use = "complete.obs")^2,
    MAE_GPP_predicted = mae(GPP_site, GPP_predicted),
    
    R2_GPPpredictedVPM_EVI = cor(GPP_site, GPPpredictedVPM_EVI, use = "complete.obs")^2,
    MAE_GPPpredictedVPM_EVI = mae(GPP_site, GPPpredictedVPM_EVI),
    
    R2_GPP_predicted_VI = cor(GPP_site, GPP_predicted_VI, use = "complete.obs")^2,
    MAE_GPP_predicted_VI = mae(GPP_site, GPP_predicted_VI),
    
    .groups = "drop"
  )

# Reshape to long format for plotting
metrics_long <- metrics_data %>%
  pivot_longer(-siteyear, names_to = "metric", values_to = "value") %>%
  separate(metric, into = c("stat", "model"), sep = "_", extra = "merge") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  dplyr::mutate(
    label = sprintf("R² = %.2f\nMAE = %.2f", R2, MAE),
    color = variable_colors[model],
    xpos = 30,  # adjust as needed
    ypos = 0.9 * max(plot_data$value, na.rm = TRUE)
  )
# Optional: Define transparency for different groups
plot_data$alpha_val <- ifelse(plot_data$variable == "GPP_site", 1, 0.5)

# Final plot
ggplot(plot_data, aes(x = DAP)) +
  geom_line(aes(y = value, color = variable),
            data = filter(plot_data, variable == "GPP_predicted"), size = 1) +
  geom_point(aes(y = value, color = variable, alpha = alpha_val),
             data = filter(plot_data, variable %in% c("GPP_site", "GPPpredictedVPM_EVI", "GPP_predicted_VI")),
             size = 1.5, shape = 16) +
  facet_wrap(~ siteyear, scales = "fixed", ncol = 4) +
  geom_text(data = metrics_long,
            aes(x = xpos, y = ypos, label = label, color = model),
            inherit.aes = FALSE, hjust = 0, vjust = 1, size = 3, family = myFont) +
  scale_color_manual(values = variable_colors) +
  scale_alpha(range = c(0.3, 1), guide = "none") +
  scale_x_continuous(limits = c(25, max(plot_data$DAP, na.rm = TRUE))) +
  scale_y_continuous(name = "GPP (gC m⁻² day⁻¹)") +
  labs(x = "Days After Planting (DAP)", color = "Variable") +
  theme_minimal(base_family = myFont) +
  theme(
    strip.text = element_text(family = myFont),
    legend.position = "bottom"
  )
ggplot(plot_data, aes(x = DAP)) +
  # Line for predicted GPP
  geom_line(aes(y = value, color = variable),
            data = filter(plot_data, variable == "GPP_predicted"), size = 1) +
  
  # Points for GPP_site
  geom_point(aes(y = value, color = variable, alpha = alpha_val),
             data = filter(plot_data, variable == "GPP_site"), 
             size = 1.5, shape = 16) +
  
  # Points for GPPpredictedVPM_EVI
  geom_point(aes(y = value, color = variable, alpha = alpha_val),
             data = filter(plot_data, variable == "GPPpredictedVPM_EVI"), 
             size = 1.5, shape = 16) +
  
  # Points for GPP_predicted_VI
  geom_point(aes(y = value, color = variable, alpha = alpha_val),
             data = filter(plot_data, variable == "GPP_predicted_VI"), 
             size = 1.5, shape = 16) +
  
  facet_wrap(~ siteyear, scales = "fixed", ncol = 4) +
  scale_color_manual(values = variable_colors) +
  scale_alpha(range = c(0.3, 1), guide = "none") +  # Hide alpha legend
  scale_x_continuous(limits = c(25, max(plot_data$DAP, na.rm = TRUE))) +
  scale_y_continuous(name = "GPP (gC m⁻² day⁻¹)") +
  labs(x = "Days After Planting (DAP)", color = "Variable") +
  theme_minimal(base_family = myFont) +
  theme(
    strip.text = element_text(family = myFont),
    legend.position = "bottom"
  )



# Calculate ET
rf_data <- rf_data %>%
  mutate(ET = Es + Ei + Ec)

# Summarize by siteyear
rf_summary <- rf_data %>%
  group_by(siteyear) %>%
  summarise(
    Mean_VPD = mean(VPD_site, na.rm = TRUE),
    Total_temperature = sum(Tair_site, na.rm = TRUE),
    total_GPP_site = sum(GPP_site, na.rm = TRUE),
    total_dayl_hour = sum(dayl_hour, na.rm = TRUE),
    total_Es = sum(Es, na.rm = TRUE),
    DOP = mean(DOP, na.rm = TRUE),
    Variety = first(Variety)
  )

# Create ET and temperature levels (optional, not used in plot)
rf_summary <- rf_summary %>%
  mutate(
    ET_level = cut(total_Es,
                   breaks = quantile(total_Es, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   labels = c("Low ET", "Medium ET", "High ET"),
                   include.lowest = TRUE),
    Temperature_level = cut(Total_temperature,
                            breaks = quantile(Total_temperature, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                            labels = c("Low T", "Medium T", "High T"),
                            include.lowest = TRUE)
  )

# Reshape only GPP_site
rf_long <- rf_summary %>%
  select(siteyear, total_dayl_hour, DOP, Variety, Mean_VPD, total_GPP_site) %>%
  rename(GPP_value = total_GPP_site)

# DOP category
rf_long_filtered <- rf_long %>%
  mutate(
    DOP_category = case_when(
      DOP <= quantile(DOP, 1/3, na.rm = TRUE) ~ "Early",
      DOP <= quantile(DOP, 2/3, na.rm = TRUE) ~ "Mid",
      TRUE ~ "Late"
    ),
    DOP_category = factor(DOP_category, levels = c("Early", "Mid", "Late"))
  )

# Calculate correlations
r_dayl <- cor(rf_long_filtered$GPP_value,
              predict(gam(GPP_value ~ s(total_dayl_hour), data = rf_long_filtered)),
              use = "complete.obs")

r_vpd <- cor(rf_long_filtered$GPP_value,
             rf_long_filtered$Mean_VPD,
             use = "complete.obs")

r_dop <- cor(rf_long_filtered$GPP_value,
             rf_long_filtered$DOP,
             use = "complete.obs")

# Create label strings
label_dayl <- paste0("GPP[EC]~vs.~Daylength~(R==", round(r_dayl, 2), ")")
label_vpd <- paste0("GPP[EC]~vs.~VPD~(R==", round(r_vpd, 2), ")")
label_dop <- paste0("GPP[EC]~vs.~DOP~(R==", round(r_dop, 2), ")")

# Final plot
plot_obj <- ggplot(rf_long_filtered, aes(x = total_dayl_hour, y = GPP_value)) +
  geom_point(
    aes(color = Variety, shape = DOP_category, size = Mean_VPD),
    alpha = 0.9
  ) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    color = "black",
    se = FALSE,
    size = 1.1
  ) +
  geom_text(
    aes(x = 1550, y = max(GPP_value, na.rm = TRUE) * 0.95, label = label_dayl),
    parse = TRUE,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = 1550, y = max(GPP_value, na.rm = TRUE) * 0.85, label = label_vpd),
    parse = TRUE,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = 1550, y = max(GPP_value, na.rm = TRUE) * 0.75, label = label_dop),
    parse = TRUE,
    hjust = 0,
    vjust = 1,
    size = 5,
    color = "black",
    inherit.aes = FALSE
  ) +
  scale_shape_manual(
    values = c("Early" = 16, "Mid" = 17, "Late" = 15),
    name = "Planting Group"
  ) +
  scale_size_continuous(
    range = c(2, 12),
    name = "Mean VPD"
  ) +
  labs(
    x = "Cumulative Seasonal Daylength (hours)",
    y = expression("Cumulative Seasonal GPP (g C"~m^{-2}~"season"^{-1}*")"),
    color = "Variety"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(2, "lines")
  )

plot_obj

# Save the plot
ggsave(
  filename = "GPP_Variety_Plot_GPPEC.png",
  plot = plot_obj,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  dpi = 300,
  width = 12,
  height = 6,
  units = "in"
)


plot(rf_long_filtered$Mean_VPD, rf_long_filtered$GPP_value)
plot(rf_long_filtered$DOP, rf_long_filtered$GPP_value)

#-----------------------------------------------------------------
#FAPAR
#----------------------------------------------------------------
library(ggplot2)
library(cowplot)

# --- Define fapar_hist_plot function ---
fapar_hist_plot <- function(data, fapar_var, lue_var, label, xlab_text, 
                            show_x = TRUE, show_x_title_only = FALSE, show_legend = TRUE) {
  # Classify into 3 groups
  data$Group <- with(data, ifelse(data[[fapar_var]] > 1, 
                                  "fAPAR > 1",
                                  ifelse(data[[lue_var]] > 1, 
                                         "LUE > 1", 
                                         "Other")))
  data$Group <- factor(data$Group, levels = c("Other", "LUE > 1", "fAPAR > 1"))
  
  # Base plot
  p <- ggplot(data, aes(x = .data[[fapar_var]], fill = Group)) +
    geom_histogram(position = "identity", alpha = 0.7, bins = 40) +
    scale_fill_manual(
      name = "Group",
      values = friendly_pal("contrast_three"),  # Replace with your color palette
      breaks = c("fAPAR > 1", "LUE > 1", "Other"),
      guide = guide_legend(override.aes = list(size = 10))  # Increased from 5 to 10
    ) +
    labs(x = xlab_text, y = "Frequency") +
    theme_minimal() +
    theme(
      text = element_text(size = 16),  # Increased base text size by ~3
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16),
      legend.title = element_text(size = 20),  # Increased legend title size
      legend.text = element_text(size = 18)    # Increased legend text size
    )
  
  # Add label top-left
  y_max <- max(table(cut(data[[fapar_var]], breaks = 40)))
  p <- p + annotate(
    "text",
    x = min(data[[fapar_var]], na.rm = TRUE),
    y = y_max * 0.95,
    label = label, hjust = 0, vjust = 1, size = 9, fontface = "bold"  # Increased from 6 to 9
  )
  
  # Conditional x-axis customization
  if (!show_x) {
    p <- p + theme(axis.title.x = element_blank(),
                   axis.text.x  = element_blank(),
                   axis.ticks.x = element_blank())
  }
  if (show_x_title_only) {
    p <- p + theme(axis.text.x  = element_blank(),
                   axis.ticks.x = element_blank())
  }
  
  # Conditional legend
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = "right")  # Move legend to right side
  }
  
  return(p)
}

# --- Create the plots ---
p1 <- fapar_hist_plot(
  rf_data, "fAPAR_evi", "LUE_evi", "A",
  xlab_text = expression(italic(f)*"APAR(EVI)"), show_x = TRUE, show_legend = TRUE
)
p2 <- fapar_hist_plot(
  rf_data, "fAPAR_ndvi", "LUE_ndvi", "B",
  xlab_text = expression(italic(f)*"APAR(NDVI)"), show_x_title_only = TRUE, show_legend = FALSE
)
p3 <- fapar_hist_plot(
  rf_data, "fAPAR_lai", "LUE_lai", "C",
  xlab_text = expression(italic(f)*"APAR(LAI)"), show_x = TRUE, show_legend = FALSE
)

# --- Extract shared legend from p1 with increased size ---
shared_legend <- get_legend(
  p1 + theme(
    legend.position = "right",
    legend.title = element_text(size = 22),  # Further increased size
    legend.text = element_text(size = 20),   # Further increased size
    legend.key.size = unit(1.5, "cm")       # Increased legend key size
  )
)

# --- Remove legends from plots for combining ---
p1_nolegend <- p1 + theme(legend.position = "none")

# --- Combine plots vertically with legend on the right ---
combined_plots <- plot_grid(
  p1_nolegend, p2, p3,
  ncol = 1,
  align = "v"
)

# --- Combine plots with legend on the right side ---
final_plot <- plot_grid(
  combined_plots,
  shared_legend,
  ncol = 2,
  rel_widths = c(3, 0.8)  # Adjust width ratio between plot and legend
)

final_plot

# --- Save figure with increased dimensions to accommodate larger text ---
ggsave(
  filename = "combined_fapar_plot.png",
  plot = final_plot,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 14,  # Increased width to accommodate legend on the right
  height = 12,
  dpi = 300
)

library(ggplot2)
library(dplyr)

# ------------------------------------------------------------------------------
# Prepare metrics labels for each facet
# ------------------------------------------------------------------------------

metrics_plot_labels_block <- metrics_plot_labels %>%  ###change here with case when siteyear for new graph
  distinct(siteyear, model, R2, MAE, Bias) %>%
  mutate(
    xpos = case_when(
      model == "GPP_predicted" ~ 0,
      model == "GPPpredictedVPM_EVI" ~ 1775,
      model == "GPP_predicted_VI" ~ 900,
      TRUE ~ NA_real_
    ),
    ypos =  case_when(
      model == "GPP_predicted" ~ 43,
      model == "GPPpredictedVPM_EVI" ~ 43,
      model == "GPP_predicted_VI" ~ 7,
      TRUE ~ NA_real_
    ),
    label = paste0("R² = ", round(R2, 2),
                   "\nMAE = ", round(MAE, 2),
                   "\nBias = ", round(Bias, 2)),
    siteyear_formatted = paste0(siteyear)
  ) %>%
  group_by(siteyear_formatted, model) %>%
  slice(1) %>%   # only 1 label per facet × model
  ungroup()

# ------------------------------------------------------------------------------
# Prepare main plot data
# ------------------------------------------------------------------------------
plot_data_long <- plot_data_for_main_geoms %>%
  dplyr::filter(variable %in% c("GPP_predicted", "GPPpredictedVPM_EVI", "GPP_predicted_VI", "GPP_site")) %>%
  mutate(
    model = variable,
    siteyear_formatted = paste0(siteyear)
  )

# ------------------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------------------
p <- ggplot(plot_data_long, aes(x = cumulative_gdd, y = value, color = model)) +
  
  # Points: use constant size/shape/alpha to avoid extra legends
  geom_point(data = plot_data_long %>% dplyr::filter(model == "GPP_predicted"),
             size = 1, shape = 16, alpha = 1) +
  geom_point(data = plot_data_long %>% dplyr::filter(model %in% c("GPP_site", "GPPpredictedVPM_EVI", "GPP_predicted_VI")),
             size = 1.5, shape = 16) +
  
  # Facets
  facet_wrap(~ siteyear_formatted, scales = "fixed", ncol = 4) +
  
  # Metrics labels
  geom_text(
    data = metrics_plot_labels_block,
    aes(x = xpos, y = ypos, label = label, color = model),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,
    size = 4, fontface = "bold", lineheight = 0.7,
    show.legend = FALSE
  ) +
  
  # Color scale and legend
  # Color scale and legend
  scale_color_manual(
    values = variable_colors,
    name = "Model",
    labels = c(
      "GPP_site" = expression(GPP[EC]),
      "GPPpredictedVPM_EVI" = expression(GPP[VPM]),
      "GPP_predicted_VI" = expression(GPP[VI]),
      "GPP_predicted" = expression(GPP[LUERF])
    )
  ) +
  
  # Legend appearance
  guides(color = guide_legend(override.aes = list(
    shape = c(16, 16, 16, 16),
    size = 5,
    alpha = c(1, 1, 1, 1)
  ))) +
  
  # Axes
  scale_x_continuous(limits = c(min_cumulative_gdd_limit, plot_actual_max_cumulative_gdd)) +
  scale_y_continuous(name = expression(GPP~(gC~m^{-2}~day^{-1}))) +
  
  # Labels
  labs(x = "Cumulative Growing Degree Days (°C)") +
  
  # Theme
  theme_minimal(base_size = 18) +
  theme(
    strip.text = element_text(size = 18, face = "bold", margin = ggplot2::margin(t = 5, b = 5)),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    legend.position = "bottom",
    legend.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.spacing.x = grid::unit(0.5, 'cm'),
    legend.text.align = 0,
    plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10)
  )

# ------------------------------------------------------------------------------
# Save
# ------------------------------------------------------------------------------
p
ggsave(
  filename = "GPPmultiple.png",
  plot = p,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
  width = 16,
  height = 11,
  dpi = 200
)



#-------------------------------------------------
#Comparison GPP DAYLength 
#------------------------------------------------
# Calculate ET
rf_data <- joined_df %>%
  mutate(ET = Es )

# Summarize by siteyear
rf_summary <- joined_df %>%
  group_by(siteyear) %>%
  summarise(
    Mean_VPD = mean(VPD_site, na.rm = TRUE),
    Total_temperature = sum(Tair_site, na.rm = TRUE),
    total_GPP_site = sum(GPP_site, na.rm = TRUE),
    total_dayl_hour = sum(dayl_hour, na.rm = TRUE),
    total_Es = sum(Es, na.rm = TRUE),
    DOP = mean(DOP, na.rm = TRUE),
    Variety = first(Variety)
  )

# Create ET and temperature levels (optional, not used in plot)
rf_summary <- rf_summary %>%
  mutate(
    ET_level = cut(total_Es,
                   breaks = quantile(total_Es, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                   labels = c("Low ET", "Medium ET", "High ET"),
                   include.lowest = TRUE),
    Temperature_level = cut(Total_temperature,
                            breaks = quantile(Total_temperature, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                            labels = c("Low T", "Medium T", "High T"),
                            include.lowest = TRUE)
  )

# Reshape only GPP_site
rf_long <- rf_summary %>%
  select(siteyear, total_dayl_hour, DOP, Variety, Mean_VPD, total_GPP_site) %>%
  rename(GPP_value = total_GPP_site)


# DOP category
rf_long_filtered <- rf_long %>%
  mutate(
    DOP_category = case_when(
      DOP <= quantile(DOP, 1/3, na.rm = TRUE) ~ "Early",
      DOP <= quantile(DOP, 2/3, na.rm = TRUE) ~ "Mid",
      TRUE ~ "Late"
    ),
    DOP_category = factor(DOP_category, levels = c("Early", "Mid", "Late"))
  )

# Calculate correlations
r_dayl <- cor(rf_long_filtered$GPP_value,
              predict(gam(GPP_value ~ s(total_dayl_hour), data = rf_long_filtered)),
              use = "complete.obs")

r_vpd <- cor(rf_long_filtered$GPP_value,
             rf_long_filtered$Mean_VPD,
             use = "complete.obs")

r_dop <- cor(rf_long_filtered$GPP_value,
             rf_long_filtered$DOP,
             use = "complete.obs")

# Create label strings
label_dayl <- paste0("GPP[EC]~vs.~Daylength~(R==", round(r_dayl, 2), ")")
label_vpd <- paste0("GPP[EC]~vs.~VPD~(R==", round(r_vpd, 2), ")")
label_dop <- paste0("GPP[EC]~vs.~DOP~(R==", round(r_dop, 2), ")")

# Final plot
# Final plot with larger legend symbols
plot_obj <- ggplot(rf_long_filtered, aes(x = total_dayl_hour, y = GPP_value)) +
  geom_point(
    aes(color = Variety, shape = DOP_category, size = Mean_VPD),
    alpha = 0.9
  ) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x),
    color = "black",
    se = FALSE,
    size = 1.1
  ) +
  geom_text(
    aes(x = 1550, y = max(GPP_value, na.rm = TRUE) * 0.95, label = label_dayl),
    parse = TRUE, hjust = 0, vjust = 1, size = 5, color = "black", inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = 1550, y = max(GPP_value, na.rm = TRUE) * 0.85, label = label_vpd),
    parse = TRUE, hjust = 0, vjust = 1, size = 5, color = "black", inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = 1550, y = max(GPP_value, na.rm = TRUE) * 0.75, label = label_dop),
    parse = TRUE, hjust = 0, vjust = 1, size = 5, color = "black", inherit.aes = FALSE
  ) +
  scale_shape_manual(
    values = c("Early" = 16, "Mid" = 17, "Late" = 15),
    name = "Planting Group"
  ) +
  scale_size_continuous(
    range = c(2, 12),
    name = "Mean VPD"
  ) +
  labs(
    x = "Cumulative Seasonal Daylength (hours)",
    y = expression("Cumulative Seasonal GPP (g C"~m^{-2}~"season"^{-1}*")"),
    color = "Variety"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray85"),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(2, "lines")
  ) +
  # Increase legend symbol sizes
  guides(
    color = guide_legend(override.aes = list(size = 6)),    # Variety points
    shape = guide_legend(override.aes = list(size = 6)),    # Planting Group symbols
    #size = guide_legend(override.aes = list(size = 6))      # VPD legend
  )

plot_obj

# Define file path and name
file_path <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/GPP_vs_Daylength.png"

# Save the plot
ggsave(filename = file_path,
       plot = plot_obj,
       width = 12,       # width in inches
       height = 8,       # height in inches
       dpi = 300)        # resolution



#-------------------------------------------------
#ALL VARIABLES TIMESERIES
#-------------------------------------------------
library(ggplot2)
library(gridExtra)

# Variables (exclude DAP vs DAP)
vars <- mean_importance$Variable
vars <- vars[vars != "DAP"]

# Output folder
out_dir <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/Features"

# Create folder if it doesn't exist
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# List to store individual plots
plot_list <- list()

# Loop through each variable and create a plot
for(v in vars){
  p <- ggplot(rf_data, aes_string(x = "DAP", y = v)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", se = TRUE, color = "blue") +
    labs(x = "DAP", y = v, title = paste(v, "vs DAP")) +
    theme_minimal(base_size = 12)
  
  # Save individual plot
  ggsave(filename = file.path(out_dir, paste0(v, "_vs_DAP.png")),
         plot = p, width = 6, height = 4)
  
  # Store for combined plot
  plot_list[[v]] <- p
}

# Arrange all plots in a 5x5 grid (adjust number of rows and columns)
# gridExtra can handle up to 25 plots nicely
combined_plot <- gridExtra::grid.arrange(grobs = plot_list, nrow = 5, ncol = 5)

# Save combined plot
ggsave(filename = file.path(out_dir, "All_Variables_vs_DAP.png"),
       plot = combined_plot, width = 20, height = 20)


#-------------------------------------------------------------
#OLD CODE
#--------------------------------------------------------------
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



library(ggplot2)
library(dplyr)
library(tidyr)

