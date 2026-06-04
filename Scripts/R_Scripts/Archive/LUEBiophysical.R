# ==============================================================================
# SECTION 3: BIOPHYSICAL LUE - DAP COLORED PLOT
# ==============================================================================
# Load required libraries
library(ggplot2)
library(patchwork)
library(wesanderson)
library(viridis) # Added for continuous color scale

# ------------------------------------------------------------------------------
# 1. Statistical Helper Functions (R² is omitted as it's not used in annotation)
# ------------------------------------------------------------------------------

# Function to calculate Pearson's R
# Pearson's R
calc_pearson <- function(x, y) {
  cor(x, y, method = "pearson", use = "complete.obs")
}

# Kendall's tau
calc_kendall <- function(x, y) {
  cor(x, y, method = "kendall", use = "complete.obs")
}

# ------------------------------------------------------------------------------
# 2. Aesthetics and Theme Definitions
# ------------------------------------------------------------------------------

# Axis label
lue_lab <- expression("Light Use Efficiency (gC mol"^{-1}~"photon)")

# Base theme
my_theme <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none" # Keep base theme legend off by default
  )

# Theme for plots without y-axis
theme_no_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

# ------------------------------------------------------------------------------
# 3. Core Plotting Function: plot_lue_by_dap
#    Plots only LUE (Observed), points colored by DAP
# ------------------------------------------------------------------------------

plot_lue_by_dap <- function(xvar, xlab, show_y = FALSE, show_legend = FALSE) {
  
  # Verify required columns exist
  required_cols <- c(xvar, "LUE", "DAP")
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
  
  # Calculate statistics (only for LUE vs xvar)
  tau_lue <- safe_calc(calc_kendall, plot_data[[xvar]], plot_data$LUE)
  r_lue <- safe_calc(calc_pearson, plot_data[[xvar]], plot_data$LUE)
  
  # Default position parameters (using original LUE positions)
  xpos <- Inf
  ypos_lue <- Inf
  hjust_val <- 1.1
  vjust_lue <- 1.5 # Adjusted vjust since only one line of stats is present
  
  # Custom position logic (based on original script)
  if (xvar %in% c("rH_site", "DBSI", "MLSWI26", "IAVI", "Tair_site")) {
    xpos <- -Inf
    ypos_lue <- Inf
    hjust_val <- -0.1
    vjust_lue <- 1.5
  } else if (xvar == "VPD_site") {
    xpos <- 11  # adjust if needed
    ypos_lue <- 0.825 # Centered between the original two annotations
    hjust_val <- 0
    vjust_lue <- 0
  } else if (xvar == "Es") {
    xpos <- Inf
    ypos_lue <- Inf
    hjust_val <- 1.1
    vjust_lue <- 1.5
  } else if (xvar == "cumulative_gdd") {
    xpos <- Inf
    ypos_lue <- -Inf
    hjust_val <- 1.1
    vjust_lue <- -1.5
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = .data[[xvar]])) +
    
    # 1. Scatter points colored by DAP
    geom_point(aes(y = LUE, color = .data[["DAP"]]), alpha = 0.8, na.rm = TRUE) +
    
    # 2. LOESS smoother for overall LUE trend (black line, no se)
    geom_smooth(aes(y = LUE), 
                color = "black", 
                method = "loess", 
                linewidth = 1.5, 
                se = FALSE, # Removing SE for cleaner look with the dense points
                na.rm = TRUE) +
    
    # 3. Continuous color scale for DAP
    # REMOVED the explicit guide=guide_colorbar(direction="horizontal") here
    # to let patchwork collect and control the single guide globally.
    scale_color_viridis_c(name = "Days After Planting (DAP)", 
                          option = "D") +
    
    labs(x = xlab) +
    
    # 4. Add LUE statistics annotation
    annotate("text", 
             x = xpos, 
             y = ypos_lue,
             label = paste0("τ = ", tau_lue, ", R = ", r_lue),
             color = "black", # Black color for single annotation
             hjust = hjust_val, 
             vjust = vjust_lue, 
             size = 5, 
             fontface = "bold") + 
    my_theme
  
  # Add y-axis if requested
  if (show_y) {
    p <- p + ylab(lue_lab)
  } else {
    p <- p + theme_no_y
  }
  
  # The show_legend argument is no longer used for local theme customization
  
  return(p)
}

# ------------------------------------------------------------------------------
# 4. Create & Combine Plots (Using the same enhanced theme as original p1)
# ------------------------------------------------------------------------------

# Define the common plot styling for the final figure aesthetic
plot_style <- list(
  theme_classic(),
  theme(
    # Ensure local legend is off for all plots. This makes patchwork look for a global spot.
    legend.position = "none", 
    axis.line = element_line(linewidth = 1.5, color = "black"),
    axis.ticks = element_line(linewidth = 1),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16),
    plot.margin = ggplot2::margin(5, 5, 5, 5, unit = "pt") # Ensure margins are consistent
  )
)

# Create all plots, applying the style which includes legend.position = "none"
p1 <- plot_lue_by_dap("VPD_site", "VPD (kPa)", show_y = TRUE) + plot_style
p2 <- plot_lue_by_dap("rH_site", "Relative Humidity (%)") + plot_style + labs(y = NULL)
p3 <- plot_lue_by_dap("DBSI", "DBSI") + plot_style + labs(y = NULL)
p4 <- plot_lue_by_dap("AWEInsh", "AWEInsh") + plot_style + labs(y = NULL)
p5 <- plot_lue_by_dap("IAVI", "IAVI", show_y = TRUE) + plot_style
p6 <- plot_lue_by_dap("Tair_site", "Air Temperature (°C)") + plot_style + labs(y = NULL)
p7 <- plot_lue_by_dap("Es", "Es") + plot_style + labs(y = NULL)
p8 <- plot_lue_by_dap("cumulative_gdd", "Cumulative GDD (°C)") + plot_style + labs(y = NULL)

# Combine the plots
LUE_DAP_Biophysical <- ((p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)) +
  plot_annotation(
    tag_levels = 'A',
    tag_prefix = "",
    theme = theme(
      plot.tag = element_text(size = 18, face = "bold", hjust = 0, vjust = 1), # Larger, left-aligned tags
      plot.tag.position = 'topleft'
    )
  ) +
  # Use plot_layout to explicitly collect and combine the guides from all plots
  plot_layout(guides = "collect") & 
  
  # Apply global legend styling for horizontal bar at the bottom
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.margin = ggplot2::margin(t = 0.5, unit = "cm"), # Spacing between plots and legend
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.width = unit(3.5, "cm"), # Make the color bar wide
    legend.key.height = unit(0.5, "cm"), # Adjust height
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold")
  )

# ------------------------------------------------------------------------------
# 5. Save the Plot
# ------------------------------------------------------------------------------
LUE_DAP_Biophysical
ggsave(
  filename = "LUE_DAP_Biophysical.png",
  plot = LUE_DAP_Biophysical,
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure", # Use your actual desired path
  width = 17,
  height = 10,
  dpi = 300,
  units = "in"
)

ggplot(joined_df, aes(x = Et, y = GPP_site, color = DAP)) +
  geom_point() +
  scale_color_viridis_c() +                  # optional: continuous color scale
  theme_classic() +
  labs(
    x = "Days After Planting (DAP)",
    y = "Et",
    color = "DAP"
  )

library(ggplot2)
library(ggplot2)
library(viridis)

# List of water indices
water_indices <- c(
  "ANDWI", "AWEInsh", "AWEIsh", "FAI", "LSWI", "MBWI", "MLSWI26", "MLSWI27",
  "MNDWI", "MuWIR", "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI",
  "OSI", "PI", "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI"
)

# Output folder
output_folder <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/WaterIndices"

# Loop through each index and save the plot
for (index in water_indices) {
  
  # Check if the column exists in joined_df
  if (!(index %in% names(joined_df))) {
    warning(paste("Column", index, "not found in joined_df. Skipping."))
    next
  }
  
  # Create plot
  p <- ggplot(joined_df, aes_string(x = "DAP", y = index, color = "DAP")) +
    geom_point() +
    facet_wrap(~ siteyear, scales = "free") +
    scale_color_viridis_c() +
    theme_classic() +
    labs(
      x = "Days After Planting (DAP)",
      y = index,
      color = "DAP"
    )
  
  # Save plot
  ggsave(
    filename = paste0(index, ".png"),
    plot = p,
    path = output_folder,
    width = 10,
    height = 6,
    dpi = 300,
    units = "in"
  )
}



library(ggplot2)
library(viridis)

# List of soil indices
soil_indices <- c(
  "BI", "BITM", "BIXS", "BaI", "DBSI", "EMBI", "MBI",
  "NDSoI", "NSDS", "NSDSI1", "NSDSI2", "NSDSI3", "RI4XS"
)

# Output folder for soil indices plots
output_folder_soil <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/SoilINdices"

# Loop through each soil index and save the plot
for (index in soil_indices) {
  
  # Check if the column exists in joined_df
  if (!(index %in% names(joined_df))) {
    warning(paste("Column", index, "not found in joined_df. Skipping."))
    next
  }
  
  # Create plot
  p <- ggplot(joined_df, aes_string(x = "DAP", y = index, color = "DAP")) +
    geom_point() +
    facet_wrap(~ siteyear, scales = "free") +
    scale_color_viridis_c() +
    theme_classic() +
    labs(
      x = "Days After Planting (DAP)",
      y = index,
      color = "DAP"
    )
  
  # Save plot
  ggsave(
    filename = paste0(index, ".png"),
    plot = p,
    path = output_folder_soil,
    width = 10,
    height = 6,
    dpi = 300,
    units = "in"
  )
}


library(ggplot2)
library(viridis)

# List of evaporation components
evap_indices <- c("Ec", "Ei", "Es")

# Create total evaporation column
joined_df$Et <- rowSums(joined_df[, evap_indices], na.rm = TRUE)

# Add Et to the list of indices to plot
evap_indices_all <- c(evap_indices, "Et")

# Output folder
output_folder_evap <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/Evaporation"

# Loop through each evaporation index and save the plot
for (index in evap_indices_all) {
  
  # Check if the column exists in joined_df
  if (!(index %in% names(library(ggplot2)
library(viridis)

# List of evaporation components
evap_indices <- c("Ec", "Ei", "Es")

# Create total evaporation column
joined_df$Et <- rowSums(joined_df[, evap_indices], na.rm = TRUE)

# Add Et to the list of indices to plot
evap_indices_all <- c(evap_indices, "Et")

# Output folder
output_folder_evap <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/Evaporation"

# Loop through each evaporation index and save the plot
for (index in evap_indices_all) {
  
  # Check if the column exists in joined_df
  if (!(index %in% names(joined_df))) {
    warning(paste("Column", index, "not found in joined_df. Skipping."))
    next
  }

  # Create plot
  p <- ggplot(joined_df, aes_string(x = "DAP", y = index, color = "DAP")) +
    geom_point() +
    facet_wrap(~ siteyear, scales = "free") +
    scale_color_viridis_c() +
    theme_classic() +
    labs(
      x = "Days After Planting (DAP)",
      y = index,
      color = "DAP"
    )

  # Save plot
  ggsave(
    filename = paste0(index, ".png"),
    plot = p,
    path = output_folder_evap,
    width = 10,
    height = 6,
    dpi = 300,
    units = "in"
  )
}
))) {
    warning(paste("Column", index, "not found in joined_df. Skipping."))
    next
  }
  
  # Create plot
  p <- ggplot(joined_df, aes_string(x = "DAP", y = index, color = "DAP")) +
    geom_point() +
    facet_wrap(~ siteyear, scales = "free") +
    scale_color_viridis_c() +
    theme_classic() +
    labs(
      x = "Days After Planting (DAP)",
      y = index,
      color = "DAP"
    )
  
  # Save plot
  ggsave(
    filename = paste0(index, ".png"),
    plot = p,
    path = output_folder_evap,
    width = 10,
    height = 6,
    dpi = 300,
    units = "in"
  )
}


# ==============================================================================
# SECTION 3: BIOPHYSICAL LUE - DAP COLORED PLOT
# ==============================================================================

# ==============================================================================
# SECTION 3: BIOPHYSICAL LUE
# ==============================================================================     
# ==============================================================================
# SECTION 3: BIOPHYSICAL LUE
# ==============================================================================     
library(ggplot2)
library(patchwork)
library(wesanderson)
library(viridis)

# Functions
calc_pearson <- function(x, y) cor(x, y, method = "pearson", use = "complete.obs")
calc_r2 <- function(x, y) summary(lm(y ~ x))$r.squared
calc_kendall <- function(x, y) cor(x, y, method = "kendall", use = "complete.obs")

# Colors
line_colors <- c("LUE" = wes_palette("Darjeeling2")[2],
                 "LUEpredicted" = wes_palette("Chevalier1")[1])

# Axis label
lue_lab <- expression("Light Use Efficiency (gC mol"^{-1}~"photon)")

# Themes
my_theme <- theme_minimal() +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  )
theme_no_y <- theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank()
)

# Hybrid plot function
plot_dual <- function(xvar, xlab, show_y = FALSE, show_legend = FALSE) {
  required_cols <- c(xvar, "LUE", "LUEpredicted", "DAP")
  missing_cols <- setdiff(required_cols, names(rf_data))
  if(length(missing_cols) > 0) stop(paste("Missing columns:", paste(missing_cols, collapse=", ")))
  
  plot_data <- rf_data[complete.cases(rf_data[, required_cols]), ]
  
  safe_calc <- function(fun, x, y) {
    tryCatch({
      if(length(x) > 0 && length(y) > 0) round(fun(x, y), 2) else NA
    }, error = function(e) NA)
  }
  
  tau_lue <- safe_calc(calc_kendall, plot_data[[xvar]], plot_data$LUE)
  r_lue <- safe_calc(calc_pearson, plot_data[[xvar]], plot_data$LUE)
  tau_pred <- safe_calc(calc_kendall, plot_data[[xvar]], plot_data$LUEpredicted)
  r_pred <- safe_calc(calc_pearson, plot_data[[xvar]], plot_data$LUEpredicted)
  
  xpos <- Inf; ypos_lue <- Inf; ypos_pred <- Inf; hjust_val <- 1.1; vjust_lue <- 3; vjust_pred <- 1.5
  if (xvar %in% c("rH_site", "DBSI", "MLSWI26", "IAVI", "Tair_site")) { xpos <- -Inf; hjust_val <- -0.1 }
  if (xvar == "VPD_site") { xpos <- 11; ypos_lue <- 0.85; ypos_pred <- 0.80; hjust_val <- 0; vjust_lue <- 0; vjust_pred <- 0 }
  if (xvar == "cumulative_gdd") { ypos_lue <- -Inf; ypos_pred <- -Inf; vjust_lue <- -1; vjust_pred <- -2.5 }
  
  p <- ggplot(plot_data, aes(x = .data[[xvar]])) +
    # Points colored by DAP
    geom_point(aes(y = LUE, color = DAP), alpha = 0.8, na.rm = TRUE) +
    geom_point(aes(y = LUEpredicted, color = DAP), alpha = 0.3, na.rm = TRUE) +
    scale_color_viridis_c(
      option = "magma",
      direction = -1,
      breaks = seq(floor(min(plot_data$DAP, na.rm = TRUE)), 
                   ceiling(max(plot_data$DAP, na.rm = TRUE)), 
                   by = 20),
      guide = guide_colorbar(
        barwidth = 30,
        barheight = 2,
        ticks = TRUE,
        ticks.linewidth = 1.2,
        title.position = "left",
        label.theme = element_text(size = 15)  # increase font by 5
      )
    ) +
    
    # Regression lines with fixed colors
    geom_smooth(aes(y = LUE), method = "loess", size = 2, color = line_colors["LUE"], se = TRUE, na.rm = TRUE) +
    geom_smooth(aes(y = LUEpredicted), method = "loess", size = 2, color = line_colors["LUEpredicted"], se = TRUE, na.rm = TRUE) +
    
    labs(x = xlab) +
    
    # Annotate text with line colors
    annotate("text", x = xpos, y = ypos_lue, label = paste0("τ = ", tau_lue, ", R = ", r_lue),
             color = line_colors["LUE"], hjust = hjust_val, vjust = vjust_lue, size = 5, fontface = "bold") +
    annotate("text", x = xpos, y = ypos_pred, label = paste0("τ = ", tau_pred, ", R = ", r_pred),
             color = line_colors["LUEpredicted"], hjust = hjust_val, vjust = vjust_pred, size = 5, fontface = "bold")
  
  if (show_y) p <- p + ylab(lue_lab) else p <- p + theme_no_y
  if (show_legend) p <- p + theme(legend.position = c(0.98,0.98), legend.justification = c(1,1))
  
  return(p)
}

# Ensure predicted column exists
rf_data$LUEpredicted <- rf_data$LUE_predicted


# -----------------------------
# Common plot styling
# -----------------------------
plot_style <- list(
  theme_classic(),
  theme(
    legend.position = "none",                # Local legends off
    axis.line = element_line(linewidth = 1.5, color = "black"),
    axis.ticks = element_line(linewidth = 1),
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    plot.margin = ggplot2::margin(5, 5, 5, 5, unit = "pt")
  )
)

# -----------------------------
# Example: create plots with plot_style
# -----------------------------
p1 <- plot_dual("VPD_site", "VPD (kPa)", show_y = TRUE, show_legend = TRUE) + plot_style
p2 <- plot_dual("rH_site", "Relative Humidity (%)") + plot_style + labs(y = NULL)
p3 <- plot_dual("DBSI", "DBSI") + plot_style + labs(y = NULL)
p4 <- plot_dual("AWEInsh", "AWEInsh") + plot_style + labs(y = NULL)
p5 <- plot_dual("IAVI", "IAVI", show_y = TRUE) + plot_style
p6 <- plot_dual("Tair_site", "Air Temperature (°C)") + plot_style + labs(y = NULL)
p7 <- plot_dual("Et", "Evapotranspiration (mm/day)") + plot_style + labs(y = NULL)
p8 <- plot_dual("cumulative_gdd", "Cumulative GDD (°C)") + plot_style + labs(y = NULL)

# -----------------------------
# Combine plots with single legend and global styling
# -----------------------------
LUEbiophysical <- ((p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)) +
  plot_annotation(
    tag_levels = 'A',
    tag_prefix = "",
    theme = theme(
      plot.tag = element_text(size = 18, face = "bold", hjust = 0, vjust = 1),
      plot.tag.position = 'topright'
    )
  ) +
  # Collect legends from all plots
  plot_layout(guides = "collect") & 
  
  # Global legend styling at bottom
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.margin = ggplot2::margin(t = 0.5, unit = "cm"),
    legend.box.margin = ggplot2::margin(0, 0, 0, 0),
    legend.key.width = unit(3.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold")
  )
LUEbiophysical
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
