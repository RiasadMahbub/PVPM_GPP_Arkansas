# =============================================================================
# LIGHT USE EFFICIENCY (LUE) ANALYSIS SCRIPT
# =============================================================================
# Purpose: Analyze relationships between fAPAR, LUE and vegetation indices
#          using different calculation methods (EVI, NDVI, LAI-based)
# LUE unit: gC mol⁻¹ PAR


# Load required libraries
library(ggpubr)        # For enhanced ggplot functionality
library(randomForest)  # For random forest modeling
library(caTools)       # For data splitting tools
library(viridis)       # For color scales
library(ggplot2)       # For data visualization
library(dplyr)         # For data manipulation


# Optional: Load external debugging script
# source("VIMeteoCheck.R")  # For additional diagnostic checks

# =============================================================================
# DATA PREPARATION
# =============================================================================
# === Inspect Join Results ===
# Join site data with satellite-derived VI and meteo data
joined_df <- dplyr::left_join(sitecombineddata, VImeteo20152018combine, 
                              by = "siteyeardate")

# Inspect join results
cat("Unique siteyears in joined data:", toString(unique(joined_df$siteyear)), "\n")
cat("Unique siteyears in original data:", toString(unique(sitecombineddata$siteyear)), "\n")


# =============================================================================
# fAPAR CALCULATION FUNCTIONS
# =============================================================================

# EVI-based fAPAR calculation
calculate_fapar_evi <- function(evi) {
  1.25 * (evi - 0.1)  # Standard EVI to fAPAR conversion
}

# NDVI-based fAPAR calculation
calculate_fapar_ndvi <- function(ndvi) {
  fpar_max <- 0.95
  f_ndvi <- pmin(pmax((ndvi - 0.1) / (0.9 - 0.1), 0), 1)  # NDVI normalization
  fpar_max * f_ndvi
}

# LAI-based fAPAR calculation (Beer-Lambert law)
calculate_fapar_lai <- function(LAI, K = 0.5) {
  1 - exp(-K * LAI)  # K = light extinction coefficient
}


# =============================================================================
# APPLY fAPAR AND LUE CALCULATIONS
# =============================================================================

# EVI-based calculations
joined_df <- joined_df %>%
  mutate(
    fAPAR_evi = calculate_fapar_evi(EVI),
    APAR_evi = fAPAR_evi * PAR_site,
    LUE_evi = GPP_site / APAR_evi
  )

# NDVI-based calculations
joined_df <- joined_df %>%
  mutate(
    fAPAR_ndvi = calculate_fapar_ndvi(NDVI),
    APAR_ndvi = fAPAR_ndvi * PAR_site,
    LUE_ndvi = GPP_site / APAR_ndvi
  )

# LAI-based calculations
joined_df <- joined_df %>%
  mutate(
    fAPAR_lai = calculate_fapar_lai(Lai),
    APAR_lai = fAPAR_lai * PAR_site,
    LUE_lai = GPP_site / APAR_lai
  )

# Primary calculations (using LAI method by default)
joined_df <- joined_df %>%
  mutate(
    fAPAR = fAPAR_lai,
    APAR = APAR_lai,
    LUE = LUE_lai
  )

# =============================================================================
# PREPARE DATA FOR ANALYSIS
# =============================================================================

# Select relevant columns for modeling
rf_data <- joined_df %>%
  select(
    GPP_site, PAR_site, fAPAR, LUE, VPD_site, Tair_site, rH_site,
    kNDVI, NIRv, NDVI, LSWI, nir, sNIRvNDPI, SAVI2, TDVI, Lai,
    DAP, siteyear, Ec, Ei, Es, avgRH, dayl, ppt,
    Variety, DOP, EVI, LUE_ndvi, LUE_lai, LUE_evi, 
    fAPAR_lai, fAPAR_evi, fAPAR_ndvi
  )

# Create complete copy for reference
rf_data_whole <- rf_data
# =============================================================================
# DATA QUALITY CHECKS
# =============================================================================

# Check for problematic LUE values
cat("\nData quality checks:\n")
cat("Rows with fAPAR > 1:", sum(joined_df$fAPAR > 1, na.rm = TRUE), "\n")
cat("Rows with infinite/NA LUE:", sum(!is.finite(joined_df$LUE) | is.na(joined_df$LUE)), "\n")
cat("Rows with LUE > 1:", sum(joined_df$LUE > 1, na.rm = TRUE), "\n")

# View problematic cases
# View(joined_df[joined_df$fAPAR > 1, ])
# View(joined_df[!is.finite(joined_df$LUE) | is.na(joined_df$LUE), ])

# === Exploratory Views ===
View(rf_data[rf_data$fAPAR > 1, ])
View(rf_data[!is.finite(rf_data$LUE) | is.na(rf_data$LUE), ])
View(rf_data[rf_data$LUE > 1, ])

# === Histograms to Explore Distributions ===
hist(rf_data$LUE_evi, main = "Distribution of LUE based on EVI", xlab = "LUE (gC mol⁻¹ photon)", col = "gray", breaks = 20)
hist(rf_data$LUE_ndvi, main = "Distribution of LUE based on NDVI", xlab = "LUE (gC mol⁻¹ photon)", col = "gray", breaks = 20)
hist(rf_data$LUE_lai, main = "Distribution of LUE based on LAI", xlab = "LUE (gC mol⁻¹ photon)", col = "gray", breaks = 20)


# LAI Distributions Based on LUE Threshold
hist(rf_data$fAPAR_evi[rf_data$LUE_evi > 1], main = "fAPAR based on EVI when LUE > 1", xlab = "fAPAR EVI", col = "red", breaks = 20)
hist(rf_data$fAPAR_evi[rf_data$LUE_evi <= 1], main = "fAPAR based on EVI when LUE ≤ 1", xlab = "fAPAR EVI", col = "blue", breaks = 20)

# NDVI Distributions Based on LUE Threshold
hist(rf_data$fAPAR_ndvi[rf_data$LUE_ndvi > 1], main = "fAPAR based on NDVI when LUE > 1", xlab = "fAPAR NDVI", col = "red", breaks = 20)
hist(rf_data$fAPAR_ndvi[rf_data$LUE_ndvi <= 1], main = "fAPAR based onNDVI when LUE ≤ 1", xlab = "fAPAR NDVI", col = "blue", breaks = 20)

# NDVI Distributions Based on LUE Threshold
hist(rf_data$fAPAR_lai[rf_data$LUE_lai > 1], main = "fAPAR based on LAI when LUE > 1", xlab = "fAPAR LAI", col = "red", breaks = 20)
hist(rf_data$fAPAR_lai[rf_data$LUE_lai <= 1], main = "fAPAR based on LAI when LUE ≤ 1", xlab = "fAPAR LAI", col = "blue", breaks = 20)

# =============================================================================
# EXPLORATORY VISUALIZATIONS
# =============================================================================

# Histograms of LUE distributions
hist_plot <- function(data, var, title) {
  ggplot(data, aes(x = .data[[var]])) +
    geom_histogram(bins = 20, fill = "gray", color = "black") +
    labs(title = title, x = "LUE (gC mol⁻¹ photon)", y = "Frequency") +
    theme_minimal()
}

# Generate histograms
hist_plot(rf_data, "LUE_evi", "Distribution of LUE (EVI-based)")
hist_plot(rf_data, "LUE_ndvi", "Distribution of LUE (NDVI-based)")
hist_plot(rf_data, "LUE_lai", "Distribution of LUE (LAI-based)")

# fAPAR distributions conditional on LUE
fapar_hist_plot <- function(data, fapar_var, lue_var, title) {
  ggplot(data, aes(x = .data[[fapar_var]], fill = .data[[lue_var]] > 1)) +
    geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
    scale_fill_manual(values = c("blue", "red")) +
    labs(title = title, x = fapar_var, y = "Frequency") +
    theme_minimal()
}

# Generate conditional histograms
fapar_hist_plot(rf_data, "fAPAR_evi", "LUE_evi", "fAPAR distribution by LUE threshold (EVI)")
fapar_hist_plot(rf_data, "fAPAR_ndvi", "LUE_ndvi", "fAPAR distribution by LUE threshold (NDVI)")
fapar_hist_plot(rf_data, "fAPAR_lai", "LUE_lai", "fAPAR distribution by LUE threshold (LAI)")

# =============================================================================
# TIME SERIES VISUALIZATIONS
# =============================================================================
ggplot(rf_data, aes(x = DAP, y = LUE_evi, color = fAPAR_evi, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  scale_color_viridis(option = "D", direction = -1) +
  theme_minimal() +
  labs(
    color = "fAPAR",
    size = "GPP_site",
    x = "Days After Planting (DAP)",
    y = "Light Use Efficiency (gC mol⁻¹ photon)"
  )   +
  ggtitle("LUE Based on EVI")

ggplot(rf_data, aes(x = DAP, y = LUE_ndvi, color = fAPAR_ndvi, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  scale_color_viridis(option = "D", direction = -1) +
  theme_minimal() +
  labs(
    color = "fAPAR",
    size = "GPP_site",
    x = "Days After Planting (DAP)",
    y = "Light Use Efficiency (gC mol⁻¹ photon)"
  ) +
  ggtitle("LUE Based on NDVI")



ggplot(rf_data, aes(x = DAP, y = LUE_lai, color = fAPAR_lai, size = GPP_site)) +
  geom_point(alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE, color = "black", linetype = "dashed", size = 1) +
  scale_color_viridis(option = "D", direction = -1) +
  theme_minimal() +
  labs(
    color = "fAPAR",
    size = "GPP_site",
    x = "Days After Planting (DAP)",
    y = "Light Use Efficiency (gC mol⁻¹ photon)"
  ) +
  ggtitle("LUE Based on LAI")

# Check for rows where LUE_evi, LUE_ndvi, and LUE_lai are infinite or NA
lue_evi_infinite_na_count <- sum(!is.finite(rf_data$LUE_evi) | is.na(rf_data$LUE_evi))
lue_ndvi_infinite_na_count <- sum(!is.finite(rf_data$LUE_ndvi) | is.na(rf_data$LUE_ndvi))
lue_lai_infinite_na_count <- sum(!is.finite(rf_data$LUE_lai) | is.na(rf_data$LUE_lai))

# Display the results
print(paste("Number of rows with infinite or NA values in LUE_evi:", lue_evi_infinite_na_count))
print(paste("Number of rows with infinite or NA values in LUE_ndvi:", lue_ndvi_infinite_na_count))
print(paste("Number of rows with infinite or NA values in LUE_lai:", lue_lai_infinite_na_count))


# View rows with infinite or NA values in LUE_evi, LUE_ndvi, and LUE_lai
View(rf_data[!is.finite(rf_data$LUE_evi) | is.na(rf_data$LUE_evi), ])
View(rf_data[!is.finite(rf_data$LUE_ndvi) | is.na(rf_data$LUE_ndvi), ])
View(rf_data[!is.finite(rf_data$LUE_lai) | is.na(rf_data$LUE_lai), ])


# =============================================================================
# RANDOM FOREST MODELING
# =============================================================================
# =============================================================================
# RANDOM FOREST MODELING (EVI, NDVI, LAI) - Cleaned version
# =============================================================================

library(dplyr)
library(ggplot2)
library(randomForest)

# Define the RF importance function once (no extra functions requested)
run_rf_importance <- function(seed_base, data, response_var) {
  importance_list <- list()
  for (i in 1:30) {
    set.seed(seed_base + i)
    formula <- as.formula(paste(response_var, "~ ."))
    rf_model <- randomForest(
      formula, 
      data = data, 
      ntree = 100,
      importance = TRUE
    )
    importance_list[[i]] <- importance(rf_model, type = 1)
  }
  importance_array <- simplify2array(importance_list)
  mean_importance <- apply(importance_array, 1:2, mean)
  df <- as.data.frame(mean_importance)
  df$Variable <- rownames(df)
  df <- df[order(-df$`%IncMSE`), ]
  return(df)
}

# --- EVI-BASED LUE MODELING ---
rf_data_evi <- rf_data %>% dplyr::filter(is.finite(LUE_evi), !is.na(LUE_evi))

train_siteyears <- c("USHRA2016", "USOF22017", "USOF12017", "USHRA2015", 
                     "USBDA2016", "USBDC2015", "USBDC2016", "USHRA2017",
                     "USHRC2015", "USHRC2016", "USOF62018", "USOF52018")

train_evi <- rf_data_evi %>% dplyr::filter(siteyear %in% train_siteyears)

train_evi_model <- train_evi %>% 
  dplyr::select(-c(siteyear, GPP_site, PAR_site, fAPAR, LUE_ndvi, LUE_lai, LUE_ndvi,
            fAPAR_lai, fAPAR_evi, fAPAR_ndvi, LUE))

imp100 <- run_rf_importance(100, train_evi_model, "LUE_evi")
imp200 <- run_rf_importance(200, train_evi_model, "LUE_evi")
imp300 <- run_rf_importance(300, train_evi_model, "LUE_evi")
imp400 <- run_rf_importance(400, train_evi_model, "LUE_evi")

df100 <- imp100 %>% mutate(Seed = "Seed 100")
df200 <- imp200 %>% mutate(Seed = "Seed 200")
df300 <- imp300 %>% mutate(Seed = "Seed 300")
df400 <- imp400 %>% mutate(Seed = "Seed 400")

combined_evi <- bind_rows(df100, df200, df300, df400)

ordered_vars_evi <- combined_evi %>%
  group_by(Variable) %>%
  summarise(meanImp = mean(`%IncMSE`)) %>%
  arrange(meanImp) %>%
  pull(Variable)

combined_evi$Variable <- factor(combined_evi$Variable, levels = ordered_vars_evi)

ggplot(combined_evi, aes(x = `%IncMSE`, y = Variable, fill = Seed)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "% Increase in MSE", y = NULL, title = "Variable Importance across Seeds 100–400 for LUE based on EVI") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")



# --- NDVI-BASED LUE MODELING ---
rf_data_ndvi <- rf_data %>% dplyr::filter(is.finite(LUE_ndvi), !is.na(LUE_ndvi))

train_ndvi <- rf_data_ndvi %>% dplyr::filter(siteyear %in% train_siteyears)

train_ndvi_model <- train_ndvi %>%
  dplyr::select(-c(siteyear, GPP_site, PAR_site, fAPAR, LUE_lai, LUE_evi,
            fAPAR_lai, fAPAR_evi, fAPAR_ndvi, LUE, NDVI))

imp100 <- run_rf_importance(100, train_ndvi_model, "LUE_ndvi")
imp200 <- run_rf_importance(200, train_ndvi_model, "LUE_ndvi")
imp300 <- run_rf_importance(300, train_ndvi_model, "LUE_ndvi")
imp400 <- run_rf_importance(400, train_ndvi_model, "LUE_ndvi")

df100 <- imp100 %>% mutate(Seed = "Seed 100")
df200 <- imp200 %>% mutate(Seed = "Seed 200")
df300 <- imp300 %>% mutate(Seed = "Seed 300")
df400 <- imp400 %>% mutate(Seed = "Seed 400")

combined_ndvi <- bind_rows(df100, df200, df300, df400)

ordered_vars_ndvi <- combined_ndvi %>%
  group_by(Variable) %>%
  summarise(meanImp = mean(`%IncMSE`)) %>%
  arrange(meanImp) %>%
  pull(Variable)

combined_ndvi$Variable <- factor(combined_ndvi$Variable, levels = ordered_vars_ndvi)

ggplot(combined_ndvi, aes(x = `%IncMSE`, y = Variable, fill = Seed)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "% Increase in MSE", y = NULL, title = "Variable Importance across Seeds 100–400 for LUE based on NDVI") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")



# --- LAI-BASED LUE MODELING ---
rf_data_lai <- rf_data %>% dplyr::filter(is.finite(LUE_lai), !is.na(LUE_lai))

train_lai <- rf_data_lai %>% dplyr::filter(siteyear %in% train_siteyears)

train_lai_model <- train_lai %>% 
  dplyr::select(-c(siteyear, GPP_site, PAR_site, fAPAR, LUE_evi, Lai, LUE_ndvi,
            fAPAR_lai, fAPAR_evi, fAPAR_ndvi, LUE, DOP))

imp100 <- run_rf_importance(100, train_lai_model, "LUE_lai")
imp200 <- run_rf_importance(200, train_lai_model, "LUE_lai")
imp300 <- run_rf_importance(300, train_lai_model, "LUE_lai")
imp400 <- run_rf_importance(400, train_lai_model, "LUE_lai")

df100 <- imp100 %>% mutate(Seed = "Seed 100")
df200 <- imp200 %>% mutate(Seed = "Seed 200")
df300 <- imp300 %>% mutate(Seed = "Seed 300")
df400 <- imp400 %>% mutate(Seed = "Seed 400")

combined_lai <- bind_rows(df100, df200, df300, df400)

ordered_vars_lai <- combined_lai %>%
  group_by(Variable) %>%
  summarise(meanImp = mean(`%IncMSE`)) %>%
  arrange(meanImp) %>%
  pull(Variable)

combined_lai$Variable <- factor(combined_lai$Variable, levels = ordered_vars_lai)

ggplot(combined_lai, aes(x = `%IncMSE`, y = Variable, fill = Seed)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "% Increase in MSE", y = NULL, title = "Variable Importance across Seeds 100–400 for LUE based on LAI") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

# =============================================================================
# END OF ANALYSIS
# =============================================================================


library(dplyr)

# Get unique site-years where ANY LUE > 1.5
high_lue_siteyears <- joined_df %>%
  dplyr::filter(LUE > 1.5) %>%
  distinct(siteyear) %>%
  pull(siteyear)

# Print results
cat("Site-years with LUE > 1.5:", paste(high_lue_siteyears, collapse = ", "), "\n")

# Count occurrences (optional)
high_lue_counts <- joined_df %>%
  dplyr::filter(LUE > 1.5) %>%
  count(siteyear, name = "Count") %>%
  arrange(desc(Count))

print(high_lue_counts)

library(ggplot2)
library(dplyr)

# Define the siteyears of interest
target_siteyears <- c(
  "USOF22017", "USBDA2016", "USBDC2016", "USHRA2017", "USOF32017"
)

# Base font size (adjust as needed)
base_font_size <- 12

# Loop through each siteyear and create/save plots
for (siteyear in target_siteyears) {
  
  # Filter data for the current siteyear
  site_data <- rf_data %>% 
    dplyr::filter(siteyear == !!siteyear)
  
  # Skip if no data exists
  if (nrow(site_data) == 0) next
  
  # Create the plot
  LUEtimeseries <- ggplot(site_data, aes(x = cumulative_IAVI, y = LUE, color = PAR_site, size = GPP_site)) +
    geom_point(alpha = 0.8) +
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, 
                color = "black", linetype = "dashed", size = 1) +
    scale_color_viridis(option = "D", direction = -1) +
    theme_minimal(base_size = base_font_size) +
    labs(
      color = "PAR_site",  
      size = expression("GPP (gC m"^{-2}~"d"^{-1}~")"),  
      x = "cumulative_IAVI",  
      y = expression("Light Use Efficiency (gC mol"^{-1}~"photon)"),
      title = paste("Site-Year:", siteyear)  # Add title for clarity
    )
  
  # Save the plot
  ggsave(
    filename = paste0("LUE_", siteyear, ".png"),
    plot = LUEtimeseries, 
    path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
    width = 8, 
    height = 6, 
    dpi = 300
  )
  
  cat("Saved plot for:", siteyear, "\n")
}

# Define the siteyears of interest
target_siteyears <- c(
  "USHRA2016", "USHRA2017", "USHRC2017", "USBDA2015", "USOF42018"
)
# Base font size (adjust as needed)
base_font_size <- 12

# Loop through each siteyear and create/save plots
for (siteyear in target_siteyears) {
  
  # Filter data for the current siteyear
  site_data <- rf_data %>% 
    dplyr::filter(siteyear == !!siteyear)
  
  # Skip if no data exists
  if (nrow(site_data) == 0) next
  
  # Create the plot
  LUEtimeseries <- ggplot(site_data, aes(x = nir, y = LUE, color = PAR_site, size = GPP_site)) +
    geom_point(alpha = 0.8) +
    geom_smooth(aes(group = 1), method = "loess", se = FALSE, 
                color = "black", linetype = "dashed", size = 1) +
    scale_color_viridis(option = "D", direction = -1) +
    theme_minimal(base_size = base_font_size) +
    labs(
      color = "PAR_site",  
      size = expression("GPP (gC m"^{-2}~"d"^{-1}~")"),  
      x = "nir",  
      y = expression("Light Use Efficiency (gC mol"^{-1}~"photon)"),
      title = paste("Site-Year:", siteyear)  # Add title for clarity
    )
  
  # Save the plot
  ggsave(
    filename = paste0("LUE_testval", siteyear, ".png"),
    plot = LUEtimeseries, 
    path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure",
    width = 8, 
    height = 6, 
    dpi = 300
  )
  
  cat("Saved plot for:", siteyear, "\n")
}


