# ===================================================
# R Code for Analyzing LUE and GPP Prediction Metrics
# ===================================================

# Load required libraries
library(Metrics)
library(dplyr)

# =======================================
# 1. Load and Prepare Data
# =======================================

# Assumes rf_data contains the following columns:
# - rf_data$LUE              : Observed Light Use Efficiency
# - rf_data$LUEpredicted     : Predicted Light Use Efficiency
# - rf_data$GPP_site         : Observed Gross Primary Production
# - rf_data$GPP_predicted    : Predicted Gross Primary Production
# - rf_data$cumulative_gdd   : Cumulative Growing Degree Days
# - rf_data$siteyear         : Site-Year Identifier

# =======================================
# 2. Average LUE in Broad GDD Bins
# =======================================

# Define GDD breaks for broad bins
broad_breaks <- c(0, 500, 1000, 1500, 2000)
rf_data$gdd_bin_broad <- cut(
  rf_data$cumulative_gdd,
  breaks = broad_breaks,
  right = FALSE,
  labels = paste(head(broad_breaks, -1), broad_breaks[-1] - 1, sep = "–")
)

# Compute mean LUE in each bin
lue_means_broad <- tapply(rf_data$LUE, rf_data$gdd_bin_broad, mean, na.rm = TRUE)

# Print mean LUEs
cat("\n--- Average LUE for Broad GDD Bins ---\n")
for (i in seq_along(lue_means_broad)) {
  gdd_range <- names(lue_means_broad)[i]
  cat(sprintf("The average LUE of %s \u2103 cumulative GDD is %.4f (gC mol\u207B\u00B9 photon)\n",
              gdd_range, lue_means_broad[i]))
}

# =======================================
# 3. Absolute Maximum and Minimum LUE
# =======================================

max_lue <- max(rf_data$LUE, na.rm = TRUE)
max_lue_gdd <- rf_data$cumulative_gdd[which.max(rf_data$LUE)]
min_lue <- min(rf_data$LUE, na.rm = TRUE)
min_lue_gdd <- rf_data$cumulative_gdd[which.min(rf_data$LUE)]

cat("\n--- Absolute Maximum and Minimum LUE ---\n")
cat(sprintf("The maximum LUE is %.4f (gC mol\u207B\u00B9 photon) at cumulative GDD of %.2f \u2103\n", 
            max_lue, max_lue_gdd))
cat(sprintf("The minimum LUE is %.4f (gC mol\u207B\u00B9 photon) at cumulative GDD of %.2f \u2103\n", 
            min_lue, min_lue_gdd))

# =======================================
# 4. Average LUE in Fine GDD Bins (10 GDD)
# =======================================

fine_breaks <- seq(0, 2000, by = 10)
fine_labels <- paste(head(fine_breaks, -1), fine_breaks[-1] - 1, sep = "–")

rf_data$gdd_bin_fine <- cut(
  rf_data$cumulative_gdd,
  breaks = fine_breaks,
  include.lowest = TRUE,
  right = FALSE,
  labels = fine_labels
)

binwise_avg_lue <- c()
bin_labels <- c()

for (bin in levels(rf_data$gdd_bin_fine)) {
  subset_data <- rf_data[rf_data$gdd_bin_fine == bin & !is.na(rf_data$LUE), ]
  if (nrow(subset_data) > 0) {
    binwise_avg_lue <- c(binwise_avg_lue, mean(subset_data$LUE, na.rm = TRUE))
    bin_labels <- c(bin_labels, bin)
  }
}

# =======================================
# 5. Max & Min Average LUE in Fine Bins
# =======================================
max_avg_lue <- max(binwise_avg_lue)
min_avg_lue <- min(binwise_avg_lue)
max_bin <- bin_labels[which.max(binwise_avg_lue)]
min_bin <- bin_labels[which.min(binwise_avg_lue)]

cat("\n--- Maximum and Minimum Average LUE in Fine GDD Bins ---\n")
cat(sprintf("The average maximum LUE is %.4f (gC mol\u207B\u00B9 photon) in GDD bin %s\n", 
            max_avg_lue, max_bin))
cat(sprintf("The average minimum LUE is %.4f (gC mol\u207B\u00B9 photon) in GDD bin %s\n", 
            min_avg_lue, min_bin))

# =======================================
# 6. Define Site-Year Groups
# =======================================
library(Metrics)
library(dplyr)

train_siteyears <- c("USOF22017", "USOF12017", "USBDA2016", "USBDC2016", 
                     "USHRC2016", "USOF62018", "USOF52018", "USHRC2015", 
                     "USHRA2015", "USBDC2015", "USOF32017")

test_siteyears <- c("USHRC2017", "USBDA2015", "USOF42018", 
                    "USHRA2016", "USHRA2017")

# Function to compute and print model metrics
print_metrics <- function(data, obs_col, pred_col, label, var_name) {
  obs <- data[[obs_col]]
  pred <- data[[pred_col]]
  
  r2 <- round(cor(obs, pred)^2, 2)
  rmse_val <- round(rmse(obs, pred), 2)
  mae_val <- round(mae(obs, pred), 2)
  bias_val <- round(mean(pred - obs), 3)
  
  cat(sprintf(
    "\nThe RMSE, MAE, R², and Bias of %s in the %s were %.2f, %.2f, %.2f, and %.3f respectively.\n",
    var_name, label, rmse_val, mae_val, r2, bias_val
  ))
}

evaluate_all_sets <- function(data) {
  sets <- list(
    "entire dataset" = data,
    "training dataset" = dplyr::filter(data, siteyear %in% train_siteyears),
    "testing dataset" = dplyr::filter(data, siteyear %in% test_siteyears)
  )
  
  for (label in names(sets)) {
    subset <- sets[[label]]
    print_metrics(subset, "GPP_site", "GPP_predicted", label, "GPP")
    print_metrics(subset, "LUE", "LUEpredicted", label, "LUE")
  }
}

# Run evaluation
cat("\n--- Model Evaluation Metrics ---\n")
evaluate_all_sets(rf_data)


##########################
# Calculate and print R² values between each VI and GPP_site
for (vi in vi_list) {
  if (vi %in% names(rf_data)) {
    cor_test <- cor.test(rf_data[[vi]], rf_data$GPP_site, use = "complete.obs")
    r_squared <- round(cor_test$estimate^2, 3)
    cat(sprintf("The R² value between GPP and %s across the available dataset was %.2f.\n", vi, r_squared))
  }
}



# Assuming your data is stored in metrics_data

# Define model names based on column names
models <- list(
  "GPP_predicted" = c("R2_GPP_predicted", "MAE_GPP_predicted", "Bias_GPP_predicted"),
  "VPM_EVI" = c("R2_GPPpredictedVPM_EVI", "MAE_GPPpredictedVPM_EVI", "Bias_GPPpredictedVPM_EVI"),
  "VI" = c("R2_GPP_predicted_VI", "MAE_GPP_predicted_VI", "Bias_GPP_predicted_VI")
)

# Loop through each site-year
for (i in 1:nrow(metrics_data)) {
  site <- metrics_data$siteyear[i]
  cat("\nPerformance metrics for", site, ":\n")
  cat("--------------------------------\n")
  
  # Loop through each model
  for (model_name in names(models)) {
    cols <- models[[model_name]]
    r2 <- metrics_data[i, cols[1]]
    mae <- metrics_data[i, cols[2]]
    bias <- metrics_data[i, cols[3]]
    
    cat(sprintf("%s model: R2 = %.3f, MAE = %.3f, Bias = %.3f\n", 
                model_name, r2, mae, bias))
  }
}

#====================================================
##LUE features
#====================================================
# Function to extract and format correlation results
print_cor_results <- function(xvar, xlab) {
  # Get clean data
  required_cols <- c(xvar, "LUE", "LUEpredicted")
  plot_data <- rf_data[complete.cases(rf_data[, required_cols]), ]
  
  # Calculate correlations
  tau_lue <- round(calc_kendall(plot_data[[xvar]], plot_data$LUE), 2)
  r_lue <- round(calc_pearson(plot_data[[xvar]], plot_data$LUE), 2)
  tau_pred <- round(calc_kendall(plot_data[[xvar]], plot_data$LUEpredicted), 2)
  r_pred <- round(calc_pearson(plot_data[[xvar]], plot_data$LUEpredicted), 2)
  
  # Format results
  cat(sprintf(
    "For %s:\n  Observed LUE: τ = %.2f, R = %.2f\n  Predicted LUE: τ = %.2f, R = %.2f\n\n",
    xlab, tau_lue, r_lue, tau_pred, r_pred
  ))
}

# Print results for all 8 features
print_cor_results("VPD_site", "VPD (kPa)")
print_cor_results("Es", "Es")

print_cor_results("rH_site", "Relative Humidity (%)")
print_cor_results("GDVI", "GDVI")

print_cor_results("nir", "NIR")
print_cor_results("MLSWI26", "MLSWI26")
print_cor_results("Tair_site", "Air Temperature (°C)")
print_cor_results("cumulative_gdd", "Cumulative GDD (°C)")

#======================================================
# Text of the Temporal patterns of the model
#======================================================
# Calculate and print averages with standard deviations
max_gpp_results <- rf_data %>%
  group_by(siteyear) %>%
  summarise(
    # GPP_site
    max_GPP_site = max(GPP_site, na.rm = TRUE),
    cumulative_gdd_max_GPP_site = cumulative_gdd[which.max(GPP_site)],
    
    # GPP_predicted
    max_GPP_predicted = max(GPP_predicted, na.rm = TRUE),
    cumulative_gdd_max_GPP_predicted = cumulative_gdd[which.max(GPP_predicted)],
    
    # GPPpredictedVPM_EVI
    max_GPPpredictedVPM_EVI = max(GPPpredictedVPM_EVI, na.rm = TRUE),
    cumulative_gdd_max_GPPpredictedVPM_EVI = cumulative_gdd[which.max(GPPpredictedVPM_EVI)],
    
    # GPP_predicted_VI
    max_GPP_predicted_VI = max(GPP_predicted_VI, na.rm = TRUE),
    cumulative_gdd_max_GPP_predicted_VI = cumulative_gdd[which.max(GPP_predicted_VI)]
  ) %>%
  ungroup()
avg_results <- max_gpp_results %>%
  summarise(
    avg_max_GPP_site = mean(max_GPP_site, na.rm = TRUE),
    sd_max_GPP_site = sd(max_GPP_site, na.rm = TRUE),
    avg_cumgdd_max_GPP_site = mean(cumulative_gdd_max_GPP_site, na.rm = TRUE),
    sd_cumgdd_max_GPP_site = sd(cumulative_gdd_max_GPP_site, na.rm = TRUE),
    n_sites_GPP_site = sum(!is.na(max_GPP_site)),
    
    avg_max_GPP_predicted = mean(max_GPP_predicted, na.rm = TRUE),
    sd_max_GPP_predicted = sd(max_GPP_predicted, na.rm = TRUE),
    avg_cumgdd_max_GPP_predicted = mean(cumulative_gdd_max_GPP_predicted, na.rm = TRUE),
    sd_cumgdd_max_GPP_predicted = sd(cumulative_gdd_max_GPP_predicted, na.rm = TRUE),
    n_sites_GPP_predicted = sum(!is.na(max_GPP_predicted)),
    
    avg_max_GPPpredictedVPM_EVI = mean(max_GPPpredictedVPM_EVI, na.rm = TRUE),
    sd_max_GPPpredictedVPM_EVI = sd(max_GPPpredictedVPM_EVI, na.rm = TRUE),
    avg_cumgdd_max_GPPpredictedVPM_EVI = mean(cumulative_gdd_max_GPPpredictedVPM_EVI, na.rm = TRUE),
    sd_cumgdd_max_GPPpredictedVPM_EVI = sd(cumulative_gdd_max_GPPpredictedVPM_EVI, na.rm = TRUE),
    n_sites_GPPpredictedVPM_EVI = sum(!is.na(max_GPPpredictedVPM_EVI)),
    
    avg_max_GPP_predicted_VI = mean(max_GPP_predicted_VI, na.rm = TRUE),
    sd_max_GPP_predicted_VI = sd(max_GPP_predicted_VI, na.rm = TRUE),
    avg_cumgdd_max_GPP_predicted_VI = mean(cumulative_gdd_max_GPP_predicted_VI, na.rm = TRUE),
    sd_cumgdd_max_GPP_predicted_VI = sd(cumulative_gdd_max_GPP_predicted_VI, na.rm = TRUE),
    n_sites_GPP_predicted_VI = sum(!is.na(max_GPP_predicted_VI))
  )


cat("\n=== AVERAGE ACROSS ALL SITE-YEARS (MEAN ± SD) ===\n")
cat(sprintf("GPP_site:         Avg max = %6.2f ± %.2f gC/m²/day at GDD %5.1f ± %.1f (n=%d)\n", 
            avg_results$avg_max_GPP_site,
            avg_results$sd_max_GPP_site,
            avg_results$avg_cumgdd_max_GPP_site,
            avg_results$sd_cumgdd_max_GPP_site,
            avg_results$n_sites_GPP_site))

cat(sprintf("GPP_predicted:    Avg max = %6.2f ± %.2f gC/m²/day at GDD %5.1f ± %.1f (n=%d)\n", 
            avg_results$avg_max_GPP_predicted,
            avg_results$sd_max_GPP_predicted,
            avg_results$avg_cumgdd_max_GPP_predicted,
            avg_results$sd_cumgdd_max_GPP_predicted,
            avg_results$n_sites_GPP_predicted))

cat(sprintf("GPPpredictedVPM:  Avg max = %6.2f ± %.2f gC/m²/day at GDD %5.1f ± %.1f (n=%d)\n", 
            avg_results$avg_max_GPPpredictedVPM_EVI,
            avg_results$sd_max_GPPpredictedVPM_EVI,
            avg_results$avg_cumgdd_max_GPPpredictedVPM_EVI,
            avg_results$sd_cumgdd_max_GPPpredictedVPM_EVI,
            avg_results$n_sites_GPPpredictedVPM_EVI))

cat(sprintf("GPP_predicted_VI: Avg max = %6.2f ± %.2f gC/m²/day at GDD %5.1f ± %.1f (n=%d)\n", 
            avg_results$avg_max_GPP_predicted_VI,
            avg_results$sd_max_GPP_predicted_VI,
            avg_results$avg_cumgdd_max_GPP_predicted_VI,
            avg_results$sd_cumgdd_max_GPP_predicted_VI,
            avg_results$n_sites_GPP_predicted_VI))

cat("\n=== PEAK GPP AND GDD PER SITE-YEAR ===\n")
library(glue)

max_gpp_results %>%
  select(siteyear,
         max_GPP_site, cumulative_gdd_max_GPP_site,
         max_GPP_predicted, cumulative_gdd_max_GPP_predicted,
         max_GPPpredictedVPM_EVI, cumulative_gdd_max_GPPpredictedVPM_EVI,
         max_GPP_predicted_VI, cumulative_gdd_max_GPP_predicted_VI) %>%
  rowwise() %>%
  mutate(summary_text = glue(
    "Siteyear: {siteyear}
  - GPP_site:         Max = {max_GPP_site} at GDD = {cumulative_gdd_max_GPP_site}
  - GPP_predicted:    Max = {max_GPP_predicted} at GDD = {cumulative_gdd_max_GPP_predicted}
  - GPP_VPM_EVI:      Max = {max_GPPpredictedVPM_EVI} at GDD = {cumulative_gdd_max_GPPpredictedVPM_EVI}
  - GPP_predicted_VI: Max = {max_GPP_predicted_VI} at GDD = {cumulative_gdd_max_GPP_predicted_VI}"
  )) %>%
  pull(summary_text) %>%
  cat(sep = "\n\n")


