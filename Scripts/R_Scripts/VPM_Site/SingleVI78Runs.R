# Load necessary libraries (ensure these are installed: install.packages(c("dplyr", "purrr", "ggplot2", "Metrics", "hydroGOF", "tidyr", "progressr")))
library(dplyr)
library(purrr)
library(ggplot2)
library(Metrics)
library(hydroGOF)
library(tidyr) # For pivot_longer
library(progressr) # For progress bar

# ============================================================================
# IMPORTANT: Assume 'joined_df' is already loaded and available in your R environment.
# Example: joined_df <- read.csv("path/to/your/data.csv")
# Make sure 'joined_df' contains columns like 'siteyear', 'GPP_site', 'PAR_site',
# and all VIs listed in 'vi_list', including 'IAVI'.
# ============================================================================

# 1. DATA PREPARATION ========================================================
# User-specified subset of VIs
vi_list <- c("IAVI", "VARI", "NDVI", "RNDVI", "TSAVI", "kNDVI", "ATSAVI", "EVI")

# 2. CREATE DATA SPLITS ======================================================
original_train_siteyears <- c("USBDA2016", "USBDC2016", "USOF22017", "USHRC2016",
                              "USOF62018", "USOF52018", "USHRC2015", "USHRA2015",
                              "USBDC2015", "USOF32017")
original_val_siteyears <- c("USOF12017", "USHRA2016", "USHRA2017")
original_test_siteyears <- c("USHRC2017", "USBDA2015", "USOF42018")

train_val_siteyears <- c(original_train_siteyears, original_val_siteyears)
test_siteyears <- original_test_siteyears

# Generate all combinations of 3 site-years for inner cross-validation
cv_inner_folds <- combn(train_val_siteyears, 3, simplify = FALSE)

# 3. MODEL EVALUATION FUNCTIONS ==============================================

#' Evaluate a linear model's performance.
#'
#' @param model A fitted linear model object.
#' @param df A dataframe containing the actual and predicted values.
#' @return A list of performance metrics (R2, MAE, RMSE, Bias, NSE).
evaluate_model <- function(model, df) {
  y <- df$GPP_site
  y_pred <- predict(model, newdata = df)
  
  # Filter out NA values from both observed and predicted simultaneously
  # This ensures both vectors have the same length and no NA pairs.
  valid_indices <- complete.cases(y, y_pred)
  y_filtered <- y[valid_indices]
  y_pred_filtered <- y_pred[valid_indices]
  
  # Handle cases with insufficient data for meaningful metrics after filtering
  if (length(y_filtered) < 2 || length(unique(y_filtered)) < 2) {
    return(list(R2 = NA, MAE = NA, RMSE = NA, Bias = NA, NSE = NA))
  }
  
  ss_res <- sum((y_filtered - y_pred_filtered)^2)
  ss_tot <- sum((y_filtered - mean(y_filtered, na.rm = TRUE))^2)
  r2 <- if (ss_tot == 0) 1 else 1 - ss_res / ss_tot
  mae <- Metrics::mae(y_filtered, y_pred_filtered)
  rmse <- Metrics::rmse(y_filtered, y_pred_filtered)
  bias <- mean(y_pred_filtered - y_filtered, na.rm = TRUE)
  # Calculate NSE, explicitly converting to vector to satisfy hydroGOF::NSE requirements
  nse <- hydroGOF::NSE(sim = as.vector(y_pred_filtered), obs = as.vector(y_filtered))
  
  list(R2 = r2, MAE = mae, RMSE = rmse, Bias = bias, NSE = nse)
}

#' Evaluate a single vegetation index using nested cross-validation.
#'
#' @param selected_vi The name of the vegetation index to evaluate (as a string).
#' @return A list containing detailed inner CV results, summary test metrics,
#'         and test predictions for the given VI.
evaluate_vi <- function(selected_vi) {
  cat("\n===================================================\n")
  cat("Evaluating VI:", selected_vi, "\n")
  cat("===================================================\n")
  
  # Prepare data for the current VI
  model_df <- joined_df %>%
    dplyr::filter(!is.na(GPP_site), !is.na(PAR_site), !is.na(.data[[selected_vi]])) %>%
    dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site) # Create interaction term
  
  # Initialize empty tibbles for results to prevent errors if data is insufficient
  # This ensures map_dfr and bind_rows always receive a data frame structure.
  empty_detailed_results <- tibble(Fold = integer(),
                                   Train_R2 = double(), Train_MAE = double(), Train_RMSE = double(), Train_Bias = double(), Train_NSE = double(),
                                   Val_R2 = double(), Val_MAE = double(), Val_RMSE = double(), Val_Bias = double(), Val_NSE = double(),
                                   VI = character())
  empty_test_predictions <- tibble(siteyear = character(), GPP_site = double(), Predicted = double(), VI_used = character())
  
  # Check for sufficient data
  if (nrow(model_df) < 20) {
    cat("Insufficient data for", selected_vi, "- skipping\n")
    # Return empty tibbles for detailed and test_predictions, and NULL for summary
    return(list(detailed = empty_detailed_results, summary = NULL, test_predictions = empty_test_predictions))
  }
  
  # Separate test data
  test_data <- model_df %>% dplyr::filter(siteyear %in% test_siteyears)
  if (nrow(test_data) == 0) {
    cat("No test data found for", selected_vi, "- skipping test evaluation.\n")
    # Return empty tibble for test_predictions, and NULL for summary
    # detailed results might still be present if inner CV could run
    return(list(detailed = empty_detailed_results, summary = NULL, test_predictions = empty_test_predictions))
  }
  
  cat("  Starting inner cross-validation...\n")
  # Perform inner cross-validation
  fold_results <- map_dfr(seq_along(cv_inner_folds), function(i) {
    val_sy <- cv_inner_folds[[i]]
    train_sy <- setdiff(train_val_siteyears, val_sy)
    
    train_data <- model_df %>% dplyr::filter(siteyear %in% train_sy)
    val_data <- model_df %>% dplyr::filter(siteyear %in% val_sy)
    
    # Check for sufficient data within each fold
    if (nrow(train_data) < 20 || nrow(val_data) < 5) {
      cat(sprintf("    Fold %d: Insufficient data (Train: %d, Val: %d) - returning NA for this fold.\n",
                  i, nrow(train_data), nrow(val_data)))
      return(tibble(Fold = i,
                    Train_R2 = NA, Train_MAE = NA, Train_RMSE = NA, Train_Bias = NA, Train_NSE = NA,
                    Val_R2 = NA, Val_MAE = NA, Val_RMSE = NA, Val_Bias = NA, Val_NSE = NA))
    }
    
    # Train model on training data
    model <- lm(GPP_site ~ VI_PAR, data = train_data)
    
    # Evaluate on training and validation data for logging
    train_metrics <- evaluate_model(model, train_data)
    val_metrics <- evaluate_model(model, val_data)
    
    cat(sprintf("    Fold %d\n", i))
    cat(sprintf("      Train - R2: %.3f, MAE: %.3f, RMSE: %.3f, NSE: %.3f\n",
                train_metrics$R2, train_metrics$MAE, train_metrics$RMSE, train_metrics$NSE))
    cat(sprintf("      Val   - R2: %.3f, MAE: %.3f, RMSE: %.3f, NSE: %.3f\n\n",
                val_metrics$R2, val_metrics$MAE, val_metrics$RMSE, val_metrics$NSE))
    
    # Return both training and validation metrics for the current fold
    tibble(Fold = i,
           Train_R2 = train_metrics$R2,
           Train_MAE = train_metrics$MAE,
           Train_RMSE = train_metrics$RMSE,
           Train_Bias = train_metrics$Bias,
           Train_NSE = train_metrics$NSE,
           Val_R2 = val_metrics$R2,
           Val_MAE = val_metrics$MAE,
           Val_RMSE = val_metrics$RMSE,
           Val_Bias = val_metrics$Bias,
           Val_NSE = val_metrics$NSE)
  }) %>%
    # Add the VI column to the detailed fold results
    mutate(VI = selected_vi)
  
  cat("  Training final model on all", length(train_val_siteyears), "training/validation site-years...\n")
  # Train final model on all training/validation data
  full_train_data <- model_df %>% dplyr::filter(siteyear %in% train_val_siteyears)
  final_model <- lm(GPP_site ~ VI_PAR, data = full_train_data)
  
  # Evaluate final model on the independent test set
  test_metrics <- evaluate_model(final_model, test_data)
  
  # Generate predictions for the test set
  test_predictions <- test_data %>%
    mutate(Predicted = predict(final_model, newdata = test_data),
           VI_used = selected_vi) %>% # Add VI_used for plotting
    select(siteyear, GPP_site, Predicted, VI_used)
  
  list(detailed = fold_results, # Inner CV results
       summary = test_metrics,  # Final test set metrics (now includes NSE)
       test_predictions = test_predictions) # Predictions on test set
}

# 4. RUN EVALUATION FOR ALL VIS ==============================================
results_list <- list()
start_time <- Sys.time() # Start timer here
progressr::with_progress({
  p <- progressr::progressor(along = vi_list)
  for (vi in vi_list) {
    p(sprintf("Evaluating %s", vi))
    results_list[[vi]] <- evaluate_vi(vi)
  }
})

# 5. Save or Process Results =================================================
cat("\nSimulation finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
elapsed_time <- Sys.time() - start_time
cat("Elapsed time:", elapsed_time, "\n")

# --- Combine and present results ---

# Summary table of final test set performance (sorted by R2 descending)
cat("\n--- Summary of Final Test Set Performance (per VI) ---\n")
summary_df <- purrr::map_dfr(results_list, function(x) x$summary, .id = "VI") %>%
  arrange(desc(R2))
print(summary_df)

# Combine all detailed inner cross-validation results into one dataframe
cat("\n--- Detailed Inner Cross-Validation Results (all VIs and Folds) ---\n")
all_cv_fold_results_df <- purrr::map_dfr(results_list, ~ .x$detailed) %>%
  # Filter out rows where all metrics are NA (due to insufficient data in a fold)
  dplyr::filter(!(is.na(Val_R2) & is.na(Val_MAE) & is.na(Val_RMSE) & is.na(Val_Bias) & is.na(Val_NSE)))

print(all_cv_fold_results_df)

# Optional: Calculate average metrics for inner CV per VI (Validation)
cat("\n--- Average Inner Cross-Validation Metrics (per VI) ---\n")
avg_val_metrics_df <- all_cv_fold_results_df %>%
  group_by(VI) %>%
  summarise(
    Avg_R2 = mean(Val_R2, na.rm = TRUE),
    Avg_MAE = mean(Val_MAE, na.rm = TRUE),
    Avg_RMSE = mean(Val_RMSE, na.rm = TRUE),
    Avg_Bias = mean(Val_Bias, na.rm = TRUE),
    Avg_NSE = mean(Val_NSE, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_R2))
print(avg_val_metrics_df)

# New: Calculate average training metrics for inner CV per VI
cat("\n--- Average Training Metrics (per VI) from Inner Cross-Validation ---\n")
avg_train_metrics_df <- all_cv_fold_results_df %>%
  group_by(VI) %>%
  summarise(
    Avg_R2 = mean(Train_R2, na.rm = TRUE),
    Avg_MAE = mean(Train_MAE, na.rm = TRUE),
    Avg_RMSE = mean(Train_RMSE, na.rm = TRUE),
    Avg_Bias = mean(Train_Bias, na.rm = TRUE),
    Avg_NSE = mean(Train_NSE, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_R2))
print(avg_train_metrics_df)

# Combine all test predictions for plotting
cat("\nGenerating plot of Observed vs. Predicted GPP on Test Site-Years...\n")
test_predictions_df <- purrr::map_dfr(results_list, ~ .x$test_predictions) %>%
  dplyr::filter(!is.na(Predicted)) # Filter out any NA predictions

# Plot predicted vs observed GPP on the independent test set
gpp_plot <- ggplot(test_predictions_df, aes(x = GPP_site, y = Predicted)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  facet_wrap(~ VI_used, scales = "free") +
  labs(
    title = "Observed vs. Predicted GPP on Test Site-Years",
    x = "Observed GPP (µmol m⁻² s⁻¹)",
    y = "Predicted GPP (µmol m⁻² s⁻¹)"
  ) +
  theme_minimal(base_size = 14)

print(gpp_plot) # Display the plot

# New section for plotting mean train metrics per VI
cat("\nGenerating plots of Mean Training Metrics per VI...\n")

# Prepare data for plotting mean training metrics
plot_data_train_metrics <- avg_train_metrics_df %>%
  select(VI, Avg_R2, Avg_MAE, Avg_RMSE, Avg_Bias, Avg_NSE) %>%
  pivot_longer(cols = c(Avg_R2, Avg_MAE, Avg_RMSE, Avg_Bias, Avg_NSE), names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("Avg_R2", "Avg_MAE", "Avg_RMSE", "Avg_Bias", "Avg_NSE"),
                         labels = c("Mean R2", "Mean MAE", "Mean RMSE", "Mean Bias", "Mean NSE")))

train_metrics_plot <- ggplot(plot_data_train_metrics, aes(x = VI, y = Value, fill = VI)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", size = 0.5) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Mean Training Performance Metrics Across Vegetation Indices",
    x = "Vegetation Index",
    y = "Metric Value",
    fill = "Vegetation Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(train_metrics_plot)

# Helper function to format mean ± sd
format_mean_sd <- function(x) {
  sprintf("%.2f \U00B1 %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# Get formatted training metrics
train_summary <- all_cv_fold_results_df %>%
  group_by(VI) %>%
  summarise(
    Train_R2 = format_mean_sd(Train_R2),
    Train_MAE = format_mean_sd(Train_MAE),
    Train_RMSE = format_mean_sd(Train_RMSE),
    Train_Bias = format_mean_sd(Train_Bias),
    Train_NSE = format_mean_sd(Train_NSE)
  )

# Get formatted validation metrics
val_summary <- all_cv_fold_results_df %>%
  group_by(VI) %>%
  summarise(
    Val_R2 = format_mean_sd(Val_R2),
    Val_MAE = format_mean_sd(Val_MAE),
    Val_RMSE = format_mean_sd(Val_RMSE),
    Val_Bias = format_mean_sd(Val_Bias),
    Val_NSE = format_mean_sd(Val_NSE)
  )

# Get test metrics from summary_df
test_summary <- summary_df %>%
  mutate(
    Test_R2 = sprintf("%.3f", R2),
    Test_MAE = sprintf("%.3f", MAE),
    Test_RMSE = sprintf("%.3f", RMSE),
    Test_Bias = sprintf("%.3f", Bias),
    Test_NSE = sprintf("%.3f", NSE)
  ) %>%
  select(VI, Test_R2, Test_MAE, Test_RMSE, Test_Bias, Test_NSE)

# Join all together
full_metrics_summary <- train_summary %>%
  left_join(val_summary, by = "VI") %>%
  left_join(test_summary, by = "VI") %>%
  arrange(desc(Test_R2))

# Print summary table
print(full_metrics_summary, row.names = FALSE)

# --- New plotting sections as per user request ---

# Define custom colors
custom_colors <- c("Train" = "#C39BD3", "Val" = "#85C1E9", "Test" = "#F7DC6F")

# 1. Prepare combined metrics data for plotting (Train, Validation, Test)
test_metrics_long <- summary_df %>%
  pivot_longer(cols = c(R2, MAE, RMSE, Bias, NSE), names_to = "Metric", values_to = "Value") %>%
  mutate(Dataset = "Test") %>%
  rename(VI = VI)

train_val_metrics_long <- all_cv_fold_results_df %>%
  pivot_longer(cols = c(Train_R2, Train_MAE, Train_RMSE, Train_Bias, Train_NSE,
                        Val_R2, Val_MAE, Val_RMSE, Val_Bias, Val_NSE),
               names_to = "Metric_Dataset", values_to = "Value") %>%
  mutate(
    Metric = sub("^(Train|Val)_", "", Metric_Dataset),
    Dataset = sub("_(R2|MAE|RMSE|Bias|NSE)$", "", Metric_Dataset)
  ) %>%
  select(VI, Fold, Metric, Dataset, Value)

combined_metrics_for_plots <- bind_rows(
  train_val_metrics_long,
  test_metrics_long %>% select(VI, Metric, Dataset, Value)
) %>%
  mutate(Dataset = factor(Dataset, levels = c("Train", "Val", "Test")),
         Metric = factor(Metric, levels = c("R2", "MAE", "RMSE", "Bias", "NSE")))


# Plot 1: Box plots for RMSE (Train, Validation, Test) per VI
cat("\nGenerating Box Plots for RMSE (Train, Validation, Test) per VI...\n")

rmse_boxplot <- combined_metrics_for_plots %>%
  dplyr::filter(Metric == "RMSE") %>%
  ggplot(aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, color = "darkblue") +
  facet_wrap(~ VI, scales = "free_y", ncol = 3) +
  labs(
    title = "RMSE Distribution Across Train, Validation, and Test Sets per VI",
    x = "Dataset",
    y = "RMSE Value",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = custom_colors)

print(rmse_boxplot)


# Plot 2: Faceted Box plots for R2, MAE, RMSE, Bias, NSE (Train, Validation, Test) per VI
cat("\nGenerating Faceted Box Plots for All Metrics (Train, Validation, Test) per VI...\n")

all_metrics_faceted_boxplot <- ggplot(combined_metrics_for_plots, aes(x = Dataset, y = Value, fill = Dataset)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1.5, color = "darkblue") +
  geom_boxplot(outlier.shape = NA) +
  
  facet_grid(Metric ~ VI, scales = "free_y", switch = "y") +
  labs(
    title = "Performance Metrics Distribution Across Datasets per VI",
    x = "Dataset",
    y = "Metric Value",
    fill = "Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.placement = "outside",
        strip.background = element_rect(fill = "grey90", color = NA),
        panel.spacing = unit(1, "lines")) +
  scale_fill_manual(values = custom_colors)

print(all_metrics_faceted_boxplot)


# --- New section: Add predicted_GPP_VI column to joined_df based on IAVI model ---

cat("\n--- Adding predicted_GPP_VI column to joined_df using IAVI model ---\n")

# Ensure joined_df is available. If not, this section will cause an error.
# For demonstration, I will assume joined_df is pre-loaded or created.
# Example: joined_df <- read.csv("your_data.csv") # User needs to define this if not already done

# 1. Prepare data specifically for the IAVI model training
# We need to recreate the training context for the final IAVI model
# The model formula is GPP_site ~ VI_PAR, where VI_PAR is IAVI * PAR_site

# Ensure IAVI is in the vi_list and that the evaluation was successful
if ("IAVI" %in% vi_list && !is.null(results_list[["IAVI"]])) {
  
  # Filter joined_df to create the model_df for IAVI, just like in evaluate_vi
  # This ensures we use the same data preparation logic used for the final model
  model_df_iavi <- joined_df %>%
    dplyr::filter(!is.na(GPP_site), !is.na(PAR_site), !is.na(IAVI)) %>%
    dplyr::mutate(VI_PAR = IAVI * PAR_site) # Create interaction term
  
  # Define the full training data used for the final model (train_val_siteyears)
  full_train_data_iavi <- model_df_iavi %>%
    dplyr::filter(siteyear %in% train_val_siteyears)
  
  # Check if there's enough data to train the model
  if (nrow(full_train_data_iavi) > 1) {
    cat("  Training final IAVI model on combined train/validation data...\n")
    # Train the final IAVI model
    final_iavi_model <- lm(GPP_site ~ VI_PAR, data = full_train_data_iavi)
    
    cat("  Generating predictions for predicted_GPP_VI on the entire joined_df...\n")
    # Prepare joined_df for prediction using the IAVI model structure
    # Create the VI_PAR term across the *entire* joined_df for prediction
    # This ensures consistency in feature creation before prediction
    joined_df_for_prediction <- joined_df %>%
      dplyr::mutate(VI_PAR = IAVI * PAR_site)
    
    # Predict GPP using the trained IAVI model
    # The predict function will automatically handle NA values in 'VI_PAR' by returning NA for those rows.
    joined_df$predicted_GPP_VI <- predict(final_iavi_model, newdata = joined_df_for_prediction)
    
    cat("  'predicted_GPP_VI' column added to 'joined_df'.\n")
    
    # Optional: Display a glimpse of joined_df with the new column
    cat("\n--- Glimpse of 'joined_df' with the new 'predicted_GPP_VI' column ---\n")
    print(dplyr::glimpse(joined_df))
    
  } else {
    cat("  Insufficient data to train the final IAVI model. 'predicted_GPP_VI' column not added.\n")
    # If not enough data, initialize the column with NAs to avoid errors later
    joined_df$predicted_GPP_VI <- NA
  }
} else {
  cat("  IAVI not found in vi_list or IAVI evaluation failed. 'predicted_GPP_VI' column not added.\n")
  joined_df$predicted_GPP_VI <- NA
}

# Save the model performance summary as a CSV file for later use in figures or analysis
write.csv(full_metrics_summary, 
          file = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/6VIperformance.csv", 
          row.names = FALSE)
