#############################################
## Main Analysis Script for LUE and GPP Modeling
## Using Random Forest with Cross-Validation
#############################################
rf_data <- joined_df
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(randomForest)
library(viridis)
library(Metrics)
library(utils)

#unload ggplot2 from current R environment
unloadNamespace("hydroGOF")
library(hydroGOF)

# -------------------------------------------------------------------
# Custom Functions
# -------------------------------------------------------------------

calculate_gdd <- function(tmax, tmin, tbase) {
  tmax_adjusted <- ifelse(tmax > 30, 30, tmax)
  tmin_adjusted <- ifelse(tmin < 10, 10, tmin)
  tmean <- (tmax_adjusted + tmin_adjusted) / 2
  gdd <- tmean - tbase
  ifelse(gdd < 0, 0, gdd)
}

calculate_fapar_beer <- function(LAI, K = 0.5) {
  1 - exp(-K * LAI)
}

make_metric_df <- function(train_metric, val_metric, metric_name) {
  bind_rows(
    data.frame(MetricValue = train_metric, Dataset = "Train", Metric = metric_name),
    data.frame(MetricValue = val_metric, Dataset = "Validation", Metric = metric_name)
  )
}

# -------------------------------------------------------------------
# Data Preparation
# -------------------------------------------------------------------

# Example: Load and prepare data
# sitecombineddata <- read.csv("path/to/sitecombineddata.csv")
# VImeteo20152018combine <- read.csv("path/to/VImeteo20152018combine.csv")
# joined_df <- left_join(sitecombineddata, VImeteo20152018combine, by = "siteyeardate")

# Derived variable calculations
tbase <- 10
joined_df <- joined_df %>%
  mutate(
    gdd = calculate_gdd(tmax, tmin, tbase),
    cumulative_gdd = ave(gdd, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    cumulative_VARI = ave(VARI, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    cumulative_dayl = ave(dayl, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    fAPAR = calculate_fapar_beer(Lai),
    APAR = fAPAR * PAR_site,
    LUE = GPP_site / APAR,
    LUE = ifelse(is.finite(LUE), LUE, NA)
  )


# -------------------------------------------------------------------
# Custom Functions
# -------------------------------------------------------------------

calculate_gdd <- function(tmax, tmin, tbase) {
  tmax_adjusted <- ifelse(tmax > 30, 30, tmax)
  tmin_adjusted <- ifelse(tmin < 10, 10, tmin)
  tmean <- (tmax_adjusted + tmin_adjusted) / 2
  gdd <- tmean - tbase
  ifelse(gdd < 0, 0, gdd)
}

calculate_fapar_beer <- function(LAI, K = 0.5) {
  1 - exp(-K * LAI)
}

make_metric_df <- function(train_metric, val_metric, metric_name) {
  bind_rows(
    data.frame(MetricValue = train_metric, Dataset = "Train", Metric = metric_name),
    data.frame(MetricValue = val_metric, Dataset = "Validation", Metric = metric_name)
  )
}

# -------------------------------------------------------------------
# Data Preparation
# -------------------------------------------------------------------

# Derived variable calculations
tbase <- 10
joined_df <- joined_df %>%
  mutate(
    gdd = calculate_gdd(tmax, tmin, tbase),
    cumulative_gdd = ave(gdd, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    cumulative_VARI = ave(VARI, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    cumulative_dayl = ave(dayl, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    fAPAR = calculate_fapar_beer(Lai),
    APAR = fAPAR * PAR_site,
    LUE = GPP_site / APAR,
    LUE = ifelse(is.finite(LUE), LUE, NA)
  )


# -------------------------------------------------------------------
# Model Setup
# -------------------------------------------------------------------

# Predictor columns (all columns that might be used as predictors or are needed in rf_data)
predictor_columns <- c("GPP_site", "PAR_site", "fAPAR", "LUE", "VPD_site", "Tair_site",
                       "siteyear", "Es", "rH_site", "dayl", "cumulative_gdd",
                       "cumulative_dayl", "DOP", "DAP", "nir", "MBWI", "Lai", "MLSWI26",
                       "TVI", "GDVI", "NDWI", "IAVI", "kNDVI", "NDVI", "VARI", "TSAVI",
                       "RNDVI", "IPVI", "PI", "EVI", "ATSAVI", "LSWI", "blue")


predictor_columns <- c("GPP_site", "PAR_site", "fAPAR", "LUE", 
  "IAVI"  , "VARI", "TVI","ExG","VPD_site", "dayl","rH_site",     
                        "Tair_site" ,"DAP"     , "DBSI","TGI", "NMDI"   ,"MLSWI26",  "MuWIR"  ,       
                        "GARI","cumulative_gdd" ,"swir1","NDDI","siteyear", "DOP","Es", "RI4XS", 
  "Lai",
  "siteyeardate","Et", 
  #"dayl","cumulative_gdd", 
  "AWEInsh",
  "Variety",
  "ATSAVI",
  #"VARI",
  "EVI", "LSWI" ,"TSAVI", "RNDVI", "kNDVI", "EVI",  "cumulative_dayl",
  "NDYI", "EMBI", "BCC", "DOP" )
# Filter and select data, ensuring 'siteyeardate' is kept for merging predictions
all_relevant_columns <- unique(c("siteyeardate", predictor_columns))

# Keep full dataset
rf_data <- joined_df
rf_data_full <- rf_data   # <- all columns preserved

rf_data  <- joined_df %>%
  dplyr::select(all_of(all_relevant_columns)) %>%
  dplyr::filter(is.finite(LUE) & !is.na(LUE))

# Columns to remove from modeling (augmented to include siteyeardate and other non-predictors)
cols_to_remove_for_model <- c("siteyear", "GPP_site", "PAR_site", "fAPAR", "Lai",
                              "IAVI", "VARI", 
                              "ATSAVI", "LSWI", "kNDVI", "dayl",
                              "IPVI", "PI", "TVI", "MLSWI26", "RNDVI", "EVI", "MBWI",
                              "siteyeardate") # Add siteyeardate to be excluded from RF predictors
# Columns to remove from modeling (augmented to include siteyeardate and other non-predictors)
cols_to_remove_for_model <- c("siteyear", "GPP_site", "PAR_site", "fAPAR", "Lai",
                              "siteyeardate",
                              #"dayl","cumulative_gdd", "DOP"
                              #"AWEInsh",#Es, 
                              "Variety",
                              "ATSAVI",
                              #"VARI",
                              "EVI", "LSWI" ,"TSAVI", "RNDVI", "kNDVI", "EVI",  "cumulative_dayl",
                              #"MuWIR",
                              #"NMDI", 
                              "GARI", "Es","TGI", "NDDI",
                              "NDYI", "EMBI", "BCC" ) # Add siteyeardate to be excluded from RF predictors

# Site-year groups
# These are now directly the 'original' names from the mock data section
cv_pool_siteyears <- c(original_train_siteyears, original_val_siteyears)
fixed_test_siteyears <- original_test_siteyears
# -------------------------------------------------------------------
# Cross-Validation Setup
# -------------------------------------------------------------------

set.seed(54)
validation_combinations <- combn(unique(cv_pool_siteyears), 3)
num_combinations <- ncol(validation_combinations)

cv_results <- data.frame(
  Fold = integer(num_combinations),
  Val_SiteYears = character(num_combinations),
  Train_R2 = numeric(num_combinations), Train_MAE = numeric(num_combinations),
  Train_RMSE = numeric(num_combinations), Train_Bias = numeric(num_combinations),
  Train_NSE = numeric(num_combinations),
  Val_R2 = numeric(num_combinations), Val_MAE = numeric(num_combinations),
  Val_RMSE = numeric(num_combinations), Val_Bias = numeric(num_combinations),
  Val_NSE = numeric(num_combinations),
  GPP_Train_R2 = numeric(num_combinations), GPP_Train_MAE = numeric(num_combinations),
  GPP_Train_RMSE = numeric(num_combinations), GPP_Train_Bias = numeric(num_combinations),
  GPP_Train_NSE = numeric(num_combinations),
  GPP_Val_R2 = numeric(num_combinations), GPP_Val_MAE = numeric(num_combinations),
  GPP_Val_RMSE = numeric(num_combinations), GPP_Val_Bias = numeric(num_combinations),
  GPP_Val_NSE = numeric(num_combinations),
  stringsAsFactors = FALSE
)

# List to store predictions from each validation fold for later averaging
all_cv_val_preds <- list()

# List to store variable importance matrices from each fold
all_lue_importance_measures <- list()

# -------------------------------------------------------------------
# Cross-Validation Loop
# -------------------------------------------------------------------

print("Starting Cross-Validation...")

for (i in 1:num_combinations) {
  current_val_sites <- validation_combinations[, i]
  current_train_sites <- setdiff(unique(cv_pool_siteyears), current_val_sites)
  
  cat(paste0("\nCV Fold ", i, "/", num_combinations, "\n"))
  cat("Validation Site-Years: ", paste(current_val_sites, collapse = ", "), "\n")
  
  # Subset data using the original rf_data which includes 'siteyeardate'
  current_train_set <- subset(rf_data, siteyear %in% current_train_sites)
  current_val_set <- subset(rf_data, siteyear %in% current_val_sites)
  
  if (nrow(current_train_set) == 0 || nrow(current_val_set) == 0) {
    cat("Skipping fold due to empty train or validation set.\n")
    # Store NA for importance if fold is skipped
    all_lue_importance_measures[[i]] <- NA
    next
  }
  
  # Prepare model input: select only actual predictors + LUE response
  # Use setdiff to exclude all columns that are not predictors for the RF model
  predictor_names_for_rf <- setdiff(names(current_train_set),
                                    c(cols_to_remove_for_model, "LUE"))
  
  train_model_input <- current_train_set %>%
    dplyr::select(all_of(predictor_names_for_rf))
  train_model_input$LUE <- current_train_set$LUE # Add LUE back as response
  
  val_predict_data <- current_val_set %>%
    dplyr::select(all_of(predictor_names_for_rf))
  
  # Train Random Forest
  # Ensure the model is trained on complete cases for LUE and predictors
  rf_model <- randomForest(
    LUE ~ ., data = train_model_input,
    ntree = 200, importance = TRUE, na.action = na.omit # Set importance to TRUE
  )
  
  # Store variable importance for this fold
  all_lue_importance_measures[[i]] <- randomForest::importance(rf_model)
  
  # Predict LUE
  current_train_set$LUE_pred <- predict(rf_model, newdata = train_model_input)
  current_val_set$LUE_pred <- predict(rf_model, newdata = val_predict_data)
  
  # Calculate GPP
  current_train_set$GPP_pred <- current_train_set$LUE_pred * current_train_set$PAR_site * current_train_set$fAPAR
  current_val_set$GPP_pred <- current_val_set$LUE_pred * current_val_set$PAR_site * current_val_set$fAPAR
  
  # Store predictions for this fold's validation set, along with siteyeardate
  val_preds_for_this_fold <- current_val_set %>%
    dplyr::select(siteyeardate, LUE_pred, GPP_pred)
  all_cv_val_preds[[i]] <- val_preds_for_this_fold
  
  # Store metrics
  cv_results[i, "Fold"] <- i
  cv_results[i, "Val_SiteYears"] <- paste(current_val_sites, collapse = ", ")
  
  # LUE Metrics (using Metrics:: for clarity and avoiding potential conflicts)
  # Ensure no NA values in LUE or LUE_pred for metric calculations
  valid_lue_train <- complete.cases(current_train_set$LUE, current_train_set$LUE_pred)
  valid_lue_val <- complete.cases(current_val_set$LUE, current_val_set$LUE_pred)
  
  cv_results[i, "Train_R2"] <- cor(current_train_set$LUE[valid_lue_train], current_train_set$LUE_pred[valid_lue_train])^2
  cv_results[i, c("Train_MAE", "Train_RMSE")] <- c(
    Metrics::mae(current_train_set$LUE[valid_lue_train], current_train_set$LUE_pred[valid_lue_train]),
    Metrics::rmse(current_train_set$LUE[valid_lue_train], current_train_set$LUE_pred[valid_lue_train])
  )
  cv_results[i, "Train_Bias"] <- mean(current_train_set$LUE_pred[valid_lue_train] - current_train_set$LUE[valid_lue_train])
  # Check if hydroGOF is loaded, otherwise use a placeholder or alternative NSE
  if ("package:hydroGOF" %in% search()) {
    cv_results[i, "Train_NSE"] <- hydroGOF::NSE(sim = as.vector(current_train_set$LUE_pred[valid_lue_train]), obs = as.vector(current_train_set$LUE[valid_lue_train]))
  } else {
    cv_results[i, "Train_NSE"] <- NA # Or implement a simple NSE if hydroGOF is not desired
  }
  
  cv_results[i, "Val_R2"] <- cor(current_val_set$LUE[valid_lue_val], current_val_set$LUE_pred[valid_lue_val])^2
  cv_results[i, c("Val_MAE", "Val_RMSE")] <- c(
    Metrics::mae(current_val_set$LUE[valid_lue_val], current_val_set$LUE_pred[valid_lue_val]),
    Metrics::rmse(current_val_set$LUE[valid_lue_val], current_val_set$LUE_pred[valid_lue_val])
  )
  cv_results[i, "Val_Bias"] <- mean(current_val_set$LUE_pred[valid_lue_val] - current_val_set$LUE[valid_lue_val])
  if ("package:hydroGOF" %in% search()) {
    cv_results[i, "Val_NSE"] <- hydroGOF::NSE(sim = as.vector(current_val_set$LUE_pred[valid_lue_val]), obs = as.vector(current_val_set$LUE[valid_lue_val]))
  } else {
    cv_results[i, "Val_NSE"] <- NA
  }
  
  # GPP Metrics
  valid_gpp_train <- complete.cases(current_train_set$GPP_site, current_train_set$GPP_pred)
  valid_gpp_val <- complete.cases(current_val_set$GPP_site, current_val_set$GPP_pred)
  
  cv_results[i, "GPP_Train_R2"] <- cor(current_train_set$GPP_site[valid_gpp_train], current_train_set$GPP_pred[valid_gpp_train])^2
  cv_results[i, c("GPP_Train_MAE", "GPP_Train_RMSE")] <- c(
    Metrics::mae(current_train_set$GPP_site[valid_gpp_train], current_train_set$GPP_pred[valid_gpp_train]),
    Metrics::rmse(current_train_set$GPP_site[valid_gpp_train], current_train_set$GPP_pred[valid_gpp_train])
  )
  cv_results[i, "GPP_Train_Bias"] <- mean(current_train_set$GPP_pred[valid_gpp_train] - current_train_set$GPP_site[valid_gpp_train])
  if ("package:hydroGOF" %in% search()) {
    cv_results[i, "GPP_Train_NSE"] <- hydroGOF::NSE(sim = as.vector(current_train_set$GPP_pred[valid_gpp_train]), obs = as.vector(current_train_set$GPP_site[valid_gpp_train]))
  } else {
    cv_results[i, "GPP_Train_NSE"] <- NA
  }
  
  cv_results[i, "GPP_Val_R2"] <- cor(current_val_set$GPP_site[valid_gpp_val], current_val_set$GPP_pred[valid_gpp_val])^2
  cv_results[i, c("GPP_Val_MAE", "GPP_Val_RMSE")] <- c(
    Metrics::mae(current_val_set$GPP_site[valid_gpp_val], current_val_set$GPP_pred[valid_gpp_val]),
    Metrics::rmse(current_val_set$GPP_site[valid_gpp_val], current_val_set$GPP_pred[valid_gpp_val])
  )
  cv_results[i, "GPP_Val_Bias"] <- mean(current_val_set$GPP_pred[valid_gpp_val] - current_val_set$GPP_site[valid_gpp_val])
  if ("package:hydroGOF" %in% search()) {
    cv_results[i, "GPP_Val_NSE"] <- hydroGOF::NSE(sim = as.vector(current_val_set$GPP_pred[valid_gpp_val]), obs = as.vector(current_val_set$GPP_site[valid_gpp_val]))
  } else {
    cv_results[i, "GPP_Val_NSE"] <- NA
  }
  
  # Print metrics for current fold
  cat(sprintf("LUE Train - R2: %.3f, MAE: %.3f, NSE: %.3f\n",
              cv_results[i, "Train_R2"], cv_results[i, "Train_MAE"], cv_results[i, "Train_NSE"]))
  cat(sprintf("LUE Val   - R2: %.3f, MAE: %.3f, NSE: %.3f\n",
              cv_results[i, "Val_R2"], cv_results[i, "Val_MAE"], cv_results[i, "Val_NSE"]))
  cat(sprintf("GPP Train - R2: %.3f, MAE: %.3f, NSE: %.3f\n",
              cv_results[i, "GPP_Train_R2"], cv_results[i, "GPP_Train_MAE"], cv_results[i, "GPP_Train_NSE"]))
  cat(sprintf("GPP Val   - R2: %.3f, MAE: %.3f, NSE: %.3f\n\n",
              cv_results[i, "GPP_Val_R2"], cv_results[i, "GPP_Val_MAE"], cv_results[i, "GPP_Val_NSE"]))
}

print("Cross-Validation Complete.")

# -------------------------------------------------------------------
# Process and Average Variable Importance from CV runs
# -------------------------------------------------------------------

print("Calculating mean variable importance from CV runs...")

# Filter out NA entries (from skipped folds)
valid_importance_measures <- all_lue_importance_measures[!is.na(all_lue_importance_measures)]

if (length(valid_importance_measures) > 0) {
  # Combine all importance matrices into a single data frame
  # This ensures proper alignment by variable name
  combined_importance_df <- bind_rows(lapply(valid_importance_measures, function(x) {
    as.data.frame(x) %>%
      tibble::rownames_to_column("Variable")
  }))
  
  # Calculate the mean for each importance metric for each variable
  mean_importance <- combined_importance_df %>%
    group_by(Variable) %>%
    summarise(
      Mean_IncMSE = mean(`%IncMSE`, na.rm = TRUE),
      Mean_IncNodePurity = mean(`IncNodePurity`, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Sort by Mean_IncMSE in descending order for better readability
    arrange(desc(Mean_IncMSE))
  
  cat("\n=== Mean LUE Variable Importance Across CV Folds ===\n")
  print(mean_importance)
  
} else {
  cat("\nNo valid variable importance measures to average from CV runs.\n")
}


# Final Model Training
# ===================================================

print("\nTraining final model on full CV pool...")
# Prepare model input for final training, excluding non-predictors
final_train_set <- subset(rf_data, siteyear %in% cv_pool_siteyears)
predictor_names_for_final_rf <- setdiff(names(final_train_set),
                                        c(cols_to_remove_for_model, "LUE"))

final_train_input <- final_train_set %>%
  dplyr::select(all_of(predictor_names_for_final_rf))
final_train_input$LUE <- final_train_set$LUE

final_rf_model <- randomForest(LUE ~ ., data = final_train_input,
                               ntree = 100, importance = TRUE, na.action = na.omit)

# Test Set Evaluation
# ====================================================

test_set <- subset(rf_data, siteyear %in% fixed_test_siteyears)
# Prepare test input, excluding non-predictors
predictor_names_for_test_input <- setdiff(names(test_set),
                                          c(cols_to_remove_for_model, "LUE"))
test_input <- test_set %>%
  dplyr::select(all_of(predictor_names_for_test_input))

# LUE predictions and metrics
test_set$LUE_pred <- predict(final_rf_model, newdata = test_input)

# Filter for complete cases for test set metrics as well
valid_lue_test <- complete.cases(test_set$LUE, test_set$LUE_pred)
test_LUE_R2 <- cor(test_set$LUE[valid_lue_test], test_set$LUE_pred[valid_lue_test])^2
test_LUE_MAE <- Metrics::mae(test_set$LUE[valid_lue_test], test_set$LUE_pred[valid_lue_test])
test_LUE_RMSE <- Metrics::rmse(test_set$LUE[valid_lue_test], test_set$LUE_pred[valid_lue_test])
test_LUE_Bias <- mean(test_set$LUE_pred[valid_lue_test] - test_set$LUE[valid_lue_test], na.rm = TRUE)
if ("package:hydroGOF" %in% search()) {
  test_LUE_NSE <- hydroGOF::NSE(sim = as.vector(test_set$LUE_pred[valid_lue_test]), obs = as.vector(test_set$LUE[valid_lue_test]))
} else {
  test_LUE_NSE <- NA
}

# GPP predictions and metrics
test_set$GPP_pred <- test_set$LUE_pred * test_set$PAR_site * test_set$fAPAR

valid_gpp_test <- complete.cases(test_set$GPP_site, test_set$GPP_pred)
test_GPP_R2 <- cor(test_set$GPP_site[valid_gpp_test], test_set$GPP_pred[valid_gpp_test])^2
test_GPP_MAE <- Metrics::mae(test_set$GPP_site[valid_gpp_test], test_set$GPP_pred[valid_gpp_test])
test_GPP_RMSE <- Metrics::rmse(test_set$GPP_site[valid_gpp_test], test_set$GPP_pred[valid_gpp_test])
test_GPP_Bias <- mean(test_set$GPP_pred[valid_gpp_test] - test_set$GPP_site[valid_gpp_test], na.rm = TRUE)
if ("package:hydroGOF" %in% search()) {
  test_GPP_NSE <- hydroGOF::NSE(sim = as.vector(test_set$GPP_pred[valid_gpp_test]), obs = as.vector(test_set$GPP_site[valid_gpp_test]))
} else {
  test_GPP_NSE <- NA
}

# Add LUE_predicted and GPP_predicted columns to rf_data
# ================

# Step 1: Calculate mean CV predictions for the cv_pool_siteyears
# Combine all validation predictions collected during the CV loop
combined_cv_val_preds <- bind_rows(all_cv_val_preds)

# Group by siteyeardate and calculate the mean prediction
mean_cv_predictions <- combined_cv_val_preds %>%
  group_by(siteyeardate) %>%
  summarise(
    LUE_predicted_cv_mean = mean(LUE_pred, na.rm = TRUE),
    GPP_predicted_cv_mean = mean(GPP_pred, na.rm = TRUE),
    .groups = 'drop' # Important to drop grouping after summarising
  )

# Step 2: Prepare final test set predictions with distinct names
final_test_predictions_df <- test_set %>%
  dplyr::select(siteyeardate, LUE_pred, GPP_pred) %>%
  rename(LUE_predicted_final_test = LUE_pred, GPP_predicted_final_test = GPP_pred)

# Step 3: Initialize rf_data with the new prediction columns
rf_data_with_final_predictions <- rf_data %>%
  mutate(LUE_predicted = NA_real_, GPP_predicted = NA_real_)

# Step 4: Populate LUE_predicted and GPP_predicted based on siteyear category

# For siteyears in cv_pool_siteyears, use the mean CV predictions
rf_data_with_final_predictions <- rf_data_with_final_predictions %>%
  left_join(mean_cv_predictions, by = "siteyeardate") %>%
  mutate(
    LUE_predicted = ifelse(siteyear %in% cv_pool_siteyears, LUE_predicted_cv_mean, LUE_predicted),
    GPP_predicted = ifelse(siteyear %in% cv_pool_siteyears, GPP_predicted_cv_mean, GPP_predicted)
  ) %>%
  dplyr::select(-LUE_predicted_cv_mean, -GPP_predicted_cv_mean) # Clean up temporary columns

# For siteyears in fixed_test_siteyears, use the final test set predictions
rf_data_with_final_predictions <- rf_data_with_final_predictions %>%
  left_join(final_test_predictions_df, by = "siteyeardate") %>%
  mutate(
    LUE_predicted = ifelse(siteyear %in% fixed_test_siteyears, LUE_predicted_final_test, LUE_predicted),
    GPP_predicted = ifelse(siteyear %in% fixed_test_siteyears, GPP_predicted_final_test, GPP_predicted)
  ) %>%
  dplyr::select(-LUE_predicted_final_test, -GPP_predicted_final_test) # Clean up temporary columns

# Overwrite the original rf_data with the updated version
rf_data <- rf_data_with_final_predictions

# At this point, rf_data now contains the 'LUE_predicted' and 'GPP_predicted' columns
# with values populated as per your requirements.
# You can inspect the first few rows to verify:
# print(head(rf_data %>% dplyr::select(siteyeardate, LUE, LUE_predicted, GPP_site, GPP_predicted)))


# Visualization Functions
# ================================================

plot_pred_vs_obs <- function(observed, predicted, title, metrics, color) {
  # Ensure that observed and predicted are filtered for complete cases before plotting
  valid_indices <- complete.cases(observed, predicted)
  df <- data.frame(Observed = observed[valid_indices], Predicted = predicted[valid_indices])
  
  metrics_text <- paste0(
    "R\u00B2 = ", round(metrics[1], 3), "\n",
    "MAE = ", round(metrics[2], 3), "\n",
    "RMSE = ", round(metrics[3], 3), "\n",
    "Bias = ", round(metrics[4], 3), "\n",
    "NSE = ", round(metrics[5], 3)) # NSE added to text
  
  ggplot(df, aes(x = Observed, y = Predicted)) +
    geom_point(alpha = 0.4, color = color) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    ggtitle(title) +
    theme_minimal() +
    annotate("text",
             x = min(df$Observed, na.rm = TRUE),
             y = max(df$Predicted, na.rm = TRUE),
             label = metrics_text,
             hjust = 0, vjust = 1,
             size = 4, color = "black",
             fontface = "bold",
             lineheight = 1.1)
}

plot_performance_metrics <- function(df_all, df_test_points, title, colors) {
  ggplot(df_all, aes(x = Dataset, y = MetricValue, fill = Dataset)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.4, size = 0.8, color = "black") +
    geom_point(data = df_test_points, aes(x = Dataset, y = MetricValue),
               color = colors["Test"], size = 4) +
    facet_wrap(~Metric, scales = "free_y", nrow = 2) +
    theme_minimal() +
    labs(title = title, y = "Metric Value", x = "") +
    scale_fill_manual(values = colors) +
    theme(legend.position = "none",
          strip.text = element_text(size = 12, face = "bold"))
}

# Generate Plots
# =========================================================

# Custom colors
custom_colors <- c("Train" = "#C39BD3", "Validation" = "#85C1E9", "Test" = "#F7DC6F")

# LUE Plots
plot_pred_vs_obs(test_set$LUE, test_set$LUE_pred,
                 "Final RF Model: Predicted vs Observed LUE (Test Set)",
                 c(test_LUE_R2, test_LUE_MAE, test_LUE_RMSE, test_LUE_Bias, test_LUE_NSE),
                 "#F7DC6F")

# Prepare LUE metric data
df_lue_all <- bind_rows(
  make_metric_df(cv_results$Train_R2, cv_results$Val_R2, "R2"),
  make_metric_df(cv_results$Train_MAE, cv_results$Val_MAE, "MAE"),
  make_metric_df(cv_results$Train_RMSE, cv_results$Val_RMSE, "RMSE"),
  make_metric_df(cv_results$Train_Bias, cv_results$Val_Bias, "Bias"),
  make_metric_df(cv_results$Train_NSE, cv_results$Val_NSE, "NSE")
)

df_lue_test_points <- data.frame(
  MetricValue = c(test_LUE_R2, test_LUE_MAE, test_LUE_RMSE, test_LUE_Bias, test_LUE_NSE),
  Dataset = factor("Test", levels = c("Train", "Validation", "Test")),
  Metric = c("R2", "MAE", "RMSE", "Bias", "NSE")
)

plot_performance_metrics(df_lue_all, df_lue_test_points,
                         "LUE Performance Metrics Across CV and Test Sets",
                         custom_colors)

# GPP Plots
plot_pred_vs_obs(test_set$GPP_site, test_set$GPP_pred,
                 "Final RF Model: Predicted vs Observed GPP (Test Set)",
                 c(test_GPP_R2, test_GPP_MAE, test_GPP_RMSE, test_GPP_Bias, test_GPP_NSE),
                 "#F7DC6F")

# Prepare GPP metric data
df_gpp_all <- bind_rows(
  make_metric_df(cv_results$GPP_Train_R2, cv_results$GPP_Val_R2, "R2"),
  make_metric_df(cv_results$GPP_Train_MAE, cv_results$GPP_Val_MAE, "MAE"),
  make_metric_df(cv_results$GPP_Train_RMSE, cv_results$GPP_Val_RMSE, "RMSE"),
  make_metric_df(cv_results$GPP_Train_Bias, cv_results$GPP_Val_Bias, "Bias"),
  make_metric_df(cv_results$GPP_Train_NSE, cv_results$GPP_Val_NSE, "NSE")
)

df_gpp_test_points <- data.frame(
  MetricValue = c(test_GPP_R2, test_GPP_MAE, test_GPP_RMSE, test_GPP_Bias, test_GPP_NSE),
  Dataset = factor("Test", levels = c("Train", "Validation", "Test")),
  Metric = c("R2", "MAE", "RMSE", "Bias", "NSE")
)

plot_performance_metrics(df_gpp_all, df_gpp_test_points,
                         "GPP Performance Metrics Across CV and Test Sets",
                         custom_colors)

# Variable Importance
# =====================================================
# Only run if final_rf_model was successfully trained and is not NULL
if (!is.null(final_rf_model)) {
  varImpPlot(final_rf_model, main = "Random Forest Variable Importance (Final Model)")
} else {
  cat("\nFinal Random Forest model was not trained. Skipping Variable Importance Plot.\n")
}

# Results Summary
# ========================================================

cat("\n=== Final Model Performance ===\n")
cat("LUE Test R2:", round(test_LUE_R2, 3), "\n")
cat("LUE Test MAE:", round(test_LUE_MAE, 3), "\n")
cat("LUE Test RMSE:", round(test_LUE_RMSE, 3), "\n")
cat("LUE Test Bias:", round(test_LUE_Bias, 3), "\n")
cat("LUE Test NSE:", round(test_LUE_NSE, 3), "\n\n")

cat("GPP Test R2:", round(test_GPP_R2, 3), "\n")
cat("GPP Test MAE:", round(test_GPP_MAE, 3), "\n")
cat("GPP Test RMSE:", round(test_GPP_RMSE, 3), "\n")
cat("GPP Test Bias:", round(test_GPP_Bias, 3), "\n")
cat("GPP Test NSE:", round(test_GPP_NSE, 3), "\n")

# Identify problematic site-years
# Split the Val_SiteYears string into a list (assuming cv_results is populated)
siteyear_list <- strsplit(cv_results$Val_SiteYears, ",\\s*")

# Flatten to a single vector of site-years
all_siteyears <- unlist(siteyear_list)

# Count occurrences
siteyear_counts <- sort(table(all_siteyears), decreasing = TRUE)

# Show site-years that appear more than once
problematic_siteyears <- names(siteyear_counts[siteyear_counts > 1])
if (length(problematic_siteyears) > 0) {
  cat("\nProblematic Site-Years (appearing more than once in validation sets):\n")
  print(siteyear_counts[problematic_siteyears])
} else {
  cat("\nNo site-years appeared more than once in validation sets.\n")
}

# Output result
cat("\nOverall Site-Year Counts in Validation Sets:\n")
print(siteyear_counts)

# Create a helper function to format mean ± SD
format_mean_sd <- function(x) {
  sprintf("%.2f \U00B1 %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)) # Using unicode for +/-
}

# Create the summary table
mean_sd_metrics <- data.frame(
  Metric = c("R2", "MAE", "RMSE", "Bias", "NSE"),
  Train = c(format_mean_sd(cv_results$GPP_Train_R2),
            format_mean_sd(cv_results$GPP_Train_MAE),
            format_mean_sd(cv_results$GPP_Train_RMSE),
            format_mean_sd(cv_results$GPP_Train_Bias),
            format_mean_sd(cv_results$GPP_Train_NSE)),
  Validation = c(format_mean_sd(cv_results$GPP_Val_R2),
                 format_mean_sd(cv_results$GPP_Val_MAE),
                 format_mean_sd(cv_results$GPP_Val_RMSE),
                 format_mean_sd(cv_results$GPP_Val_Bias),
                 format_mean_sd(cv_results$GPP_Val_NSE)),
  Test = sprintf("%.2f", c(test_GPP_R2, test_GPP_MAE, test_GPP_RMSE, test_GPP_Bias, test_GPP_NSE))
)

cat("\n=== GPP Performance Summary (Mean ± SD for CV, Single Value for Test) ===\n")
print(mean_sd_metrics, row.names = FALSE)




# LUE Performance Summary Table
mean_sd_metrics_lue <- data.frame(
  Metric = c("R2", "MAE", "RMSE", "Bias", "NSE"),
  Train = c(format_mean_sd(cv_results$Train_R2),
            format_mean_sd(cv_results$Train_MAE),
            format_mean_sd(cv_results$Train_RMSE),
            format_mean_sd(cv_results$Train_Bias),
            format_mean_sd(cv_results$Train_NSE)),
  Validation = c(format_mean_sd(cv_results$Val_R2),
                 format_mean_sd(cv_results$Val_MAE),
                 format_mean_sd(cv_results$Val_RMSE),
                 format_mean_sd(cv_results$Val_Bias),
                 format_mean_sd(cv_results$Val_NSE)),
  Test = sprintf("%.2f", c(test_LUE_R2, test_LUE_MAE, test_LUE_RMSE, test_LUE_Bias, test_LUE_NSE))
)
# ==============================================================================
# 6. PLOT AVERAGED VARIABLE IMPORTANCE =========================================
# ==============================================================================

if (!is.null(mean_importance) && nrow(mean_importance) > 0) {
  # Plot for Mean %IncMSE
  importance_plot <- ggplot(mean_importance, aes(x = Mean_IncMSE, y = reorder(Variable, Mean_IncMSE))) +
    geom_bar(stat = "identity", fill = "#4CAF50") + # Using a single pleasant green color
    labs(
      x = "% Increase in MSE (Mean)",
      y = NULL,
      title = "Mean Variable Importance (%IncMSE) Across CV Folds"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") # No legend needed for single fill
  
  print(importance_plot) # Display the plot
  
  # Note: ggsave commented out as saving to local path might not be relevant in all environments
  # ggsave(
  #   filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/mean_incmse_importance.png",
  #   plot = importance_plot,
  #   width = 10,
  #   height = 8,
  #   dpi = 300
  # )
  
  # Plot for Mean Decrease in Gini (Node Impurity)
  gini_plot <- ggplot(mean_importance, aes(x = Mean_IncNodePurity, y = reorder(Variable, Mean_IncNodePurity))) +
    geom_bar(stat = "identity", fill = "#2196F3") + # Using a single pleasant blue color
    labs(
      x = "Mean Decrease in Gini (Node Impurity)",
      y = NULL,
      title = "Mean Variable Importance (Node Impurity) Across CV Folds"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") # No legend needed for single fill
  
  print(gini_plot) # Display the plot
  
  # Note: ggsave commented out as saving to local path might not be relevant in all environments
  # ggsave(
  #   filename = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/PaperFigure/mean_gini_importance.png",
  #   plot = gini_plot,
  #   width = 10,
  #   height = 8,
  #   dpi = 300
  # )
} else {
  cat("\nCannot plot variable importance: 'mean_importance' dataframe is empty or NULL.\n")
}

colnames(rf_data)
mean_importance$Variable
mean_importance

