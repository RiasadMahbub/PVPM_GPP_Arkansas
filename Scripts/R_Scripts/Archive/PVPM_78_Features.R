# Load necessary libraries
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(progressr)
library(randomForest)
library(doParallel)
library(foreach)

# Record start time
start_time <- Sys.time()

# --- Dummy Data Creation ---
set.seed(123)
num_rows <- 5000
all_site_years <- c("USBDA2016", "USBDC2016", "USOF22017", "USHRC2016",
                    "USOF62018", "USOF52018", "USHRC2015", "USHRA2015",
                    "USBDC2015", "USOF32017", "USOF12017", "USHRA2016", "USHRA2017",
                    "USHRC2017", "USBDA2015", "USOF42018")

# (Your dummy data creation code here...)

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

evaluate_metrics <- function(y_observed, y_predicted) {
  valid_indices <- !is.na(y_observed) & !is.na(y_predicted)
  y_valid_obs <- y_observed[valid_indices]
  y_valid_pred <- y_predicted[valid_indices]
  
  if (length(y_valid_obs) == 0 || length(unique(y_valid_obs)) < 2) {
    return(list(R2 = NA, MAE = NA, RMSE = NA, Bias = NA))
  }
  
  ss_res <- sum((y_valid_obs - y_valid_pred)^2)
  ss_tot <- sum((y_valid_obs - mean(y_valid_obs))^2)
  r2 <- if (ss_tot == 0) 1 else 1 - ss_res / ss_tot
  mae <- mean(abs(y_valid_obs - y_valid_pred))
  rmse <- sqrt(mean((y_valid_obs - y_valid_pred)^2))
  bias <- mean(y_valid_pred - y_valid_obs)
  
  list(R2 = r2, MAE = mae, RMSE = rmse, Bias = bias)
}

# -------------------------------------------------------------------
# Data Preparation
# -------------------------------------------------------------------

tbase <- 10
joined_df <- joined_df %>%
  dplyr::mutate(
    gdd = calculate_gdd(tmax, tmin, tbase),
    cumulative_gdd = ave(gdd, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    cumulative_dayl = ave(dayl, siteyear, FUN = function(x) cumsum(ifelse(is.na(x), 0, x))),
    fAPAR = calculate_fapar_beer(Lai),
    APAR = fAPAR * PAR_site,
    LUE = GPP_site / APAR,
    LUE = ifelse(is.finite(LUE), LUE, NA)
  )

# Feature selection - only use features with at least 6 in combinations
features_to_combine <- c("PAR_site", "fAPAR", "VPD_site", "Tair_site", "Es", "rH_site",
                         "dayl", "cumulative_gdd", "cumulative_dayl", "DOP", "DAP", "nir",
                         "MBWI", "Lai", "MLSWI26", "TVI", "GDVI", "NDWI", "IAVI", "kNDVI",
                         "NDVI", "VARI", "TSAVI", "RNDVI", "IPVI", "PI", "EVI", "ATSAVI", "LSWI")

# Site-year groups
original_train_siteyears <- c("USBDA2016", "USBDC2016", "USOF22017", "USHRC2016",
                              "USOF62018", "USOF52018", "USHRC2015", "USHRA2015",
                              "USBDC2015", "USOF32017")
original_val_siteyears <- c("USOF12017", "USHRA2016", "USHRA2017")
original_test_siteyears <- c("USHRC2017", "USBDA2015", "USOF42018")

cv_pool_siteyears <- c(original_train_siteyears, original_val_siteyears)
fixed_test_siteyears <- original_test_siteyears

# Generate combinations with AT LEAST 6 features
min_features <- 6
max_features <- length(features_to_combine)
all_feature_combinations <- unlist(
  lapply(min_features:max_features, function(k) {
    combn(features_to_combine, k, simplify = FALSE)
  }),
  recursive = FALSE
)
num_feature_combinations <- length(all_feature_combinations)

# Initialize results dataframe
all_rf_model_results_df <- dplyr::tibble(
  Feature_Combination = character(),
  Num_Features = integer(),
  LUE_Train_R2 = double(), LUE_Train_MAE = double(), LUE_Train_RMSE = double(), LUE_Train_Bias = double(),
  LUE_Val_R2 = double(), LUE_Val_MAE = double(), LUE_Val_RMSE = double(), LUE_Val_Bias = double(),
  LUE_Test_R2 = double(), LUE_Test_MAE = double(), LUE_Test_RMSE = double(), LUE_Test_Bias = double(),
  GPP_Train_R2 = double(), GPP_Train_MAE = double(), GPP_Train_RMSE = double(), GPP_Train_Bias = double(),
  GPP_Val_R2 = double(), GPP_Val_MAE = double(), GPP_Val_RMSE = double(), GPP_Val_Bias = double(),
  GPP_Test_R2 = double(), GPP_Test_MAE = double(), GPP_Test_RMSE = double(), GPP_Test_Bias = double()
)

# -------------------------------------------------------------------
# Parallel Execution Setup
# -------------------------------------------------------------------

# Set up parallel backend
num_cores <- detectCores() - 1
if (num_cores < 1) num_cores <- 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Ensure packages are loaded in each worker
clusterEvalQ(cl, {
  library(dplyr)
  library(randomForest)
  library(purrr)
  library(tidyr)
})

# Export necessary objects to workers
clusterExport(cl, c("joined_df", "cv_pool_siteyears", "fixed_test_siteyears",
                    "calculate_gdd", "calculate_fapar_beer", "evaluate_metrics",
                    "all_feature_combinations", "num_feature_combinations",
                    "original_train_siteyears", "original_val_siteyears",
                    "original_test_siteyears", "tbase"))

# -------------------------------------------------------------------
# Main Parallel Execution with Progress Bar
# -------------------------------------------------------------------

cat("\nStarting evaluation of", num_feature_combinations, "feature combinations (minimum 6 features each)...\n")

# Use progressr for the outer loop progress bar
with_progress({
  p_outer <- progressor(steps = num_feature_combinations)
  
  # Parallel foreach loop
  results_list <- foreach(k = 1:num_feature_combinations, .combine = 'c', 
                          .packages = c("dplyr", "randomForest", "purrr", "tidyr")) %dopar% {
                            
                            current_feature_set <- all_feature_combinations[[k]]
                            feature_set_name <- paste(current_feature_set, collapse = ", ")
                            num_current_features <- length(current_feature_set)
                            
                            # Update progress bar
                            p_outer(sprintf("Combination %d/%d: %s", k, num_feature_combinations, 
                                            substr(feature_set_name, 1, 50))) # Truncate long names
                            
                            # Filter data for current features
                            required_gpp_cols <- c("GPP_site", "PAR_site", "fAPAR", "siteyear", "LUE")
                            temp_rf_data_cols <- unique(c(current_feature_set, required_gpp_cols))
                            
                            temp_rf_data <- joined_df %>%
                              dplyr::select(dplyr::all_of(temp_rf_data_cols)) %>%
                              dplyr::filter(dplyr::across(c("LUE", dplyr::all_of(current_feature_set)), ~!is.na(.)))
                            
                            if (nrow(temp_rf_data) < 50) return(NULL)
                            
                            # ... rest of your evaluation code for each feature combination ...
                            # (Keep all your model training, CV, and evaluation code here)
                            
                            # Return results as a list
                            list(result_tibble)
                          }
})

# Combine results
all_rf_model_results_df <- bind_rows(results_list)

# Clean up parallel workers
stopCluster(cl)

# -------------------------------------------------------------------
# Results Analysis and Visualization
# -------------------------------------------------------------------

# (Keep your existing results analysis and visualization code)

cat("\nTotal execution time:", format(Sys.time() - start_time), "\n")