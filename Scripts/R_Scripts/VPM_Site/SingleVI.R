


###############################################################################
# VEGETATION INDEX EVALUATION WITH NESTED CROSS-VALIDATION
# This script evaluates vegetation indices (VIs) for GPP prediction using:
# 1. 13 site-years for training/validation (split into 11 training + 2 validation)
# 2. 3 fixed site-years for final testing
# Model: GPP_site ~ VI * PAR_site
###############################################################################

# Load required packages
library(dplyr)    # Data manipulation
library(purrr)    # Functional programming
library(Metrics)  # Evaluation metrics (rmse, mae)
library(caret)    # Machine learning functions
library(tibble)   # Tidy data frames
library(eventr)
library(progressr)

# Start simulation timing
start_time <- Sys.time()
cat("Simulation started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Setup progress bar
handlers(global = TRUE)
handlers("txtprogressbar")
# Set random seed for reproducibility

set.seed(42)

# 1. DATA PREPARATION ========================================================

# Define vegetation indices to evaluate (subset for demonstration)
#vi_list <- c("kNDVI", "NDVI", "EVI", "NIRv") 
# List of all vegetation indices to evaluate
vi_list <- c(
  "AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI", 
  "CIG", "CVI", "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",
  "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR", "FCVI", "GARI", 
  "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI", 
  "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2", 
  "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",
  "NDDI", "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI", 
  "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", "OCVI", "OSAVI", 
  "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", "SAVI2", "SEVI", 
  "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI", 
  "VARI", "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI", 
  "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR", "ANDWI", "AWEInsh", 
  "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR", 
  "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",
  "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",
  "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2", 
  "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI"
)

# Get all unique siteyears from dataset
all_siteyears <- unique(joined_df$siteyear)  
stopifnot(length(all_siteyears) == 16)  # Verify we have 16 site-years

# 2. CREATE DATA SPLITS ======================================================

# Fixed test set (3 random site-years)
test_siteyears <- sample(all_siteyears, 3)  

# Remaining 13 site-years for training/validation
train_val_siteyears <- setdiff(all_siteyears, test_siteyears)  

# Generate all possible 11/2 splits (78 combinations) for cross-validation
val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)  

# 3. MODEL EVALUATION FUNCTIONS ==============================================

# Helper function to calculate performance metrics
evaluate_model <- function(model, data) {
  pred <- predict(model, newdata = data)
  list(
    RMSE = rmse(data$GPP_site, pred),  # Root Mean Square Error
    MAE = mae(data$GPP_site, pred),    # Mean Absolute Error
    R2 = cor(data$GPP_site, pred)^2,   # R-squared
    Bias = mean(pred - data$GPP_site)  # Prediction bias
  )
}

# Main function to evaluate a single VI
evaluate_vi <- function(selected_vi) {
  cat("\nEvaluating VI:", selected_vi, "\n")
  
  # 3.1 DATA PREP ============================================================
  model_df <- joined_df %>%
    dplyr::filter(!is.na(GPP_site), !is.na(PAR_site), !is.na(.data[[selected_vi]])) %>%
    dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site)
  
  if (nrow(model_df) < 20) {
    cat("Insufficient data for", selected_vi, "- skipping\n")
    return(list(detailed = NULL, summary = NULL))
  }
  
  test_data <- model_df %>% dplyr::filter(siteyear %in% test_siteyears)
  
  # 3.2 CROSS-VALIDATION (78 FOLDS) ==========================================
  fold_results <- purrr::map_dfr(val_folds, function(val_fold) {
    train_sites <- setdiff(train_val_siteyears, val_fold)
    train_data <- model_df %>% dplyr::filter(siteyear %in% train_sites)
    val_data <- model_df %>% dplyr::filter(siteyear %in% val_fold)
    
    model <- tryCatch(
      lm(GPP_site ~ VI_PAR, data = train_data),
      error = function(e) NULL
    )
    if (is.null(model)) return(NULL)
    
    # Extract coefficients
    coeffs <- coef(model)
    
    train_metrics <- evaluate_model(model, train_data)
    val_metrics <- evaluate_model(model, val_data)
    
    tibble::tibble(
      VI = selected_vi,
      Train_Sites = paste(train_sites, collapse = "|"),
      Val_Sites = paste(val_fold, collapse = "|"),
      # Metrics
      Train_RMSE = train_metrics$RMSE,
      Train_MAE = train_metrics$MAE,
      Train_R2 = train_metrics$R2,
      Train_Bias = train_metrics$Bias,
      Val_RMSE = val_metrics$RMSE,
      Val_MAE = val_metrics$MAE,
      Val_R2 = val_metrics$R2,
      Val_Bias = val_metrics$Bias,
      # Coefficients
      Intercept = coeffs[1],
      VI_PAR_Slope = coeffs[2]
    )
  })
  
  # 3.3 AGGREGATE RESULTS ====================================================
  avg_results <- fold_results %>%
    dplyr::group_by(VI) %>%
    dplyr::summarise(
      # Average metrics
      Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),
      Avg_Train_MAE = mean(Train_MAE, na.rm = TRUE),
      Avg_Train_R2 = mean(Train_R2, na.rm = TRUE),
      Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),
      Avg_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),
      Avg_Val_MAE = mean(Val_MAE, na.rm = TRUE),
      Avg_Val_R2 = mean(Val_R2, na.rm = TRUE),
      Avg_Val_Bias = mean(Val_Bias, na.rm = TRUE),
      # Average coefficients across folds
      Avg_Intercept = mean(Intercept, na.rm = TRUE),
      Avg_VI_PAR_Slope = mean(VI_PAR_Slope, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 3.4 FINAL TEST EVALUATION ================================================
  final_model <- lm(GPP_site ~ VI_PAR, 
                    data = model_df %>% dplyr::filter(siteyear %in% train_val_siteyears))
  test_metrics <- evaluate_model(final_model, test_data)
  final_coeffs <- coef(final_model)
  
  # 3.5 COMBINE ALL RESULTS ==================================================
  summary_results <- avg_results %>%
    dplyr::mutate(
      # Test metrics
      Test_RMSE = test_metrics$RMSE,
      Test_MAE = test_metrics$MAE,
      Test_R2 = test_metrics$R2,
      Test_Bias = test_metrics$Bias,
      # Final model coefficients
      Final_Intercept = final_coeffs[1],
      Final_VI_PAR_Slope = final_coeffs[2],
      # Metadata
      Test_SiteYears = paste(test_siteyears, collapse = "|"),
      Num_Folds = nrow(fold_results)
    )
  
  return(list(
    detailed = fold_results,
    summary = summary_results
  ))
}

# 4. EXECUTE EVALUATION ======================================================

# Evaluate all VIs and store results
all_results <- purrr::map(vi_list, evaluate_vi)

# 5. PROCESS RESULTS =========================================================

# 5.1 DETAILED RESULTS (78 rows per VI) =====================================
detailed_results_df <- purrr::map_dfr(all_results, ~ .x$detailed)

# 5.2 SUMMARY RESULTS (1 row per VI) =========================================
summary_results_df <- purrr::map_dfr(all_results, ~ .x$summary) %>%
  dplyr::arrange(desc(Avg_Val_R2)) %>%            # Sort by validation R2
  dplyr::mutate(dplyr::across(where(is.numeric),  # Format numbers
                              ~ signif(., 4)))

# 6. OUTPUT RESULTS ==========================================================

# View detailed fold-by-fold results
print("Detailed Results (All Folds):")
print(detailed_results_df)

# View formatted summary results
print("\nSummary Results (Averages):")
View(summary_results_df)

# Print top 20 VIs with lowest Avg_Train_MAE
top_20_lowest_train_mae <- summary_results_df %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::pull(VI)

# Optional: Save results to CSV
write.csv(summary_results_df, "vi_evaluation_results.csv", row.names = FALSE)
# End simulation timing
end_time <- Sys.time()
duration <- end_time - start_time

# Print timing results
cat("Simulation ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total simulation time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")



# 6. DAP> ==========================================================
# 1. DATA PREPARATION ========================================================

# Create a filtered dataset for DAP > 150
joined_df_150DAP <- joined_df %>% 
  dplyr::filter(DAP > 100)

####################################################
###Faster code by chatgpt#########################
##################################################
###############################################################################
# VEGETATION INDEX EVALUATION WITH NESTED CROSS-VALIDATION
# Predicting GPP using VI * PAR interaction across site-years
# - 13 site-years used in 78 CV splits (11 train + 2 validation)
# - 3 held-out site-years for final testing
###############################################################################

# Load required packages
library(dplyr)       # Data manipulation
library(purrr)       # Functional programming
library(Metrics)     # RMSE, MAE
library(caret)       # ML workflows
library(tibble)      # Tidy data frames
library(progressr)   # Progress bar for long loops

# Start simulation timing
start_time <- Sys.time()
cat("Simulation started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")

# Setup progress bar
handlers(global = TRUE)
handlers("txtprogressbar")

# Set reproducibility
set.seed(42)

# 1. DATA PREPARATION ========================================================

# Define VIs to evaluate (short example or full list)
vi_list <- c(
  "AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI", 
  "CIG", "CVI", "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",
  "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR", "FCVI", "GARI", 
  "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI", 
  "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2", 
  "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",
  "NDDI", "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI", 
  "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", "OCVI", "OSAVI", 
  "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", "SAVI2", "SEVI", 
  "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI", 
  "VARI", "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI", 
  "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR", "ANDWI", "AWEInsh", 
  "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR", 
  "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",
  "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",
  "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2", 
  "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI"
)
# Ensure correct number of siteyears
     # Data manipulation library(purrr)       # Functional programming library(Metrics)     # RMSE, MAE library(caret)       # ML workflows library(tibble)      # Tidy data frames library(progressr)   # Progress bar for long loops  # Start simulation timing start_time <- Sys.time() cat("Simulation started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")  # Setup progress bar handlers(global = TRUE) handlers("txtprogressbar")  # Set reproducibility set.seed(42)  # 1. DATA PREPARATION ========================================================  # Define VIs to evaluate (short example or full list) vi_list <- c(   "AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI",    "CIG", "CVI", "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",   "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR", "FCVI", "GARI",    "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI",    "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2",    "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",   "NDDI", "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI",    "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", "OCVI", "OSAVI",    "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", "SAVI2", "SEVI",    "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI",    "VARI", "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI",    "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR", "ANDWI", "AWEInsh",    "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR",    "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",   "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",   "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2",    "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI" ) # Ensure correct number of siteyears all_siteyears <- unique( # Load required packages library(dplyr)       # Data manipulation library(purrr)       # Functional programming library(Metrics)     # RMSE, MAE library(caret)       # ML workflows library(tibble)      # Tidy data frames library(progressr)   # Progress bar for long loops  # Start simulation timing start_time <- Sys.time() cat("Simulation started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")  # Setup progress bar handlers(global = TRUE) handlers("txtprogressbar")  # Set reproducibility set.seed(42)  # 1. DATA PREPARATION ========================================================  # Define VIs to evaluate (short example or full list) vi_list <- c(   "AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI",    "CIG", "CVI", "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",   "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR", "FCVI", "GARI",    "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI",    "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2",    "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",   "NDDI", "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI",    "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", "OCVI", "OSAVI",    "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", "SAVI2", "SEVI",    "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI",    "VARI", "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI",    "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR", "ANDWI", "AWEInsh",    "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR",    "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",   "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",   "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2",    "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI" ) # Ensure correct number of siteyears all_siteyears <- unique(joined_df_150DAP$siteyear) stopifnot(length(all_siteyears) == 16)  # Define test and training-validation siteyears test_siteyears <- sample(all_siteyears, 3) train_val_siteyears <- setdiff(all_siteyears, test_siteyears)  # Generate 78 validation folds: all 2-siteyear combinations val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)  # 2. MODEL FUNCTIONS =========================================================  # Evaluate prediction performance evaluate_model <- function(model, data) {   pred <- predict(model, newdata = data)   list(     RMSE = rmse(data$GPP_site, pred),     MAE = mae(data$GPP_site, pred),     R2 = cor(data$GPP_site, pred)^2,     Bias = mean(pred - data$GPP_site)   ) }  # Evaluate a single VI using nested CV evaluate_vi <- function(selected_vi) {   cat("\nEvaluating VI:", selected_vi, "\n")      model_df <- joined_df_150DAP %>%     dplyr::filter(       !is.na(GPP_site),       !is.na(PAR_site),       !is.na(.data[[selected_vi]])     ) %>%     dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site)      if (nrow(model_df) < 20) {     cat("Insufficient data for", selected_vi, "- skipping\n")     return(list(detailed = NULL, summary = NULL))   }      test_data <- dplyr::filter(model_df, siteyear %in% test_siteyears)      fold_results <- purrr::map_dfr(val_folds, function(val_fold) {     train_sites <- setdiff(train_val_siteyears, val_fold)     train_data <- dplyr::filter(model_df, siteyear %in% train_sites)     val_data <- dplyr::filter(model_df, siteyear %in% val_fold)          model <- tryCatch(       lm(GPP_site ~ VI_PAR, data = train_data),       error = function(e) NULL     )     if (is.null(model)) return(NULL)          train_metrics <- evaluate_model(model, train_data)     val_metrics <- evaluate_model(model, val_data)          tibble::tibble(       VI = selected_vi,       Train_Sites = paste(train_sites, collapse = "|"),       Val_Sites = paste(val_fold, collapse = "|"),       Train_RMSE = train_metrics$RMSE,       Train_MAE = train_metrics$MAE,       Train_R2 = train_metrics$R2,       Train_Bias = train_metrics$Bias,       Val_RMSE = val_metrics$RMSE,       Val_MAE = val_metrics$MAE,       Val_R2 = val_metrics$R2,       Val_Bias = val_metrics$Bias     )   })      avg_results <- dplyr::group_by(fold_results, VI) %>%     dplyr::summarise(       Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),       Avg_Train_MAE = mean(Train_MAE, na.rm = TRUE),       Avg_Train_R2 = mean(Train_R2, na.rm = TRUE),       Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),       Avg_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),       Avg_Val_MAE = mean(Val_MAE, na.rm = TRUE),       Avg_Val_R2 = mean(Val_R2, na.rm = TRUE),       Avg_Val_Bias = mean(Val_Bias, na.rm = TRUE),       .groups = "drop"     )      final_model <- lm(GPP_site ~ VI_PAR,                     data = dplyr::filter(model_df, siteyear %in% train_val_siteyears))   test_metrics <- evaluate_model(final_model, test_data)      summary_results <- avg_results %>%     dplyr::mutate(       Test_RMSE = test_metrics$RMSE,       Test_MAE = test_metrics$MAE,       Test_R2 = test_metrics$R2,       Test_Bias = test_metrics$Bias     )      list(detailed = fold_results, summary = summary_results) }  # 3. RUN ALL VIs =============================================================  # Enable progress bar handlers(global = TRUE) progressr::with_progress({   p <- progressr::progressor(steps = length(vi_list))      vi_results <- purrr::map(vi_list, function(vi) {     p(sprintf("Evaluating %s", vi))     evaluate_vi(vi)   }) })  # Combine results all_detailed_results <- purrr::map_dfr(vi_results, "detailed", .id = "VI_index") all_summary_results <- purrr::map_dfr(vi_results, "summary", .id = "VI_index")  # 5.2 SUMMARY RESULTS (1 row per VI) ========================================= summary_results_df <- purrr::map_dfr(all_results, ~ .x$summary) %>%   dplyr::arrange(Avg_Train_MAE) %>%               # Sort by lowest train MAE   dplyr::mutate(dplyr::across(where(is.numeric),  # Format numbers                               ~ signif(., 4)))  View(summary_results_df)  # End simulation timing end_time <- Sys.time() duration <- end_time - start_time  # Print timing results cat("Simulation ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n") cat("Total simulation time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")  View(all_summary_results) # Print top 20 VIs with lowest Avg_Train_MAE top_20_lowest_train_mae <- summary_results_df %>%   dplyr::slice_head(n = 30) %>%   dplyr::pull(VI)  print(top_20_lowest_train_mae)$siteyear) stopifnot(length(all_siteyears) == 16)  # Define test and training-validation siteyears test_siteyears <- sample(all_siteyears, 3) train_val_siteyears <- setdiff(all_siteyears, test_siteyears)  # Generate 78 validation folds: all 2-siteyear combinations val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)  # 2. MODEL FUNCTIONS =========================================================  # Evaluate prediction performance evaluate_model <- function(model, data) {   pred <- predict(model, newdata = data)   list(     RMSE = rmse(data$GPP_site, pred),     MAE = mae(data$GPP_site, pred),     R2 = cor(data$GPP_site, pred)^2,     Bias = mean(pred - data$GPP_site)   ) }  # Evaluate a single VI using nested CV evaluate_vi <- function(selected_vi) {   cat("\nEvaluating VI:", selected_vi, "\n")      model_df <- joined_df_150DAP %>%     dplyr::filter(       !is.na(GPP_site),       !is.na(PAR_site),       !is.na(.data[[selected_vi]])     ) %>%     dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site)      if (nrow(model_df) < 20) {     cat("Insufficient data for", selected_vi, "- skipping\n")     return(list(detailed = NULL, summary = NULL))   }      test_data <- dplyr::filter(model_df, siteyear %in% test_siteyears)      fold_results <- purrr::map_dfr(val_folds, function(val_fold) {     train_sites <- setdiff(train_val_siteyears, val_fold)     train_data <- dplyr::filter(model_df, siteyear %in% train_sites)     val_data <- dplyr::filter(model_df, siteyear %in% val_fold)          model <- tryCatch(       lm(GPP_site ~ VI_PAR, data = train_data),       error = function(e) NULL     )     if (is.null(model)) return(NULL)          train_metrics <- evaluate_model(model, train_data)     val_metrics <- evaluate_model(model, val_data)          tibble::tibble(       VI = selected_vi,       Train_Sites = paste(train_sites, collapse = "|"),       Val_Sites = paste(val_fold, collapse = "|"),       Train_RMSE = train_metrics$RMSE,       Train_MAE = train_metrics$MAE,       Train_R2 = train_metrics$R2,       Train_Bias = train_metrics$Bias,       Val_RMSE = val_metrics$RMSE,       Val_MAE = val_metrics$MAE,       Val_R2 = val_metrics$R2,       Val_Bias = val_metrics$Bias     )   })      avg_results <- dplyr::group_by(fold_results, VI) %>%     dplyr::summarise(       Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),       Avg_Train_MAE = mean(Train_MAE, na.rm = TRUE),       Avg_Train_R2 = mean(Train_R2, na.rm = TRUE),       Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),       Avg_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),       Avg_Val_MAE = mean(Val_MAE, na.rm = TRUE),       Avg_Val_R2 = mean(Val_R2, na.rm = TRUE),       Avg_Val_Bias = mean(Val_Bias, na.rm = TRUE),       .groups = "drop"     )      final_model <- lm(GPP_site ~ VI_PAR,                     data = dplyr::filter(model_df, siteyear %in% train_val_siteyears))   test_metrics <- evaluate_model(final_model, test_data)      summary_results <- avg_results %>%     dplyr::mutate(       Test_RMSE = test_metrics$RMSE,       Test_MAE = test_metrics$MAE,       Test_R2 = test_metrics$R2,       Test_Bias = test_metrics$Bias     )      list(detailed = fold_results, summary = summary_results) }  # 3. RUN ALL VIs =============================================================  # Enable progress bar handlers(global = TRUE) progressr::with_progress({   p <- progressr::progressor(steps = length(vi_list))      vi_results <- purrr::map(vi_list, function(vi) {     p(sprintf("Evaluating %s", vi))     evaluate_vi(vi)   }) })  # Combine results all_detailed_results <- purrr::map_dfr(vi_results, "detailed", .id = "VI_index") all_summary_results <- purrr::map_dfr(vi_results, "summary", .id = "VI_index")  # 5.2 SUMMARY RESULTS (1 row per VI) ========================================= summary_results_df <- purrr::map_dfr(all_results, ~ .x$summary) %>%   dplyr::arrange(Avg_Train_MAE) %>%               # Sort by lowest train MAE   dplyr::mutate(dplyr::across(where(is.numeric),  # Format numbers                               ~ signif(., 4)))  View(summary_results_df)  # End simulation timing end_time <- Sys.time() duration <- end_time - start_time  # Print timing results cat("Simulation ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n") cat("Total simulation time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")  View(all_summary_results) # Print top 20 VIs with lowest Avg_Train_MAE top_20_lowest_train_mae <- summary_results_df %>%   dplyr::slice_head(n = 30) %>%   dplyr::pull(VI)  print(top_20_lowest_train_mae)$siteyear)


# Define test and training-validation siteyears
test_siteyears <- sample(all_siteyears, 3)
train_val_siteyears <- setdiff(all_siteyears, test_siteyears)

# Generate 78 validation folds: all 2-siteyear combinations
val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)

# 2. MODEL FUNCTIONS =========================================================

# Evaluate prediction performance
evaluate_model <- function(model, data) {
  pred <- predict(model, newdata = data)
  list(
    RMSE = rmse(data$GPP_site, pred),
    MAE = mae(data$GPP_site, pred),
    R2 = cor(data$GPP_site, pred)^2,
    Bias = mean(pred - data$GPP_site)
  )
}

# Evaluate a single VI using nested CV
evaluate_vi <- function(selected_vi) {
  cat("\nEvaluating VI:", selected_vi, "\n")
  
  model_df <-  # Load required packages library(dplyr)       # Data manipulation library(purrr)       # Functional programming library(Metrics)     # RMSE, MAE library(caret)       # ML workflows library(tibble)      # Tidy data frames library(progressr)   # Progress bar for long loops  # Start simulation timing start_time <- Sys.time() cat("Simulation started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")  # Setup progress bar handlers(global = TRUE) handlers("txtprogressbar")  # Set reproducibility set.seed(42)  # 1. DATA PREPARATION ========================================================  # Define VIs to evaluate (short example or full list) vi_list <- c(   "AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI",    "CIG", "CVI", "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",   "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR", "FCVI", "GARI",    "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI",    "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2",    "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",   "NDDI", "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI",    "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", "OCVI", "OSAVI",    "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", "SAVI2", "SEVI",    "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI",    "VARI", "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI",    "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR", "ANDWI", "AWEInsh",    "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR",    "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",   "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",   "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2",    "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI" ) # Ensure correct number of siteyears all_siteyears <- unique( # Load required packages library(dplyr)       # Data manipulation library(purrr)       # Functional programming library(Metrics)     # RMSE, MAE library(caret)       # ML workflows library(tibble)      # Tidy data frames library(progressr)   # Progress bar for long loops  # Start simulation timing start_time <- Sys.time() cat("Simulation started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")  # Setup progress bar handlers(global = TRUE) handlers("txtprogressbar")  # Set reproducibility set.seed(42)  # 1. DATA PREPARATION ========================================================  # Define VIs to evaluate (short example or full list) vi_list <- c(   "AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI",    "CIG", "CVI", "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",   "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR", "FCVI", "GARI",    "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI",    "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2",    "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",   "NDDI", "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI",    "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", "OCVI", "OSAVI",    "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", "SAVI2", "SEVI",    "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI",    "VARI", "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI",    "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR", "ANDWI", "AWEInsh",    "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR",    "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",   "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",   "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2",    "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI" ) # Ensure correct number of siteyears all_siteyears <- unique(joined_df_150DAP$siteyear) stopifnot(length(all_siteyears) == 16)  # Define test and training-validation siteyears test_siteyears <- sample(all_siteyears, 3) train_val_siteyears <- setdiff(all_siteyears, test_siteyears)  # Generate 78 validation folds: all 2-siteyear combinations val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)  # 2. MODEL FUNCTIONS =========================================================  # Evaluate prediction performance evaluate_model <- function(model, data) {   pred <- predict(model, newdata = data)   list(     RMSE = rmse(data$GPP_site, pred),     MAE = mae(data$GPP_site, pred),     R2 = cor(data$GPP_site, pred)^2,     Bias = mean(pred - data$GPP_site)   ) }  # Evaluate a single VI using nested CV evaluate_vi <- function(selected_vi) {   cat("\nEvaluating VI:", selected_vi, "\n")      model_df <- joined_df_150DAP %>%     dplyr::filter(       !is.na(GPP_site),       !is.na(PAR_site),       !is.na(.data[[selected_vi]])     ) %>%     dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site)      if (nrow(model_df) < 20) {     cat("Insufficient data for", selected_vi, "- skipping\n")     return(list(detailed = NULL, summary = NULL))   }      test_data <- dplyr::filter(model_df, siteyear %in% test_siteyears)      fold_results <- purrr::map_dfr(val_folds, function(val_fold) {     train_sites <- setdiff(train_val_siteyears, val_fold)     train_data <- dplyr::filter(model_df, siteyear %in% train_sites)     val_data <- dplyr::filter(model_df, siteyear %in% val_fold)          model <- tryCatch(       lm(GPP_site ~ VI_PAR, data = train_data),       error = function(e) NULL     )     if (is.null(model)) return(NULL)          train_metrics <- evaluate_model(model, train_data)     val_metrics <- evaluate_model(model, val_data)          tibble::tibble(       VI = selected_vi,       Train_Sites = paste(train_sites, collapse = "|"),       Val_Sites = paste(val_fold, collapse = "|"),       Train_RMSE = train_metrics$RMSE,       Train_MAE = train_metrics$MAE,       Train_R2 = train_metrics$R2,       Train_Bias = train_metrics$Bias,       Val_RMSE = val_metrics$RMSE,       Val_MAE = val_metrics$MAE,       Val_R2 = val_metrics$R2,       Val_Bias = val_metrics$Bias     )   })      avg_results <- dplyr::group_by(fold_results, VI) %>%     dplyr::summarise(       Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),       Avg_Train_MAE = mean(Train_MAE, na.rm = TRUE),       Avg_Train_R2 = mean(Train_R2, na.rm = TRUE),       Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),       Avg_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),       Avg_Val_MAE = mean(Val_MAE, na.rm = TRUE),       Avg_Val_R2 = mean(Val_R2, na.rm = TRUE),       Avg_Val_Bias = mean(Val_Bias, na.rm = TRUE),       .groups = "drop"     )      final_model <- lm(GPP_site ~ VI_PAR,                     data = dplyr::filter(model_df, siteyear %in% train_val_siteyears))   test_metrics <- evaluate_model(final_model, test_data)      summary_results <- avg_results %>%     dplyr::mutate(       Test_RMSE = test_metrics$RMSE,       Test_MAE = test_metrics$MAE,       Test_R2 = test_metrics$R2,       Test_Bias = test_metrics$Bias     )      list(detailed = fold_results, summary = summary_results) }  # 3. RUN ALL VIs =============================================================  # Enable progress bar handlers(global = TRUE) progressr::with_progress({   p <- progressr::progressor(steps = length(vi_list))      vi_results <- purrr::map(vi_list, function(vi) {     p(sprintf("Evaluating %s", vi))     evaluate_vi(vi)   }) })  # Combine results all_detailed_results <- purrr::map_dfr(vi_results, "detailed", .id = "VI_index") all_summary_results <- purrr::map_dfr(vi_results, "summary", .id = "VI_index")  # 5.2 SUMMARY RESULTS (1 row per VI) ========================================= summary_results_df <- purrr::map_dfr(all_results, ~ .x$summary) %>%   dplyr::arrange(Avg_Train_MAE) %>%               # Sort by lowest train MAE   dplyr::mutate(dplyr::across(where(is.numeric),  # Format numbers                               ~ signif(., 4)))  View(summary_results_df)  # End simulation timing end_time <- Sys.time() duration <- end_time - start_time  # Print timing results cat("Simulation ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n") cat("Total simulation time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")  View(all_summary_results) # Print top 20 VIs with lowest Avg_Train_MAE top_20_lowest_train_mae <- summary_results_df %>%   dplyr::slice_head(n = 30) %>%   dplyr::pull(VI)  print(top_20_lowest_train_mae)$siteyear) stopifnot(length(all_siteyears) == 16)  # Define test and training-validation siteyears test_siteyears <- sample(all_siteyears, 3) train_val_siteyears <- setdiff(all_siteyears, test_siteyears)  # Generate 78 validation folds: all 2-siteyear combinations val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)  # 2. MODEL FUNCTIONS =========================================================  # Evaluate prediction performance evaluate_model <- function(model, data) {   pred <- predict(model, newdata = data)   list(     RMSE = rmse(data$GPP_site, pred),     MAE = mae(data$GPP_site, pred),     R2 = cor(data$GPP_site, pred)^2,     Bias = mean(pred - data$GPP_site)   ) }  # Evaluate a single VI using nested CV evaluate_vi <- function(selected_vi) {   cat("\nEvaluating VI:", selected_vi, "\n")      model_df <- joined_df_150DAP %>%     dplyr::filter(       !is.na(GPP_site),       !is.na(PAR_site),       !is.na(.data[[selected_vi]])     ) %>%     dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site)      if (nrow(model_df) < 20) {     cat("Insufficient data for", selected_vi, "- skipping\n")     return(list(detailed = NULL, summary = NULL))   }      test_data <- dplyr::filter(model_df, siteyear %in% test_siteyears)      fold_results <- purrr::map_dfr(val_folds, function(val_fold) {     train_sites <- setdiff(train_val_siteyears, val_fold)     train_data <- dplyr::filter(model_df, siteyear %in% train_sites)     val_data <- dplyr::filter(model_df, siteyear %in% val_fold)          model <- tryCatch(       lm(GPP_site ~ VI_PAR, data = train_data),       error = function(e) NULL     )     if (is.null(model)) return(NULL)          train_metrics <- evaluate_model(model, train_data)     val_metrics <- evaluate_model(model, val_data)          tibble::tibble(       VI = selected_vi,       Train_Sites = paste(train_sites, collapse = "|"),       Val_Sites = paste(val_fold, collapse = "|"),       Train_RMSE = train_metrics$RMSE,       Train_MAE = train_metrics$MAE,       Train_R2 = train_metrics$R2,       Train_Bias = train_metrics$Bias,       Val_RMSE = val_metrics$RMSE,       Val_MAE = val_metrics$MAE,       Val_R2 = val_metrics$R2,       Val_Bias = val_metrics$Bias     )   })      avg_results <- dplyr::group_by(fold_results, VI) %>%     dplyr::summarise(       Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),       Avg_Train_MAE = mean(Train_MAE, na.rm = TRUE),       Avg_Train_R2 = mean(Train_R2, na.rm = TRUE),       Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),       Avg_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),       Avg_Val_MAE = mean(Val_MAE, na.rm = TRUE),       Avg_Val_R2 = mean(Val_R2, na.rm = TRUE),       Avg_Val_Bias = mean(Val_Bias, na.rm = TRUE),       .groups = "drop"     )      final_model <- lm(GPP_site ~ VI_PAR,                     data = dplyr::filter(model_df, siteyear %in% train_val_siteyears))   test_metrics <- evaluate_model(final_model, test_data)      summary_results <- avg_results %>%     dplyr::mutate(       Test_RMSE = test_metrics$RMSE,       Test_MAE = test_metrics$MAE,       Test_R2 = test_metrics$R2,       Test_Bias = test_metrics$Bias     )      list(detailed = fold_results, summary = summary_results) }  # 3. RUN ALL VIs =============================================================  # Enable progress bar handlers(global = TRUE) progressr::with_progress({   p <- progressr::progressor(steps = length(vi_list))      vi_results <- purrr::map(vi_list, function(vi) {     p(sprintf("Evaluating %s", vi))     evaluate_vi(vi)   }) })  # Combine results all_detailed_results <- purrr::map_dfr(vi_results, "detailed", .id = "VI_index") all_summary_results <- purrr::map_dfr(vi_results, "summary", .id = "VI_index")  # 5.2 SUMMARY RESULTS (1 row per VI) ========================================= summary_results_df <- purrr::map_dfr(all_results, ~ .x$summary) %>%   dplyr::arrange(Avg_Train_MAE) %>%               # Sort by lowest train MAE   dplyr::mutate(dplyr::across(where(is.numeric),  # Format numbers                               ~ signif(., 4)))  View(summary_results_df)  # End simulation timing end_time <- Sys.time() duration <- end_time - start_time  # Print timing results cat("Simulation ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n") cat("Total simulation time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")  View(all_summary_results) # Print top 20 VIs with lowest Avg_Train_MAE top_20_lowest_train_mae <- summary_results_df %>%   dplyr::slice_head(n = 30) %>%   dplyr::pull(VI)  print(top_20_lowest_train_mae) %>%
    dplyr::filter(
      !is.na(GPP_site),
      !is.na(PAR_site),
      !is.na(.data[[selected_vi]])
    ) %>%
    dplyr::mutate(VI_PAR = .data[[selected_vi]] * PAR_site)
  
  if (nrow(model_df) < 20) {
    cat("Insufficient data for", selected_vi, "- skipping\n")
    return(list(detailed = NULL, summary = NULL))
  }
  
  test_data <- dplyr::filter(model_df, siteyear %in% test_siteyears)
  
  fold_results <- purrr::map_dfr(val_folds, function(val_fold) {
    train_sites <- setdiff(train_val_siteyears, val_fold)
    train_data <- dplyr::filter(model_df, siteyear %in% train_sites)
    val_data <- dplyr::filter(model_df, siteyear %in% val_fold)
    
    model <- tryCatch(
      lm(GPP_site ~ VI_PAR, data = train_data),
      error = function(e) NULL
    )
    if (is.null(model)) return(NULL)
    
    train_metrics <- evaluate_model(model, train_data)
    val_metrics <- evaluate_model(model, val_data)
    
    tibble::tibble(
      VI = selected_vi,
      Train_Sites = paste(train_sites, collapse = "|"),
      Val_Sites = paste(val_fold, collapse = "|"),
      Train_RMSE = train_metrics$RMSE,
      Train_MAE = train_metrics$MAE,
      Train_R2 = train_metrics$R2,
      Train_Bias = train_metrics$Bias,
      Val_RMSE = val_metrics$RMSE,
      Val_MAE = val_metrics$MAE,
      Val_R2 = val_metrics$R2,
      Val_Bias = val_metrics$Bias
    )
  })
  
  avg_results <- dplyr::group_by(fold_results, VI) %>%
    dplyr::summarise(
      Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),
      Avg_Train_MAE = mean(Train_MAE, na.rm = TRUE),
      Avg_Train_R2 = mean(Train_R2, na.rm = TRUE),
      Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),
      Avg_Val_RMSE = mean(Val_RMSE, na.rm = TRUE),
      Avg_Val_MAE = mean(Val_MAE, na.rm = TRUE),
      Avg_Val_R2 = mean(Val_R2, na.rm = TRUE),
      Avg_Val_Bias = mean(Val_Bias, na.rm = TRUE),
      .groups = "drop"
    )
  
  final_model <- lm(GPP_site ~ VI_PAR,
                    data = dplyr::filter(model_df, siteyear %in% train_val_siteyears))
  test_metrics <- evaluate_model(final_model, test_data)
  
  summary_results <- avg_results %>%
    dplyr::mutate(
      Test_RMSE = test_metrics$RMSE,
      Test_MAE = test_metrics$MAE,
      Test_R2 = test_metrics$R2,
      Test_Bias = test_metrics$Bias
    )
  
  list(detailed = fold_results, summary = summary_results)
}

# 3. RUN ALL VIs =============================================================

# Enable progress bar
handlers(global = TRUE)
progressr::with_progress({
  p <- progressr::progressor(steps = length(vi_list))
  
  vi_results <- purrr::map(vi_list, function(vi) {
    p(sprintf("Evaluating %s", vi))
    evaluate_vi(vi)
  })
})

# Combine results
all_detailed_results <- purrr::map_dfr(vi_results, "detailed", .id = "VI_index")
all_summary_results <- purrr::map_dfr(vi_results, "summary", .id = "VI_index")

# 5.2 SUMMARY RESULTS (1 row per VI) =========================================
summary_results_df <- purrr::map_dfr(all_results, ~ .x$summary) %>%
  dplyr::arrange(Avg_Train_MAE) %>%               # Sort by lowest train MAE
  dplyr::mutate(dplyr::across(where(is.numeric),  # Format numbers
                              ~ signif(., 4)))

View(summary_results_df)

# End simulation timing
end_time <- Sys.time()
duration <- end_time - start_time

# Print timing results
cat("Simulation ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total simulation time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")

View(all_summary_results)
# Print top 20 VIs with lowest Avg_Train_MAE
top_20_lowest_train_mae <- summary_results_df %>%
  dplyr::slice_head(n = 30) %>%
  dplyr::pull(VI)

print(top_20_lowest_train_mae)
