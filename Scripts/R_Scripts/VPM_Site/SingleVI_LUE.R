###############################################################################
# LIGHT USE EFFICIENCY (LUE) PREDICTION FROM VEGETATION INDICES
###############################################################################

# 1. SETUP AND CONFIGURATION =================================================

library(dplyr)
library(purrr)
library(Metrics)
library(caret)
library(tibble)
library(progressr)

start_time <- Sys.time()
cat("Analysis started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
set.seed(42)

# 2. DATA PREPARATION ========================================================

calculate_fapar <- function(LAI, K = 0.5) {
  1 - exp(-K * LAI)
}

joined_df <- dplyr::left_join(
  sitecombineddata, 
  VImeteo20152018combine, 
  by = "siteyeardate"
) %>%
  dplyr::mutate(
    fAPAR = calculate_fapar(Lai),
    APAR = fAPAR * PAR_site,
    LUE = GPP_site / APAR
  )

rf_data <- joined_df %>%
  dplyr::filter(is.finite(LUE), !is.na(LUE))

all_siteyears <- unique(rf_data$siteyear)
stopifnot(length(all_siteyears) == 16)

# 3. CROSS-VALIDATION SETUP ==================================================

test_siteyears <- sample(all_siteyears, 3)
train_val_siteyears <- setdiff(all_siteyears, test_siteyears)
val_folds <- combn(train_val_siteyears, 2, simplify = FALSE)

# 4. VEGETATION INDICES TO EVALUATE ==========================================

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


# 5. MODELING FUNCTIONS ======================================================

evaluate_model <- function(model, data) {
  pred <- predict(model, newdata = data)
  list(
    RMSE = rmse(data$LUE, pred),
    MAE = mae(data$LUE, pred),
    R2 = cor(data$LUE, pred, use = "complete.obs")^2,
    Bias = mean(pred - data$LUE, na.rm = TRUE)
  )
}

evaluate_vi <- function(selected_vi) {
  model_df <- rf_data %>%
    dplyr::filter(!is.na(LUE), !is.na(.data[[selected_vi]])) %>%
    dplyr::mutate(VI_value = .data[[selected_vi]])
  
  if (nrow(model_df) < 20) return(NULL)
  
  test_data <- model_df %>% 
    dplyr::filter(siteyear %in% test_siteyears)
  
  # Store detailed fold-by-fold metrics
  detailed_results <- purrr::map_dfr(val_folds, function(val_fold) {
    train_data <- model_df %>% 
      dplyr::filter(siteyear %in% setdiff(train_val_siteyears, val_fold))
    val_data <- model_df %>% 
      dplyr::filter(siteyear %in% val_fold)
    
    model <- tryCatch(
      lm(LUE ~ VI_value, data = train_data),
      error = function(e) NULL
    )
    if (is.null(model)) return(NULL)
    
    train_metrics <- evaluate_model(model, train_data)
    val_metrics <- evaluate_model(model, val_data)
    
    tibble::tibble(
      VI = selected_vi,
      Fold = paste(val_fold, collapse = "|"),
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
  
  # Average metrics across folds
  summary_metrics <- detailed_results %>%
    dplyr::group_by(VI) %>%
    dplyr::summarise(
      Avg_Train_RMSE = mean(Train_RMSE, na.rm = TRUE),
      Avg_Train_MAE  = mean(Train_MAE, na.rm = TRUE),
      Avg_Train_R2   = mean(Train_R2, na.rm = TRUE),
      Avg_Train_Bias = mean(Train_Bias, na.rm = TRUE),
      Avg_Val_RMSE   = mean(Val_RMSE, na.rm = TRUE),
      Avg_Val_MAE    = mean(Val_MAE, na.rm = TRUE),
      Avg_Val_R2     = mean(Val_R2, na.rm = TRUE),
      Avg_Val_Bias   = mean(Val_Bias, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Test set evaluation
  final_model <- lm(LUE ~ VI_value, data = model_df %>% dplyr::filter(siteyear %in% train_val_siteyears))
  test_metrics <- evaluate_model(final_model, test_data)
  
  test_metrics_df <- tibble::tibble(
    VI = selected_vi,
    Test_RMSE = test_metrics$RMSE,
    Test_MAE  = test_metrics$MAE,
    Test_R2   = test_metrics$R2,
    Test_Bias = test_metrics$Bias
  )
  
  # Combine average and test into one
  full_summary <- dplyr::left_join(summary_metrics, test_metrics_df, by = "VI")
  
  return(list(
    detailed = detailed_results,
    summary = full_summary
  ))
}

# 6. EXECUTE EVALUATION ======================================================

handlers(global = TRUE)
all_results <- list()

with_progress({
  p <- progressor(steps = length(vi_list))
  all_results <- purrr::map(vi_list, function(vi) {
    p(paste("Evaluating", vi))
    evaluate_vi(vi)
  })
})

# 7. PROCESS RESULTS =========================================================

# Combine all fold-level results
detailed_results_dfLUE <- purrr::map_dfr(all_results, ~ .x$detailed)

# Combine all average (train/val) and test summaries
summary_results_dfLUE <- purrr::map_dfr(all_results, ~ .x$summary) %>%
  arrange((Test_MAE)) %>%
  mutate(across(where(is.numeric), ~ signif(., 4)))

View(summary_results_dfLUE)
# Assuming 'ID' is the index column you're interested in
top_20_vis <- head(summary_results_dfLUE$VI, 20)
top_20_vis

# 8. OUTPUT RESULTS ==========================================================

cat("\nSummary Results (Averaged + Test):\n")
print(summary_results_df)

cat("\nDetailed Fold-by-Fold Results:\n")
print(head(detailed_results_df))

# Save to CSV
write.csv(summary_results_df, "vi_lue_summary_results.csv", row.names = FALSE)
write.csv(detailed_results_df, "vi_lue_detailed_fold_results.csv", row.names = FALSE)

# 9. TIMING SUMMARY ==========================================================

end_time <- Sys.time()
duration <- end_time - start_time
cat("Analysis ended at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Total analysis time:", round(as.numeric(duration, units = "secs"), 2), "seconds\n")


# Your selected top 20 indices
top_20_vis <- c("GDVI", "ExGR", "AFRI2100", "MNDVI", "GVMI", "DBSI", "TVI", "DSI", "MSI", "NSDSI1",
                "ExR", "NSDS", "NSDSI3", "IPVI", "PI", "NDVI", "TSAVI", "RNDVI", "NDPI", "AFRI1600")

# 1. Pivot rf_data from wide to long format for plotting
long_df <- rf_data %>%
  dplyr::select(all_of(c("LUE", top_20_vis))) %>%
  pivot_longer(
    cols = all_of(top_20_vis),
    names_to = "VI",
    values_to = "Value"
  )

# 2. Plot: LUE vs VI Value for each of the top 20 indices
ggplot(long_df, aes(x = Value, y = LUE, color = VI)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ VI, scales = "free_x") +
  labs(
    title = "LUE vs. Top 20 Vegetation Indices",
    x = "Vegetation Index Value",
    y = "Light Use Efficiency (LUE)"
  ) +
  theme_bw() +
  theme(legend.position = "none")


###############################################################################
# LIGHT USE EFFICIENCY (LUE > 1) PREDICTION FROM VEGETATION INDICES
###############################################################################

# 1. SETUP AND CONFIGURATION =================================================
library(dplyr)
library(purrr)
library(Metrics)
library(caret)
library(tibble)
library(progressr)

start_time <- Sys.time()
cat("Analysis started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
set.seed(42)

# 2. DATA PREPARATION ========================================================
calculate_fapar <- function(LAI, K = 0.5) {
  1 - exp(-K * LAI)
}

joined_df <- dplyr::left_join(
  sitecombineddata, 
  VImeteo20152018combine, 
  by = "siteyeardate"
) %>%
  dplyr::mutate(
    fAPAR = calculate_fapar(Lai),
    APAR = fAPAR * PAR_site,
    LUE = GPP_site / APAR
  )

# Filter for LUE > 1 only
rf_data <- joined_df %>%
  dplyr::filter(LUE > 1, is.finite(LUE), !is.na(LUE))

cat("Number of observations with LUE > 1:", nrow(rf_data), "\n")

all_siteyears <- unique(rf_data$siteyear)
stopifnot(length(all_siteyears) >= 10)  # Adjust threshold as needed

# 3. CROSS-VALIDATION SETUP ==================================================
test_siteyears <- sample(all_siteyears, 2)
train_val_siteyears <- setdiff(all_siteyears, test_siteyears)
val_folds <- combn(train_val_siteyears, 1, simplify = FALSE)

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
# [Rest of the code remains identical from evaluate_model() through execution]
# 5. MODELING FUNCTIONS ======================================================
evaluate_model <- function(model, data) {
  pred <- predict(model, newdata = data)
  list(
    RMSE = Metrics::rmse(data$LUE, pred),
    MAE = Metrics::mae(data$LUE, pred),
    R2 = cor(data$LUE, pred, use = "complete.obs")^2,
    Bias = mean(pred - data$LUE, na.rm = TRUE)
  )
}

evaluate_vi <- function(selected_vi) {
  model_df <- rf_data %>%
    dplyr::filter(!is.na(.data[[selected_vi]])) %>%
    dplyr::mutate(VI_value = .data[[selected_vi]])
  
  if (nrow(model_df) < 20) {
    cat("Insufficient data for", selected_vi, "- skipping\n")
    return(NULL)
  }
  
  test_data <- model_df %>% 
    dplyr::filter(siteyear %in% test_siteyears)
  
  fold_results <- purrr::map_dfr(val_folds, function(val_fold) {
    train_data <- model_df %>% 
      dplyr::filter(siteyear %in% setdiff(train_val_siteyears, val_fold))
    val_data <- model_df %>% 
      dplyr::filter(siteyear %in% val_fold)
    
    model <- tryCatch(
      lm(LUE ~ VI_value, data = train_data),
      error = function(e) NULL
    )
    if (is.null(model)) return(NULL)
    
    train_metrics <- evaluate_model(model, train_data)
    val_metrics <- evaluate_model(model, val_data)
    
    tibble::tibble(
      VI = selected_vi,
      Train_Sites = paste(setdiff(train_val_siteyears, val_fold), collapse = "|"),
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
  
  # Final model evaluation
  final_model <- lm(LUE ~ VI_value, 
                    data = model_df %>% 
                      dplyr::filter(siteyear %in% train_val_siteyears))
  test_metrics <- evaluate_model(final_model, test_data)
  
  # Summary statistics
  avg_results <- fold_results %>%
    dplyr::group_by(VI) %>%
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
  
  # Combine with test results
  summary_results <- avg_results %>%
    dplyr::mutate(
      Test_RMSE = test_metrics$RMSE,
      Test_MAE = test_metrics$MAE,
      Test_R2 = test_metrics$R2,
      Test_Bias = test_metrics$Bias,
      N_Obs = nrow(model_df)
    )
  
  list(detailed = fold_results, summary = summary_results)
}

# 6. EXECUTION WITH PROGRESS TRACKING ========================================
handlers(global = TRUE)
all_results <- list()

with_progress({
  p <- progressor(steps = length(vi_list))
  all_results <- purrr::map(vi_list, function(vi) {
    p(paste("Evaluating", vi))
    evaluate_vi(vi)
  })
})

# 7. RESULTS PROCESSING ======================================================
# Combine all results
detailed_results <- purrr::map_dfr(all_results, ~ .x$detailed)
summary_results <- purrr::map_dfr(all_results, ~ .x$summary) %>%
  dplyr::arrange(Test_MAE) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~ signif(., 4)))

# Identify top performers
top_10_vis <- summary_results %>%
  dplyr::select(VI, Test_MAE, Test_R2) %>%
  dplyr::slice_head(n = 10)

# 8. OUTPUT AND VISUALIZATION ================================================
cat("\n=== Top 10 VIs for LUE > 1 Prediction ===\n")
print(top_10_vis$VI)

# Save results
write.csv(summary_results, "high_lue_vi_performance.csv", row.names = FALSE)


# Generate performance plot
performance_plot <- ggplot(top_10_vis, aes(x = reorder(VI, -Test_MAE), y = Test_MAE, fill = Test_R2)) +
  geom_col() +
  scale_fill_viridis_c(option = "D", name = "R²") +
  labs(
    title = "Top Vegetation Indices for High-LUE Prediction",
    x = "Vegetation Index",
    y = "Test MAE (Lower is Better)"
  ) +
  theme_minimal() +
  coord_flip()
performance_plot
ggsave("high_lue_vi_performance.png", plot = performance_plot, width = 10, height = 6)

# 9. COMPLETION ==============================================================
end_time <- Sys.time()
duration <- end_time - start_time
cat("\nAnalysis completed at:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")


top_10_vis
# Your selected top top_10_vis
top_10_vis <- c("WI2015", "MBWI", "MuWIR", "MLSWI26", "AWEIsh", "NDVIMNDWI", "WRI", "EVI", "AWEInsh","EVIv")

# 1. Pivot rf_data from wide to long format for plotting
long_df <- rf_data %>%
  dplyr::select(all_of(c("LUE", top_10_vis))) %>%
  pivot_longer(
    cols = all_of(top_10_vis),
    names_to = "VI",
    values_to = "Value"
  )

# 2. Plot: LUE vs VI Value for each of the top 20 indices
ggplot(long_df, aes(x = Value, y = LUE, color = VI)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ VI, scales = "free_x") +
  labs(
    title = "LUE vs. Top 20 Vegetation Indices",
    x = "Vegetation Index Value",
    y = "Light Use Efficiency (LUE)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

plot(joined_df$MuWIR, joined_df$LUE)
