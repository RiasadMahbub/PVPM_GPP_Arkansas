###### THis code finds the single VI

# ""AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI", "CIG", "CVI",
#  "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI",
#  "ENDVI", "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR","FCVI", 
#  "GARI", "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", "GOSAVI", "GRNDVI", "GRVI", "GSAVI", 
#  "GVMI","IAVI", "IKAW", "IPVI",
#  "MCARI1", "MCARI2", "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2",
#  "NDDI",  "NDII", "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv",
#   "NLI", "NMDI", "NRFIg", "NRFIr", "NormG", "NormNIR", "NormR", 
#   "OCVI", "OSAVI", "RCC", "RDVI", "RGBVI", "RGRI", "RI",
#   "SARVI", "SAVI", "SAVI2", "SEVI", "SI",  "SLAVI", "SR", "SR2", 
#   "TDVI", "TGI", "TSAVI", "TVI", "TriVI", "VARI", "VIG", "WDRVI", "WDVI",
#   "bNIRv", "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR",
#   "ANDWI", "AWEInsh", "AWEIsh",  "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", "MuWIR", 
#    "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI",
#    "RNDVI", "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS",
#    "BaI", "DBSI", "EMBI", "MBI", "NDSoI", "NSDS", "NSDSI1", "NSDSI2", "NSDSI3",
#     "RI4XS", "kIPVI", "kNDVI", "kRVI"

# 
# unique(joined_df$siteyear)
# [1] "USOF62018" "USOF52018" "USOF42018" "USOF32017" "USOF22017" "USOF12017" "USBDA2015"
# [8] "USBDA2016" "USBDC2015" "USBDC2016" "USHRA2017" "USHRC2017" "USHRA2015" "USHRA2016"
# [15] "USHRC2015" "USHRC2016"

###Write a linear model
### Spatial PAR = 
## y = GPP_site
## x = f(VI*PAR)

### knn validation 16 site years: 11 training; 

joined_df

### Choose the VI

### Write a linear model

## Calculate RMSE, MAE, R2 and bias

## Apply the function in 20% validation set

## Change the training and testing sets



library(dplyr)
library(purrr)
library(Metrics)  # for rmse(), mae()

# List of all VIs to evaluate
vi_list <- c("AFRI1600", "AFRI2100", "ARVI", "ATSAVI", "AVI", "BCC", "BNDVI", "BWDRVI", "CIG", "CVI",
             "DSI", "DSWI1", "DSWI2", "DSWI3", "DSWI4", "DSWI5", "DVI", "ENDVI", "EVI", "EVI2", "EVIv", 
             "ExG", "ExGR", "ExR", "FCVI", "GARI", "GBNDVI", "GCC", "GDVI", "GEMI", "GLI", "GNDVI", 
             "GOSAVI", "GRNDVI", "GRVI", "GSAVI", "GVMI", "IAVI", "IKAW", "IPVI", "MCARI1", "MCARI2", 
             "MGRVI", "MNDVI", "MNLI", "MRBVI", "MSAVI", "MSI", "MSR", "MTVI1", "MTVI2", "NDDI", "NDII", 
             "NDMI", "NDPI", "NDVI", "NDYI", "NGRDI", "NIRv", "NLI", "NMDI", "NRFIg", "NRFIr", "NormG", 
             "NormNIR", "NormR", "OCVI", "OSAVI", "RCC", "RDVI", "RGBVI", "RGRI", "RI", "SARVI", "SAVI", 
             "SAVI2", "SEVI", "SI", "SLAVI", "SR", "SR2", "TDVI", "TGI", "TSAVI", "TVI", "TriVI", "VARI", 
             "VIG", "WDRVI", "WDVI", "bNIRv", "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", 
             "sNIRvSWIR", "ANDWI", "AWEInsh", "AWEIsh", "LSWI", "MBWI", "MLSWI26", "MLSWI27", "MNDWI", 
             "MuWIR", "NDPonI", "NDTI", "NDVIMNDWI", "NDWI", "NDWIns", "NWI", "OSI", "PI", "RNDVI", 
             "SWM", "WI1", "WI2", "WI2015", "WRI", "BI", "BITM", "BIXS", "BaI", "DBSI", "EMBI", "MBI", 
             "NDSoI", "NSDS", "NSDSI1", "NSDSI2", "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI")

# Set seed for reproducibility
set.seed(42)

# Loop through each VI and evaluate model performance
results_df <- map_dfr(vi_list, function(selected_vi) {
  # Filter and prepare data
  model_df <- joined_df %>%
    filter(!is.na(GPP_site), !is.na(PAR_site), !is.na(.data[[selected_vi]])) %>%
    mutate(VI_PAR = .data[[selected_vi]] * PAR_site)
  
  # Skip if not enough data
  if (nrow(model_df) < 30) return(NULL)
  
  siteyears <- unique(model_df$siteyear)
  if (length(siteyears) < 5) return(NULL)
  
  n_val <- ceiling(0.2 * length(siteyears))
  val_siteyears <- sample(siteyears, n_val)
  train_siteyears <- setdiff(siteyears, val_siteyears)
  
  train_data <- model_df %>% filter(siteyear %in% train_siteyears)
  val_data <- model_df %>% filter(siteyear %in% val_siteyears)
  
  # Fit model and predict
  lm_model <- tryCatch(
    lm(GPP_site ~ VI_PAR, data = train_data),
    error = function(e) return(NULL)
  )
  if (is.null(lm_model)) return(NULL)
  
  val_data <- val_data %>%
    mutate(predicted = predict(lm_model, newdata = val_data))
  
  # Calculate evaluation metrics
  rmse_val <- rmse(val_data$GPP_site, val_data$predicted)
  mae_val <- mae(val_data$GPP_site, val_data$predicted)
  r2_val <- cor(val_data$GPP_site, val_data$predicted)^2
  bias_val <- mean(val_data$predicted - val_data$GPP_site)
  
  # Return results
  tibble(
    VI = selected_vi,
    RMSE = rmse_val,
    MAE = mae_val,
    R2 = r2_val,
    Bias = bias_val,
    Train_SiteYears = paste(train_siteyears, collapse = ", "),
    Val_SiteYears = paste(val_siteyears, collapse = ", ")
  )
})

# Print results sorted by R2
results_df <- results_df %>% arrange(desc(R2))
View(results_df)


library(dplyr)
library(purrr)
library(Metrics)
library(caret)
library(tibble)

set.seed(42)

# List of VIs to evaluate
vi_list <- c("kNDVI", "NDVI", "EVI", "NIRv")  # use full list later if needed

# Fixed test siteyears
all_siteyears <- unique(joined_df$siteyear)
test_siteyears <- sample(all_siteyears, 3)
remaining_siteyears <- setdiff(all_siteyears, test_siteyears)

# Define outer folds (from remaining 13 siteyears)
outer_folds <- combn(remaining_siteyears, 2, simplify = FALSE)  # all 2-siteyear combinations for validation


results_df <- map_dfr(vi_list, function(selected_vi) {
  cat("Processing VI:", selected_vi, "\n")
  
  model_df <- joined_df %>%
    filter(!is.na(GPP_site), !is.na(PAR_site), !is.na(.data[[selected_vi]])) %>%
    mutate(VI_PAR = .data[[selected_vi]] * PAR_site)
  
  if (nrow(model_df) < 50) return(NULL)
  
  # Separate fixed test set
  test_data <- model_df %>% filter(siteyear %in% test_siteyears)
  
  fold_results <- map_dfr(outer_folds, function(val_fold) {
    train_siteyears <- setdiff(remaining_siteyears, val_fold)
    
    train_data <- model_df %>% filter(siteyear %in% train_siteyears)
    val_data <- model_df %>% filter(siteyear %in% val_fold)
    
    # Nested CV using caret (on training data only)
    train_control <- trainControl(method = "cv", number = 5)
    
    knn_fit <- tryCatch({
      train(
        GPP_site ~ VI_PAR,
        data = train_data,
        method = "knn",
        tuneLength = 5,
        trControl = train_control
      )
    }, error = function(e) return(NULL))
    
    if (is.null(knn_fit)) return(NULL)
    
    # Predict on validation fold
    val_pred <- predict(knn_fit, newdata = val_data)
    val_rmse <- rmse(val_data$GPP_site, val_pred)
    val_r2 <- cor(val_data$GPP_site, val_pred)^2
    val_mae <- mae(val_data$GPP_site, val_pred)
    val_bias <- mean(val_pred - val_data$GPP_site)
    
    # Predict on test set
    test_pred <- predict(knn_fit, newdata = test_data)
    test_rmse <- rmse(test_data$GPP_site, test_pred)
    test_r2 <- cor(test_data$GPP_site, test_pred)^2
    test_mae <- mae(test_data$GPP_site, test_pred)
    test_bias <- mean(test_pred - test_data$GPP_site)
    
    tibble(
      VI = selected_vi,
      Val_Fold = paste(val_fold, collapse = ", "),
      Train_SiteYears = paste(train_siteyears, collapse = ", "),
      Val_RMSE = val_rmse,
      Val_MAE = val_mae,
      Val_R2 = val_r2,
      Val_Bias = val_bias,
      Test_RMSE = test_rmse,
      Test_MAE = test_mae,
      Test_R2 = test_r2,
      Test_Bias = test_bias
    )
  })
  
  fold_results
})


# View all results
print(results_df)

# Optional: summarize per VI (mean across folds)
summary_by_vi <- results_df %>%
  group_by(VI) %>%
  summarise(
    Mean_Val_R2 = mean(Val_R2, na.rm = TRUE),
    Mean_Test_R2 = mean(Test_R2, na.rm = TRUE),
    Mean_Test_RMSE = mean(Test_RMSE, na.rm = TRUE),
    Mean_Test_MAE = mean(Test_MAE, na.rm = TRUE),
    Mean_Test_Bias = mean(Test_Bias, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Mean_Test_R2))

print(summary_by_vi)


if (nrow(model_df) < 50) return(NULL)
model_df <- joined_df %>%
  filter(!is.na(GPP_site), !is.na(PAR_site), !is.na(.data[[selected_vi]])) %>%
  mutate(VI_PAR = .data[[selected_vi]] * PAR_site)

cat("Rows in model_df for", selected_vi, ":", nrow(model_df), "\n")
if (nrow(model_df) < 50) return(NULL)


if (nrow(model_df) < 20) return(NULL)  # Try a lower threshold
if (nrow(model_df) < 50) {
  cat("Skipping", selected_vi, "- too few rows:", nrow(model_df), "\n")
  return(NULL)
}
joined_df %>%
  summarise(
    kNDVI = sum(!is.na(kNDVI)),
    NDVI = sum(!is.na(NDVI)),
    EVI = sum(!is.na(EVI)),
    NIRv = sum(!is.na(NIRv))
  )

