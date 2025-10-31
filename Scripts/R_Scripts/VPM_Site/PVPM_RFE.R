# ------------------------------------------------------->
# Libraries
# ------------------------------------------------------->
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(doParallel)
library(pbapply)   # progress bar for parallel loops

# ------------------------------------------------------->
# Custom functions
# ------------------------------------------------------->
calculate_gdd <- function(tmax, tmin, tbase) {
  tmax_adj <- ifelse(tmax > 30, 30, tmax)
  tmin_adj <- ifelse(tmin < 10, 10, tmin)
  tmean <- (tmax_adj + tmin_adj) / 2
  gdd <- tmean - tbase
  ifelse(gdd < 0, 0, gdd)
}

calculate_fapar_beer <- function(LAI, K = 0.5) {
  1 - exp(-K * LAI)
}

evaluate_metrics <- function(y_obs, y_pred) {
  valid <- !is.na(y_obs) & !is.na(y_pred)
  y_obs <- y_obs[valid]
  y_pred <- y_pred[valid]
  
  if(length(y_obs) == 0 || length(unique(y_obs)) < 2) {
    return(list(R2 = NA, MAE = NA, RMSE = NA, Bias = NA))
  }
  
  ss_res <- sum((y_obs - y_pred)^2)
  ss_tot <- sum((y_obs - mean(y_obs))^2)
  r2 <- if(ss_tot == 0) 1 else 1 - ss_res / ss_tot
  mae <- mean(abs(y_obs - y_pred))
  rmse <- sqrt(mean((y_obs - y_pred)^2))
  bias <- mean(y_pred - y_obs)
  
  list(R2 = r2, MAE = mae, RMSE = rmse, Bias = bias)
}

# ------------------------------------------------------->
# Parallel backend setup
# ------------------------------------------------------->
num_cores <- max(parallel::detectCores() - 1, 1)
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)

# ------------------------------------------------------->
# Data preparation
# ------------------------------------------------------->
set.seed(123)
tbase <- 10

# NOTE: The 'joined_df' data frame is assumed to be available in the environment
# with the necessary columns (tmax, tmin, dayl, Lai, PAR_site, GPP_site)

# Assuming joined_df is loaded and available
# joined_df <- ... 

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
joined_df$Et = joined_df$Ec + joined_df$Ei + joined_df$Es
# ------------------------------------------------------->
# Features and siteyear splits
# ------------------------------------------------------->
features_to_combine <- c("PAR_site", "GPP_site", "fAPAR", "VPD_site", "Tair_site", "Es", "rH_site",
                         "dayl", "cumulative_gdd", "cumulative_dayl", "DOP", "DAP", "nir",
                         "MBWI", "MLSWI26", "TVI", "GDVI", "NDWI", "IAVI", "kNDVI",
                         "NDVI", "VARI", "TSAVI", "RNDVI", "IPVI", "PI", "EVI", "ATSAVI", "LSWI",
                         "ARVI", "ATSAVI", "AVI", "AWEInsh", "AWEIsh", "BCC",
                         "BI", "BITM", "BIXS", "BNDVI", "BWDRVI", "BaI",
                         "CIG", "CVI", "DBSI", "DSI", "DSWI1", "DSWI2",
                         "DSWI3", "DSWI4", "DSWI5", "DVI", "EMBI", "ENDVI",
                         "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR",
                         "FCVI", "GARI", "GBNDVI", "GCC", "GDVI", "GEMI",
                         "GLI", "GNDVI", "GOSAVI", "GRNDVI", "GRVI", "GSAVI",
                         "GVMI", "IAVI", "IKAW", "IPVI", "LSWI", "MBI",
                         "MBWI", "MCARI1", "MCARI2", "MGRVI", "MLSWI26", "MLSWI27",
                         "MNDVI", "MNDWI", "MNLI", "MRBVI", "MSAVI", "MSI",
                         "MSR", "MTVI1", "MTVI2", "MuWIR", #"NDDI", - wrong values
                         "NDII","Et",
                         "NDMI", "NDPI", "NDPonI", "NDSoI", "NDTI", "NDVI",
                         "NDVIMNDWI", "NDWI", "NDWIns", "NDYI", "NGRDI", "NIRv",
                         "NLI", "NMDI", "NRFIg", "NRFIr", "NSDS", "NSDSI1",
                         "NSDSI2", "NSDSI3", "NWI", "NormG", "NormNIR", "NormR",
                         "OCVI", "OSAVI", "OSI", "PI", "RCC", "RDVI",
                         "RGBVI", "RGRI", "RI", "RI4XS", "RNDVI", "SARVI",
                         "SAVI", "SAVI2", "SEVI", "SI", "SLAVI", "SR",
                         "SR2", "SWM", "TDVI", "TGI", "TSAVI",
                         "TVI", "TriVI", "VARI", "VIG", "WDRVI", "WDVI",
                         "WI1", "WI2", "WI2015", "WRI", "bNIRv",
                         "kIPVI", "kNDVI", "kRVI", "nir", "property_size",
                         "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR",
                         "swir1", "swir2", "siteyear")

features_no_siteyear <- setdiff(features_to_combine, c("siteyear", "PAR_site", "GPP_site", "fAPAR"))
train_siteyears <- c("USBDA2016","USBDC2016","USOF22017","USHRC2016",
                     "USOF62018","USOF52018","USHRC2015","USHRA2015",
                     "USBDC2015","USOF32017")
val_siteyears <- c("USOF12017","USHRA2016","USHRA2017")
test_siteyears <- c("USHRC2017","USBDA2015","USOF42018")

train_val_df <- joined_df %>%
  dplyr::filter(siteyear %in% c(train_siteyears, val_siteyears)) %>%
  dplyr::select(all_of(features_no_siteyear), LUE, GPP_site) %>%
  dplyr::filter(is.finite(LUE), is.finite(GPP_site)) %>%
  tidyr::drop_na()

test_df <- joined_df %>%
  dplyr::filter(siteyear %in% test_siteyears) %>%
  dplyr::select(all_of(features_no_siteyear), LUE, GPP_site) %>%
  dplyr::filter(is.finite(LUE), is.finite(GPP_site)) %>%
  tidyr::drop_na()

# ------------------------------------------------------->
# RFE controls
# ------------------------------------------------------->
set.seed(123)
ctrl <- caret::rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE,
  allowParallel = TRUE
)

# Number of candidate features
num_features <- length(features_no_siteyear)
# Maximum subset size capped at 25
max_subset <- min(19, num_features)
# Sequence of subset sizes from 5 to max_subset, step 2
subset_sizes <- seq(5, max_subset, by = 2)

# ------------------------------------------------------->
# RFE for LUE
# ------------------------------------------------------->
cat("\nRunning RFE for LUE...\n")
rfe_lue <- caret::rfe(
  x = train_val_df[, features_no_siteyear],
  y = train_val_df$LUE,
  sizes = subset_sizes,
  rfeControl = ctrl
)
cat("\nOptimal features for LUE:\n")

# Correctly selecting the top 'rfe_lue$bestSubset' features
best_features_lue <- head(rfe_lue$optVariables, rfe_lue$bestSubset)

# Custom check to ensure dayl and cumulative_dayl are not selected together
if ("dayl" %in% best_features_lue && "cumulative_dayl" %in% best_features_lue) {
  # If both are present, remove cumulative_dayl as dayl is a more fundamental measurement.
  best_features_lue <- best_features_lue[best_features_lue != "cumulative_dayl"]
  cat("\nNote: 'cumulative_dayl' was removed as 'dayl' was also selected.\n")
}

# Enforce a hard cap of 25 features to ensure the final list is not larger than desired
best_features_lue <- head(best_features_lue, 19)
print(best_features_lue)

final_model_lue <- randomForest::randomForest(
  x = train_val_df[, best_features_lue],
  y = train_val_df$LUE
)

pred_test_lue <- predict(final_model_lue, newdata = test_df[, best_features_lue])
metrics_lue <- evaluate_metrics(test_df$LUE, pred_test_lue)
print(metrics_lue)

# ------------------------------------------------------->
# RFE for GPP
# ------------------------------------------------------->
cat("\nRunning RFE for GPP...\n")
rfe_gpp <- caret::rfe(
  x = train_val_df[, features_no_siteyear],
  y = train_val_df$GPP_site,
  sizes = subset_sizes,
  rfeControl = ctrl
)
cat("\nOptimal features for GPP:\n")

# Correctly selecting the top 'rfe_gpp$bestSubset' features
best_features_gpp <- head(rfe_gpp$optVariables, rfe_gpp$bestSubset)

# Custom check to ensure dayl and cumulative_dayl are not selected together
if ("dayl" %in% best_features_gpp && "cumulative_dayl" %in% best_features_gpp) {
  # If both are present, remove cumulative_dayl as dayl is a more fundamental measurement.
  best_features_gpp <- best_features_gpp[best_features_gpp != "cumulative_dayl"]
  cat("\nNote: 'cumulative_dayl' was removed as 'dayl' was also selected.\n")
}

# Enforce a hard cap of 25 features to ensure the final list is not larger than desired
best_features_gpp <- head(best_features_gpp, 19)
print(best_features_gpp)

final_model_gpp <- randomForest::randomForest(
  x = train_val_df[, best_features_gpp],
  y = train_val_df$GPP_site
)

pred_test_gpp <- predict(final_model_gpp, newdata = test_df[, best_features_gpp])
metrics_gpp <- evaluate_metrics(test_df$GPP_site, pred_test_gpp)
print(metrics_gpp)


# -------------------------------------------------------
# Stop parallel cluster
# -------------------------------------------------------
parallel::stopCluster(cl)


##------------------------------------------------------
#PLOT THE DATA
##------------------------------------------------------
library(ggplot2)
library(dplyr)
library(GGally)  # for ggpairs

# ------------------------------------------------------------------
# 1. Subset the features and LUE
# ------------------------------------------------------------------
selected_features <- c(
  "rH_site", "VPD_site", "DOP", "Es", "DAP", "dayl", "NDDI", "RI4XS",
  "MLSWI26", "Tair_site", "cumulative_dayl", "AWEInsh", "swir1", 
  "NMDI", "MuWIR", "ExG", "TGI", "OSI", "BCC", "MRBVI", "NDYI", 
  "WI2", "EMBI"
)

df_subset <- train_val_df %>%
  dplyr::select(LUE, all_of(selected_features)) %>%
  tidyr::drop_na()  # remove any NA

# ------------------------------------------------------------------
# 2. Correlation plot
# ------------------------------------------------------------------
cor_mat <- cor(df_subset)
library(corrplot)
corrplot::corrplot(cor_mat, method = "color", type = "upper", 
                   tl.cex = 0.8, tl.col = "black", addCoef.col = "black")

# Save the figure
ggsave(
  filename = "LUE_scatter_5x5.png",
  path = "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LUE",
  width = 15,   # 5 columns × 3 inch each
  height = 15,  # Adjust for number of rows
  units = "in",
  dpi = 300
)


# -------------------------------------------------------
# RFE for LUE without preset number of features
# -------------------------------------------------------
set.seed(123)
ctrl <- caret::rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE,
  allowParallel = TRUE
)

# Run RFE using all features, sequence from 2 to total number of features
rfe_lue <- caret::rfe(
  x = train_val_df[, features_no_siteyear],
  y = train_val_df$LUE,
  sizes = seq(2, length(features_no_siteyear), by = 2),
  rfeControl = ctrl
)

cat("\nOptimal features for LUE (determined by RFE):\n")
best_features_lue <- rfe_lue$optVariables
print(best_features_lue)

# -------------------------------------------------------
# Plot RFE performance for LUE
# -------------------------------------------------------
# Default caret plot
plot(rfe_lue, type = c("g", "o"))  # g = performance metric, o = optimal size

# Optional ggplot2 visualization
library(ggplot2)
perf_lue <- rfe_lue$results
ggplot(perf_lue, aes(x = Variables)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_point(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = Rsquared), color = "red") +
  geom_point(aes(y = Rsquared), color = "red") +
  scale_y_continuous(
    name = "RMSE (blue)",
    sec.axis = sec_axis(~., name = "R² (red)")
  ) +
  labs(title = "RFE Performance vs Number of Features (LUE)",
       x = "Number of Features") +
  theme_minimal()


#-----------------------------------------------------
##-----------END OF CODE
#-----------------------------------------------------
# -------------------------------------------------------
# RFE for GPP without preset number of features
# -------------------------------------------------------
rfe_gpp <- caret::rfe(
  x = train_val_df[, features_no_siteyear],
  y = train_val_df$GPP_site,
  sizes = seq(2, length(features_no_siteyear), by = 2),
  rfeControl = ctrl
)

cat("\nOptimal features for GPP (determined by RFE):\n")
best_features_gpp <- rfe_gpp$optVariables
print(best_features_gpp)

# -------------------------------------------------------
# Plot RFE performance for GPP
# -------------------------------------------------------
plot(rfe_gpp, type = c("g", "o"))

perf_gpp <- rfe_gpp$results
ggplot(perf_gpp, aes(x = Variables)) +
  geom_line(aes(y = RMSE), color = "blue") +
  geom_point(aes(y = RMSE), color = "blue") +
  geom_line(aes(y = Rsquared), color = "red") +
  geom_point(aes(y = Rsquared), color = "red") +
  scale_y_continuous(
    name = "RMSE (blue)",
    sec.axis = sec_axis(~., name = "R² (red)")
  ) +
  labs(title = "RFE Performance vs Number of Features (GPP)",
       x = "Number of Features") +
  theme_minimal()





#---------------------------------------------------------
#---------------------------------------------------------
#---------------------------------------------------------
#RFE optimized
#---------------------------------------------------------
# ------------------------------------------------------->
# Libraries
# ------------------------------------------------------->
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(doParallel)
library(pbapply)    # progress bar for parallel loops

# ------------------------------------------------------->
# Custom functions
# ------------------------------------------------------->
calculate_gdd <- function(tmax, tmin, tbase) {
  # Caps Tmax at 30C and floors Tmin at 10C
  tmax_adj <- ifelse(tmax > 30, 30, tmax)
  tmin_adj <- ifelse(tmin < 10, 10, tmin)
  tmean <- (tmax_adj + tmin_adj) / 2
  gdd <- tmean - tbase
  ifelse(gdd < 0, 0, gdd)
}

calculate_fapar_beer <- function(LAI, K = 0.5) {
  # Beer-Lambert law for fAPAR
  1 - exp(-K * LAI)
}

evaluate_metrics <- function(y_obs, y_pred) {
  # Calculate standard model performance metrics (R2, MAE, RMSE, Bias)
  valid <- !is.na(y_obs) & !is.na(y_pred)
  y_obs <- y_obs[valid]
  y_pred <- y_pred[valid]
  
  if(length(y_obs) == 0 || length(unique(y_obs)) < 2) {
    return(list(R2 = NA, MAE = NA, RMSE = NA, Bias = NA))
  }
  
  ss_res <- sum((y_obs - y_pred)^2)
  ss_tot <- sum((y_obs - mean(y_obs))^2)
  # Handle case where ss_tot is zero to prevent division by zero
  r2 <- if(ss_tot == 0) 1 else 1 - ss_res / ss_tot
  mae <- mean(abs(y_obs - y_pred))
  rmse <- sqrt(mean((y_obs - y_pred)^2))
  bias <- mean(y_pred - y_obs) # Predict - Observed
  
  list(R2 = r2, MAE = mae, RMSE = rmse, Bias = bias)
}

# ------------------------------------------------------->
# Parallel backend setup
# ------------------------------------------------------->
num_cores <- max(parallel::detectCores() - 1, 1)
cl <- parallel::makeCluster(num_cores)
doParallel::registerDoParallel(cl)
cat(paste("Parallel processing enabled using", num_cores, "cores.\n"))

# ------------------------------------------------------->
# Data preparation (Assuming joined_df is loaded)
# ------------------------------------------------------->
set.seed(123)
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

# ------------------------------------------------------->
# Feature Definition and Splitting (MODIFIED)
# ------------------------------------------------------->

# Full list of features (as defined in the original script)
features_to_combine <- c("PAR_site", "GPP_site", "fAPAR", "VPD_site", "Tair_site", 
                        # "Es", 
                         "rH_site",
                         "dayl", "cumulative_gdd", "cumulative_dayl", "DOP", "DAP", "nir",
                         "MBWI", "MLSWI26", "TVI", "GDVI", "NDWI", "IAVI", "kNDVI",
                         "NDVI", "VARI", "TSAVI", "RNDVI", "IPVI", "PI", "EVI", "ATSAVI", "LSWI",
                         "ARVI", "ATSAVI", "AVI", "AWEInsh", "AWEIsh", "BCC",
                         "BI", "BITM", "BIXS", "BNDVI", "BWDRVI", "BaI",
                         "CIG", "CVI", "DBSI", "DSI", "DSWI1", "DSWI2",
                         "DSWI3", "DSWI4", "DSWI5", "DVI", "EMBI", "ENDVI",
                         "EVI", "EVI2", "EVIv", "ExG", "ExGR", "ExR",
                         "FCVI", "GARI", "GBNDVI", "GCC", "GDVI", "GEMI",
                         "GLI", "GNDVI", "GOSAVI", "GRNDVI", "GRVI", "GSAVI",
                         "GVMI", "IAVI", "IKAW", "IPVI", "LSWI", "MBI",
                         "MBWI", "MCARI1", "MCARI2", "MGRVI", "MLSWI26", "MLSWI27",
                         "MNDVI", "MNDWI", "MNLI", "MRBVI", "MSAVI", "MSI",
                         "MSR", "MTVI1", "MTVI2", "MuWIR", #"NDDI", 
                         "NDII","Et",
                         "NDMI", "NDPI", "NDPonI", "NDSoI", "NDTI", "NDVI",
                         "NDVIMNDWI", "NDWI", "NDWIns", "NDYI", "NGRDI", "NIRv",
                         "NLI", "NMDI", "NRFIg", "NRFIr", "NSDS", "NSDSI1",
                         "NSDSI2", "NSDSI3", "NWI", "NormG", "NormNIR", "NormR",
                         "OCVI", "OSAVI", "OSI", "PI", "RCC", "RDVI",
                         "RGBVI", "RGRI", "RI", "RI4XS", "RNDVI", "SARVI",
                         "SAVI", "SAVI2", "SEVI", "SI", "SLAVI", "SR",
                         "SR2", "SWM", "TDVI", "TGI", "TSAVI",
                         "TVI", "TriVI", "VARI", "VIG", "WDRVI", "WDVI",
                         "WI1", "WI2", "WI2015", "WRI", "bNIRv",
                         "kIPVI", "kNDVI", "kRVI", "nir", "property_size",
                         "sNIRvLSWI", "sNIRvNDPI", "sNIRvNDVILSWIP", "sNIRvNDVILSWIS", "sNIRvSWIR",
                         "swir1", "swir2", "siteyear", "Et")

# Features to exclude from RFE candidate pool
excluded_cols <- c("siteyear", "PAR_site", "GPP_site", "fAPAR")
all_candidate_features <- setdiff(features_to_combine, excluded_cols)

# --- USER REQUIREMENTS IMPLEMENTATION ---
mandatory_features <- c("IAVI", "VARI", "TVI", "ExG", "DBSI", "AWEInsh", "cumulative_gdd", "Et")
rfe_candidate_features <- setdiff(all_candidate_features, mandatory_features)

train_siteyears <- c("USBDA2016","USBDC2016","USOF22017","USHRC2016",
                     "USOF62018","USOF52018","USHRC2015","USHRA2015",
                     "USBDC2015","USOF32017")
val_siteyears <- c("USOF12017","USHRA2016","USHRA2017")
test_siteyears <- c("USHRC2017","USBDA2015","USOF42018")

# Select all features for data filtering (we will subset for RFE later)
features_for_df_prep <- c(all_candidate_features, "LUE", "GPP_site")



# ------------------------------------------------------->
# RFE controls (MODIFIED SUBSET SIZES)
# ------------------------------------------------------->
set.seed(123)
ctrl <- caret::rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 10,
  verbose = TRUE,
  allowParallel = TRUE
)

# Max total features desired: 19
max_total_features <- 19
num_mandatory <- length(mandatory_features)
# RFE will search for 19 - 4 = 15 additional features maximum.
max_rfe_selection <- max_total_features - num_mandatory

# Sequence of subset sizes for the RFE search space (1 to 15, stepping by 2)
subset_sizes_rfe <- seq(1, max_rfe_selection, by = 2)
cat(paste("RFE will search for subsets of size:", paste(subset_sizes_rfe, collapse = ", "), "from the candidate pool.\n"))


# ------------------------------------------------------->
# RFE for LUE (Modified to use candidate features)
# ------------------------------------------------------->
cat("\nRunning RFE for LUE on candidate features to find optimal *additional* predictors...\n")
rfe_lue <- caret::rfe(
  x = train_val_df[, rfe_candidate_features],
  y = train_val_df$LUE,
  sizes = subset_sizes_rfe,
  rfeControl = ctrl
)

# Get the optimal number of features selected by RFE from the candidate pool
num_rfe_features_lue <- rfe_lue$bestSubset
best_rfe_features_lue <- head(rfe_lue$optVariables, num_rfe_features_lue)

# Combine mandatory features with the RFE selected features
best_features_lue <- c(mandatory_features, best_rfe_features_lue)
cat("\nInitial optimal features for LUE (Mandatory + RFE selection, max 19):\n")


# Custom check (Requirement 1: dayl vs cumulative_dayl)
if ("dayl" %in% best_features_lue && "cumulative_dayl" %in% best_features_lue) {
  # If both are present, remove cumulative_dayl as dayl is a more fundamental measurement.
  best_features_lue <- best_features_lue[best_features_lue != "cumulative_dayl"]
  cat("\nNote: 'cumulative_dayl' was removed as 'dayl' was also selected (to reduce collinearity).\n")
}

# Final check for hard cap of 19 (Requirement 2 is maintained through design)
best_features_lue <- head(best_features_lue, max_total_features)
print(best_features_lue)
cat(paste("Final LUE Feature Count:", length(best_features_lue), "\n"))
train_val_df <- joined_df %>%
  dplyr::filter(siteyear %in% c(train_siteyears, val_siteyears)) %>%
  dplyr::select(all_of(features_for_df_prep)) %>%
  dplyr::filter(is.finite(LUE), is.finite(GPP_site)) %>%
  tidyr::drop_na()

test_df <- joined_df %>%
  dplyr::filter(siteyear %in% test_siteyears) %>%
  dplyr::select(all_of(features_for_df_prep)) %>%
  dplyr::filter(is.finite(LUE), is.finite(GPP_site)) %>%
  tidyr::drop_na()

# Final Model Training for LUE
final_model_lue <- randomForest::randomForest(
  x = train_val_df[, best_features_lue],
  y = train_val_df$LUE
)

pred_test_lue <- predict(final_model_lue, newdata = test_df[, best_features_lue])
metrics_lue <- evaluate_metrics(test_df$LUE, pred_test_lue)
cat("\n--- LUE Model Performance on Test Data ---\n")
print(metrics_lue)

# ------------------------------------------------------->
# RFE for GPP (Modified to use candidate features)
# ------------------------------------------------------->
cat("\nRunning RFE for GPP on candidate features to find optimal *additional* predictors...\n")
rfe_gpp <- caret::rfe(
  x = train_val_df[, rfe_candidate_features],
  y = train_val_df$GPP_site,
  sizes = subset_sizes_rfe,
  rfeControl = ctrl
)

# Get the optimal number of features selected by RFE from the candidate pool
num_rfe_features_gpp <- rfe_gpp$bestSubset
best_rfe_features_gpp <- head(rfe_gpp$optVariables, num_rfe_features_gpp)

# Combine mandatory features with the RFE selected features
best_features_gpp <- c(mandatory_features, best_rfe_features_gpp)
cat("\nInitial optimal features for GPP (Mandatory + RFE selection, max 19):\n")

# Custom check (Requirement 1: dayl vs cumulative_dayl)
if ("dayl" %in% best_features_gpp && "cumulative_dayl" %in% best_features_gpp) {
  # If both are present, remove cumulative_dayl as dayl is a more fundamental measurement.
  best_features_gpp <- best_features_gpp[best_features_gpp != "cumulative_dayl"]
  cat("\nNote: 'cumulative_dayl' was removed as 'dayl' was also selected (to reduce collinearity).\n")
}

# Final check for hard cap of 19 (Requirement 2 is maintained through design)
best_features_gpp <- head(best_features_gpp, max_total_features)
print(best_features_gpp)
cat(paste("Final GPP Feature Count:", length(best_features_gpp), "\n"))

# Final Model Training for GPP
final_model_gpp <- randomForest::randomForest(
  x = train_val_df[, best_features_gpp],
  y = train_val_df$GPP_site
)

pred_test_gpp <- predict(final_model_gpp, newdata = test_df[, best_features_gpp])
metrics_gpp <- evaluate_metrics(test_df$GPP_site, pred_test_gpp)
cat("\n--- GPP Model Performance on Test Data ---\n")
print(metrics_gpp)


# -------------------------------------------------------
# Stop parallel cluster
# -------------------------------------------------------
parallel::stopCluster(cl)
cat("\nParallel cluster stopped.\n")

