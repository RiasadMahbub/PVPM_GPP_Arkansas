# ==============================================================================
# LIBRARIES ====================================================================
# ==============================================================================
library(randomForest)
library(dplyr)
library(ggplot2)
library(progressr)

# Enable progress reporting
handlers(global = TRUE)
handlers("txtprogressbar")  # You can switch to "progress" or "cli" if desired

# ==============================================================================
# START TIMER ==================================================================
# ==============================================================================
start_time <- Sys.time()

# ==============================================================================
# 1. JOIN DATA & CALCULATE BIOPHYSICAL VARIABLES ===============================
# ==============================================================================
joined_df <- dplyr::left_join(sitecombineddata, VImeteo20152018combine, by = "siteyeardate")

# fAPAR using Beer’s Law
calculate_fapar_beer <- function(LAI, K = 0.5) {
  fapar <- 1 - exp(-K * LAI)
  return(fapar)
}
joined_df$fAPAR <- calculate_fapar_beer(joined_df$Lai)

# Calculate APAR and LUE
joined_df$APAR <- joined_df$fAPAR * joined_df$PAR_site
joined_df$LUE  <- joined_df$GPP / joined_df$APAR  # LUE = GPP / APAR

########################################
#### Growing degree days equation ####
########################################
calculate_gdd <- function(tmax, tmin, tbase) {
  # Apply the upper and lower temperature thresholds
  tmax_adjusted <- ifelse(tmax > 30, 30, tmax)
  tmin_adjusted <- ifelse(tmin < 10, 10, tmin)
  # Calculate the mean temperature
  tmean <- (tmax_adjusted + tmin_adjusted) / 2
  # Calculate Growing Degree Days
  gdd <- tmean - tbase
  return(gdd)
}
# Define the base temperature (T_base)
tbase <- 10 # You might need to adjust this based on the specific plant/insect
# Calculate GDD for each day and store it in a new column 'gdd'
joined_df$gdd <- calculate_gdd(joined_df$tmax, joined_df$tmin, tbase)
# Calculate cumulative GDD by siteyear
joined_df$cumulative_gdd <- ave(joined_df$gdd, 
                                joined_df$siteyear, 
                                FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))
joined_df$cumulative_dayl <- ave(joined_df$dayl, 
                                 joined_df$siteyear, 
                                 FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))
joined_df$cumulative_VARI <- ave(joined_df$VARI, 
                                 joined_df$siteyear, 
                                 FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))
joined_df$cumulative_IAVI <- ave(joined_df$IAVI, 
                                 joined_df$siteyear, 
                                 FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))
joined_df$cumulative_NDVI <- ave(joined_df$NDVI, 
                                 joined_df$siteyear, 
                                 FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))

#############

# ==============================================================================
# 2. SELECT FEATURES AND FILTER ================================================
# ==============================================================================

rf_data <- joined_df[, c(  "GPP_site", "PAR_site", "fAPAR", "LUE", "VPD_site", "Tair_site", 
                           
                           "siteyear", "Es",  "rH_site", "dayl","cumulative_gdd",   
                           
                           "cumulative_dayl", "DOP",  "DAP",  "nir", 
                           "MBWI",  "Lai", "MLSWI26",
                           "TVI", "GDVI", "NDWI", "Variety", "IAVI"
                           #,"GCC", "NDDI","GSAVI", "GEMI",
                           #"MSAVI", "GOSAVI", "BWDRVI", "cumulative_IAVI"
                           #"GCC", "NDDI",  "MuWIR",
                           #"",
                           
                           #"NMDI",
                           #"kNDVI", "srad", 
                           #"Ec", "Ei", 
                           #"ExR", "NSDS", "NSDSI3", "IPVI",  "NDPI", "AFRI1600",
                           #"EVI",  "MNDVI", "GVMI", "DBSI", "DSI", "MSI", "NSDSI1",
                           #"ppt","TSAVI",  "PI", "TVI","OSAVI","ATSAVI","GRNDVI","kNDVI", "NIRv",
                           #"IAVI", "VARI", "RNDVI",   "RDVI", "EVI2", "GDVI", "ExGR", "AFRI2100",
                           #, "cumulative_VARI","cumulative_NDVI"
                           # "NDVI", "LSWI",  "sNIRvNDPI", "SAVI2", "TDVI",  "DAP",
                           #"cumulative_IAVI",
                           
)]
# Keep only rows with finite LUE
rf_data <- rf_data[is.finite(rf_data$LUE) & !is.na(rf_data$LUE), ]

# ==============================================================================
# 3. RANDOM FOREST VARIABLE IMPORTANCE FUNCTION ================================
# ==============================================================================

# Runs random forest 30 times with different seeds and averages variable importance
run_rf_importance <- function(seed_base, data) {
  importance_list <- list()
  progressr::with_progress({
    p <- progressr::progressor(steps = 30)
    for (i in 1:30) {
      set.seed(seed_base + i)
      rf_model <- randomForest::randomForest(
        LUE ~ ., 
        data = data, 
        ntree = 100,
        importance = TRUE
      )
      importance_list[[i]] <- randomForest::importance(rf_model, type = 1)
      p(sprintf("Seed %d - Iteration %d", seed_base, i))
    }
  })
  importance_array <- simplify2array(importance_list)
  mean_importance <- apply(importance_array, 1:2, mean)
  df <- as.data.frame(mean_importance)
  df$Variable <- rownames(df)
  df <- df[order(-df$`%IncMSE`), ]
  return(df)
}

# ==============================================================================
# 4. TRAINING DATA SELECTION ===================================================
# ==============================================================================
train_siteyears <- c("USHRA2016", "USOF22017", "USOF12017", "USHRA2015", 
                     "USBDA2016", "USBDC2015", "USBDC2016", "USHRA2017",
                     "USHRC2015", "USHRC2016", "USOF62018", "USOF52018")
test_siteyears  <- c("USHRC2017", "USBDA2015", "USOF42018")

train <- subset(rf_data, siteyear %in% train_siteyears)
train_model_input <- subset(train, select = -c(siteyear, GPP_site, PAR_site, fAPAR, Lai))

# ==============================================================================
# 5. RUN RANDOM FORESTS ACROSS MULTIPLE SEEDS ==================================
# ==============================================================================
mean_importance_df100 <- run_rf_importance(100, train_model_input)
mean_importance_df200 <- run_rf_importance(200, train_model_input)
mean_importance_df300 <- run_rf_importance(300, train_model_input)
mean_importance_df400 <- run_rf_importance(400, train_model_input)

# Add seed labels
df100 <- mean_importance_df100 %>% mutate(Seed = "Seed 100")
df200 <- mean_importance_df200 %>% mutate(Seed = "Seed 200")
df300 <- mean_importance_df300 %>% mutate(Seed = "Seed 300")
df400 <- mean_importance_df400 %>% mutate(Seed = "Seed 400")

# Combine all results
combined_df <- bind_rows(df100, df200, df300, df400)

# Order variables by average importance
ordered_vars <- combined_df %>%
  group_by(Variable) %>%
  summarise(meanImp = mean(`%IncMSE`), .groups = "drop") %>%
  arrange(meanImp) %>%
  pull(Variable)

combined_df$Variable <- factor(combined_df$Variable, levels = ordered_vars)

# ==============================================================================
# 6. PLOT VARIABLE IMPORTANCE ==================================================
# ==============================================================================
library(wesanderson)
ggplot(combined_df, aes(x = `%IncMSE`, y = Variable, fill = Seed)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    x = "% Increase in MSE",
    y = NULL,
    title = "Variable Importance across Seeds 100–400"
  ) +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "bottom")

# ==============================================================================
# END TIMER ====================================================================
# ==============================================================================
end_time <- Sys.time()
time_diff <- end_time - start_time
print(paste("Time taken for the entire script:", round(time_diff, 2)))


plot(rf_data$Ei, rf_data$LUE)


####################################
run_rf_impurity <- function(seed_base, data) {
  importance_list <- list()
  progressr::with_progress({
    p <- progressr::progressor(steps = 30)
    for (i in 1:30) {
      set.seed(seed_base + i)
      rf_model <- randomForest::randomForest(
        LUE ~ ., 
        data = data, 
        ntree = 100,
        importance = TRUE
      )
      imp <- importance(rf_model, type = 2)
      importance_list[[i]] <- imp
      p(sprintf("Seed %d - Iteration %d", seed_base, i))
    }
  })
  
  # Combine and average
  importance_array <- simplify2array(importance_list)
  mean_importance <- apply(importance_array, 1:2, mean)
  df <- as.data.frame(mean_importance, stringsAsFactors = FALSE)
  df$Variable <- rownames(df)
  
  # Rename Gini column if needed
  colnames(df)[1] <- "MeanDecreaseGini"
  
  # Order by Gini
  df <- df[order(-df$MeanDecreaseGini), ]
  return(df)
}

impurity_df100 <- run_rf_impurity(100, train_model_input)
impurity_df200 <- run_rf_impurity(200, train_model_input)
impurity_df300 <- run_rf_impurity(300, train_model_input)
impurity_df400 <- run_rf_impurity(400, train_model_input)

df100_gini <- impurity_df100 %>% mutate(Seed = "Seed 100")
df200_gini <- impurity_df200 %>% mutate(Seed = "Seed 200")
df300_gini <- impurity_df300 %>% mutate(Seed = "Seed 300")
df400_gini <- impurity_df400 %>% mutate(Seed = "Seed 400")

gini_combined <- bind_rows(df100_gini, df200_gini, df300_gini, df400_gini)

ordered_vars_gini <- gini_combined %>%
  group_by(Variable) %>%
  summarise(meanImp = mean(MeanDecreaseGini), .groups = "drop") %>%
  arrange(meanImp) %>%
  pull(Variable)

gini_combined$Variable <- factor(gini_combined$Variable, levels = ordered_vars_gini)
library(wesanderson)



# Assuming rf.model is your random forest model (from randomForest package)
# and test is your test dataframe with the true outcome in 'outcome' column.

# Make predictions - get class probabilities (votes)
test.predictions <- predict(rf_model, newdata = test)

# Load pROC package for ROC and AUC calculation
library(pROC)

# Calculate ROC curve: true labels vs predicted probabilities of class 2 (positive class)
roc.test <- roc(test$LUE, test.predictions)

# Calculate AUC
auc_value <- auc(roc.test)

# Print AUC
print(auc_value)
