######################################
######################################
## site data = sitecombineddata
## spatial data = VImeteo20152018combine
library(ggpubr)
library(randomForest)
library(caTools)
library(viridis)
library(ggplot2)
#library(yardstick)

#source("VIMeteoCheck.R")  # This loads all variables created in that script
# Add a 'site' column to each dataframe based on the list name (without .csv)
##########
#####Join Site data and satellite data :sitecombineddata$siteyeardate
joined_df <- dplyr::left_join(sitecombineddata, VImeteo20152018combine, by = "siteyeardate")
unique(joined_df$siteyear)
unique(sitecombineddata$siteyear)
nrow(joined_df)

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
joined_df$cumulative_kNDVI <- ave(joined_df$kNDVI, 
                                 joined_df$siteyear, 
                                 FUN = function(x) cumsum(ifelse(is.na(x), 0, x)))

############################################################################
#### Calculate Cumulative Growing Degree Days at Day of Phenological Observation ####
############################################################################


### years  of site data not available will show NA values
###Calculate fPAR as a function of LAI
# Define the function ###Calculate fPAR as a function of EVI
calculate_fapar_beer <- function(LAI, K = 0.5) {
  fapar <- 1 - exp(-K * LAI)
  return(fapar)
}

###Calculate fPAR
# Apply the function to the EVI column in joined_df
joined_df$fAPAR <- calculate_fapar_beer(joined_df$Lai)
joined_df$APAR <- joined_df$fAPAR * joined_df$PAR_site
joined_df$LUE<- (joined_df$GPP) / (joined_df$APAR) ###Calculate LUE

#joined_df <- joined_df %>%
  #dplyr::filter(LUE < 1)

####Random Forest ###########
# Select predictors and response variable
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

rf_data_whole<-rf_data
# Drop rows with NA values
# Remove rows with NA or Inf in LUE
rf_data <- rf_data[is.finite(rf_data$LUE), ]
rf_data <- rf_data[!is.na(rf_data$LUE), ]#where the LUE value is NA, keeping NAs in other columns
nrow(rf_data)
nrow(joined_df)

# Scale numeric columns (excluding the target variable and any non-numeric columns)
numeric_cols <- sapply(rf_data, is.numeric)
rf_data_scaled <- rf_data
#rf_data_scaled[, numeric_cols] <- scale(rf_data_scaled[, numeric_cols])

# Split the dataset into training and testing sets
set.seed(54)
# Define siteyears for each split
# 3.2 Train/test split visualization
train_siteyears <- c("USOF12017", "USOF32017", 
                     "USBDA2016",  "USBDC2016", "USOF22017","USHRC2016", 
                     "USOF62018" ,"USOF52018"
                     ,"USHRC2015", "USHRA2015", "USBDC2015"
                     )
val_siteyears<- c("USHRA2016",  "USHRA2017")
test_siteyears  <- c("USHRC2017", "USBDA2015", "USOF42018")


# Subset the data
train <- subset(rf_data_scaled, siteyear %in% train_siteyears)
#validation <- subset(rf_data_scaled, siteyear %in% val_siteyears)
test <- subset(rf_data_scaled, siteyear %in% test_siteyears)
val <- subset(rf_data_scaled, siteyear %in% val_siteyears)

# Fit the Random Forest model
train_model_input <- subset(train, select = -c(siteyear, GPP_site, PAR_site, fAPAR,Lai, IAVI
                                               #, ,DOP,  Lai, EVI, NDVI,Variety
                                               ))
rf_model <- randomForest(LUE ~ ., data = train_model_input, ntree = 100, do.trace = 10, importance = TRUE)

# Plot MSE over trees
plot(1:100, rf_model$mse, type = "l", xlab = "Number of Trees", ylab = "MSE", main = "Random Forest MSE")


# Variable importance plot
varImpPlot(rf_model, main = "Random Forest Variable Importance")
# Predict
train$predicted_LUE <- predict(rf_model, newdata = train)
test$predicted_LUE <- predict(rf_model, newdata = test)
val$predicted_LUE <- predict(rf_model, newdata = val)

# Convert LUE to plain numeric first
train$LUE <- as.numeric(train$LUE)
test$LUE <- as.numeric(test$LUE)
val$LUE <- as.numeric(val$LUE)

# Calculate R² and MAE for training
train_R2 <- round(summary(lm(LUE ~ predicted_LUE, data = train))$r.squared, 2)
mae_val <- Metrics::mae(train$LUE, train$predicted_LUE)
ggplot(train, aes(x = LUE, y = predicted_LUE)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred") +
  labs(title = "Training Set Performance - Random Forest",
       x = "Observed LUE", y = "Predicted LUE")+
  annotate("text", 
           x = min(train$LUE), 
           y = max(train$predicted_LUE), 
           label = paste0("R² = ", round(train_R2, 2), 
                          "\nMAE = ", round(mae_val, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

### Testing Performance
test$predicted_LUE <- predict(rf_model, newdata = test)
test_R2 <- round(summary(lm(LUE ~ predicted_LUE, data = test))$r.squared, 2)
test_MAE <- round(mean(abs(test$LUE - test$predicted_LUE)), 2)

ggplot(test, aes(x = LUE, y = predicted_LUE)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Testing Set Performance - Random Forest",
       x = "Observed LUE", y = "Predicted LUE")+
  annotate("text", 
           x = min(test$LUE), 
           y = max(test$predicted_LUE), 
           label = paste0("R² = ", round(test_R2, 2), 
                          "\nMAE = ", round(test_MAE, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

### valing Performance
val$predicted_LUE <- predict(rf_model, newdata = val)
val_R2 <- round(summary(lm(LUE ~ predicted_LUE, data = val))$r.squared, 2)
val_MAE <- round(mean(abs(val$LUE - val$predicted_LUE)), 2)

ggplot(val, aes(x = LUE, y = predicted_LUE, color = PAR_site)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "valing Set Performance - Random Forest",
       x = "Observed LUE", y = "Predicted LUE")+
  annotate("text", 
           x = min(val$LUE), 
           y = max(val$predicted_LUE), 
           label = paste0("R² = ", round(val_R2, 2), 
                          "\nMAE = ", round(val_MAE, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

# Predict LUE (Light Use Efficiency) using the trained random forest model on scaled data
rf_data$LUEpredicted <- predict(rf_model, newdata = rf_data_scaled)

# Calculate predicted GPP (Gross Primary Productivity) using the light-use efficiency model:
rf_data$GPP_predicted <- rf_data$LUEpredicted * rf_data$PAR_site * rf_data$fAPAR



####oob
# Access the Out-of-Bag (OOB) predictions
p.rf.oob <- predict(rf_model)  # No dataset specified, OOB predictions will be used
0# Calculate residuals (actual - predicted) for OOB predictions
r.rpp.oob <- train$LUE - p.rf.oob  # Actual values minus OOB predictions
# Summary statistics of residuals
summary(r.rpp.oob)
# Calculate RMSE for OOB
rmse.oob <- sqrt(mean(r.rpp.oob^2))  # RMSE formula
# Print RMSE
cat("RMSE (OOB):", rmse.oob, "\n")
# Plot OOB predictions vs actual values
plot(train$LUE ~ p.rf.oob, asp = 1, pch = 20,
     xlab = "Out-of-bag cross-validation estimates",
     ylab = "Actual", xlim = c(0, 3), ylim = c(0, 3),
     main = "LUE - Out-of-bag Cross-Validation (Random Forest)")
grid()
abline(0, 1)  # Adds a line of slope = 1 for reference


##################################
###################################
# Define number of runs
n <- 48
rf.stats <- data.frame(rep=1:n, rsq=as.numeric(NA), mse=as.numeric(NA))
# Run the Random Forest multiple times and calculate performance metrics
system.time(
  for (i in 1:n) {
    # Train the Random Forest model on the training data
    model.rf <- randomForest(LUE ~ ., data=train_model_input, ntree=1000, importance=T, na.action=na.omit)
    # Collect R² and MSE statistics from the model
    rf.stats[i, "rsq"] <- median(model.rf$rsq)   # R² value for the run
    rf.stats[i, "mse"] <- median(model.rf$mse)   # MSE value for the run
  }
)

# Print summary of R² and MSE over the 48 runs
summary(rf.stats[, 2:3])

# Plot histograms for R² and MSE
hist(rf.stats[,"rsq"], xlab="RandomForest R^2", breaks = 16, main = "Frequency of fits (R^2)")
rug(rf.stats[,"rsq"])  # Adds a rug plot to visualize the distribution

hist(rf.stats[,"mse"], xlab="RandomForest RMSE", breaks = 16, main = "Frequency of OOB accuracy (RMSE)")
rug(rf.stats[,"mse"])  # Adds a rug plot to visualize the distribution


# Initial splits (already done)
train_data <- subset(rf_data, siteyear %in% train_siteyears)
test_data  <- subset(rf_data, siteyear %in% test_siteyears)
val_data   <- subset(rf_data, siteyear %in% val_siteyears)

# Remove siteyear for modeling
train_base <- subset(train_data, select = -siteyear)
test_base  <- subset(test_data, select = -siteyear)
val_base   <- subset(val_data, select = -siteyear)

# Initialize empty vectors to store R² and RMSE for each iteration
r2_values <- numeric(1000)
rmse_values <- numeric(1000)

# Run the loop for 1000 iterations
for (i in 1:1000) {
  cat("Iteration:", i, "\n")
  train_base <- na.omit(train_base)  # Clean training data
  rf_model <- randomForest(LUE ~ ., data = train_base, ntree = 100, importance = FALSE) # Train RF model
  test_data$predicted_LUE <- predict(rf_model, newdata = test_data[, !(names(test_data) %in% c("siteyear", "predicted_LUE", "error"))])
  test_data$error <- abs(test_data$LUE - test_data$predicted_LUE)# Compute error
  top_test <- test_data[order(test_data$error), ][1:floor(0.05 * nrow(test_data)), ]# Take top 5% most accurate predictions
  train_base <- rbind(train_base, subset(top_test, select = -c(siteyear, predicted_LUE, error)))  # Add to training set
  # Remove added rows from test set
  test_data <- test_data[!rownames(test_data) %in% rownames(top_test), ]
  # Check if there are valid rows for lm()
  valid_data <- test_data[!is.na(test_data$LUE) & !is.na(test_data$predicted_LUE), ]
  # If there's valid data, proceed with lm() and calculate R² and RMSE
  if (nrow(valid_data) > 0) {
    # Calculate R²
    lm_model <- lm(LUE ~ predicted_LUE, data = valid_data)
    r2_values[i] <- summary(lm_model)$r.squared
    # Calculate RMSE
    rmse_values[i] <- sqrt(mean((valid_data$LUE - valid_data$predicted_LUE)^2))
    # Print the R² and RMSE for the current iteration
    cat("Iteration:", i, " R²:", r2_values[i], " RMSE:", rmse_values[i], "\n")
  } else {
    cat("Iteration:", i, "No valid data for R² and RMSE calculation.\n")
  }
}

# Optionally: Plot R² and RMSE over iterations
plot(1:1000, r2_values, type = "l", col = "blue", xlab = "Iteration", ylab = "R²", main = "R² Over Iterations")
plot(1:1000, rmse_values, type = "l", col = "red", xlab = "Iteration", ylab = "RMSE", main = "RMSE Over Iterations")


# Optionally: You can plot R² and RMSE over iterations
plot(1:1000, r2_values, type = "l", col = "blue", xlab = "Iteration", ylab = "R²", main = "R² Over Iterations")
plot(1:1000, rmse_values, type = "l", col = "red", xlab = "Iteration", ylab = "RMSE", main = "RMSE Over Iterations")

top_test <- test_data[complete.cases(test_data), ]
top_test <- top_test[order(top_test$error), ][1:floor(0.05 * nrow(test_data)), ]

val_data$predicted_LUE <- predict(rf_model, newdata = val_base)
val_rmse <- sqrt(mean((val_data$LUE - val_data$predicted_LUE)^2, na.rm = TRUE))
cat("Validation RMSE after 1000 iterations:", val_rmse, "\n")


# Predict on validation set
val_data$predicted_LUE <- predict(rf_model, newdata = val_base)

# Calculate R² and MAE for validation
val_R2 <- round(summary(lm(LUE ~ predicted_LUE, data = val_data))$r.squared, 2)
val_MAE <- round(mean(abs(val_data$LUE - val_data$predicted_LUE)), 2)

# Plot validation set performance
ggplot(val_data, aes(x = LUE, y = predicted_LUE)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "Validation Set Performance - Random Forest",
       x = "Observed LUE", y = "Predicted LUE") +
  annotate("text", 
           x = min(val_data$LUE), 
           y = max(val_data$predicted_LUE), 
           label = paste0("R² = ", round(val_R2, 2), 
                          "\nMAE = ", round(val_MAE, 2)),
           hjust = 0, vjust = 1, size = 5, fontface = "bold")

View(val_data)








###################################################
############## Meteo +VI ####################################
###################################################




###################################################
############## VI ####################################
###################################################




###################################################
############## Meteo ####################################
###################################################

# --- Hyperparameter Tuning using Cross-Validation (Example with tuneRanger) ---
# This is a more robust way to find good hyperparameters and can help prevent overfitting
if(requireNamespace("tuneRanger", quietly = TRUE) && requireNamespace("mlr", quietly = TRUE)) {
  library(tuneRanger)
  library(mlr)
  
  # Create a task for mlr
  task <- makeRegrTask(data = train, target = "LUE")
  
  # Define the parameter space for tuning
  params <- list(
    ntree = c(500, 1000, 1500),
    mtry = floor(ncol(train_model_input) * c(0.3, 0.5, 0.7)), # Try different fractions of features
    min.node.size = c(1, 5, 10) # Control tree complexity
  )
  
  # Define a resampling strategy (e.g., 5-fold cross-validation)
  rdesc <- makeResampleDesc("CV", iters = 5)
  
  # Tune the hyperparameters
  tuned_params <- tuneRanger(task = task,
                             parameters = params,
                             resampling = rdesc,
                             measures = mae, # Optimize for MAE
                             control = makeTuneControlGrid())
  
  print("Tuned Hyperparameters:")
  print(tuned_params$best.parameters)
  
  # Fit the Random Forest model with the best hyperparameters
  train_model_input <- subset(train, select = -c(siteyear, GPP_site, PAR_site, fAPAR,  Variety, predicted_LUE))
  rf_model <- randomForest(LUE ~ .,
                           data = train_model_input,
                           ntree = tuned_params$best.parameters$ntree,
                           mtry = tuned_params$best.parameters$mtry,
                           min.node.size = tuned_params$best.parameters$min.node.size,
                           importance = TRUE)
  
} else {
  # If tuneRanger or mlr are not installed, use your original model fitting
  cat("tuneRanger and mlr not installed. Using default Random Forest parameters.\n")
  train_model_input <- subset(train, select = -c(siteyear, GPP_site, PAR_site, fAPAR,  Variety))
  rf_model <- randomForest(LUE ~ ., data = train_model_input, ntree = 1000, importance = TRUE)
}

# Plot MSE over trees (if default parameters are used)
if(!requireNamespace("tuneRanger", quietly = TRUE) || !requireNamespace("mlr", quietly = TRUE)) {
  plot(1:rf_model$ntree, rf_model$mse, type = "l", xlab = "Number of Trees", ylab = "MSE", main = "Random Forest MSE")
}

# Variable importance plot
varImpPlot(rf_model, main = "Random Forest Variable Importance")

# Predict
train$predicted_LUE <- predict(rf_model, newdata = train)
test$predicted_LUE <- predict(rf_model, newdata = test)

# Convert LUE to plain numeric first
train$LUE <- as.numeric(train$LUE)
test$LUE <- as.numeric(test$LUE)

# Calculate R² and MAE for training
train_R2 <- round(cor(train$LUE, train$predicted_LUE)^2, 2)
mae_val <- Metrics::mae(train$LUE, train$predicted_LUE)
ggplot(train, aes(x = LUE, y = predicted_LUE)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred") +
  labs(title = paste("Training Set Performance - Random Forest (R²=", train_R2, ", MAE=", round(mae_val, 2), ")"),
       x = "Observed LUE", y = "Predicted LUE") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") # Add 1:1 line

### Testing Performance
test$predicted_LUE <- predict(rf_model, newdata = test)
test_R2 <- round(cor(test$LUE, test$predicted_LUE)^2, 2)
test_MAE <- round(mean(abs(test$LUE - test$predicted_LUE)), 2)

ggplot(test, aes(x = LUE, y = predicted_LUE)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = paste("Testing Set Performance - Random Forest (R²=", test_R2, ", MAE=", test_MAE, ")"),
       x = "Observed LUE", y = "Predicted LUE") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") # Add 1:1 line
