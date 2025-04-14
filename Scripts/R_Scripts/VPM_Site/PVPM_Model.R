######################################
######################################
## site data = sitecombineddata
## spatial data = VImeteo20152018combine

library(ggpubr)
source("VIMeteoCheck.R")  # This loads all variables created in that script
#####Join Site data and satellite data
sitecombineddata$siteyeardate
# Add a 'site' column to each dataframe based on the list name (without .csv)


##########
joined_df <- dplyr::left_join(sitecombineddata, VImeteo20152018combine, by = "siteyeardate")
unique(joined_df$siteyear)
unique(sitecombineddata$siteyear)

View(joined_df)
nrow(joined_df)
### years  of site data not available will show NA values

###Calculate LUE

###Calculate fPAR
###Calculate fPAR as a function of LAI

###Calculate fPAR as a function of EVI
# Define the function
calculate_fapar <- function(evi) {
  fapar <- 1.24 * evi - 0.168
  fapar <- pmax(pmin(fapar, 1), 0)  # Clip values to [0, 1]
  return(fapar)
}

# Apply the function to the EVI column in joined_df
joined_df$fAPAR <- calculate_fapar(joined_df$EVI)
joined_df$APAR <- joined_df$fAPAR * joined_df$PAR_site
joined_df$LUE<- (joined_df$GPP) / (joined_df$APAR)
###Calculate fPAR


####Random FOrest ###########
# Load necessary libraries
library(randomForest)
library(caTools)
library(ggplot2)

# Select predictors and response variable
rf_data <- joined_df[, c("LUE",  "VPD_site", "Tair_site", "rH_site", 
                         "kNDVI", "NIRv",  "NDVI", "LSWI", "nir", "sNIRvNDPI",
                         "SAVI2", "TDVI", "DAP", "siteyear", "GPP_site", "PAR_site", "fAPAR", 
                         "Ec", "Ei", "Es", "Lai", "avgRH", "dayl", "ppt"
                         #, "LST_1KM"
                         )]

# Drop rows with NA values
# Remove rows with NA or Inf in LUE
rf_data <- rf_data[is.finite(rf_data$LUE), ]
rf_data <- rf_data[!is.na(rf_data$LUE), ]#where the LUE value is NA, keeping NAs in other columns


rf_data <- rf_data[rf_data$LUE <= 10, ]  # Remove rows where LUE is greater than 2
hist(joined_df$LUE)
hist(rf_data$LUE)
nrow(rf_data)
# Scale numeric columns (excluding the target variable and any non-numeric columns)
numeric_cols <- sapply(rf_data, is.numeric)
rf_data <- rf_data
rf_data[, numeric_cols] <- scale(rf_data[, numeric_cols])
View(rf_data)
# Split the dataset into training and testing sets
set.seed(123)
# Define siteyears for each split
train_siteyears <- c("USHRA2016", "USOF22017", "USOF12017", "USHRA2015", "USBDA2016", "USBDC2015", "USBDC2016", "USHRA2017")
test_siteyears  <- c("USHRC2017", "USBDA2015")
val_siteyears <- c("USHRC2015", "USHRC2016")

# Subset the data
train <- subset(rf_data, siteyear %in% train_siteyears)
validation <- subset(rf_data, siteyear %in% val_siteyears)
test <- subset(rf_data, siteyear %in% test_siteyears)

# Fit the Random Forest model
train_model_input <- subset(train, select = -c(siteyear, GPP_site, PAR_site, fAPAR))
rf_model <- randomForest(LUE ~ ., data = train_model_input, ntree = 1000, do.trace = 100, importance = TRUE)

# Plot MSE over trees
plot(1:1000, rf_model$mse, type = "l", xlab = "Number of Trees", ylab = "MSE", main = "Random Forest MSE")

# Variable importance plot
varImpPlot(rf_model, main = "Random Forest Variable Importance")
# Predict
train$predicted_LUE <- predict(rf_model, newdata = train)
test$predicted_LUE <- predict(rf_model, newdata = test)

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


# Predict LUE for all rows in rf_data using the trained model
rf_data$LUEpredicted <- predict(rf_model, newdata = rf_data)

plot(rf_data$LUEpredicted, rf_data$LUE)
joined_df$GPP_predicted <- joined_df$LUEpredicted * joined_df$PAR_site * joined_df$fAPAR
plot(joined_df$GPP_predicted, joined_df$GPP_site)
plot(joined_df$LUEpredicted, joined_df$GPP_site)
####oob
# Access the Out-of-Bag (OOB) predictions
p.rf.oob <- predict(rf_model)  # No dataset specified, OOB predictions will be used
# Calculate residuals (actual - predicted) for OOB predictions
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
     ylab = "Actual", xlim = c(2, 3.3), ylim = c(2, 3.3),
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


  # Load corrplot
library(corrplot)

# Select only numeric columns
numeric_df <- joined_df[sapply(joined_df, is.numeric)]

# Compute correlation matrix
cor_matrix <- cor(rf_data, use = "pairwise.complete.obs")

# Plot the correlation matrix with numbers
corrplot(cor_matrix, method = "number")
# Compute correlation matrix
cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")

# Save plot as high-resolution PNG
png("correlation_plot.png", width = 6, height = 6, units = "in", res = 1000)
corrplot(cor_matrix, method = "number")
dev.off()

plot(rf_data$DAP, rf_data$LUE)


### Why some LUE are so high 
# Filter and print rows where LUE > 1
high_LUE_rows <- rf_data[rf_data$LUE > 1, ]
View(high_LUE_rows)
nrow(high_LUE_rows)
nrow(rf_data)
hist(rf_data$LUE)
### Why some LUE are infinite 

plot(rf_data, rf_data$LUE)

rf_data$LUE

plot(joined_df$kNDVI, joined_df$GPP_site)
plot(joined_df$SAVI2, joined_df$GPP_site)
plot(joined_df$GNDVI, joined_df$GPP_site)
plot(joined_df$LSWI, joined_df$GPP_site)
plot(rf_data$DAP, rf_data$LUE)
plot(joined_df$DAP, joined_df$LUE)
rf_data$LUE
ggplot(data=rf_data, aes(x =GPP_site , y =GPP_predicted, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(expression(paste(GPP~VPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  stat_regline_equation(label.x = -5, label.y = 23, size = 16)+
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 21, size = 16)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  # ggplot2::annotate("text", x = -0.95, y = 29, label = sprintf("RMSE: %0.2f", GPPsatelliteVPMrmse), size = 16, fontface = 'italic') +
  # ggplot2::annotate("text", x = -1.5, y = 25, label = sprintf("MAE:  %0.2f",GPPsatelliteVPMmae), size = 16, fontface = 'italic') +
  # ggplot2::annotate("text", x = -1.5, y = 27, label = sprintf("Bias: %0.2f", GPPsatelliteVPMbias), size = 16, fontface = 'italic') +
  # ggplot2::annotate("text", x = 8.5, y = 29, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  # ggplot2::annotate("text", x = 7.5, y = 25, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  # ggplot2::annotate("text", x = 7.5, y = 27, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  # #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))



set.seed(123)

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
  
  # Clean training data
  train_base <- na.omit(train_base)
  
  # Train RF model
  rf_model <- randomForest(LUE ~ ., data = train_base, ntree = 100, importance = FALSE)
  
  # Predict on test data
  test_data$predicted_LUE <- predict(rf_model, newdata = test_data[, !(names(test_data) %in% c("siteyear", "predicted_LUE", "error"))])
  
  # Compute error
  test_data$error <- abs(test_data$LUE - test_data$predicted_LUE)
  
  # Take top 5% most accurate predictions
  top_test <- test_data[order(test_data$error), ][1:floor(0.05 * nrow(test_data)), ]
  
  # Add to training set
  train_base <- rbind(train_base, subset(top_test, select = -c(siteyear, predicted_LUE, error)))
  
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

require(ranger)
set.seed(314)
m.lzn.ra <- ranger(logZn ~ ffreq + x + y + dist.m + elev + soil + lime, 
                   data = meuse, 
                   importance = 'permutation',
                   scale.permutation.importance = TRUE,
                   mtry = 3)
print(m.lzn.ra)

set.seed(314)
m.lzn.ra.i <- ranger(logZn ~ ffreq + x + y + dist.m + elev + soil + lime, 
                     data = meuse, 
                     importance = 'impurity',
                     mtry = 3)
print(m.lzn.ra.i)

p.ra <- predict(m.lzn.ra, data=meuse)
str(p.ra)
summary(r.rap <- meuse$logZn - p.ra$predictions)
(rmse.ra <- sqrt(sum(r.rap^2)/length(r.rap)))

c(rmse.ra, rmse.rf)
par(mfrow=c(1,2))
plot(meuse$logZn ~ p.ra$predictions, asp=1, pch=20, xlab="fitted", ylab="actual", xlim=c(2,3.3),          ylim=c(2,3.3), main="log10(Zn), Meuse topsoils, Ranger")
grid(); abline(0,1)
plot(meuse$logZn ~ p.rf, asp=1, pch=20, xlab="fitted", ylab="actual", xlim=c(2,3.3),          ylim=c(2,3.3), main="log10(Zn), Meuse topsoils, Random Forest")
grid(); abline(0,1)

summary(m.lzn.ra$predictions)
summary(p.rf.oob)
summary(m.lzn.ra$predictions - p.rf.oob)  # difference

par(mfrow=c(1,2))
plot(meuse$logZn ~ m.lzn.ra$predictions, asp=1, pch=20,
     ylab="actual", xlab="OOB X-validation estimates",
     xlim=c(2,3.3), ylim=c(2,3.3),
     main="ranger")
abline(0,1); grid()
plot(meuse$logZn ~ p.rf.oob, asp=1, pch=20,
     xlab="OOB X-validation estimates",
     ylab="actual", xlim=c(2,3.3), ylim=c(2,3.3),
     main="RandomForest")
grid(); abline(0,1)

par(mfrow=c(1,1))

cbind(ranger = ranger::importance(m.lzn.ra),
      rF = randomForest::importance(m.lzn.rf)[,1])

cbind(ranger =ranger::importance(m.lzn.ra.i),
      rF = randomForest::importance(m.lzn.rf)[,2])

require(vip)

v1 <- vip(m.lzn.ra, title = "Ranger")
v2 <- vip(m.lzn.rf, title = "randomForest")
grid.arrange(v1, v2, ncol=2)


n <- 48
ra.stats <- data.frame(rep=1:10, rsq=as.numeric(NA), mse=as.numeric(NA))
system.time(
  for (i in 1:n) {
    model.ra <- ranger(logZn ~ ffreq + x + y + dist.m + elev + soil + lime,
                       data=meuse, importance="none", mtry=5,
                       write.forest = FALSE)
    ra.stats[i, "mse"] <- model.ra$prediction.error
    ra.stats[i, "rsq"] <- model.ra$r.squared
  }
)

summary(ra.stats[,2:3])

hist(ra.stats[,"rsq"], xlab="ranger R^2", breaks = 16, main = "Frequency of fits (R^2)")
rug(ra.stats[,"rsq"])
hist(ra.stats[,"mse"], xlab="ranger RMSE", breaks = 16, main = "Frequency of OOB accuracy (RMSE)")
rug(ra.stats[,"mse"])
summary(ra.stats[,2:3])
summary(rf.stats[,2:3])
sd(ra.stats[,2])
sd(rf.stats[,2])
sd(ra.stats[,3])
sd(rf.stats[,3])

m.lzn.qrf <- ranger(logZn ~ ffreq + x + y + dist.m + elev + soil + lime, 
                    data = meuse, 
                    quantreg = TRUE,
                    importance = 'permutation',
                    keep.inbag=TRUE,  # needed for QRF
                    scale.permutation.importance = TRUE,
                    mtry = 3)
pred.qrf <- predict(m.lzn.qrf, type = "quantiles",
                    # default is c(0.1, 0.5, 0.9)
                    quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
summary(pred.qrf$predictions)

set.seed(314)
m.lzn.rf.l <- randomForest(logZn ~ ffreq + x + y + dist.m + elev + soil + lime, data=meuse, 
                           localImp = TRUE, nperm = 3, 
                           na.action = na.omit, mtry = 3)
dim(m.lzn.rf.l$localImportance)

m.lzn.rf.l$localImportance[, 1:6]

set.seed(314)
m.lzn.ra.l <- ranger(logZn ~ ffreq + x + y + dist.m + elev + soil + lime, 
                     data = meuse, 
                     importance = 'permutation',
                     local.importance = TRUE,
                     scale.permutation.importance = TRUE,
                     mtry = 3)
print(m.lzn.ra.l)

dim(m.lzn.ra.l$variable.importance.local)
dim(m.lzn.ra.l$variable.importance.local)

m.lzn.ra.l$variable.importance.local[1:6,]

ranger::importance(m.lzn.ra.l)
summary(m.lzn.ra.l$variable.importance.local[,1:7])

plot(meuse$elev,
     m.lzn.ra.l$variable.importance.local[,"elev"],
     xlab = "elevation", ylab = "importance of `elevation`",
     pch = 20)
abline(h = ranger::importance(m.lzn.ra.l)["elev"], col = "red")

plot(meuse$dist.m,
     m.lzn.ra.l$variable.importance.local[,"dist.m"],
     xlab = "dist.m", ylab = "importance of `dist.m`",
     pch = 20)
abline(h = ranger::importance(m.lzn.ra.l)["dist.m"], col = "red")

plot(meuse$ffreq,
     m.lzn.ra.l$variable.importance.local[,"ffreq"],
     xlab = "ffreq", ylab = "importance of `ffreq`",
     pch = 20)
abline(h = ranger::importance(m.lzn.ra.l)["ffreq"], col = "red")


require(iml)
vars <- c("ffreq","x","y","dist.m","elev","soil","lime")
X <-  meuse[, vars]
predictor <- Predictor$new(model = m.lzn.ra, data = X, y = meuse$logZn)
str(predictor)

predictor.qrf <- Predictor$new(model = m.lzn.qrf, data = X, y = meuse$logZn)
str(predictor.qrf)

imp.mae <- iml::FeatureImp$new(predictor, loss = "mae")
imp.mse <- iml::FeatureImp$new(predictor, loss = "mse")
require("ggplot2")
plot(imp.mae)

plot(imp.mse)

print(imp.mae)

print(imp.mse)

ale <- FeatureEffect$new(predictor, feature = "elev")
ale$plot()

pdp <- FeatureEffect$new(predictor, feature = "elev", method = "pdp")
pdp$plot()

ice <- FeatureEffect$new(predictor, feature = "elev", method = "ice")
ice$plot()

ice.dist.m <- FeatureEffect$new(predictor, feature = "dist.m", method = "ice")
ice.dist.m$plot()

interact <- Interaction$new(predictor)
plot(interact)

interact.elev <- Interaction$new(predictor, feature = "elev")
plot(interact.elev)

meuse[1,]

lime.explain <- iml::LocalModel$new(predictor, x.interest = X[1, ])
lime.explain$results
plot(lime.explain)

ix.maxdist <- which.max(meuse$dist.m)
meuse[ix.maxdist, ]

lime.explain <- LocalModel$new(predictor, x.interest = X[ix.maxdist, ])
lime.explain$results

plot(lime.explain)

str(predictor)

shapley <- iml::Shapley$new(predictor, x.interest = X[1, ])
shapley$plot()
shapley.maxdist <- Shapley$new(predictor, x.interest = X[ix.maxdist, ])
shapley.maxdist$plot()

(results <- shapley$results)
sum(shapley$results$phi)
(results <- shapley.maxdist$results)
sum(shapley.maxdist$results$phi)

shapley.qrf <- iml::Shapley$new(predictor.qrf, x.interest = X[1, ])
shapley.qrf$plot()

(results <- shapley.qrf$results)
shapley.maxdist.qrf <- Shapley$new(predictor.qrf, x.interest = X[ix.maxdist, ])
shapley.maxdist.qrf$plot()

(results <- shapley.maxdist.qrf$results)

require(fastshap)
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}
fshap <- fastshap::explain(object = m.lzn.ra, 
                           X = X, pred_wrapper = pfun,
                           nsim = 24)
head(fshap)

shapley <- iml::Shapley$new(predictor, x.interest = X[1, ])
rbind(fshap[1,], shapley$results$phi) 

autoplot(fshap)

autoplot(fshap, 
         type = "dependence",
         feature = "elev",
         X = X,
         color_by = "ffreq")

autoplot(fshap, 
         type = "dependence",
         feature = "dist.m",
         X = X,
         color_by = "ffreq")

autoplot(fshap,
         type = "contribution",
         row_num = 1)

autoplot(fshap,
         type = "contribution",
         row_num = ix.maxdist)

str(fshap)

fshap.m <- as.matrix(fshap)
dim(fshap.m)

library(shapviz)
sv.fshap <- shapviz(fshap.m,
                    X = meuse[ , c("ffreq", "x", "y", "dist.m", "elev", "soil",  "lime")],
                    bg_X = meuse) # small dataset, can see all of them

sv_importance(sv.fshap, kind = "bee")
# auto-select most important interacting feature
sv_dependence(sv.fshap, v = "dist.m", color_var = "auto")

library(kernelshap)
# use existing ranger fits
s <- kernelshap(m.lzn.ra.l, 
                X = meuse[ , c("ffreq", "x", "y", "dist.m", "elev", "soil",  "lime")],
                bg_X = meuse) # small dataset, can see all of them

#
str(s)

sv <- shapviz(s)
sv_importance(sv, kind = "bee")

# auto-select most important interacting feature
sv_dependence(sv, v = "dist.m", color_var = "auto")

Sys.which("make")


# Check if Rcpp is loaded
search()
# If it is, detach it
detach("package:Rcpp", unload = TRUE, force = TRUE)

# Then unload its DLL manually
dyn.unload(file.path(.libPaths()[1], "Rcpp", "libs", "x64", "Rcpp.dll"))
install.packages("Rcpp", type = "source")


