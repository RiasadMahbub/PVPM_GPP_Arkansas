library(randomForest)
library(randomForestExplainer)
require(ranger)
require(vip)
library(tidyverse)
library(gridExtra)
require(iml)
require(fastshap)


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
joined_df$cumulative_NDVI <- ave(joined_df$NDVI, 
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

# Subset the data
train <- subset(rf_data_scaled, siteyear %in% train_siteyears)
validation <- subset(rf_data_scaled, siteyear %in% val_siteyears)
test <- subset(rf_data_scaled, siteyear %in% test_siteyears)

# Updated feature list
vars <- c("VPD_site", "Tair_site", "rH_site", 
          "NDVI", "EVI2", "NIRv", "NDWI", "LSWI", "SAVI2", "TDVI",
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
          "NSDSI3", "RI4XS", "kIPVI", "kNDVI", "kRVI")
# Select predictors and response variable
rf_data <- joined_df[, c(  "VPD_site", "Tair_site", "rH_site","GPP_site", "PAR_site", "fAPAR","LUE",
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
                           
)]
# Prepare training input
train_model_input <- subset(rf_data, select = -c( GPP_site, PAR_site, fAPAR))
train_model_input$VPD_site <- as.numeric(train_model_input$VPD_site)
train_model_input$Tair_site <- as.numeric(train_model_input$Tair_site)

missing_vars <- vars[!vars %in% colnames(train_model_input)]
print(missing_vars)

"LUE" %in% colnames(train_model_input)

# 1. Random Forest with randomForest package
set.seed(314)
m.lzn.rf <- randomForest::randomForest(LUE ~ ., 
                         data=train_model_input[, c("LUE", vars)],
                         importance=TRUE, nperm=3,
                         na.action=na.omit, mtry=3)

print(m.lzn.rf)
plot(m.lzn.rf)
randomForest::importance(m.lzn.rf, type=1)
randomForest::importance(m.lzn.rf, type=2)

par(mfrow = c(1, 2))
varImpPlot(m.lzn.rf, type=1, main = "Importance: permutation")
varImpPlot(m.lzn.rf, type=2, main = "Importance: node impurity")
par(mfrow = c(1, 1))

# 2. Goodness-of-fit on training data
p.rf <- predict(m.lzn.rf, newdata=train_model_input)
summary(r.rpp <- train_model_input$LUE - p.rf)
(rmse.rf <- sqrt(sum(r.rpp^2)/length(r.rpp)))

plot(train_model_input$LUE ~ p.rf, asp=1, pch=20, xlab="fitted", 
     ylab="actual", xlim=c(0,8), ylim=c(0,8), 
     main="LUE prediction, Random Forest")
grid(); abline(0,1)

# 3. Out-of-bag cross-validation
p.rf.oob <- predict(m.lzn.rf)
summary(r.rpp.oob <- train_model_input$LUE - p.rf.oob)
(rmse.oob <- sqrt(sum(r.rpp.oob^2)/length(r.rpp.oob)))

plot(train_model_input$LUE ~ p.rf.oob, asp=1, pch=20,
     xlab="Out-of-bag cross-validation estimates",
     ylab="actual", xlim=c(0,8), ylim=c(0,8),
     main="LUE prediction, Random Forest")
grid(); abline(0,1)

# 4. Stability check across multiple RF fits
n <- 48
rf.stats <- data.frame(rep=1:n, rsq=NA, mse=NA)
system.time(
  for (i in 1:n) {
    model.rf <- randomForest(LUE ~ ., 
                             data=train_model_input[, c("LUE", vars)],
                             importance=TRUE, na.action=na.omit, mtry=5)
    rf.stats[i, "rsq"] <- median(model.rf$rsq)
    rf.stats[i, "mse"] <- median(model.rf$mse)
  }
)
summary(rf.stats[,2:3])
hist(rf.stats[,"rsq"], xlab="RandomForest R^2", breaks = 16, main = "Frequency of fits (R^2)")
rug(rf.stats[,"rsq"])

hist(rf.stats[,"mse"], xlab="RandomForest RMSE", breaks = 16, main = "Frequency of OOB accuracy (RMSE)")
rug(rf.stats[,"mse"])
train_model_input[] <- lapply(train_model_input, function(x) as.numeric(as.character(x)))
sum(is.na(train_model_input$LUE))
# 5. Random forest with ranger
m.lzn.ra <- ranger::ranger(LUE ~ ., 
                   data=train_model_input[, c("LUE", vars)],
                   importance = 'permutation',
                   scale.permutation.importance = TRUE,
                   mtry = 3)
print(m.lzn.ra)

set.seed(314)
m.lzn.ra.i <- ranger(LUE ~ ., 
                     data=train_model_input[, c("LUE", vars)],
                     importance = 'impurity',
                     mtry = 3)
print(m.lzn.ra.i)

# Prediction and comparison
p.ra <- predict(m.lzn.ra, data=train_model_input)
summary(r.rap <- train_model_input$LUE - p.ra$predictions)
(rmse.ra <- sqrt(sum(r.rap^2)/length(r.rap)))

c(rmse.ra, rmse.rf)

par(mfrow=c(1,2))
plot(train_model_input$LUE ~ p.ra$predictions, asp=1, pch=20, xlab="fitted", ylab="actual", 
     xlim=c(0,8), ylim=c(0,8), main="LUE Ranger")
grid(); abline(0,1)
plot(train_model_input$LUE ~ p.rf, asp=1, pch=20, xlab="fitted", ylab="actual", 
     xlim=c(0,8), ylim=c(0,8), main="LUE Random Forest")
grid(); abline(0,1)
par(mfrow=c(1,1))

# Out-of-bag validation comparison
summary(m.lzn.ra$predictions)
summary(p.rf.oob)
summary(m.lzn.ra$predictions - p.rf.oob)

par(mfrow=c(1,2))
plot(train_model_input$LUE ~ m.lzn.ra$predictions, asp=1, pch=20,
     ylab="actual", xlab="OOB X-validation estimates",
     xlim=c(0,8), ylim=c(0,8),
     main="ranger")
abline(0,1); grid()

plot(train_model_input$LUE ~ p.rf.oob, asp=1, pch=20,
     xlab="OOB X-validation estimates",
     ylab="actual", xlim=c(0,8), ylim=c(0,8),
     main="RandomForest")
grid(); abline(0,1)
par(mfrow=c(1,1))

# Variable importance comparison
importance_table <- cbind(
  ranger_perm = ranger::importance(m.lzn.ra),
  ranger_imp = ranger::importance(m.lzn.ra.i),
  rf_perm = randomForest::importance(m.lzn.rf)[,1],
  rf_imp = randomForest::importance(m.lzn.rf)[,2]
)
print(importance_table)

