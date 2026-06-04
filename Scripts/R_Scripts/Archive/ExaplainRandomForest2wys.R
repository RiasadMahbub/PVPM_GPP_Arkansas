library(randomForest)
library(randomForestExplainer)
require(ranger)
require(vip)
library(tidyverse)
library(gridExtra)
require(iml)
require(fastshap)

# Subset the data
train <- subset(rf_data_scaled, siteyear %in% train_siteyears)
validation <- subset(rf_data_scaled, siteyear %in% val_siteyears)
test <- subset(rf_data_scaled, siteyear %in% test_siteyears)

vars <- c("VPD_site", "Tair_site", "rH_site", "kNDVI", "NIRv", "NDVI", "LSWI", "nir", "sNIRvNDPI",
          "SAVI2", "TDVI", "DAP", "Ec", "Ei", "Es",
          "Lai", "avgRH", "dayl", "ppt")
# Fit the Random Forest model
train_model_input <- subset(train, select = -c(siteyear, GPP_site, PAR_site, fAPAR))
train_model_input$LUE
train_model_input$VPD_site <- as.numeric(train_model_input$VPD_site)
train_model_input$Tair_site <- as.numeric(train_model_input$Tair_site)

set.seed(314)
m.lzn.rf <- randomForest(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                         + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                           Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                         data=train_model_input, 
                         # three permutations per tree to estimate importance
                         importance = TRUE, nperm = 3, 
                         na.action = na.omit, mtry = 3)
print(m.lzn.rf)
plot(m.lzn.rf)
randomForest::importance(m.lzn.rf, type=1)
randomForest::importance(m.lzn.rf, type=2)

par(mfrow = c(1, 2))
varImpPlot(m.lzn.rf, type=1, main = "Importance: permutation")
varImpPlot(m.lzn.rf, type=2, main = "Importance: node impurity")
par(mfrow = c(1, 1))

#2.3 Goodness-of-fit
p.rf <- predict(m.lzn.rf, newdata=train_model_input)
length(unique(p.rf))
summary(r.rpp <- train_model_input$LUE - p.rf)
(rmse.rf <- sqrt(sum(r.rpp^2)/length(r.rpp)))

plot(train_model_input$LUE ~ p.rf, asp=1, pch=20, xlab="fitted", 
     ylab="actual", xlim=c(0,8),ylim=c(0,8), 
     main="LUE prediction, Random Forest")
grid(); abline(0,1)

#2.4 Out-of bag cross-validation
p.rf.oob <- predict(m.lzn.rf)
summary(r.rpp.oob <- train_model_input$LUE - p.rf.oob)
(rmse.oob <- sqrt(sum(r.rpp.oob^2)/length(r.rpp.oob)))

plot(train_model_input$LUE ~ p.rf.oob, asp=1, pch=20,
     xlab="Out-of-bag cross-validation estimates",
     ylab="actual", xlim=c(0,8), ylim=c(0,8),
     main="LUE prediction, Random Forest")
grid()
abline(0,1)


#2.5
n <- 48
rf.stats <- data.frame(rep=1:10, rsq=as.numeric(NA), mse=as.numeric(NA))
system.time(
  for (i in 1:n) {
    model.rf <- randomForest(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                             + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                               Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                             data=train_model_input,
                             importance=T, na.action=na.omit, mtry=5)
    summary(model.rf$rsq)
    summary(model.rf$mse)
    rf.stats[i, "rsq"] <- median(summary(model.rf$rsq))
    rf.stats[i, "mse"] <- median(summary(model.rf$mse))
  }
)
summary(rf.stats[,2:3])
hist(rf.stats[,"rsq"], xlab="RandomForest R^2", breaks = 16, main = "Frequency of fits (R^2)")
rug(rf.stats[,"rsq"])

hist(rf.stats[,"mse"], xlab="RandomForest RMSE", breaks = 16, main = "Frequency of OOB acuracy (RMSE)")
rug(rf.stats[,"mse"])


#3 Random forest with ranger
m.lzn.ra  <- ranger(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                         + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                           Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                         data=train_model_input, 
                         # three permutations per tree to estimate importance
                    importance = 'permutation',
                    scale.permutation.importance = TRUE,
                    mtry = 3)
print(m.lzn.ra)

set.seed(314)
m.lzn.ra.i <- ranger(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                     + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                       Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                     data=train_model_input,  
                     importance = 'impurity',
                     mtry = 3)
print(m.lzn.ra.i)

p.ra <- predict(m.lzn.ra, data=train_model_input)
str(p.ra)

summary(r.rap <- train_model_input$LUE - p.ra$predictions)
(rmse.ra <- sqrt(sum(r.rap^2)/length(r.rap)))
c(rmse.ra, rmse.rf)

par(mfrow=c(1,2))
plot(train_model_input$LUE ~ p.ra$predictions, asp=1, pch=20, xlab="fitted", ylab="actual", 
     xlim=c(0,8), ylim=c(0,8), main="LUE Ranger")
grid(); abline(0,1)
plot(train_model_input$LUE  ~ p.rf, asp=1, pch=20, xlab="fitted", ylab="actual", 
     xlim=c(0,8),ylim=c(0,8), main="LUE Random Forest")
grid(); abline(0,1)
par(mfrow=c(1,1))

#3.3 Out-of-bag cross-validation
summary(m.lzn.ra$predictions)
summary(p.rf.oob)
summary(m.lzn.ra$predictions - p.rf.oob)  # difference

par(mfrow=c(1,2))
plot(train_model_input$LUE  ~ m.lzn.ra$predictions, asp=1, pch=20,
     ylab="actual", xlab="OOB X-validation estimates",
     xlim=c(0,8), ylim=c(0,8),
     main="ranger")
abline(0,1); grid()
plot(train_model_input$LUE  ~ p.rf.oob, asp=1, pch=20,
     xlab="OOB X-validation estimates",
     ylab="actual", xlim=c(0,8), ylim=c(0,8),
     main="RandomForest")
grid(); abline(0,1)
par(mfrow=c(1,1))

#3.4 Variable importance
cbind(ranger = ranger::importance(m.lzn.ra),
      rF = randomForest::importance(m.lzn.rf)[,1])

cbind(ranger =ranger::importance(m.lzn.ra.i),
      rF = randomForest::importance(m.lzn.rf)[,2])

v1 <- vip(m.lzn.ra, title = "Ranger")
v2 <- vip(m.lzn.rf, title = "randomForest")
grid.arrange(v1, v2, ncol=2)

#3.5
n <- 48
ra.stats <- data.frame(rep=1:10, rsq=as.numeric(NA), mse=as.numeric(NA))
system.time(
  for (i in 1:n) {
    model.ra <- ranger(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                       + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                         Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                       data=train_model_input, 
                       importance="none", mtry=5,
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

#5 Quantile Regression Forest
m.lzn.qrf <- ranger(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                    + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                      Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                    data=train_model_input, 
                    quantreg = TRUE,
                    importance = 'permutation',
                    keep.inbag=TRUE,  # needed for QRF
                    scale.permutation.importance = TRUE,
                    mtry = 3)
pred.qrf <- predict(m.lzn.qrf, type = "quantiles",
                    # default is c(0.1, 0.5, 0.9)
                    quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
summary(pred.qrf$predictions)

#6 Local measures
#6.1 randomForest
set.seed(314)
m.lzn.rf.l <- randomForest(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                           + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                             Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                           data=train_model_input,  
                           localImp = TRUE, nperm = 3, 
                           na.action = na.omit, mtry = 3)
dim(m.lzn.rf.l$localImportance)
m.lzn.rf.l$localImportance[, 1:6]

#6.2 ranger
set.seed(314)
m.lzn.ra.l <- ranger(LUE ~ VPD_site + Tair_site + rH_site + kNDVI + NIRv 
                     + NDVI + LSWI + nir + sNIRvNDPI + SAVI2 + TDVI + DAP + 
                       Ec + Ei + Es + Lai + avgRH + dayl + ppt, 
                     data=train_model_input,
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

plot(train_model_input$LUE,
     m.lzn.ra.l$variable.importance.local[,"NIRv"],
     xlab = "NIRv", ylab = "importance of `NIRv`",
     pch = 20)
abline(h = ranger::importance(m.lzn.ra.l)["NIRv"], col = "red")

plot(train_model_input$LUE,
     m.lzn.ra.l$variable.importance.local[,"kNDVI"],
     xlab = "kNDVI", ylab = "importance of `kNDVI`",
     pch = 20)
abline(h = ranger::importance(m.lzn.ra.l)["kNDVI"], col = "red")

plot(train_model_input$Es,
     m.lzn.ra.l$variable.importance.local[,"Es"],
     xlab = "Es", ylab = "importance of `Es`",
     pch = 20)
abline(h = ranger::importance(m.lzn.ra.l)["Es"], col = "red")

#7 iml package: Interpretable Machine Learning
vars <- c("VPD_site", "Tair_site", "rH_site", "kNDVI", "NIRv", "NDVI", "LSWI", "nir", 
          "sNIRvNDPI",
          "SAVI2", "TDVI", "DAP", "Ec", "Ei", "Es",
          "Lai", "avgRH", "dayl", "ppt")

train_model_input$VPD_site <- as.numeric(train_model_input$VPD_site)
train_model_input$Tair_site <- as.numeric(train_model_input$Tair_site)
train_model_input$LUE <- as.numeric(train_model_input$LUE)
X <-  train_model_input[, vars]

str(X)
predictor <- Predictor$new(model = m.lzn.ra, data = X, y = train_model_input$LUE)
str(predictor)

predictor.qrf <- Predictor$new(model = m.lzn.qrf, data = X, y = train_model_input$LUE)
str(predictor.qrf)

imp.mae <- iml::FeatureImp$new(predictor, loss = "mae")
imp.mse <- iml::FeatureImp$new(predictor, loss = "mse")
require("ggplot2")
imp.mae
plot(imp.mae)
plot(imp.mse)
predictor

ale <- FeatureEffect$new(predictor, feature = "NIRv")
ale$plot()

pdp <- FeatureEffect$new(predictor, feature = "TDVI", method = "pdp")
pdp$plot()

ice <- FeatureEffect$new(predictor, feature = "NIRv", method = "ice")
ice$plot()

ice.dist.m <- FeatureEffect$new(predictor, feature = "NIRv", method = "ice")
ice.dist.m$plot()

interact <- Interaction$new(predictor)
plot(interact)

interact.NIRv <- Interaction$new(predictor, feature = "NIRv")
plot(interact.NIRv)

#7.3 Explain single predictions with a local model
train_model_input[1,]
lime.explain <- iml::LocalModel$new(predictor, x.interest = X[1, ])
lime.explain$results
plot(lime.explain)


ix.maxdist <- which.max(train_model_input$NIRv)
train_model_input[ix.maxdist, ]

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

#8.1 Visualization of all individual Shapley values
str(fshap)
fshap.m <- as.matrix(fshap)
dim(fshap.m)

library(shapviz)
sv.fshap <- shapviz(fshap.m,
                    X = train_model_input[ , c("VPD_site", "Tair_site", "rH_site", "kNDVI", "NIRv", "NDVI", "LSWI", "nir", 
                                   "sNIRvNDPI",
                                   "SAVI2", "TDVI", "DAP", "Ec", "Ei", "Es",
                                   "Lai", "avgRH", "dayl", "ppt")],
                    bg_X = train_model_input) # small dataset, can see all of them

sv_importance(sv.fshap, kind = "bee")

# auto-select most important interacting feature
sv_dependence(sv.fshap, v = "dist.m", color_var = "auto")

library(kernelshap)
# use existing ranger fits
s <- kernelshap(m.lzn.ra.l, 
                X = train_model_input[ , c("VPD_site", "Tair_site", "rH_site", "kNDVI", "NIRv", "NDVI", "LSWI", "nir", 
                               "sNIRvNDPI",
                               "SAVI2", "TDVI", "DAP", "Ec", "Ei", "Es",
                               "Lai", "avgRH", "dayl", "ppt")],
                bg_X = train_model_input) # small dataset, can see all of them

#
str(s)
sv <- shapviz(s)
sv_importance(sv, kind = "bee")

# auto-select most important interacting feature
sv_dependence(sv, v = "dist.m", color_var = "auto")
