#### Site Calibration
library(caret) ### ML package
library(lattice)
library(tidyverse)
######## K Fold Validation ########################################
### Calculate metrics for training and testing
calculate_metrics <- function(data) {
  Tmin <- -1 
  Tmax <- 48 
  T <- data$Tair_site
  Topt_values <- seq(27, 35, by = 0.1)
  Ts_values <- sapply(Topt_values, function(Topt) {
    calculate_Ts(T, Tmin, Tmax, Topt)
  })
  LUEmax_values <- seq(0.03, 0.08, by = 0.01)
  Ws <- data$Ws
  PAR <- data$PAR_site
  FPAR <- data$FAPAR_sg
  evaluation_metrics <- data.frame(LUEmax = numeric(), Topt = numeric(), RMSE = numeric(), MAE = numeric(), Bias = numeric(), R_squared = numeric())
  for (Topt in Topt_values) {
    Ts <- Ts_values[, which(Topt_values == Topt)]
    for (LUEmax in LUEmax_values) {
      GPP <- calculate_GPP(Ts, LUEmax, Ws, PAR, FPAR)
      observed <- data$GPP_site
      predicted <- GPP
      RMSE <- sqrt(mean((observed - predicted)^2))
      MAE <- mean(abs(observed - predicted))
      Bias <- mean(predicted - observed)
      R_squared <- cor(observed, predicted)^2
      evaluation_metrics <- rbind(evaluation_metrics, data.frame(LUEmax = LUEmax, Topt = Topt, RMSE = RMSE, MAE = MAE, Bias = Bias, R_squared = R_squared))
    }
  }
  # Find the best evaluation metrics
  # Define the weights for each metric
  weight_rmse <- 0.33
  weight_mae <- 0.33
  weight_bias <- 0.33
  
  # Calculate the combined score for each model
  evaluation_metrics$combined_score <- 1 / (evaluation_metrics$RMSE * weight_rmse) + 
    1 / (evaluation_metrics$MAE * weight_mae) +
    1 / (abs(evaluation_metrics$Bias) * weight_bias)
  
  # Find the index of the model with the highest combined score
  best_model_index <- which.max(evaluation_metrics$combined_score)
  best_model <- evaluation_metrics[best_model_index, ]
  
  return(best_model)
}

### Calculate metrics for validation### Calculate metrics for validation
calculate_metrics_validation <- function(data, mean_LUEmax, mean_Topt) {
  Tmin <- -1 
  Tmax <- 48 
  T <- data$Tair_site
  Topt_values <- mean_Topt  # Adjusted to include a range around the mean
  Ts_values <- sapply(Topt_values, function(Topt) {
    calculate_Ts(T, Tmin, Tmax, Topt)
  })
  LUEmax_values <- mean_LUEmax  # Adjusted to include a range around the mean
  Ws <- data$Ws
  PAR <- data$PAR_site
  FPAR <- data$FAPAR_sg
  predicted_GPP <- NULL
  for (Topt in Topt_values) {
    Ts <- Ts_values[, which(Topt_values == Topt)]
    for (LUEmax in LUEmax_values) {
      GPP <- calculate_GPP(Ts, LUEmax, Ws, PAR, FPAR)
      predicted_GPP <- GPP
      observed <- data$GPP_site
      RMSE <- sqrt(mean((observed - predicted_GPP)^2))
      MAE <- mean(abs(observed - predicted_GPP))
      Bias <- mean(predicted_GPP - observed)
      R_squared <- cor(observed, predicted_GPP)^2
      
      # Exclude validation sites if they are present in the data
      if (!is.null(validation_sites) && any(data$siteyear.x %in% validation_sites)) {
        validation_data <- data[data$siteyear.x %in% validation_sites, ]
        observed_validation <- validation_data$GPP_site
        predicted_validation <- calculate_GPP(validation_data$Ts, LUEmax, validation_data$Ws, validation_data$PAR_site, validation_data$FAPAR_sg)
        RMSE_validation <- sqrt(mean((observed_validation - predicted_validation)^2))
        MAE_validation <- mean(abs(observed_validation - predicted_validation))
        Bias_validation <- mean(predicted_validation - observed_validation)
        R_squared_validation <- cor(observed_validation, predicted_validation)^2
        print("Performance Metrics for Validation Data:")
        print(data.frame(RMSE = RMSE_validation, MAE = MAE_validation, Bias = Bias_validation, R_squared = R_squared_validation))
      } else {
        print("Performance Metrics for Original Data:")
        print(data.frame(RMSE = RMSE, MAE = MAE, Bias = Bias, R_squared = R_squared))
      }
    }
  }
  return(predicted_GPP)
}

calculate_Ts <- function(T, Tmin, Tmax, Topt) {
  Ts <- ((T - Tmin) * (T - Tmax)) / (((T - Tmin) * (T - Tmax)) - (T - Topt)^2)
  return(Ts)
}

calculate_GPP <- function(Ts, LUEmax, Ws, PAR, FPAR) {
  GPP <- Ts * Ws * PAR * FPAR * LUEmax * 12.011
  return(GPP)
}



# Randomly shuffle the unique siteyear.x values
unique_siteyear <- unique(sitesatellitemergedDATA$siteyear.x)
set.seed(123)  # for reproducibility

# Validation sites you want
validation_sites <- c("USBDA2015", "USHRC2016", "USOF52018")

# Filter data for validation set
validation_df <- sitesatellitemergedDATA %>% filter(siteyear.x %in% validation_sites)
# Filter data for training and testing sets (excluding validation sites)
training_testing_df <- sitesatellitemergedDATA %>% 
  filter(!siteyear.x %in% validation_sites)

# Number of unique sites
num_sites <- length(unique_siteyear)
# Number of sites for training
num_train <- 11
# Number of sites for testing
num_test <- 2

# Initialize a dataframe to store combinations
combinations_df <- data.frame(matrix(ncol = 2, nrow = num_sites - num_train + 1))
colnames(combinations_df) <- c("Training_Sites", "Testing_Sites")
# Initialize data frames to store information for each run
training_df <- data.frame(Run = integer(), Combination = integer(), Training_Sites = character(), RMSE_Training = numeric(), MAE_Training = numeric(), Bias_Training = numeric(), R_squared_Training = numeric(), combined_score_Training = numeric(), stringsAsFactors = FALSE)
testing_df <- data.frame(Run = integer(), Combination = integer(), Testing_Sites = character(), RMSE_Testing = numeric(), MAE_Testing = numeric(), Bias_Testing = numeric(), R_squared_Testing = numeric(), combined_score_Testing = numeric(), stringsAsFactors = FALSE)

# Initialize data frames to store information for each run
training_df <- data.frame(Run = integer(), Training_Sites = character(), LUEmax = numeric(), Topt = numeric(), RMSE_Training = numeric(), MAE_Training = numeric(), Bias_Training = numeric(), R_squared_Training = numeric(), combined_score_Training = numeric(), stringsAsFactors = FALSE)
testing_df <- data.frame(Run = integer(), Testing_Sites = character(), LUEmax = numeric(), Topt = numeric(), RMSE_Testing = numeric(), MAE_Testing = numeric(), Bias_Testing = numeric(), R_squared_Testing = numeric(), combined_score_Testing = numeric(), stringsAsFactors = FALSE)

# Loop through different runs
for (run in 1:78) {
  # Randomly shuffle the unique siteyear.x values for each run
  shuffled_siteyear <- sample(unique_siteyear)
  
  # Select sites for training
  train_sites <- shuffled_siteyear[1:num_train]
  # Select sites for testing
  test_sites <- shuffled_siteyear[(num_train + 1):num_sites]
  
  # Print the siteyear.x values for training and testing sets
  cat("Run:", run, "\n")
  cat("Training sites:", train_sites, "\n")
  cat("Testing sites:", test_sites, "\n\n")
  
  # Calculate evaluation metrics for training data
  training_data <- training_testing_df[training_testing_df$siteyear.x %in% train_sites, ]
  training_metrics <- calculate_metrics(training_data)
  print("Evaluation Metrics for Training Data:")
  print(training_metrics)
  
  # Calculate evaluation metrics for testing data
  testing_data <- training_testing_df[training_testing_df$siteyear.x %in% test_sites, ]
  testing_metrics <- calculate_metrics(testing_data)
  print("Evaluation Metrics for Testing Data:")
  print(testing_metrics)
  
  # Append information to the data frames
  training_df <- rbind(training_df, data.frame(Run = run, Training_Sites = paste(train_sites, collapse = ", "), LUEmax = training_metrics$LUEmax, Topt = training_metrics$Topt, RMSE_Training = training_metrics$RMSE, MAE_Training = training_metrics$MAE, Bias_Training = training_metrics$Bias, R_squared_Training = training_metrics$R_squared, combined_score_Training = training_metrics$combined_score))
  testing_df <- rbind(testing_df, data.frame(Run = run, Testing_Sites = paste(test_sites, collapse = ", "), LUEmax = testing_metrics$LUEmax, Topt = testing_metrics$Topt, RMSE_Testing = testing_metrics$RMSE, MAE_Testing = testing_metrics$MAE, Bias_Testing = testing_metrics$Bias, R_squared_Testing = testing_metrics$R_squared, combined_score_Testing = testing_metrics$combined_score))
}

# Save data frames as CSV files
write.csv(training_df, file = "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/training_metrics.csv", row.names = FALSE)
write.csv(testing_df, file = "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/testing_metrics.csv", row.names = FALSE)

# Print the first few rows of the training and testing metrics data frames
head(training_df)
head(testing_df)

hist((training_df$LUEmax))
hist((training_df$Topt))
hist((testing_df$LUEmax))
hist((testing_df$Topt))

# Print the results
cat("Insitu calibrated LUEmax value:", mean(training_df$LUEmax), "\n")
cat("Insitu calibrated Topt value:", mean(training_df$Topt), "\n\n")


# Calculate evaluation metrics for validation data
validation_metrics <- calculate_metrics(validation_df)
# Print evaluation metrics for validation data
print("Evaluation Metrics for Validation Data:")
print(validation_metrics)

# Identify the numeric columns except "Run" and "Training_Sites"
numeric_columns <- sapply(training_df, is.numeric)
columns_to_average <- training_df[, numeric_columns]
columns_to_average <- columns_to_average[, -c(1)]  # Remove "Run" and "Training_Sites" columns

# Calculate the mean of each numeric column
mean_training_df <- colMeans(columns_to_average)
# Round mean values to 3 decimal places
mean_training_df_rounded <- round(mean_training_df, 3)

# Calculate the standard deviation of each numeric column
sd_training_df <- apply(columns_to_average, 2, sd)
# Round standard deviation values to 3 decimal places
sd_training_df_rounded <- round(sd_training_df, 3)

print(mean_training_df_rounded)
print(sd_training_df_rounded)


# Identify the numeric columns except "Run" and "Testing_Sites"
numeric_columns_testing <- sapply(testing_df, is.numeric)
columns_to_average_testing <- testing_df[, numeric_columns_testing]
columns_to_average_testing <- columns_to_average_testing[, -c(1)]  # Remove "Run" and "Testing_Sites" columns

# Calculate the mean of each numeric column
mean_testing_df <- colMeans(columns_to_average_testing)
# Round mean values to 3 decimal places
mean_testing_df_rounded <- round(mean_testing_df, 3)

sd_testing_df <- apply(columns_to_average_testing, 2, sd)
# Round standard deviation values to 3 decimal places
sd_testing_df_rounded <- round(sd_testing_df, 3)


print(mean_testing_df_rounded)
print(sd_testing_df_rounded)




# Using mean LUEmax and Topt from the training set
mean_LUEmax <- mean_training_df_rounded["LUEmax"]
mean_Topt <- mean_training_df_rounded["Topt"]


######################################################
##### K-Fold Validation shows there is little chance of over-fitting
######################################################
##### Ben's suggestion taking the mean of the training dataset

#### All sites in one run##############
# Define the function to calculate Ts
calculate_Ts <- function(T, Tmin, Tmax, Topt) {
  Ts <- ((T - Tmin) * (T - Tmax)) / (((T - Tmin) * (T - Tmax)) - (T - Topt)^2)
  return(Ts)
}

# Define the function to calculate GPP
calculate_GPP <- function(Ts, LUEmax, Ws, PAR, FPAR) {
  GPP <- Ts * Ws * PAR * FPAR * LUEmax*12.011
  return(GPP)
}

# Example values for Tmin and Tmax
Tmin <- -1 
Tmax <- 48 

# Extract Tair_site values from df_luemaxgdd_modeled dataframe
T <- sitesatellitemergedDATA$Tair_site

# Topt values from 28 to 32
Topt_values <- seq(27, 35, by = 1)

# Calculate Ts for each Topt value
Ts_values <- sapply(Topt_values, function(Topt) {
  calculate_Ts(T, Tmin, Tmax, Topt)
})

# Example values for LUEmax
LUEmax_values <- seq(0.03, 0.08, by = 0.01)

# Example values for Ws, PAR, and FPAR
Ws <- sitesatellitemergedDATA$Ws    # Ws values from dataframe
PAR <- sitesatellitemergedDATA$PAR_site    # PAR values from dataframe
FPAR <- sitesatellitemergedDATA$FAPAR_sg   # FPAR values from dataframe

# Create an empty dataframe to store evaluation metrics
evaluation_metrics <- data.frame(LUEmax = numeric(), Topt = numeric(), RMSE = numeric(), MAE = numeric(), Bias = numeric(), R_squared = numeric())

# Iterate over each combination of LUEmax and Topt
for (Topt in Topt_values) {
  Ts <- Ts_values[, which(Topt_values == Topt)]  # Get corresponding Ts values
  for (LUEmax in LUEmax_values) {
    # Calculate GPP for the current combination of Ts and LUEmax
    GPP <- calculate_GPP(Ts, LUEmax, Ws, PAR, FPAR)
    # Add the GPP values to df_luemaxgdd_modeled dataframe
    column_name <- paste("GPP", sprintf("%.2f", LUEmax), sprintf("%.2f", Topt), sep = "_")
    sitesatellitemergedDATA[[column_name]] <- GPP
    
    # Calculate evaluation metrics
    observed <- sitesatellitemergedDATA$GPP_site
    predicted <- GPP
    RMSE <- sqrt(mean((observed - predicted)^2))
    MAE <- mean(abs(observed - predicted))
    Bias <- mean(predicted - observed)
    R_squared <- cor(observed, predicted)^2
    
    # Append the results to the evaluation_metrics dataframe
    evaluation_metrics <- rbind(evaluation_metrics, data.frame(LUEmax = LUEmax, Topt = Topt, RMSE = RMSE, MAE = MAE, Bias = Bias, R_squared = R_squared))
  }
}

# View the updated dataframe
print(sitesatellitemergedDATA)

# View the evaluation metrics dataframe
Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0.08_33.00)
Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPvpm)

# Save the evaluation_metrics dataframe as a CSV file
write.csv(evaluation_metrics, "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/evaluation_metrics_vpm.csv", row.names = FALSE)
library(ggplot2)

# Directory path
directory <- "C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/3-19-2024/LUEMax_Topt"

# Plot 1: RMSE
plot1 <- ggplot(evaluation_metrics, aes(x = LUEmax, y = RMSE, color = factor(Topt))) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = expression(paste(epsilon[0], " (mol ", CO[2], " ", mol^-1, " PPFD)")),
       y = expression(atop("Root mean square error of VPM predicted GPP", " (g " * C * " m"^{-2} * " day"^{-1} * ") averaged over 8 days")),
       color = expression("Topt (°C)")) +
  theme_minimal()+ theme(text = element_text(size = 20))

# Save Plot 1
ggsave(file.path(directory, "RMSE_plot.png"), plot = plot1)

# Plot 2: MAE

plot2 <- ggplot(evaluation_metrics, aes(x = LUEmax, y = MAE, color = factor(Topt))) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = expression(paste(epsilon[0], " (mol ", CO[2], " ", mol^-1, " PPFD)")), 
       y = expression(atop("Mean absolute error of VPM predicted GPP ", "(g " * C * " m"^{-2} * " day"^{-1} * ") averaged over 8 days")),
       color = expression("Topt (°C)")) +
  theme_minimal() +
  theme(text = element_text(size = 20))


# Save Plot 2
ggsave(file.path(directory, "MAE_plot.png"), plot = plot2)

# Plot 3: Bias
plot3 <- ggplot(evaluation_metrics, aes(x = LUEmax, y = Bias, color = factor(Topt))) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = expression(paste(epsilon[0], " (mol ", CO[2], " ", mol^-1, " PPFD)")),
       y = expression(atop("Bias of VPM predicted GPP", " (g " * C * " m"^{-2} * " day"^{-1} * ") averaged over 8 days")),
       color = expression("Topt (°C)")) +
  theme_minimal()+ theme(text = element_text(size = 20))

# Save Plot 3
ggsave(file.path(directory, "Bias_plot.png"), plot = plot3)

# Plot 4: R_squared
plot4 <- ggplot(evaluation_metrics, aes(x = LUEmax, y = R_squared, color = factor(Topt))) +
  geom_point(size = 3) +
  geom_line() +
  labs(x = expression(paste(epsilon[0], " (mol ", CO[2], " ", mol^-1, " PPFD)")),
       y = expression(atop("R squared values of VPM predicted GPP ", "(g " * C * " m"^{-2} * " day"^{-1} * ") averaged over 8 days")),
       color = expression("Topt (°C)")) +
  theme_minimal()+ theme(text = element_text(size = 20))

# Save Plot 4 with specified width and height
ggsave(file.path(directory, "R_squared_plot.png"), plot = plot4, width = 10, height = 8)

arrange <-  plot1  + plot2+plot3+plot4 & theme(legend.position = "right")
combined_plot <- arrange +plot_annotation(tag_levels = 'A') + theme(legend.position = "right")
combined_plot
ggsave(file.path(directory, "plots_grid.png"), combined_plot, width = 25, height = 14)
# Define the weights for each metric
weight_rmse <- 0.33
weight_mae <- 0.33
weight_bias <- 0.33

# Calculate the combined score for each model
evaluation_metrics$combined_score <- 1 / (evaluation_metrics$RMSE * weight_rmse) + 
  1 / (evaluation_metrics$MAE * weight_mae) +
  1 / (abs(evaluation_metrics$Bias) * weight_bias)

# Find the index of the model with the highest combined score
best_model_index <- which.max(evaluation_metrics$combined_score)
best_model <- evaluation_metrics[best_model_index, ]

# Print the output
print("Best Model Performance:")
print(best_model)

evaluation_metrics

### Calculate the GPP from the values of the training set
Tmin <- -1 
Tmax <- 48
Topt <- mean(training_df$Topt)
LUEmax_0_06512<-mean(training_df$LUEmax)
Ts_31_79 <-calculate_Ts(sitesatellitemergedDATA$Tair_site, Tmin, Tmax, Topt)
GPP_0_06512_31_79<-calculate_GPP(Ts_31_79, LUEmax_0_06512,sitesatellitemergedDATA$Ws, sitesatellitemergedDATA$PAR_site, sitesatellitemergedDATA$FAPAR_sg)
# Add GPP to the sitesatellitemergedDATA dataframe
sitesatellitemergedDATA$GPP_0_06512_31_79 <- GPP_0_06512_31_79
sitesatellitemergedDATA$GPP_0_06512_31_79<-as.numeric(sitesatellitemergedDATA$GPP_0_06512_31_79)
##### Get the annual sum correlation plot
# Concatenate the two columns
# Display the first few rows of the dataframe with the new column
sitesatellitemergedDATAsiteyear<-sitesatellitemergedDATA%>% select(gpp, GPPvpmsite, GPP_site, siteyear.x, year, site, GPP_0.05_30.00, GPP_0_06512_31_79)
sitesatellitemergedDATAannualsum<- sitesatellitemergedDATAsiteyear %>%
  dplyr::group_by(siteyear.x) %>%
  dplyr::summarise(across(c(gpp, GPPvpmsite, GPP_site, GPP_0.05_30.00, GPP_0_06512_31_79), sum, na.rm = TRUE))

# Extract site and year from siteyear.x column
sitesatellitemergedDATAannualsum$site <- substr(sitesatellitemergedDATAannualsum$siteyear.x, 1, 5)
sitesatellitemergedDATAannualsum$year <- as.integer(substr(sitesatellitemergedDATAannualsum$siteyear.x, 6, nchar(sitesatellitemergedDATAannualsum$siteyear.x)))


pal=c("#001f2e",
      "#003f5c",
      "#2f4b7c",
      "#665191",
      "#a05195",
      "#d45087",
      "#f95d6a",
      "#ff7c43",
      "#ffa600",
      "#ffc04c")
GPPsiteVPMrmseannual_0.0530<-Metrics::rmse(sitesatellitemergedDATAannualsum$GPP_site, sitesatellitemergedDATAannualsum$GPP_0.05_30.00)
GPPsiteVPMmaeannual_0.0530<-Metrics::mae(sitesatellitemergedDATAannualsum$GPP_site, sitesatellitemergedDATAannualsum$GPP_0.05_30.00)
GPPsiteVPMbiasannual_0.0530<-Metrics::bias(sitesatellitemergedDATAannualsum$GPP_site, sitesatellitemergedDATAannualsum$GPP_0.05_30.00)
GPPsiteVPMr2annual_0.0530<-cor(sitesatellitemergedDATAannualsum$GPP_site,sitesatellitemergedDATAannualsum$GPP_0.05_30.00)^2
annualgppvpmcom_0.0530 <- ggplot(data = sitesatellitemergedDATAannualsum, aes(x = GPP_site, y = GPP_0.05_30.00, fill = site, shape = factor(year))) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 22) +
  geom_smooth(aes(group =1), se = FALSE, size = 8, method = lm)+
  xlab(bquote('Annual sum of GPP EC ('*g~ 'C'~ m^-2~season^-1*')')) +
  scale_x_continuous(breaks = seq(120, 250, by = 20))+
  ylab(expression(paste("Annual sum of ", GPP~VPM[site],~'('*g~ 'C'~ m^-2~season^-1*')'))) +
  scale_shape_manual(values = c(22, 21, 22, 25))+
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(fill = "Site", shape = "Year") +  # Set legend names
  theme_classic() +
  theme(text = element_text(size = 60)) +
  theme(legend.key.size = unit(2, 'cm'), legend.key = element_rect(color = NA, fill = NA)) +
  theme(axis.line = element_line(size = 1.7)) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
  guides(fill = guide_legend(override.aes=list(shape=21))) +
  geom_text(aes(x = 142, y = 225, label = paste("RMSE =", round(GPPsiteVPMrmseannual_0.0530, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 160, y = 225, label = expression(paste("" * g ~ "C" ~ m^-2 ~ season^-1 * "")), size = 17, color = "black")+
  geom_text(aes(x = 140, y = 215, label = paste("MAE =", round(GPPsiteVPMmaeannual_0.0530, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 158, y = 215, label = expression(paste("" * g ~ "C" ~ m^-2 ~ season^-1 * "")), size = 17, color = "black")+
  geom_text(aes(x = 140, y = 205, label = paste("Bias =", round(GPPsiteVPMbiasannual_0.0530, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 158, y = 205, label = expression(paste("" * g ~ "C" ~ m^-2 ~ season^-1 * "")), size = 17, color = "black")+
  ggplot2::annotate("text", x = 135, y = 195, label = expression(paste("" * italic(R^2) * "=" * "")), size = 17, color = "black")+
  geom_text(aes(x = 140, y = 195, label = paste(round(GPPsiteVPMr2annual_0.0530, 2))), size = 17, color = "black")

annualgppvpmcom_0.0530
directory <- "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/"
# Save Plot 1
ggsave(file.path(directory, "cumulATIVE_plot0530.png"), width = 34, height =21)


### annual sum of the calibrated values
GPPsiteVPMrmseannual<-Metrics::rmse(sitesatellitemergedDATAannualsum$GPP_site, sitesatellitemergedDATAannualsum$GPP_0_06512_31_79)
GPPsiteVPMmaeannual<-Metrics::mae(sitesatellitemergedDATAannualsum$GPP_site, sitesatellitemergedDATAannualsum$GPP_0_06512_31_79)
GPPsiteVPMbiasannual<-Metrics::bias(sitesatellitemergedDATAannualsum$GPP_site, sitesatellitemergedDATAannualsum$GPP_0_06512_31_79)
GPPsiteVPMr2annual<-cor(sitesatellitemergedDATAannualsum$GPP_site,sitesatellitemergedDATAannualsum$GPP_0_06512_31_79)^2
annualgppvpmcom <- ggplot(data = sitesatellitemergedDATAannualsum, aes(x = GPP_site, y = GPP_0_06512_31_79, fill = site, shape = factor(year))) +
  geom_abline(intercept = 0, slope = 1, size = 5, col = "red", linetype = "dashed") +
  geom_point(size = 22) +
  geom_smooth(aes(group =1), se = FALSE, size = 8, method = lm)+
  xlab(bquote('Annual sum of GPP EC ('*g~ 'C'~ m^-2~season^-1*')')) +
  scale_x_continuous(breaks = seq(120, 250, by = 20))+
  ylab(expression(paste("Annual sum of ", GPP~VPM[site],~'('*g~ 'C'~ m^-2~season^-1*')'))) +
  scale_shape_manual(values = c(22, 21, 22, 25))+
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  labs(fill = "Site", shape = "Year") +  # Set legend names
  theme_classic() +
  theme(text = element_text(size = 60)) +
  theme(legend.key.size = unit(2, 'cm'), legend.key = element_rect(color = NA, fill = NA)) +
  theme(axis.line = element_line(size = 1.7)) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm")) +
  guides(fill = guide_legend(override.aes=list(shape=21))) +
  geom_text(aes(x = 142, y = 265, label = paste("RMSE =", round(GPPsiteVPMrmseannual, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 160, y = 265, label = expression(paste("" * g ~ "C" ~ m^-2 ~ season^-1 * "")), size = 17, color = "black")+
  geom_text(aes(x = 140, y = 255, label = paste("MAE =", round(GPPsiteVPMmaeannual, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 158, y = 255, label = expression(paste("" * g ~ "C" ~ m^-2 ~ season^-1 * "")), size = 17, color = "black")+
  geom_text(aes(x = 140, y = 245, label = paste("Bias =", round(GPPsiteVPMbiasannual, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 158, y = 245, label = expression(paste("" * g ~ "C" ~ m^-2 ~ season^-1 * "")), size = 17, color = "black")+
  ggplot2::annotate("text", x = 135, y = 235, label = expression(paste("" * italic(R^2) * "=" * "")), size = 17, color = "black")+
  geom_text(aes(x = 140, y = 235, label = paste(round(GPPsiteVPMr2annual, 2))), size = 17, color = "black")+
  ggplot2::annotate("text", x = 135, y = 235, label = expression(paste("" * italic(R^2) * "=" * "")), size = 17, color = "black")

annualgppvpmcom
ggsave(file.path(directory, "cumulATIVE_plot0_06512_31_79.png"), width = 34, height =21)


# In-situ calibrated LUEmax and TOPT##################
cat("Insitu calibrated LUEmax value:", mean(training_df$LUEmax), "\n")
cat("Insitu calibrated Topt value:", mean(training_df$Topt), "\n\n")
mean(training_df$LUEmax)*12.011

library(patchwork)
combinedannualgpp <-  annualgppvpmcom_0.0530  / annualgppvpmcom  & theme(legend.position = "right")
combinedannualgpp<-combinedannualgpp + plot_layout(guides = "collect")+plot_annotation(tag_levels = 'A')
combinedannualgpp
ggsave(combinedannualgpp, filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/cumulATIVEcombined.png", width = 40, height = 40)

# Reduction in RMSE
reduction_rmse <- ((GPPsiteVPMrmseannual_0.0530 - GPPsiteVPMrmseannual) / GPPsiteVPMrmseannual_0.0530) * 100

# Reduction in MAE
reduction_mae <- ((GPPsiteVPMmaeannual_0.0530 - GPPsiteVPMmaeannual) / GPPsiteVPMmaeannual_0.0530) * 100

# Reduction in bias
reduction_bias <- ((GPPsiteVPMbiasannual_0.0530 - GPPsiteVPMbiasannual) / GPPsiteVPMbiasannual_0.0530) * 100
# Absolute reduction in bias
absolute_reduction_bias <- (abs(GPPsiteVPMbiasannual_0.0530) - abs(GPPsiteVPMbiasannual)) / abs(GPPsiteVPMbiasannual_0.0530) * 100

absolute_reduction_bias
