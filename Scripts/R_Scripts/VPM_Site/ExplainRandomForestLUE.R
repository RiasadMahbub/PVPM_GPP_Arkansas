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


# what are the VIS
library(jsonlite)
print(best_features_lue) # Run this PVPM_RFE.R
best_features_lue_indices
# This script reads the spectral-indices.js file from a local path, extracts specific indices, and presents their details in a table.
# Ensure you have the 'jsonlite' package installed. # If not, run: install.packages("jsonlite")

# Define the full path to the spectral-indices.js file. # IMPORTANT: If you move this file, you must update the path below.
file_path <- "C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/spectral-indices.js"
# Read the file content as a single string.
raw_content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
# A more robust way to extract the JSON part by removing the surrounding JS code. # The JSON object starts after `var spectralIndices = ` and ends before the `exports` line.
json_start <- regexpr("var spectralIndices = ", raw_content) + nchar("var spectralIndices = ")
# Find the position of the last closing curly brace before the exports section. # This is a robust way to find the end of the JSON object.
json_end_pos <- tail(gregexpr("}", substring(raw_content, 1, regexpr("exports.indices", raw_content)), fixed = TRUE)[[1]], 1)
# Extract the JSON string, ensuring only the valid JSON is captured.
json_string <- substring(raw_content, json_start, json_end_pos)
# Check if the JSON data was successfully extracted.
if (nchar(trimws(json_string)) < 10) { # Using a more lenient check
  stop("Failed to extract the JSON data. The file format might be unexpected.")
}
# The original file uses single quotes, but JSON requires double quotes.
# This line replaces all single quotes with double quotes to make the string valid JSON.
json_string <- gsub("'", '"', json_string)
# Parse the JSON data into a list.
indices_list <- fromJSON(json_string, flatten = TRUE)
# The function processes the nested list and handles the varying lengths correctly.
process_index_data <- function(indices_list) {
  # Access the nested 'SpectralIndices' list
  indices_data <- indices_list$SpectralIndices
  # Create a data frame to store the processed data
  df <- data.frame(
    short_name = character(),
    application_domain = character(),
    bands = character(),
    formula = character(),
    stringsAsFactors = FALSE
  )
  # Iterate over each index in the list
  for (name in names(indices_data)) {
    index_entry <- indices_data[[name]]
    # Handle the 'bands' field separately to convert list to a string
    bands_str <- paste(index_entry$bands, collapse = ", ")
    # Create a new row for the data frame
    new_row <- data.frame(
      short_name = name,
      application_domain = index_entry$application_domain,
      bands = bands_str,
      formula = index_entry$formula,
      stringsAsFactors = FALSE
    )
    # Append the new row to the data frame
    df <- rbind(df, new_row)
  }
  return(df)
}
# Create the master data frame
indices_df <- process_index_data(indices_list)
# # Define the list of indices from the 'best_features_lue' variable. # We will manually filter out non-index terms.
# best_features_lue_indices <- c(
#   "NDDI", "RI4XS", "MLSWI26", "TGI", "MRBVI", "ExG", "BCC", "NDYI",
#   "EMBI", "MBWI", "NMDI", "DBSI", "BIXS", "IKAW", "DSWI3", "AWEInsh", "MuWIR"
# )
# Filter the data frame to include only the requested indices from best_features_lue.
results <- indices_df[indices_df$short_name %in% best_features_lue, ]
# Prepare the formatted table for printing.
header <- c("Short Name", "Application Domain", "Bands Used", "Formula")
separator <- c("----------", "------------------", "----------", "-------")
# Start with the markdown table headers and separators.
table_rows <- c(
  paste(header, collapse = " | "),
  paste(separator, collapse = " | ")
)
# Populate the table with the data.
for (i in 1:nrow(results)) {
  row_data <- results[i,]
  table_rows <- c(table_rows, paste(c(row_data$short_name, row_data$application_domain, row_data$bands, row_data$formula), collapse = " | "))
}
# Print the final formatted table.
cat("### Selected Spectral Index Details (from best_features_lue)\n\n")
cat(paste(table_rows, collapse = "\n"))
cat("\n\n---\n\n")
# Find and print a list of Visible-spectrum-based Vegetation Indices # Use a more reliable way to filter for visible bands
vis_indices <- indices_df[indices_df$application_domain == "vegetation" & grepl("G|R|B|A|Y", indices_df$bands), ]
# Create a new table for the visible indices
vis_table_rows <- c(
  paste(header, collapse = " | "),
  paste(separator, collapse = " | ")
)
for (i in 1:nrow(vis_indices)) {
  row_data <- vis_indices[i,]
  vis_table_rows <- c(vis_table_rows, paste(c(row_data$short_name, row_data$application_domain, row_data$bands, row_data$formula), collapse = " | "))
}
cat("### Visible-Spectrum-Based Vegetation Indices\n\n")
cat(paste(vis_table_rows, collapse = "\n"))
# This script analyzes a pre-defined table of spectral indices to categorize them
# by application domain and count the usage of each spectral band.

# This script assumes a data frame named `results_df` is already available in your
# R environment. The structure of this data frame should match the output you provided,
# with columns named `short_name`, `application_domain`, and `bands`.
# This script analyzes a pre-defined table of spectral indices to categorize them
# by application domain and count the usage of each spectral band.

# This script assumes a data frame named `results` is already available in your
# R environment. The structure of this data frame should match the output you provided,
# with columns named `short_name`, `application_domain`, and `bands`.

# --- Categorize by Application Domain ---
cat("### Spectral Indices by Application Domain\n\n")
domain_counts <- table(results$application_domain)
for (domain in names(domain_counts)) {
  indices <- results$short_name[results$application_domain == domain]
  cat(paste0("* ", toupper(substring(domain, 1, 1)), substring(domain, 2), " (", domain_counts[domain], " indices): ", paste(indices, collapse = ", "), "\n"))
}

# --- Count Band Usage ---
cat("\n\n### Band Usage Count\n\n")
# Create a vector to hold all bands from the data
all_bands <- unlist(strsplit(results$bands, ", "))
# Count the frequency of each band
band_counts <- table(all_bands)
# Sort the counts in descending order
sorted_band_counts <- sort(band_counts, decreasing = TRUE)
# Print the results in a formatted list
for (band in names(sorted_band_counts)) {
  cat(paste0("* ", band, ": ", sorted_band_counts[band], " times\n"))
}


joined_df_luebest <- joined_df %>%
  select(all_of(c("PAR_site", "GPP_site", "LUE", "fAPAR", "Lai", best_features_lue)))
joined_df_luebest <- joined_df_luebest[, !(names(joined_df_luebest) %in% 
                                             c("rH_site", "VPD_site", "DOP", "Es", "DAP", "dayl"))]

# Load required library
library(corrplot)

# Compute correlation matrix (numeric columns only)
cor_matrix <- cor(joined_df_luebest[, sapply(joined_df_luebest, is.numeric)], 
                  use = "pairwise.complete.obs")

# Save correlation plot as PNG
png("C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Figure/LUE/corrplot_joined_df_luebest.png", 
    width = 5000, height = 5000, res = 650)

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.7, number.cex = 0.6, 
         addCoef.col = "black")  # shows correlation values

dev.off()

# Extract correlations with LUE
lue_cor <- cor_matrix[, "LUE"]

# Remove self-correlation and excluded variables
exclude_vars <- c("LUE", "fAPAR", "Tair_site", "GPP_site", "Lai")
lue_cor <- lue_cor[!(names(lue_cor) %in% exclude_vars)]
# Order by absolute correlation (but keep original values)
lue_cor_sorted <- lue_cor[order(abs(lue_cor), decreasing = TRUE)]
# Get top 5 VIs correlated with LUE
top5_lue <- head(lue_cor_sorted, 5)
# Print result (with sign preserved)
print(top5_lue)


#----------------------categort--------------------
# Create a unique mapping of short_name to application_domain
# Top 5 indices from correlation with LUE
library(dplyr)

# Top 5 indices from correlation with LUE
top5_indices <- names(top5_lue)

# Create a unique mapping of index -> domain and bands
index_info_map <- results %>%
  select(short_name, application_domain, bands) %>%
  distinct(short_name, .keep_all = TRUE)

# Map top 5 indices to their domain and bands
top5_summary <- data.frame(
  Index = top5_indices,
  Correlation = top5_lue
) %>%
  left_join(index_info_map, by = c("Index" = "short_name")) %>%
  rename(Domain = application_domain, BandsUsed = bands)

# Print nicely
print(top5_summary)
