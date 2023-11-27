#### This project is gapfilling the other Arkansas Rice site data

##Site USOF1
## Read the data
# location of the data: "/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites"
# how to access 95%: https://www.r-bloggers.com/2021/07/asymptotic-confidence-intervals-for-nls-regression-in-r/

#Load the required Libraries
library(REddyProc)
library(dplyr)
library(lubridate)
library(shiny)
library(readxl)
library(tidyverse)
library(cowplot)
require(stringr)
library(viridis)
library(minpack.lm)
library(bigleaf)
library(anytime)

## Latest file with data stamp: Rice Data_Research Group_03-10-2022.xlsx 
## OF1 is in sheet 8 RiceData_ResearchGroup03-10-2022
## OF 2 is in sheet 9 RiceData_ResearchGroup03-10-2022
# Reading the data removing the unit row
## Extracting column from Excel file with read_excel()
myCols_USOF1_2017<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =8))
myCols_USOF2_2017<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =9))
myCols_USOF3_2017<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_04-05-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =10))
myCols_USOF4_2018<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =11))
myCols_USOF5_2018<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =12))
myCols_USOF6_2018<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =13))
myCols_USBDA_2015<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =14))
myCols_USBDA_2016<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =15))
myCols_USBDC_2015<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =16))
myCols_USBDC_2016<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =17))
myCols_USHRA_2015<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =5))
myCols_USHRA_2016<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =6))
myCols_USHRA_2017<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =7))
myCols_USHRC_2015<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                          n_max = 1, col_names = FALSE, sheet =2))
myCols_USHRC_2016<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =3))
myCols_USHRC_2017<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =4))



## reading the data column from Excel file and using the 
USOF1_2017<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", guess_max = 21474836,
                  skip= 2, sheet =8, col_names = myCols_USOF1_2017)
USOF2_2017<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                  skip= 2, sheet =9, col_names = myCols_USOF2_2017)
USOF3_2017<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_04-05-2022.xlsx", 
                  skip= 2, sheet =10, col_names = myCols_USOF3_2017)
USOF4_2018<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =11, col_names = myCols_USOF4_2018)
USOF5_2018<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =12, col_names = myCols_USOF5_2018)
USOF6_2018<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =13, col_names = myCols_USOF6_2018)
USBDA_2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                      skip= 2, sheet =14, col_names = myCols_USBDA_2015)
USBDA_2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                      skip= 2, sheet =15, col_names = myCols_USBDA_2016)
USBDC_2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =16, col_names = myCols_USBDC_2015)
USBDC_2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =17, col_names = myCols_USBDC_2016)
USHRA_2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =5, col_names = myCols_USHRA_2015)
USHRA_2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =6, col_names = myCols_USHRA_2016)
USHRA_2017<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =7, col_names = myCols_USHRA_2017)
USHRC_2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =2, col_names = myCols_USHRC_2015)
USHRC_2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =3, col_names = myCols_USHRC_2016)
USHRC_2017<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =15, col_names = myCols_USHRC_2017)


# Define the file path
file_path <- "/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx"

# Define the sheet names and corresponding column indices
sheet_cols <- list(
  USOF1_2017 = 8,
  USOF2_2017 = 9,
  USOF3_2017 = 10,
  USOF4_2018 = 11,
  USOF5_2018 = 12,
  USOF6_2018 = 13,
  USBDA_2015 = 14,
  USBDA_2016 = 15,
  USBDC_2015 = 16,
  USBDC_2016 = 17,
  USHRA_2015 = 5,
  USHRA_2016 = 6,
  USHRA_2017 = 7,
  USHRC_2015 = 2,
  USHRC_2016 = 3,
  USHRC_2017 = 4
)

## date that match with MODIS doy 
MODISdoy <- list(
  USOF1_2017 = yday("2017-04-07"),
  USOF2_2017 = yday("2017-04-07"),
  USOF3_2017 = yday("2017-04-07"),
  USOF4_2018 = yday("2018-04-15"),
  USOF5_2018 = yday("2018-04-15"),
  USOF6_2018 = yday("2018-04-15"),
  USBDA_2015 = yday("2015-04-7"),
  USBDA_2016 = yday("2016-03-29"),
  USBDC_2015 = yday("2015-04-7"), 
  USBDC_2016 = yday("2016-03-29"),
  USHRA_2015 = yday("2015-04-07"), 
  USHRA_2016 = yday("2016-04-30"),
  USHRA_2017 = yday("2017-04-15"),
  USHRC_2015 = yday("2015-04-15"),
  USHRC_2016 = yday("2016-04-30"),
  USHRC_2017 = yday("2017-04-23")
)


# Initialize an empty list to store the data frames
data_frames <- list()
points<- list()
DOYlist<- list()
LUEmaxGPP<- list() ## LUEmax estimation GPP-APAR curve (list)
LUEmaxNEE<-list() ## LUEmax estimation NEE-APAR curve (list)
LUEmaxGPPmax<- list() ## LUEmax estimation GPPmax-APAR curve (list)
LUEmaxNEEmax<-list()## LUEmax estimation NEEmax-APAR curve (list)
lightresponsegs<-list() ## LUEmax estimation NEE curve from bigleaf package
modellistGPP<-list() ## store the model result of the non linear regression of GPP curve
modellistNEE<-list()
modellistGPPmax<-list() ## store the model result of the non linear regression of GPP curve
modellistNEEmax<-list()
lightresponsegs_modellist<-list() ##store the model result of the non linear regression of GPP curve
dflist<-list()
stderror<-list()
rsquared<-list()
rss<-list()
rmse<-list()
avgsdevsub<-list()
avgsdevadd<-list()
DOYlist<- list()
DOYlist2<- list()

# Read the data from each sheet and store in the list
for (var_name in names(sheet_cols)) {
  sheet <- sheet_cols[[var_name]]
  col_names <- as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet = sheet))  # Read the column names from the first row of the sheet
  data <- read_excel(file_path, skip = 2, sheet = sheet, col_names = col_names) # Read the data, skipping the first two rows (header)
  data_frames[[var_name]] <- data # Store the data frame in the list (1st one is USOF12017)
  
  ### Fixing the date, based on date we will separate the 8 day bins 
  data_frames[[var_name]]$`Date Time` <- gsub("'", "", data_frames[[var_name]]$`Date Time`) #  # Remove single quotes from the 'Date Time' column
  data_frames[[var_name]]$datetime <- strptime(data_frames[[var_name]]$`Date Time`, format = '%d-%b-%Y %H:%M:%S') ## convert to datetime format in R
  data_frames[[var_name]]$Date <- as.Date(data_frames[[var_name]]$datetime) ## Extract the date from the datetime format,   # Get the date column from the dt
  data_frames[[var_name]]$DOY <- yday(data_frames[[var_name]]$datetime) # Add the DOY column
  data_frames[[var_name]] <- filter(data_frames[[var_name]], DOY >= MODISdoy[[var_name]]) # Filter the data frame based on the MODISdoy value for the current sheet
}

### Satellite Site Calibration Part
# Set the directory path where your CSV files are located
directory_path <- "/Users/riasadbinmahbub/RProgramming/GapfillingOtherRiceSites/GapfillingOtherRiceSites/Data/Satellite-SiteCalibrationCSV"

# Get a list of all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Create an empty list to store the data frames
data_frames_satellitecalibration <- list()

# Loop through each CSV file, read it, and store it as a variable with the filename
for (csv_file in csv_files) {
  # Extract the filename without extension
  file_name <- tools::file_path_sans_ext(basename(csv_file))
  
  # Read the CSV file and store it as a data frame
  data <- read.csv(csv_file)
  
  # Rename the "date" column to "Date"
  if ("date" %in% names(data)) {
    names(data)[names(data) == "date"] <- "Date"
    
    # Change the class of the "Date" column to date type
    data$Date <- as.Date(data$Date, format="%Y-%m-%d")
  }
  # Add a new column with the filename information
  data$FileName <- file_name
  # Assign the data frame to a variable with the filename
  assign(file_name, data)
  
  # Add the data frame to the list
  data_frames_satellitecalibration[[file_name]] <- data
}

# Print the list of data frames
print(data_frames_satellitecalibration)

# Create a list to store the merged data frames
merged_data_frames <- list()

# Loop through each data frame in data_frames_satellitecalibration
for (name in names(data_frames_satellitecalibration)) {
  # Check if the data frame exists in data_frames
  if (name %in% names(data_frames)) {
    # Merge the data frames based on the "Date" column
    merged_df <- merge(data_frames[[name]], data_frames_satellitecalibration[[name]], by = "Date", all.x = TRUE)
    
    # Extract columns to interpolate
    columns_to_interpolate <- c("EVI_SG", "FAPAR", "LSWI_INP")
    
    # Loop through each column and perform linear interpolation
    for (col_name in columns_to_interpolate) {
      # Perform linear interpolation
      interpolated_values <- approx(x = merged_df$Date, y = merged_df[[col_name]], method = "linear", n = nrow(merged_df))  #merging files
      
      # Update the data frame with interpolated values
      merged_df[[col_name]] <- interpolated_values$y ## interpolating the values
    }
    
    # Add the merged data frame to the list
    merged_data_frames[[name]] <- merged_df
    merged_data_frames[[name]]$APAR <- merged_data_frames[[name]]$PAR_Regression* merged_data_frames[[name]]$FAPAR## calculate the APAR
  }
}

dev.off()
plot(merged_data_frames$USBDA_2015$APAR, merged_data_frames$USBDA_2015$NEE_REddyProc, col = merged_data_frames$USBDA_2015$DOY)
plot(merged_data_frames$USBDA_2015$PAR_Regression, merged_data_frames$USBDA_2015$NEE_REddyProc, col = merged_data_frames$USBDA_2015$DOY)



for (var_name in names(sheet_cols)) {  
  points[[var_name]]<-as.integer((max(merged_data_frames[[var_name]]$DOY)- min(merged_data_frames[[var_name]] $DOY))/8) ## how many 8 day data points will be there
  DOYlist[[var_name]]<-rep(0, points[[var_name]]+1) ##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
  DOYlist2[[var_name]]<-rep(0, points[[var_name]]) ## DOY with one less DOY to merge the files later
  LUEmaxGPP[[var_name]]<-rep(0,points[[var_name]]) ## this list will store the luemax values
  LUEmaxNEE[[var_name]]<-rep(0,points[[var_name]])
  LUEmaxGPPmax[[var_name]]<-rep(0,points[[var_name]]) ## this list will store the luemax values
  LUEmaxNEEmax[[var_name]]<-rep(0,points[[var_name]])
  lightresponsegs[[var_name]]<-rep(0,points[[var_name]])## this list will store the luemax values from bigleaf analysis

  ## creating some dataframes that will store the data based on 8 day windows
  for (i in 1:(length(DOYlist[[var_name]])-1)){
    DOYlist[[var_name]][1] = MODISdoy[[var_name]]
    DOYlist[[var_name]][i+1]=DOYlist[[var_name]][i]+ 8
    DOYlist2[[var_name]][1] = MODISdoy[[var_name]]
    DOYlist2[[var_name]][i+1]=DOYlist2[[var_name]][i]+ 8
    #DOYlist2[[var_name]]<-DOYlist2[[var_name]][-length(DOYlist2[[var_name]])]
  }
  DOYlist2[[var_name]]<-DOYlist2[[var_name]][-length(DOYlist2[[var_name]])]
  
  ## Creating a list of empty dataframes
  for (i in 1:points[[var_name]]){
    dflist[[var_name]][[i]]<-data.frame()
  }
  ###Storing the data in the empty dataframes
  for (i in 1: length(dflist[[var_name]])){
    dflist[[var_name]][[i]]<- subset(merged_data_frames[[var_name]], DOY>=DOYlist[[var_name]][i] & DOY<DOYlist[[var_name]][i+1])
    dflist[[var_name]][[i]]<-subset(dflist[[var_name]][[i]], PAR_Regression>20)
    #percentile_95 <- quantile(dflist[[var_name]][[i]]$GPP_modeled, 0.85)
    #dflist[[var_name]][[i]] <- subset(dflist[[var_name]][[i]], GPP_modeled > percentile_95) ## subsetting GPP above 95th percentile
    #dflist[[var_name]][[i]]$APAR <-dflist[[var_name]][[i]]$FAPAR* dflist[[var_name]][[i]]$PAR_Regression ## calculate the APAR
  }

  for (i in 1:points[[var_name]]){
    modellistGPP[[var_name]][[i]]<-data.frame()
    modellistNEE[[var_name]][[i]]<-data.frame()
    modellistGPPmax[[var_name]][[i]]<-data.frame()
    modellistNEEmax[[var_name]][[i]]<-data.frame()
    lightresponsegs_modellist[[var_name]][[i]]<-data.frame()
  }

  stderror[[var_name]]<-rep(0, points[[var_name]])
  rsquared[[var_name]]<-rep(0, points[[var_name]])
  rss[[var_name]]<-rep(0, points[[var_name]])
  rmse[[var_name]]<-rep(0, points[[var_name]])
  ### Read the files of Site Calibration 
  ### PAR to APAR

  for (i in (1:points[[var_name]])){
    y<-dflist[[var_name]][[i]]$GPP_modeled
    x<-dflist[[var_name]][[i]]$APAR  ## changed to APAR, before it was PAR
    NEE<-dflist[[var_name]][[i]]$NEE_modeled
    Reco<-dflist[[var_name]][[i]]$Reco_modeled
    modellistGPP[[var_name]][[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.03 ,g= 25),   ## what should the parameters value
                                      lower = c(a = 0, g = 0),  # Set lower bound for 'a'
                                      upper = c(a = 100, g =200) )

    modellistNEE[[var_name]][[i]]<-nlsLM((-NEE ~ alpha * x / (1 - (x / 2000) + (alpha * x / GPP_ref)) - Reco),
                                          start=list(alpha=0.05,GPP_ref=30))
    modellistGPPmax[[var_name]][[i]]<-nlstools::confint2(modellistGPP[[var_name]][[i]], level = 0.90, method = "asymptotic")
    modellistNEEmax[[var_name]][[i]]<-nlstools::confint2(modellistNEE[[var_name]][[i]], level = 0.90, method = "asymptotic")
    LUEmaxGPP[[var_name]][[i]]<-modellistGPP[[var_name]][[i]]$m$getPars()[1]
    LUEmaxNEE[[var_name]][[i]]<-modellistNEE[[var_name]][[i]]$m$getPars()[1]
    LUEmaxGPPmax[[var_name]][[i]]<-modellistGPPmax[[var_name]][[i]][1]
    LUEmaxNEEmax[[var_name]][[i]]<-modellistNEEmax[[var_name]][[i]][1]
    
    stderror[[var_name]][i]<-summary(modellistGPP[[var_name]][[i]])$parameters[1,2]
    rss[[var_name]][i]<-sum((modellistGPP[[var_name]][[i]]$m$resid())^2)
    rmse[[var_name]][i]<-sqrt(crossprod(modellistGPP[[var_name]][[i]]$m$resid())/(length(modellistGPP[[var_name]][[i]]$m$resid())))
    rsquared[[var_name]][i] <-  1-(rss[[var_name]][i]/sum((y - mean(y))^2))
  }
 

  
  avgsdevsub[[var_name]]<-rep(0,points[[var_name]])
  avgsdevadd[[var_name]]<-rep(0,points[[var_name]])
  for (i in (1:points[[var_name]])){
    avgsdevsub[[var_name]][i]<-(summary(modellistGPP[[var_name]][[i]])$parameters[1,1]) - (summary(modellistGPP[[var_name]][[i]])$parameters[1,2])
    avgsdevadd[[var_name]][i]<- (summary(modellistGPP[[var_name]][[i]])$parameters[1,1]) + (summary(modellistGPP[[var_name]][[i]])$parameters[1,2])
  }
  lightresponsegs[[var_name]]<-light.response(data = merged_data_frames[[var_name]], merged_data_frames[[var_name]]$NEE_modeled, Reco=merged_data_frames[[var_name]]$Reco_modeled,PPFD=merged_data_frames[[var_name]]$PAR_Regression,PPFD_ref=2000)
  #lightresponsegs_modellist[[var_name]][i]<-light.response(data = merged_data_frames[[var_name]][i], merged_data_frames[[var_name]][i]$NEE_modeled, Reco=merged_data_frames[[var_name]][i]$Reco_modeled,PPFD=merged_data_frames[[var_name]][i]$PAR_Regression,PPFD_ref=2000)
  #lightresponsegs[[var_name]][[i]]<-lightresponsegs_modellist[[var_name]][[i]]$m$getPars()[1]
}

lightresponsegs
plot(LUEmaxNEE[[2]])

nlstools::confint2(modellistNEE$USOF1_2017[[1]], level = 0.60, method = "asymptotic")

LUEmaxNEEmax

lightresponsegs$USOF1_2017$m$getPars()[1]
plot_directory <- "/Users/riasadbinmahbub/RProgramming/GapfillingOtherRiceSites/GapfillingOtherRiceSites/Figure/LightResponseCurve/GPP_APAR_NLSM"
lightresponsegs$USOF1_2017$m$getPars()
lightresponsegs

plot(LUEmax[[3]])
plot(LUEmax[[1]][(4:6)])
plot(LUEmax[[13]])
rss
par(mfrow = c(1, 1))
par(mar=c(2, 2, 2, 2))
hist(merged_data_frames$USHRC_2016$GPP_modeled)
plot(merged_data_frames$USHRC_2017$APAR, merged_data_frames$USHRC_2017$GPP_modeled, col = merged_data_frames$USHRC_2016$PAR_Regression)
### Write a code that gives the plot of all the hyperbolic curve and stores in a folder

### convert the list to a dataframes
# Convert the list to a dataframe
# Convert the list to a dataframe

LUEmaxGPPdf<-stack(LUEmaxGPP)
LUEmaxNEEdf<-stack(LUEmaxNEE)
LUEmaxGPPmaxdf<-stack(LUEmaxGPPmax)
LUEmaxNEEmaxdf<-stack(LUEmaxNEEmax)
DOY_df <-stack(DOYlist2)

LUEmax<- do.call("cbind", list(LUEmaxGPPdf, LUEmaxNEEdf, LUEmaxGPPmaxdf, LUEmaxNEEmaxdf, DOY_df))
LUEmaxGDD<-do.call("rbind", list(USOF1VI8day, USOF2VI8day, USOF3VI8day, USOF4VI8day, USOF5VI8day, USOF6VI8day, USBDA2015VI8day, USBDC2015VI8day, USBDCC2016VI8day, USBDA2016VI8day, USHRA_2015VI8day, USHRA_2016VI8day, USHRA_2017VI8day, USHRC_2015VI8day, USHRC_2016VI8day, USHRC_2017VI8day))
View(LUEmax)


nrow(LUEmaxGPPdf)
nrow(LUEmaxNEEdf)
nrow(LUEmaxGPPmaxdf)
nrow(LUEmaxNEEmaxdf)
nrow(DOY_df)


stderror_df <- stack(stderror)
LUEmax_df<-stack(LUEmaxGPP)
rsquared_df<-stack(rsquared)
rss_df<-stack(rss)
rmse_df<-stack(rmse)
DOY_df <-stack(DOYlist)
avgsdevsub_df<-stack(avgsdevsub)
avgsdevadd_df<-stack(avgsdevadd)
# Rename the columns
colnames(stderror_df) <- c("StandardError", "Site")
stderror_df$index <- 1:nrow(stderror_df)
colnames(LUEmax_df)<-c("LUEmax", "Site")
LUEmax_df$index <- 1:nrow(LUEmax_df)
colnames(rsquared_df) <- c("rsquared", "Site")
rsquared_df$index <- 1:nrow(rsquared_df)
colnames(rmse_df)<-c("rmse", "Site")
rmse_df$index <- 1:nrow(rmse_df)
colnames(rss_df) <- c("rss", "Site")
rss_df$index <- 1:nrow(rss_df)
colnames(DOY_df)<-c("DOYlist", "Site")
DOY_df$index <- 1:nrow(DOY_df)
colnames(avgsdevsub_df) <- c("avgsdevsub", "Site")
colnames(avgsdevadd_df)<-c("DOYlist", "Site")

LUEmaxPerformancedf <- merge(stderror_df, LUEmax_df, by="index")
LUEmaxPerformancedf <- merge(LUEmaxPerformancedf, rsquared_df, by="index")
LUEmaxPerformancedf <- merge(LUEmaxPerformancedf, rss_df, by="index")
LUEmaxPerformancedf <- merge(LUEmaxPerformancedf, rmse_df, by="index")
LUEmaxPerformancedf <- merge(LUEmaxPerformancedf, DOY_df, by="index")

LUEmaxPerformancedf
### Merge all the LUEmax here and plot the LUEmax graph as a function of DAP and Cumulative GDD
par(mfrow = c(1, 1))
par(mar=c(2, 2, 2, 2))
plot(LUEmaxPerformancedf$DOYlist, LUEmaxPerformancedf$LUEmax, ylim = c(0,0.1))
hist(LUEmaxPerformancedf$LUEmax)
View(LUEmaxPerformancedf)

hist(LUEmax)

### Predict GPP using all the data


### Run cross validation set for all the data 80:20


### Get the optimum parameters


### Get the satellite data for EVI, PAR, Temperature


### Plot result of the 

# Set options to print numbers without exponent notation
options(scipen = 999, digits = 10)
LUEmax
dflist$USOF1_2017[[12]]
plot(dflist[[var_name]][[8]]$APAR, dflist[[var_name]][[8]]$GPP_modeled)

# Assign the data frames to individual variables (optional)
list2env(data_frames, envir = .GlobalEnv)

View(USOF2_2017)




## The qoutation around the datetime column and fixing the datetime 
USOF1$`Date Time`<-gsub("'","",USOF1$`Date Time`)
USOF1$datetime<-strptime(USOF1$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF1$Date<-as.Date(USOF1$datetime)

##ADD the DOY column
USOF1$DOY<- yday(USOF1$datetime)

###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2017-04-07")

USOF1<-filter(USOF1, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF1$DOY)- min(USOF1$DOY))/8)

## Starting DOY = 185
##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
DOYlistway32015<-rep(0, points+1)

###Creating a list of LUEmax 
LUEmax2015<-rep(0,points)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = MODISdoy
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:points){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF1, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSOF1<--rep(0, points)
for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.04124,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}


## new code including rmse, std error and r2 and residual sum of square
stderrorUSOF1<--rep(0, points)
rsquareUSOF1<--rep(0, points)
residualsumofsquareUSOF1<--rep(0, points)
rmseUSOF1<--rep(0, points)


for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.04124,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF1[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF1[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF1[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF1[i] <-  1-(residualsumofsquareUSOF1[i]/sum((y - mean(y))^2))
}

plot(LUEmax2015)

### Standard error graph
avgsdevsub<-rep(0,points)

avgsdevadd<-rep(0,points)
for (i in (5:19)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}
plot((DOYlistway32015[c(-2, -(points+1))]), as.numeric(LUEmax2015[(-2)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017",  ylim=c(0.01, 0.07))
arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

par(mfrow = c(5, 5))
par(mar=c(1, 1, 1, 1))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}
plot((DOYlistway32015[c(-2, -(points+1))]), as.numeric(LUEmax2015[(-2)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017",  ylim=c(0.01, 0.07))

plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015[]),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017",  ylim=c(0.01, 0.07))




##Big leaf
light.response(data = USOF1, USOF1$NEE_modeled, Reco=USOF1$Reco_modeled,PPFD=USOF1$PAR_Regression,PPFD_ref=2000)

##### Where the code ends

### Topt estimation

###TOPT 95 percentile
toptdf<-rep(0, points)
for (i in 1: points){
  toptdf[i]<-dflist[[i]]  %>% dplyr::filter(quantile(GPP_modeled, 0.95)<GPP_modeled) %>% summarize(meanopttemp = mean(Ta_REddyProc, na.rm = TRUE))
}
toptvector<-rep(0, points)

### the topt is added as a df but we need it as a vector
for (i in 1:points){
  toptvector[i]<-toptdf[[i]]
}


## plot topt
par(mfrow = c(1, 1))
par(mar=c(2, 3, 2, 2))
plot(DOYlistway32015[-17], toptvector,  xlab="DOY", cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     
     ylab="Tpot", main = "USOF1 2015 (95 percentile method)")
topt


##Create a dataframe
USOF1LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF1", "year" = 2017, "STD_Error" = stderrorUSOF1, "R-squared" = rsquareUSOF1, "RMSE" = rmseUSOF1, "Residual sum of square" = residualsumofsquareUSOF1,  "Topt" = toptvector )
USOF1LUEmax





### USOF2
## 2017

#Load the required Libraries
library(REddyProc)
library(dplyr)
library(lubridate)
library(shiny)
library(readxl)
library(tidyverse)
library(cowplot)
require(stringr)
library(viridis)
library(minpack.lm)
library(bigleaf)
library(anytime)


## USOF2 is in the sheet 9 of RiceData_ResearchGroup03-10-2022
## Read the data
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =9))
USOF2<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                  skip= 2, sheet =9, col_names = myCols)

## The qoutation around the datetime column and fixing the datetime 
USOF2$`Date Time`<-gsub("'","",USOF2$`Date Time`)
USOF2$datetime<-strptime(USOF2$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF2$Date<-as.Date(USOF2$datetime)

##ADD the DOY column
USOF2$DOY<- yday(USOF2$datetime)

###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-97


USOF2<-filter(USOF2, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF2$DOY)- min(USOF2$DOY))/8)

## Starting DOY = 185
##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
DOYlistway32015<-rep(0, points+1)

###Creating a list of LUEmax 
LUEmax2015<-rep(0,points)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = MODISdoy
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:points){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF2, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.04124,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

## new code including rmse, std error and r2 and residual sum of square
stderrorUSOF22017<--rep(0, points)
rsquareUSOF22017<--rep(0, points)
residualsumofsquareUSOF22017<--rep(0, points)
rmseUSOF22017<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.04124,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF22017[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF22017[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF22017[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF22017[i] <-  1-(residualsumofsquareUSOF22017[i]/sum((y - mean(y))^2))
}



LUEmax2015



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}
plot((DOYlistway32015[ -(points+1)]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017")


arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")




par(mfrow = c(5, 5))
par(mar=c(1, 1, 1, 1))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}
plot((DOYlistway32015[c(-2, -(points+1))]), as.numeric(LUEmax2015[(-2)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017",  ylim=c(0.01, 0.07))


### Topt estimation

###TOPT 95 percentile
toptdf<-rep(0, points)
for (i in 1: points){
  toptdf[i]<-dflist[[i]]  %>% dplyr::filter(quantile(GPP_modeled, 0.95)<GPP_modeled) %>% summarize(meanopttemp = mean(Ta_REddyProc, na.rm = TRUE))
}
toptvector<-rep(0, points)

### the topt is added as a df but we need it as a vector
for (i in 1:points){
  toptvector[i]<-toptdf[[i]]
}


## plot topt
par(mfrow = c(1, 1))
par(mar=c(2, 3, 2, 2))
plot(DOYlistway32015[-17], toptvector,  xlab="DOY", cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     
     ylab="Tpot", main = "USOF1 2015 (95 percentile method)")
topt


##Create a dataframe
USOF2LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF2", "year" = 2017, "STD_Error" = stderrorUSOF22017, "R-squared" = rsquareUSOF22017, "RMSE" = rmseUSOF22017, "Residual sum of square" = residualsumofsquareUSOF22017, "Topt" = toptvector)
USOF2LUEmax