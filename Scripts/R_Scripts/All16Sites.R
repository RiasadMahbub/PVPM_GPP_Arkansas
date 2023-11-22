#### This project is gapfilling the other Arkansas Rice site data

##Site USOF1
## Read the data
# location of the data: "/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites"

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
LUEmax<- list()
dflist<-list()
modellist<-list()
stderror<-list()
rsquared<-list()
rss<-list()
rmse<-list()
avgsdevsub<-list()
avgsdevadd<-list()
lightresponsegs<-list()


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
  points[[var_name]]<-as.integer((max(data_frames[[var_name]] $DOY)- min(data_frames[[var_name]] $DOY))/8) ## how many 8 day data points will be there
  DOYlist[[var_name]]<-rep(0, points[[var_name]]+1) ##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
  LUEmax[[var_name]]<-rep(0,points[[var_name]]) ## this list will store the luemax values

  ## creating some dataframes that will store the data based on 8 day windows
  for (i in 1:(length(DOYlist[[var_name]])-1)){
    DOYlist[[var_name]][1] = MODISdoy[[var_name]]
    DOYlist[[var_name]][i+1]=DOYlist[[var_name]][i]+ 8
  }
  
  ## Creating a list of empty dataframes
  for (i in 1:points[[var_name]]){
    dflist[[var_name]][[i]]<-data.frame()
  }
  ###Storing the data in the empty dataframes
  for (i in 1: length(dflist[[var_name]])){
    dflist[[var_name]][[i]]<- subset(data_frames[[var_name]], DOY>=DOYlist[[var_name]][i] & DOY<DOYlist[[var_name]][i+1])
    dflist[[var_name]][[i]]<-subset(dflist[[var_name]][[i]], PAR_Regression>20)
  }
  
  for (i in 1:points[[var_name]]){
    modellist[[var_name]][[i]]<-data.frame()
  }
  
  stderror[[var_name]]<-rep(0, points[[var_name]])
  rsquared[[var_name]]<-rep(0, points[[var_name]])
  rss[[var_name]]<-rep(0, points[[var_name]])
  rmse[[var_name]]<-rep(0, points[[var_name]])
  
  for (i in (1:points[[var_name]])){
    y<-dflist[[var_name]][[i]]$GPP_modeled
    x<-dflist[[var_name]][[i]]$PAR_Regression
    modellist[[var_name]][[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.0548 ,g= 25))
    LUEmax[[var_name]][[i]]<-modellist[[var_name]][[i]]$m$getPars()[1]
    stderror[[var_name]][i]<-summary(modellist[[var_name]][[i]])$parameters[1,2]
    rss[[var_name]][i]<-sum((modellist[[var_name]][[i]]$m$resid())^2)
    rmse[[var_name]][i]<-sqrt(crossprod(modellist[[var_name]][[i]]$m$resid())/(length(modellist[[var_name]][[i]]$m$resid())))
    rsquared[[var_name]][i] <-  1-(rss[[var_name]][i]/sum((y - mean(y))^2))
  }
  avgsdevsub[[var_name]]<-rep(0,points[[var_name]])
  avgsdevadd[[var_name]]<-rep(0,points[[var_name]])
  for (i in (1:points[[var_name]])){
    avgsdevsub[[var_name]][i]<-(summary(modellist[[var_name]][[i]])$parameters[1,1]) - (summary(modellist[[var_name]][[i]])$parameters[1,2])
    avgsdevadd[[var_name]][i]<- (summary(modellist[[var_name]][[i]])$parameters[1,1]) + (summary(modellist[[var_name]][[i]])$parameters[1,2])
  }
  lightresponsegs[[var_name]]<-light.response(data = data_frames[[var_name]], data_frames[[var_name]]$NEE_modeled, Reco=data_frames[[var_name]]$Reco_modeled,PPFD=data_frames[[var_name]]$PAR_Regression,PPFD_ref=2000)
}

lightresponsegs$USOF1_2017$m$getPars()

plot(LUEmax[[13]])




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