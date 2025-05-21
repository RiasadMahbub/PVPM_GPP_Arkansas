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
library(dplyr)
library(formattable)

# Update the file path
file_path <- "C:/Users/rbmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx"
# Read column names from the first row
myCols <- as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet = 14))
# Read the data skipping the first two rows
USBDA2015 <- read_excel(file_path, skip = 2, sheet = 14, col_names = myCols)
USBDA2015$`Date Time`<-gsub("'","",USBDA2015$`Date Time`) ## The qoutation around the datetime column and fixing the datetime 
USBDA2015$datetime<-strptime(USBDA2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')
USBDA2015$Date<-as.Date(USBDA2015$datetime) ###Get the date column from te dt
USBDA2015$DOY<- yday(USBDA2015$datetime)##ADD the DOY column
MODISdoy<-yday("2015-04-7")
USBDA2015<-dplyr::filter(USBDA2015, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDA2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USBDA2015, mean)
USBDA2015.daily$GPP_modeled<- (USBDA2015.daily$GPP_modeled)* 1e-6*86400*12.011 #Second to daily
USBDA2015.daily$PAR_Regression<- (USBDA2015.daily$PAR_Regression)* 1e-6*86400
###CHANGING THE COLUMN NAMES TO SITE
USBDA2015.daily$GPP_site<-USBDA2015.daily$GPP_modeled
USBDA2015.daily$PAR_site<-USBDA2015.daily$PAR_Regression
USBDA2015.daily$VPD_site<-USBDA2015.daily$VPD_REddyProc
USBDA2015.daily$Tair_site<-USBDA2015.daily$Ta_REddyProc
USBDA2015.daily$doy<-yday(USBDA2015.daily$Date)
USBDA2015.daily$rH_site<-USBDA2015.daily$rH_REddyProc
USBDA2015.daily<-USBDA2015.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USBDA2015.daily$GPP_site<- formattable(USBDA2015.daily$GPP_site, digits = 2, format = "f")
USBDA2015.daily$PAR_site<- formattable(USBDA2015.daily$PAR_site, digits = 2, format = "f")
USBDA2015.daily$Tair_site<- formattable(USBDA2015.daily$Tair_site, digits = 2, format = "f")
USBDA2015.daily$VPD_site<- formattable(USBDA2015.daily$VPD_site, digits = 2, format = "f")
#siteyear column by concatenating df_name and year
USBDA2015.daily$siteyear <- paste(substr(deparse(substitute(USBDA2015.daily)), 1, 9))
USBDA2015.daily$site <- paste(substr(deparse(substitute(USBDA2015.daily)), 1, 5))
USBDA2015.daily$site


myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =15))
USBDA2016<-read_excel(file_path,skip= 2, sheet =15, col_names = myCols)
USBDA2016$`Date Time`<-gsub("'","",USBDA2016$`Date Time`)## The qoutation around the datetime column and fixing the datetime 
USBDA2016$datetime<-strptime(USBDA2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')
USBDA2016$Date<-as.Date(USBDA2016$datetime)###Get the date column from the dt
USBDA2016$DOY<- yday(USBDA2016$datetime)##ADD the DOY column
###MODIS first image on date## For the new data the nearest date of MODIS is 2017_04_07 and 97## MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-03-29")
USBDA2016<-dplyr::filter(USBDA2016, DOY>=MODISdoy)## Scale to daily daily scale
## Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDA2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USBDA2016, mean)
USBDA2016.daily$GPP_modeled<- ( USBDA2016.daily$GPP_modeled)* 1e-6*86400*12.011#Second to daily
USBDA2016.daily$PAR_Regression<- ( USBDA2016.daily$PAR_Regression)* 1e-6*86400
###CHANGING THE COLUMN NAMES TO SITE
USBDA2016.daily$GPP_site<-USBDA2016.daily$GPP_modeled
USBDA2016.daily$PAR_site<-USBDA2016.daily$PAR_Regression
USBDA2016.daily$VPD_site<-USBDA2016.daily$VPD_REddyProc
USBDA2016.daily$Tair_site<-USBDA2016.daily$Ta_REddyProc
USBDA2016.daily$rH_site<-USBDA2016.daily$rH_REddyProc
USBDA2016.daily$doy<-yday(USBDA2016.daily$Date)
USBDA2016.daily<-USBDA2016.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USBDA2016.daily$GPP_site<- formattable(USBDA2016.daily$GPP_site, digits = 2, format = "f")
USBDA2016.daily$PAR_site<- formattable(USBDA2016.daily$PAR_site, digits = 2, format = "f")
USBDA2016.daily$Tair_site<- formattable(USBDA2016.daily$Tair_site, digits = 2, format = "f")
USBDA2016.daily$VPD_site<- formattable(USBDA2016.daily$VPD_site, digits = 2, format = "f")
USBDA2016.daily$rH_site<- formattable(USBDA2016.daily$rH_site, digits = 2, format = "f")
USBDA2016.daily$siteyear <- paste(substr(deparse(substitute(USBDA2016.daily)), 1, 9))
USBDA2016.daily$site <- paste(substr(deparse(substitute(USBDA2016.daily)), 1, 5))

## Read the data
myCols<-as.character(read_excel(file_path,n_max = 1, col_names = FALSE, sheet =16))
USBDC2015<-read_excel(file_path, skip= 2, sheet =16, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USBDC2015$`Date Time`<-gsub("'","",USBDC2015$`Date Time`)
USBDC2015$datetime<-strptime(USBDC2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')
USBDC2015$Date<-as.Date(USBDC2015$datetime)###Get the date column from the dt
USBDC2015$DOY<- yday(USBDC2015$datetime)##ADD the DOY column
###MODIS first image on date## For the new data the nearest date of MODIS is 2017_04_07 and 97## MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-7")
USBDC2015<-dplyr::filter(USBDC2015, DOY>=MODISdoy)
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDC2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USBDC2015, mean)
#Second to daily
USBDC2015.daily$GPP_modeled<- (USBDC2015.daily$GPP_modeled)* 1e-6*86400*12.011
USBDC2015.daily$PAR_Regression<- (USBDC2015.daily$PAR_Regression)* 1e-6*86400
USBDC2015.daily$GPP_site<-USBDC2015.daily$GPP_modeled
USBDC2015.daily$PAR_site<-USBDC2015.daily$PAR_Regression
USBDC2015.daily$VPD_site<-USBDC2015.daily$VPD_REddyProc
USBDC2015.daily$Tair_site<-USBDC2015.daily$Ta_REddyProc
USBDC2015.daily$rH_site<-USBDC2015.daily$rH_REddyProc
USBDC2015.daily$doy<-yday(USBDC2015.daily$Date)
USBDC2015.daily<-USBDC2015.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USBDC2015.daily$GPP_site<- formattable(USBDC2015.daily$GPP_site, digits = 2, format = "f")
USBDC2015.daily$PAR_site<- formattable(USBDC2015.daily$PAR_site, digits = 2, format = "f")
USBDC2015.daily$Tair_site<- formattable(USBDC2015.daily$Tair_site, digits = 2, format = "f")
USBDC2015.daily$VPD_site<- formattable(USBDC2015.daily$VPD_site, digits = 2, format = "f")
USBDC2015.daily$siteyear <- paste(substr(deparse(substitute(USBDC2015.daily)), 1, 9))
USBDC2015.daily$site <- paste(substr(deparse(substitute(USBDC2015.daily)), 1, 5))
USBDC2015.daily




myCols<-as.character(read_excel(file_path,n_max = 1, col_names = FALSE, sheet =17))
USBDC2016<-read_excel(file_path,skip= 2, sheet =17, col_names = myCols)
###Get the date column from the dt
## The qoutation around the datetime column and fixing the datetime 
USBDC2016$`Date Time`<-gsub("'","",USBDC2016$`Date Time`)
USBDC2016$datetime<-strptime(USBDC2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USBDC2016$Date<-as.Date(USBDC2016$datetime)
##ADD the DOY column
USBDC2016$DOY<- yday(USBDC2016$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-03-29")
USBDC2016<-dplyr::filter(USBDC2016, DOY>=MODISdoy)
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDC2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USBDC2016, mean)
#Second to daily
USBDC2016.daily$GPP_modeled<- ( USBDC2016.daily$GPP_modeled)* 1e-6*86400*12.011
USBDC2016.daily$PAR_Regression<- ( USBDC2016.daily$PAR_Regression)* 1e-6*86400
###CHANGING THE COLUMN NAMES TO SITE
USBDC2016.daily$GPP_site<-USBDC2016.daily$GPP_modeled
USBDC2016.daily$PAR_site<-USBDC2016.daily$PAR_Regression
USBDC2016.daily$VPD_site<-USBDC2016.daily$VPD_REddyProc
USBDC2016.daily$Tair_site<-USBDC2016.daily$Ta_REddyProc
USBDC2016.daily$doy<-yday(USBDC2016.daily$Date)
USBDC2016.daily$rH_site<-USBDC2016.daily$rH_REddyProc
USBDC2016.daily<-USBDC2016.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USBDC2016.daily$GPP_site<- formattable(USBDC2016.daily$GPP_site, digits = 2, format = "f")
USBDC2016.daily$PAR_site<- formattable(USBDC2016.daily$PAR_site, digits = 2, format = "f")
USBDC2016.daily$Tair_site<- formattable(USBDC2016.daily$Tair_site, digits = 2, format = "f")
USBDC2016.daily$VPD_site<- formattable(USBDC2016.daily$VPD_site, digits = 2, format = "f")
USBDC2016.daily$siteyear <- paste(substr(deparse(substitute(USBDC2016.daily)), 1, 9))
USBDC2016.daily$site <- paste(substr(deparse(substitute(USBDC2016.daily)), 1, 5))
USBDC2016.daily



myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =5))
USHRA2015<-read_excel(file_path, skip= 2, sheet =5, col_names = myCols)## The qoutation around the datetime column and fixing the datetime 
USHRA2015$`Date Time`<-gsub("'","",USHRA2015$`Date Time`)
USHRA2015$datetime<-strptime(USHRA2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')
USHRA2015$Date<-as.Date(USHRA2015$datetime)###Get the date column from the dt
USHRA2015$DOY<- yday(USHRA2015$datetime)##ADD the DOY column
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-07")
USHRA2015<-dplyr::filter(USHRA2015, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USHRA2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USHRA2015, mean)
#Second to daily
USHRA2015.daily$GPP_modeled<- ( USHRA2015.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA2015.daily$PAR_Regression<- ( USHRA2015.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRA2015.daily$Date,  USHRA2015.daily$GPP_modeled)
###CHANGING THE COLUMN NAMES TO SITE
USHRA2015.daily$GPP_site<-USHRA2015.daily$GPP_modeled
USHRA2015.daily$PAR_site<-USHRA2015.daily$PAR_Regression
USHRA2015.daily$VPD_site<-USHRA2015.daily$VPD_REddyProc
USHRA2015.daily$Tair_site<-USHRA2015.daily$Ta_REddyProc
USHRA2015.daily$rH_site<-USHRA2015.daily$rH_REddyProc
USHRA2015.daily<-USHRA2015.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRA2015.daily$doy<-yday(USHRA2015.daily$Date)
USHRA2015.daily<-USHRA2015.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRA2015.daily
USHRA2015.daily$GPP_site<- formattable(USHRA2015.daily$GPP_site, digits = 2, format = "f")
USHRA2015.daily$PAR_site<- formattable(USHRA2015.daily$PAR_site, digits = 2, format = "f")
USHRA2015.daily$Tair_site<- formattable(USHRA2015.daily$Tair_site, digits = 2, format = "f")
USHRA2015.daily$VPD_site<- formattable(USHRA2015.daily$VPD_site, digits = 2, format = "f")
USHRA2015.daily$siteyear <- paste(substr(deparse(substitute(USHRA2015.daily)), 1, 9))
USHRA2015.daily$site <- paste(substr(deparse(substitute(USHRA2015.daily)), 1, 5))
USHRA2015.daily




myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =6))
USHRA2016<-read_excel(file_path,  skip= 2, sheet =6, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USHRA2016$`Date Time`<-gsub("'","",USHRA2016$`Date Time`)
USHRA2016$datetime<-strptime(USHRA2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USHRA2016$Date<-as.Date(USHRA2016$datetime)
##ADD the DOY column
USHRA2016$DOY<- yday(USHRA2016$datetime)

###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-04-30")
USHRA2016<-dplyr::filter(USHRA2016, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USHRA2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USHRA2016, mean)
#Second to daily
USHRA2016.daily$GPP_modeled<- ( USHRA2016.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA2016.daily$PAR_Regression<- ( USHRA2016.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRA2016.daily$Date,  USHRA2016.daily$GPP_modeled)
USHRA2016.daily$GPP_site<-USHRA2016.daily$GPP_modeled
USHRA2016.daily$PAR_site<-USHRA2016.daily$PAR_Regression
USHRA2016.daily$VPD_site<-USHRA2016.daily$VPD_REddyProc
USHRA2016.daily$Tair_site<-USHRA2016.daily$Ta_REddyProc
USHRA2016.daily$doy<-yday(USHRA2016.daily$Date)
USHRA2016.daily$rH_site<-USHRA2016.daily$rH_REddyProc
USHRA2016.daily<-USHRA2016.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRA2016.daily$siteyear <- paste(substr(deparse(substitute(USHRA2016.daily)), 1, 9))
USHRA2016.daily$site <- paste(substr(deparse(substitute(USHRA2016.daily)), 1, 5))
USHRA2016.daily






myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =7))
USHRA2017<-read_excel(file_path,skip= 2, sheet =7, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USHRA2017$`Date Time`<-gsub("'","",USHRA2017$`Date Time`)
USHRA2017$datetime<-strptime(USHRA2017$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USHRA2017$Date<-as.Date(USHRA2017$datetime)
##ADD the DOY column
USHRA2017$DOY<- yday(USHRA2017$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2017-04-15")
USHRA2017<-dplyr::filter(USHRA2017, DOY>=MODISdoy)
##########################################################
# 8 day
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USHRA2017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USHRA2017, mean)
#Second to daily
USHRA2017.daily$GPP_modeled<- ( USHRA2017.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA2017.daily$PAR_Regression<- ( USHRA2017.daily$PAR_Regression)* 1e-6*86400
USHRA2017.daily$GPP_site<-USHRA2017.daily$GPP_modeled
USHRA2017.daily$PAR_site<-USHRA2017.daily$PAR_Regression
USHRA2017.daily$VPD_site<-USHRA2017.daily$VPD_REddyProc
USHRA2017.daily$Tair_site<-USHRA2017.daily$Ta_REddyProc
USHRA2017.daily$doy<-yday(USHRA2017.daily$Date)
USHRA2017.daily$rH_site<-USHRA2017.daily$rH_REddyProc
USHRA2017.daily<-USHRA2017.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRA2017.daily$GPP_site<- formattable(USHRA2017.daily$GPP_site, digits = 2, format = "f")
USHRA2017.daily$PAR_site<- formattable(USHRA2017.daily$PAR_site, digits = 2, format = "f")
USHRA2017.daily$Tair_site<- formattable(USHRA2017.daily$Tair_site, digits = 2, format = "f")
USHRA2017.daily$VPD_site<- formattable(USHRA2017.daily$VPD_site, digits = 2, format = "f")
USHRA2017.daily$siteyear <- paste(substr(deparse(substitute(USHRA2017.daily)), 1, 9))
USHRA2017.daily$site <- paste(substr(deparse(substitute(USHRA2017.daily)), 1, 5))
USHRA2017.daily




myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =2))
USHRC2015<-read_excel(file_path,skip= 2, sheet =2, col_names = myCols)

## The qoutation around the datetime column and fixing the datetime 
USHRC2015$`Date Time`<-gsub("'","",USHRC2015$`Date Time`)
USHRC2015$datetime<-strptime(USHRC2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USHRC2015$Date<-as.Date(USHRC2015$datetime)
##ADD the DOY column
USHRC2015$DOY<- yday(USHRC2015$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-15")
USHRC2015<-dplyr::filter(USHRC2015, DOY>=MODISdoy)
##########################################################
## 8 day mean
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USHRC2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USHRC2015, mean)
#Second to daily
USHRC2015.daily$GPP_modeled<- ( USHRC2015.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC2015.daily$PAR_Regression<- ( USHRC2015.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRC2015.daily$Date,  USHRC2015.daily$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE
USHRC2015.daily$GPP_site<-USHRC2015.daily$GPP_modeled
USHRC2015.daily$PAR_site<-USHRC2015.daily$PAR_Regression
USHRC2015.daily$VPD_site<-USHRC2015.daily$VPD_REddyProc
USHRC2015.daily$Tair_site<-USHRC2015.daily$Ta_REddyProc
USHRC2015.daily$doy<-yday(USHRC2015.daily$Date)
USHRC2015.daily$rH_site<-USHRC2015.daily$rH_REddyProc
USHRC2015.daily<-USHRC2015.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRC2015.daily
USHRC2015.daily$GPP_site<- formattable(USHRC2015.daily$GPP_site, digits = 2, format = "f")
USHRC2015.daily$PAR_site<- formattable(USHRC2015.daily$PAR_site, digits = 2, format = "f")
USHRC2015.daily$Tair_site<- formattable(USHRC2015.daily$Tair_site, digits = 2, format = "f")
USHRC2015.daily$VPD_site<- formattable(USHRC2015.daily$VPD_site, digits = 2, format = "f")
USHRC2015.daily$siteyear <- paste(substr(deparse(substitute(USHRC2015.daily)), 1, 9))
USHRC2015.daily$site <- paste(substr(deparse(substitute(USHRC2015.daily)), 1, 5))
USHRC2015.daily




myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =3))
USHRC2016<-read_excel(file_path,skip= 2, sheet =3, col_names = myCols)

## The qoutation around the datetime column and fixing the datetime 
USHRC2016$`Date Time`<-gsub("'","",USHRC2016$`Date Time`)
USHRC2016$datetime<-strptime(USHRC2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USHRC2016$Date<-as.Date(USHRC2016$datetime)
##ADD the DOY column
USHRC2016$DOY<- yday(USHRC2016$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-04-30")
USHRC2016<-dplyr::filter(USHRC2016, DOY>=MODISdoy)
##########################################################
## 8 day mean
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USHRC2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USHRC2016, mean)
#Second to daily
USHRC2016.daily$GPP_modeled<- ( USHRC2016.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC2016.daily$PAR_Regression<- ( USHRC2016.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRC2016.daily$PAR_Regression,  USHRC2016.daily$GPP_modeled)
###CHANGING THE COLUMN NAMES TO SITE
USHRC2016.daily$GPP_site<-USHRC2016.daily$GPP_modeled
USHRC2016.daily$PAR_site<-USHRC2016.daily$PAR_Regression
USHRC2016.daily$VPD_site<-USHRC2016.daily$VPD_REddyProc
USHRC2016.daily$Tair_site<-USHRC2016.daily$Ta_REddyProc
USHRC2016.daily$doy<-yday(USHRC2016.daily$Date)
USHRC2016.daily$rH_site<-USHRC2016.daily$rH_REddyProc
USHRC2016.daily<-USHRC2016.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRC2016.daily
USHRC2016.daily$GPP_site<- formattable(USHRC2016.daily$GPP_site, digits = 2, format = "f")
USHRC2016.daily$PAR_site<- formattable(USHRC2016.daily$PAR_site, digits = 2, format = "f")
USHRC2016.daily$Tair_site<- formattable(USHRC2016.daily$Tair_site, digits = 2, format = "f")
USHRC2016.daily$VPD_site<- formattable(USHRC2016.daily$VPD_site, digits = 2, format = "f")
USHRC2016.daily$siteyear <- paste(substr(deparse(substitute(USHRC2016.daily)), 1, 9))
USHRC2016.daily$site <- paste(substr(deparse(substitute(USHRC2016.daily)), 1, 5))
USHRC2016.daily




myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =4))
USHRC2017<-read_excel(file_path, skip= 2, sheet =4, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USHRC2017$`Date Time`<-gsub("'","",USHRC2017$`Date Time`)
USHRC2017$datetime<-strptime(USHRC2017$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USHRC2017$Date<-as.Date(USHRC2017$datetime)
##ADD the DOY column
USHRC2017$DOY<- yday(USHRC2017$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2017-04-23")
USHRC2017<-dplyr::filter(USHRC2017, DOY>=MODISdoy)
## 8 day mean
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USHRC2017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date,  USHRC2017, mean)
#Second to daily
USHRC2017.daily$GPP_modeled<- ( USHRC2017.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC2017.daily$PAR_Regression<- ( USHRC2017.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRC2017.daily$Date,  USHRC2017.daily$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE
USHRC2017.daily$GPP_site<-USHRC2017.daily$GPP_modeled
USHRC2017.daily$PAR_site<-USHRC2017.daily$PAR_Regression
USHRC2017.daily$VPD_site<-USHRC2017.daily$VPD_REddyProc
USHRC2017.daily$Tair_site<-USHRC2017.daily$Ta_REddyProc
USHRC2017.daily$doy<-yday(USHRC2017.daily$Date)
USHRC2017.daily$rH_site<-USHRC2017.daily$rH_REddyProc
USHRC2017.daily<-USHRC2017.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USHRC2017.daily$GPP_site<- formattable(USHRC2017.daily$GPP_site, digits = 2, format = "f")
USHRC2017.daily$PAR_site<- formattable(USHRC2017.daily$PAR_site, digits = 2, format = "f")
USHRC2017.daily$Tair_site<- formattable(USHRC2017.daily$Tair_site, digits = 2, format = "f")
USHRC2017.daily$VPD_site<- formattable(USHRC2017.daily$VPD_site, digits = 2, format = "f")
USHRC2017.daily$siteyear <- paste(substr(deparse(substitute(USHRC2017.daily)), 1, 9))
USHRC2017.daily$site <- paste(substr(deparse(substitute(USHRC2017.daily)), 1, 5))
USHRC2017.daily



# Reading the data removing the unit row
myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =8))
USOF12017<-read_excel(file_path, guess_max = 21474836,skip= 2, sheet =8, col_names = myCols)
USOF12017$`Date Time`<-gsub("'","",USOF12017$`Date Time`)## The qoutation around the datetime column and fixing the datetime 
USOF12017$datetime<-strptime(USOF12017$`Date Time`, format='%d-%b-%Y %H:%M:%S')
USOF12017$Date<-as.Date(USOF12017$datetime)###Get the date column from the dt
USOF12017$DOY<- yday(USOF12017$datetime)##ADD the DOY column
MODISdoy<-yday("2017-04-07")###MODIS first image on date### For the new data the nearest date of MODIS is 2017_04_07 and 97### MODIS image nearest one starts at 137 day May 17 for the original data
USOF12017<-dplyr::filter(USOF12017, DOY>=MODISdoy)
### Scale to daily daily scale
### In order to scale daile GPP and shortwave radiation needs to be multiplied
USOF12017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USOF12017, mean)
#Second to daily
USOF12017.daily$GPP_modeled<- (USOF12017.daily$GPP_modeled)* 1e-6*86400*12.011
USOF12017.daily$PAR_Regression<- (USOF12017.daily$PAR_Regression)* 1e-6*86400
USOF12017.daily$GPP_site<-USOF12017.daily$GPP_modeled
USOF12017.daily$PAR_site<-USOF12017.daily$PAR_Regression
USOF12017.daily$VPD_site<-USOF12017.daily$VPD_REddyProc
USOF12017.daily$Tair_site<-USOF12017.daily$Ta_REddyProc
USOF12017.daily$doy<-yday(USOF12017.daily$Date)
USOF12017.daily$rH_site<-USOF12017.daily$rH_REddyProc
USOF12017.daily<-USOF12017.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USOF12017.daily$GPP_site<- formattable(USOF12017.daily$GPP_site, digits = 2, format = "f")
USOF12017.daily$PAR_site<- formattable(USOF12017.daily$PAR_site, digits = 2, format = "f")
USOF12017.daily$Tair_site<- formattable(USOF12017.daily$Tair_site, digits = 2, format = "f")
USOF12017.daily$VPD_site<- formattable(USOF12017.daily$VPD_site, digits = 2, format = "f")
USOF12017.daily$siteyear <- paste(substr(deparse(substitute(USOF12017.daily)), 1, 9))
USOF12017.daily$site <- paste(substr(deparse(substitute(USOF12017.daily)), 1, 5))
USOF12017.daily



## USOF22017 is in the sheet 9 of RiceData_ResearchGroup03-10-2022
## Read the data
myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =9))
USOF22017<-read_excel(file_path,  skip= 2, sheet =9, col_names = myCols)
USOF22017$`Date Time`<-gsub("'","",USOF22017$`Date Time`)## The qoutation around the datetime column and fixing the datetime 
USOF22017$datetime<-strptime(USOF22017$`Date Time`, format='%d-%b-%Y %H:%M:%S')
USOF22017$Date<-as.Date(USOF22017$datetime)###Get the date column from the dt
USOF22017$DOY<- yday(USOF22017$datetime)##ADD the DOY column
MODISdoy<-97###MODIS first image on date### For the new data the nearest date of MODIS is 2017_04_07 and 97### MODIS image nearest one starts at 137 day May 17 for the original data
USOF22017<-dplyr::filter(USOF22017, DOY>=MODISdoy)
USOF22017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USOF22017, mean)### Scale to daily daily scale### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USOF22017.daily$GPP_modeled<- (USOF22017.daily$GPP_modeled)* 1e-6*86400*12.011 #Second to daily
USOF22017.daily$PAR_Regression<- (USOF22017.daily$PAR_Regression)* 1e-6*86400
plot(USOF22017.daily$Date, USOF22017.daily$GPP_modeled)
USOF22017.daily$GPP_site<-USOF22017.daily$GPP_modeled
USOF22017.daily$PAR_site<-USOF22017.daily$PAR_Regression
USOF22017.daily$VPD_site<-USOF22017.daily$VPD_REddyProc
USOF22017.daily$Tair_site<-USOF22017.daily$Ta_REddyProc
USOF22017.daily$doy<-yday(USOF22017.daily$Date)
USOF22017.daily$rH_site<-USOF22017.daily$rH_REddyProc
USOF22017.daily<-USOF22017.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USOF22017.daily$GPP_site<- formattable(USOF22017.daily$GPP_site, digits = 2, format = "f")
USOF22017.daily$PAR_site<- formattable(USOF22017.daily$PAR_site, digits = 2, format = "f")
USOF22017.daily$Tair_site<- formattable(USOF22017.daily$Tair_site, digits = 2, format = "f")
USOF22017.daily$VPD_site<- formattable(USOF22017.daily$VPD_site, digits = 2, format = "f")
USOF22017.daily$siteyear <- paste(substr(deparse(substitute(USOF22017.daily)), 1, 9))
USOF22017.daily$site <- paste(substr(deparse(substitute(USOF22017.daily)), 1, 5))
USOF22017.daily

## Read the data
### USOF32017 is in the sheet 10
myCols<-as.character(read_excel(file_path,n_max = 1, col_names = FALSE, sheet =10))
USOF32017<-read_excel(file_path, skip= 2, sheet =10, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USOF32017$`Date Time`<-gsub("'","",USOF32017$`Date Time`)
USOF32017$datetime<-strptime(USOF32017$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USOF32017$Date<-as.Date(USOF32017$datetime)
##ADD the DOY column
USOF32017$DOY<- yday(USOF32017$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-97
USOF32017<-dplyr::filter(USOF32017, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
## Daily mean
USOF32017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USOF32017, mean)
#Second to daily
USOF32017.daily$GPP_modeled<- (USOF32017.daily$GPP_modeled)* 1e-6*86400*12.011
USOF32017.daily$PAR_Regression<- (USOF32017.daily$PAR_Regression)* 1e-6*86400
###CHANGING THE COLUMNN NNAMES TO SITE
USOF32017.daily$GPP_site<-USOF32017.daily$GPP_modeled
USOF32017.daily$PAR_site<-USOF32017.daily$PAR_Regression
USOF32017.daily$VPD_site<-USOF32017.daily$VPD_REddyProc
USOF32017.daily$Tair_site<-USOF32017.daily$Ta_REddyProc
###CHANGING THE COLUMNN NNAMES TO SITE
USOF32017.daily$doy<-yday(USOF32017.daily$Date)
USOF32017.daily$rH_site<-USOF32017.daily$rH_REddyProc
USOF32017.daily<-USOF32017.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USOF32017.daily$GPP_site<- formattable(USOF32017.daily$GPP_site, digits = 2, format = "f")
USOF32017.daily$PAR_site<- formattable(USOF32017.daily$PAR_site, digits = 2, format = "f")
USOF32017.daily$Tair_site<- formattable(USOF32017.daily$Tair_site, digits = 2, format = "f")
USOF32017.daily$VPD_site<- formattable(USOF32017.daily$VPD_site, digits = 2, format = "f")
USOF32017.daily$siteyear <- paste(substr(deparse(substitute(USOF32017.daily)), 1, 9))
USOF32017.daily$site <- paste(substr(deparse(substitute(USOF32017.daily)), 1, 5))
USOF32017.daily






## Read the data
myCols<-as.character(read_excel(file_path,n_max = 1, col_names = FALSE, sheet =11))
USOF42018<-read_excel(file_path,skip= 2, sheet =11, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USOF42018$`Date Time`<-gsub("'","",USOF42018$`Date Time`)
USOF42018$datetime<-strptime(USOF42018$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USOF42018$Date<-as.Date(USOF42018$datetime)
##ADD the DOY column
USOF42018$DOY<- yday(USOF42018$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2018-04-15")
USOF42018<-dplyr::filter(USOF42018, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
## Daily mean
USOF42018.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USOF42018, mean)
#Second to daily
USOF42018.daily$GPP_modeled<- (USOF42018.daily$GPP_modeled)* 1e-6*86400*12.011
USOF42018.daily$PAR_Regression<- (USOF42018.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF42018.daily$Date, USOF42018.daily$GPP_modeled)
USOF42018.daily$GPP_site<-USOF42018.daily$GPP_modeled
USOF42018.daily$PAR_site<-USOF42018.daily$PAR_Regression
USOF42018.daily$VPD_site<-USOF42018.daily$VPD_REddyProc
USOF42018.daily$Tair_site<-USOF42018.daily$Ta_REddyProc

###CHANGING THE COLUMNN NNAMES TO SITE
USOF42018.daily$doy<-yday(USOF42018.daily$Date)
USOF42018.daily$rH_site<-USOF42018.daily$rH_REddyProc
USOF42018.daily<-USOF42018.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USOF42018.daily
USOF42018.daily$GPP_site<- formattable(USOF42018.daily$GPP_site, digits = 2, format = "f")
USOF42018.daily$PAR_site<- formattable(USOF42018.daily$PAR_site, digits = 2, format = "f")
USOF42018.daily$Tair_site<- formattable(USOF42018.daily$Tair_site, digits = 2, format = "f")
USOF42018.daily$VPD_site<- formattable(USOF42018.daily$VPD_site, digits = 2, format = "f")
USOF42018.daily$siteyear <- paste(substr(deparse(substitute(USOF42018.daily)), 1, 9))
USOF42018.daily$site <- paste(substr(deparse(substitute(USOF42018.daily)), 1, 5))
USOF42018.daily




myCols<-as.character(read_excel(file_path,n_max = 1, col_names = FALSE, sheet =12))
USOF52018<-read_excel(file_path, skip= 2, sheet =12, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USOF52018$`Date Time`<-gsub("'","",USOF52018$`Date Time`)
USOF52018$datetime<-strptime(USOF52018$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USOF52018$Date<-as.Date(USOF52018$datetime)
##ADD the DOY column
USOF52018$DOY<- yday(USOF52018$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2018-04-15")
USOF52018<-dplyr::filter(USOF52018, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
## Daily mean
USOF52018.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USOF52018, mean)
#Second to daily
USOF52018.daily$GPP_modeled<- (USOF52018.daily$GPP_modeled)* 1e-6*86400*12.011
USOF52018.daily$PAR_Regression<- (USOF52018.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF52018.daily$Date, USOF52018.daily$GPP_modeled)
###CHANGING THE COLUMNN NNAMES TO SITE
USOF52018.daily$GPP_site<-USOF52018.daily$GPP_modeled
USOF52018.daily$PAR_site<-USOF52018.daily$PAR_Regression
USOF52018.daily$VPD_site<-USOF52018.daily$VPD_REddyProc
USOF52018.daily$Tair_site<-USOF52018.daily$Ta_REddyProc
###CHANGING
USOF52018.daily$doy<-yday(USOF52018.daily$Date)
USOF52018.daily$rH_site<-USOF52018.daily$rH_REddyProc
USOF52018.daily<-USOF52018.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USOF52018.daily
USOF52018.daily$GPP_site<- formattable(USOF52018.daily$GPP_site, digits = 2, format = "f")
USOF52018.daily$PAR_site<- formattable(USOF52018.daily$PAR_site, digits = 2, format = "f")
USOF52018.daily$Tair_site<- formattable(USOF52018.daily$Tair_site, digits = 2, format = "f")
USOF52018.daily$VPD_site<- formattable(USOF52018.daily$VPD_site, digits = 2, format = "f")
USOF52018.daily$siteyear <- paste(substr(deparse(substitute(USOF52018.daily)), 1, 9))
USOF52018.daily$site <- paste(substr(deparse(substitute(USOF52018.daily)), 1, 5))
USOF52018.daily



## Read the data
myCols<-as.character(read_excel(file_path, 
                                n_max = 1, col_names = FALSE, sheet =13))
USOF62018<-read_excel(file_path, 
                      skip= 2, sheet =13, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USOF62018$`Date Time`<-gsub("'","",USOF62018$`Date Time`)
USOF62018$datetime<-strptime(USOF62018$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USOF62018$Date<-as.Date(USOF62018$datetime)
##ADD th DOY column
USOF62018$DOY<- yday(USOF62018$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2018-04-15")
USOF62018<-dplyr::filter(USOF62018, DOY>=MODISdoy)
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USOF62018.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc, rH_REddyProc) ~ Date, USOF62018, mean)
#Second to daily
USOF62018.daily$GPP_modeled<- (USOF62018.daily$GPP_modeled)* 1e-6*86400*12.011
USOF62018.daily$PAR_Regression<- (USOF62018.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF62018.daily$Date, USOF62018.daily$GPP_modeled)
USOF62018.daily$GPP_site<-USOF62018.daily$GPP_modeled
USOF62018.daily$PAR_site<-USOF62018.daily$PAR_Regression
USOF62018.daily$VPD_site<-USOF62018.daily$VPD_REddyProc
USOF62018.daily$Tair_site<-USOF62018.daily$Ta_REddyProc
###CHANGING THE COLUMNN NNAMES TO SITE
USOF62018.daily$doy<-yday(USOF62018.daily$Date)
USOF62018.daily$rH_site<-USOF62018.daily$rH_REddyProc
USOF62018.daily<-USOF62018.daily%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, rH_site)
USOF62018.daily$GPP_site<- formattable(USOF62018.daily$GPP_site, digits = 2, format = "f")
USOF62018.daily$PAR_site<- formattable(USOF62018.daily$PAR_site, digits = 2, format = "f")
USOF62018.daily$Tair_site<- formattable(USOF62018.daily$Tair_site, digits = 2, format = "f")
USOF62018.daily$VPD_site<- formattable(USOF62018.daily$VPD_site, digits = 2, format = "f")
USOF62018.daily$siteyear <- paste(substr(deparse(substitute(USOF62018.daily)), 1, 9))
USOF62018.daily$site <- paste(substr(deparse(substitute(USOF62018.daily)), 1, 5))
USOF62018.daily

sitecombineddata <-rbind(USOF62018.daily, USOF52018.daily, USOF42018.daily, USOF32017.daily, USOF22017.daily, USOF12017.daily,
                         USBDA2015.daily, USBDA2016.daily, USBDC2015.daily, USBDC2016.daily, USHRA2017.daily, USHRC2017.daily,
                         USHRA2015.daily, USHRA2016.daily, USHRC2015.daily, USHRC2016.daily)


# calculate_DOP_DAP <- function(df) {
#   DOP_values <- c("USBDA2015" = 92, "USBDA2016" = 82, "USBDC2015" = 92, "USBDC2016" = 82,
#                   "USHRA2015" = 97, "USHRA2016" = 114, "USHRA2017" = 99, "USHRC2015" = 98,
#                   "USHRC2016" = 114, "USHRC2017" = 100, "USOF12017" = 91, "USOF22017" = 91,
#                   "USOF32017" = 91, "USOF42018" = 99, "USOF52018" = 99, "USOF62018" = 99)
#   
#   df$DOP <- DOP_values[df$siteyear]
#   return(df)
# }

calculate_DOP_DAP <- function(df) {
  # Define DOP (Day of Planting) values
  DOP_values <- c("USBDA2015" = 92, "USBDA2016" = 82, "USBDC2015" = 92, "USBDC2016" = 82,
                  "USHRA2015" = 97, "USHRA2016" = 114, "USHRA2017" = 99, "USHRC2015" = 98,
                  "USHRC2016" = 114, "USHRC2017" = 100, "USOF12017" = 91, "USOF22017" = 91,
                  "USOF32017" = 91, "USOF42018" = 99, "USOF52018" = 99, "USOF62018" = 99)
  
  # Define Variety values
  Variety_values <- c("USBDA2015" = "XL745", "USBDA2016" = "XL745",
                      "USBDC2015" = "XL745", "USBDC2016" = "XL745",
                      "USHRA2015" = "XL745", "USHRA2016" = "XL745", "USHRA2017" = "XL745",
                      "USHRC2015" = "XL745", "USHRC2016" = "XL745", "USHRC2017" = "XL745",
                      "USOF12017" = "XL753", "USOF22017" = "XL753", "USOF32017" = "XL753",
                      "USOF42018" = "XL753", "USOF52018" = "XL753", "USOF62018" = "XL753")
  
  # Assign DOP and Variety columns
  df$DOP <- DOP_values[df$siteyear]
  df$Variety <- Variety_values[df$siteyear]
  
  return(df)
}

# Apply function to sitecombineddata
sitecombineddata$DOY<-yday(sitecombineddata$Date)
sitecombineddata <- calculate_DOP_DAP(sitecombineddata)
sitecombineddata$DAP <-sitecombineddata$DOY - sitecombineddata$DOP
sitecombineddata$siteyeardate<-paste0(sitecombineddata$site, "_", sitecombineddata$Date)
sitecombineddata$siteyeardate



########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################
########################################


# List of dataframes
df_list <- list(USOF62018.daily, USOF52018.daily, USOF42018.daily, USOF32017.daily, USOF22017.daily, USOF12017.daily,
                USBDA2015.daily, USBDA2016.daily, USBDC2015.daily, USBDC2016.daily, USHRA2017.daily, USHRC2017.daily,
                USHRA2015.daily, USHRA2016.daily, USHRC2015.daily, USHRC2016.daily)
# Loop through each dataframe and print its column names
for (i in seq_along(df_list)) {
  cat("\nColumn names for", df_names[i], ":\n")
  print(colnames(df_list[[i]]))
}
# List of dataframes
df_list <- list(USOF62018.daily, USOF52018.daily, USOF42018.daily, USOF32017.daily, USOF22017.daily, USOF12017.daily,
                USBDA2015.daily, USBDA2016.daily, USBDC2015.daily, USBDC2016.daily, USHRA2017.daily, USHRC2017.daily,
                USHRA2015.daily, USHRA2016.daily, USHRC2015.daily, USHRC2016.daily)

df_names <- c("USOF62018.daily", "USOF52018.daily", "USOF42018.daily", "USOF32017.daily", "USOF22017.daily", "USOF12017.daily",
              "USBDA2015.daily", "USBDA2016.daily", "USBDC2015.daily", "USBDC2016.daily", "USHRA2017.daily", "USHRC2017.daily",
              "USHRA2015.daily", "USHRA2016.daily", "USHRC2015.daily", "USHRC2016.daily")

# Get number of columns for each dataframe
num_cols <- sapply(df_list, ncol)

# Create a dataframe for easy viewing
df_info <- data.frame(DataFrame = df_names, Num_Columns = num_cols)

# Print the dataframe with column counts
print(df_info)
# Get all column names for each dataframe
colnames_list <- lapply(df_list, colnames)

# Find the unique set of column names across all dataframes
all_columns <- unique(unlist(colnames_list))

# Identify extra columns in USHRA2015.daily
extra_cols_ushra2015 <- setdiff(colnames_list[[13]], Reduce(intersect, colnames_list[-13]))

# Print the extra column(s)
cat("Extra column(s) in USHRA2015.daily:", paste(extra_cols_ushra2015, collapse=", "), "\n")


sitecombineddata

####Satellite data loading 
# Set the directory path
directory <- "C:/Users/rbmahbub/Box/Research/Data/VPM/SatelliteDataForSiteAnalysis"
# List all files in the directory
files <- list.files(directory)
# Filter only CSV files
csv_files <- files[grepl(".csv$", files)]
# Initialize an empty list to store data frames
data_list <- list()
# Read each CSV file and store in the list
for (file in csv_files) {
  file_path <- file.path(directory, file)
  # Read CSV file
  data <- read.csv(file_path)
  # Extract siteyear from filename
  siteyear <- sub(".csv$", "", file)
  # Add siteyear column
  data$siteyear <- siteyear
  # Add dataframe to the list
  data_list[[file]] <- data
}

# Combine all data frames into a single data frame
satellitecombined_data <- do.call(rbind, data_list)
# Reset row names to sequential integers starting from 1
rownames(satellitecombined_data) <- NULL

#satellitecombined_data dataframe has the information of satellite information
satellitecombined_data

# Concatenate siteyear and date to create siteyeardate column
sitecombineddata$siteyeardate <- paste(sitecombineddata$siteyear, sitecombineddata$Date, sep = "")
satellitecombined_data$siteyeardate <- paste(satellitecombined_data$siteyear, satellitecombined_data$date, sep = "")

# Assuming you have the dataframes already loaded with appropriate names, e.g., sitecombineddata, combined_data, sitesatellitemergedDATA
calculate_DOP_DAP <- function(df) {
  df$DOP <- ifelse(df$siteyear == "USBDA2015", 92,
                   ifelse(df$siteyear == "USBDA2016", 82,
                          ifelse(df$siteyear == "USBDC2015", 92,
                                 ifelse(df$siteyear == "USBDC2016", 82,
                                        ifelse(df$siteyear == "USHRA2015", 97,
                                               ifelse(df$siteyear == "USHRA2016", 114,
                                                      ifelse(df$siteyear == "USHRA2017", 99,
                                                             ifelse(df$siteyear == "USHRC2015", 98,
                                                                    ifelse(df$siteyear == "USHRC2016", 114,
                                                                           ifelse(df$siteyear == "USHRC2017", 100,
                                                                                  ifelse(df$siteyear == "USOF12017", 91,
                                                                                         ifelse(df$siteyear == "USOF22017", 91,
                                                                                                ifelse(df$siteyear == "USOF32017", 91,
                                                                                                       ifelse(df$siteyear == "USOF42018", 99,
                                                                                                              ifelse(df$siteyear == "USOF52018", 99,
                                                                                                                     ifelse(df$siteyear == "USOF62018", 99, NA))))))))))))))))
  return(df)
}

# Apply function to sitecombineddata
sitecombineddata <- calculate_DOP_DAP(sitecombineddata)
sitecombineddata$DAP <-sitecombineddata$doy - sitecombineddata$DOP
# Merge dataframes based on the siteyeardate column

nrow(sitecombineddata)

# Define the function to calculate Ts_site
calculate_Ts <- function(T, Tmin, Tmax, Topt) {
  Ts <- ((T - Tmin) * (T - Tmax)) / (((T - Tmin) * (T - Tmax)) - (T - Topt)^2)
  return(Ts)
}

# Example values for Tmin, Tmax, and Topt
Tmin <- -1 
Tmax <- 48  
Topt <- 28  
# Extract Tair_site from the dataframe
Tair_site <- sitesatellitemergedDATA$Tair_site
# Calculate Ts_site using Tair_site values
Ts_site <- calculate_Ts(Tair_site, Tmin, Tmax, Topt)
# Add Ts_site as a new column to the dataframe
sitesatellitemergedDATA <- cbind(sitesatellitemergedDATA, Ts_site = Ts_site)
# Dfine the function to calculate GPP
calculate_GPP <- function(Ts, LUEmax, Ws, PAR, FPAR) {
  GPP <- Ts * Ws * PAR * FPAR * LUEmax*12.011 
  return(GPP)
}
# Extract relevant columns from sitesatellitemergedDATA
Ts <- Ts_site  # Ts_site calculated previously
LUEmax <- 0.06  # Given value for LUEmax
Ws <- sitesatellitemergedDATA$Ws
PAR <- sitesatellitemergedDATA$PAR_site
FPAR <- sitesatellitemergedDATA$FAPAR_sg

# Calculate GPP
GPP <- calculate_GPP(Ts, LUEmax, Ws, PAR, FPAR)

# Add GPP as a new column to the dataframe
sitesatellitemergedDATA <- cbind(sitesatellitemergedDATA, GPPvpmsite = GPP)
# Extract site information and year from siteyear.x column
sitesatellitemergedDATA$site <- substr(sitesatellitemergedDATA$siteyear.x, 1, 5)
sitesatellitemergedDATA$year <- as.integer(substr(sitesatellitemergedDATA$siteyear.x, 6, 9))

sitesatellitemergedDATA$GPPvpmsite

max(sitesatellitemergedDATA$GPP_site)
max(sitesatellitemergedDATA$GPPvpmsite)
max(sitesatellitemergedDATA$gpp)


library(dplyr)

# Assuming your data frame is named 'sitesatellitemergedDATA'

# Sum of GPP_site
sum_gpp_site <- sitesatellitemergedDATA %>%
  group_by(siteyear.x) %>%
  dplyr::summarize(sum_gpp_site = sum(GPP_site, na.rm = TRUE))

# Sum of GPPvpmsite
sum_gpp_vpm_site <- sitesatellitemergedDATA %>% 
  group_by(siteyear.x) %>%
  dplyr::summarize(sum_gpp_vpm_site = sum(GPPvpmsite, na.rm = TRUE))

# Sum of gpp
sum_gpp <- sitesatellitemergedDATA %>% 
  group_by(siteyear.x) %>%
  dplyr::summarize(sum_gpp = sum(gpp, na.rm = TRUE))

# Print the sums
print(sum_gpp_site)
print(sum_gpp_vpm_site)
print(sum_gpp)



### calculate the standard deviation of the models
# Calculate standard deviations
standard_deviations <- c(
  standard_deviation_of_siteEC = sd(sitesatellitemergedDATA$GPP_site),
  GPPVPMsatellite = sd(sitesatellitemergedDATA$gpp),
  GPPVPM_site = sd(sitesatellitemergedDATA$GPPvpmsite)
)

# Output the standard deviations
standard_deviations

require(plotrix)
# Load the plotrix package
library(plotrix)

# Define observed values
y <- sitesatellitemergedDATA$GPP_site

# Define predicted values from the first model
yhat.1 <- sitesatellitemergedDATA$gpp

# Define predicted values from the second model
yhat.2 <- sitesatellitemergedDATA$GPPvpmsite

# Create Taylor diagram for the first model
taylor.diagram(y, yhat.1, col = "red", 
               pch = 1, pcex = 2,
               ngamma = 8,
               gamma.col = "green",
               sd.arcs = TRUE,
               ref.sd = TRUE)

# Add Taylor diagram for the second model
taylor.diagram(y, yhat.2, col = "green", add = TRUE)

# Calculate mean and standard deviation
mean_Ts_site <- mean(sitesatellitemergedDATA$Ts_site, na.rm = TRUE)
sd_Ts_site <- sd(sitesatellitemergedDATA$Ts_site, na.rm = TRUE)
max_Ts_site <- max(sitesatellitemergedDATA$Ts_site, na.rm = TRUE)
min_Ts_site <- min(sitesatellitemergedDATA$Ts_site, na.rm = TRUE)

mean_Ws <- mean(sitesatellitemergedDATA$Ws, na.rm = TRUE)
sd_Ws <- sd(sitesatellitemergedDATA$Ws, na.rm = TRUE)
max_Ws <- max(sitesatellitemergedDATA$Ws, na.rm = TRUE)
min_Ws <- min(sitesatellitemergedDATA$Ws, na.rm = TRUE)

# Print the mean and standard deviation
cat("Mean of Ts_site: ", mean_Ts_site, "\n")
cat("Standard deviation of Ts_site: ", sd_Ts_site, "\n")
cat("Max value of Ts_site: ", max_Ts_site, "\n")
cat("Min value of Ts_site: ", min_Ts_site, "\n")

cat("Mean of Ws: ", mean_Ws, "\n")
cat("Standard deviation of Ws: ", sd_Ws, "\n")
cat("Max value of Ws: ", max_Ws, "\n")
cat("Min value of Ws: ", min_Ws, "\n")


library(dplyr)

# Group by year and site, then calculate mean and standard deviation
grouped_data_site <- sitesatellitemergedDATA %>%
  group_by(site) %>%
  summarize(mean_Ts_site = mean(Ts_site, na.rm = TRUE),
            sd_Ts_site = sd(Ts_site, na.rm = TRUE),
            mean_Ws = mean(Ws, na.rm = TRUE),
            sd_Ws = sd(Ws, na.rm = TRUE))

# Print the grouped data
print(grouped_data_site)


# Group by year and site, then calculate mean and standard deviation
grouped_data_year <- sitesatellitemergedDATA %>%
  group_by(year) %>%
  summarize(mean_Ts_site = mean(Ts_site, na.rm = TRUE),
            sd_Ts_site = sd(Ts_site, na.rm = TRUE),
            mean_Ws = mean(Ws, na.rm = TRUE),
            sd_Ws = sd(Ws, na.rm = TRUE))

# Print the grouped data
print(grouped_data_year)



# Concatenate each row with site, mean_Ts_site, sd_Ts_site, mean_Ws, sd_Ws
concatenated_data <- grouped_data_site %>%
  mutate(concatenated_row = paste(site, "Ts_mean:", mean_Ts_site, "Ts_sd:", sd_Ts_site, "Ws_mean:", mean_Ws, "Ws_sd:", sd_Ws))

# Print the concatenated data
print(concatenated_data$concatenated_row)


### Calculation of NG GPP 
sitesatellitemergedDATAGPPng
# Create a new column 'season' in sitesatellitemergedDATA where the value is "growing" if 'DAP' has a value and "non_growing" if 'DAP' is NA
sitesatellitemergedDATAGPPng$season <- ifelse(is.na(sitesatellitemergedDATAGPPng$DAP), "non_growing", "growing")

library(dplyr)

# Group by 'siteyear.y' and summarize 'GPP' for each 'season'
gpp_summary <- sitesatellitemergedDATAGPPng %>%
  group_by(siteyear.y) %>%  # Group data by the 'siteyear.y' column
  summarize(
    gpp_growing = sum(gpp[season == "growing"]),  # Calculate the sum of 'gpp' where the season is "growing"
    gpp_non_growing = sum(gpp[season == "non_growing"]),  # Calculate the sum of 'gpp' where the season is "non_growing"
    gpp_total = gpp_growing + gpp_non_growing,  # Calculate the total 'gpp' by adding 'gpp_growing' and 'gpp_non_growing'
    gpp_growing_ratio = (gpp_growing / (gpp_growing + gpp_non_growing)) * 100  # Calculate the ratio of 'gpp_growing' to 'gpp_total' in percentage
  )

# View the summary
print(gpp_summary)


# Calculate max, mean, and min of the 'gpp_growing_ratio' column
max_gpp_growing_ratio <- max(gpp_summary$gpp_growing_ratio)  # Maximum value of 'gpp_growing_ratio'
mean_gpp_growing_ratio <- mean(gpp_summary$gpp_growing_ratio)  # Mean value of 'gpp_growing_ratio'
min_gpp_growing_ratio <- min(gpp_summary$gpp_growing_ratio)  # Minimum value of 'gpp_growing_ratio'

# Find the siteyear.x corresponding to max, mean, and min 'gpp_growing_ratio'
siteyear_max <- gpp_summary$siteyear.y[which.max(gpp_summary$gpp_growing_ratio)]  # Siteyear with max 'gpp_growing_ratio'
siteyear_min <- gpp_summary$siteyear.y[which.min(gpp_summary$gpp_growing_ratio)]  # Siteyear with min 'gpp_growing_ratio']

# Find the siteyear closest to the mean value
mean_diff <- abs(gpp_summary$gpp_growing_ratio - mean_gpp_growing_ratio)
siteyear_mean <- gpp_summary$siteyear.y[which.min(mean_diff)]  # Siteyear closest to mean 'gpp_growing_ratio'

# Print the results
cat("Siteyear with Max GPP Growing Ratio:", siteyear_max, "with ratio", max_gpp_growing_ratio, "\n")
cat("Siteyear with Mean GPP Growing Ratio:", siteyear_mean, "with ratio", mean_gpp_growing_ratio, "\n")
cat("Siteyear with Min GPP Growing Ratio:", siteyear_min, "with ratio", min_gpp_growing_ratio, "\n")

#####Plot the daily GPP graph########

#####Plot the daily Temperature graph####



############################################
library(readxl)

# Define the directory
lai_2015 <- readxl::read_excel("C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SiteLAICHdata/Daily LAI.xlsx", sheet = 1)
lai_2016 <- readxl::read_excel("C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SiteLAICHdata/Daily LAI.xlsx", sheet = 2)
lai_2017 <- readxl::read_excel("C:/Users/rbmahbub/Documents/RProjects/GapfillingOtherRiceSites/Data/SiteLAICHdata/Daily LAI.xlsx", sheet = 3)

########fix one of the LAI unit
library(dplyr)

# Join data for USHRA2015, USHRA2016, and USHRA2017 based on the Date column
USHRA2015.daily <- left_join(USHRA2015.daily, lai_2015 %>% select(Date, `Way 4`), by = "Date") %>%
  rename(LAIsite = `Way 4`)
USHRA2016.daily <- left_join(USHRA2016.daily, lai_2016 %>% select(Date, `Way 4`), by = "Date") %>%
  rename(LAIsite = `Way 4`)
USHRA2017.daily <- left_join(USHRA2017.daily, lai_2017 %>% select(Date, `Way 4`), by = "Date") %>%
  rename(LAIsite = `Way 4`)
# Join data for USHRC2015, USHRC2016, and USHRC2017 based on the Date column
USHRC2015.daily <- left_join(USHRC2015.daily, lai_2015 %>% select(Date, `Way 3`), by = "Date") %>%
  rename(LAIsite = `Way 3`)
USHRC2016.daily <- left_join(USHRC2016.daily, lai_2016 %>% select(Date, `Way 3`), by = "Date") %>%
  rename(LAIsite = `Way 3`)
USHRC2017.daily <- left_join(USHRC2017.daily, lai_2017 %>% select(Date, `Way 3`), by = "Date") %>%
  rename(LAIsite = `Way 3`)

# Adding LAIsite column with NA for the other 10 sites
USOF62018.daily$LAIsite <- NA
USOF52018.daily$LAIsite <- NA
USOF42018.daily$LAIsite <- NA
USOF32017.daily$LAIsite <- NA
USOF22017.daily$LAIsite <- NA
USOF12017.daily$LAIsite <- NA
USBDA2015.daily$LAIsite <- NA
USBDA2016.daily$LAIsite <- NA
USBDC2015.daily$LAIsite <- NA
USBDC2016.daily$LAIsite <- NA
USBDC2016.daily
