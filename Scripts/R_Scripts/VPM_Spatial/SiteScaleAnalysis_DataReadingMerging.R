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
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDA2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USBDA2015, mean)
USBDA2015.daily$GPP_modeled<- (USBDA2015.daily$GPP_modeled)* 1e-6*86400*12.011 #Second to daily
USBDA2015.daily$PAR_Regression<- (USBDA2015.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
USBDA2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USBDA2015.daily, mean)
USBDA2015.8day$Date<-USBDA2015.8day$`cut(Date, "8 days")`
###CHANGING THE COLUMN NAMES TO SITE
USBDA2015.8day$Date<-USBDA2015.8day$`cut(Date, "8 days")`
USBDA2015.8day$GPP_site<-USBDA2015.8day$GPP_modeled
USBDA2015.8day$PAR_site<-USBDA2015.8day$PAR_Regression
USBDA2015.8day$VPD_site<-USBDA2015.8day$VPD_REddyProc
USBDA2015.8day$Tair_site<-USBDA2015.8day$Ta_REddyProc
USBDA2015.8day<-USBDA2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USBDA2015.8day$doy<-yday(USBDA2015.8day$Date)
USBDA2015.8day<-USBDA2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDA2015.8day$GPP_site<- formattable(USBDA2015.8day$GPP_site, digits = 2, format = "f")
USBDA2015.8day$PAR_site<- formattable(USBDA2015.8day$PAR_site, digits = 2, format = "f")
USBDA2015.8day$Tair_site<- formattable(USBDA2015.8day$Tair_site, digits = 2, format = "f")
USBDA2015.8day$VPD_site<- formattable(USBDA2015.8day$VPD_site, digits = 2, format = "f")
#siteyear column by concatenating df_name and year
USBDA2015.8day$siteyear <- paste(substr(deparse(substitute(USBDA2015.8day)), 1, 9))
# View the modified dataframe
USBDA2015.8day


myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =15))
USBDA2016<-read_excel(file_path,skip= 2, sheet =15, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USBDA2016$`Date Time`<-gsub("'","",USBDA2016$`Date Time`)
USBDA2016$datetime<-strptime(USBDA2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USBDA2016$Date<-as.Date(USBDA2016$datetime)
##ADD the DOY column
USBDA2016$DOY<- yday(USBDA2016$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-03-29")
USBDA2016<-dplyr::filter(USBDA2016, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDA2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USBDA2016, mean)
#Second to daily
USBDA2016.daily$GPP_modeled<- ( USBDA2016.daily$GPP_modeled)* 1e-6*86400*12.011
USBDA2016.daily$PAR_Regression<- ( USBDA2016.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
USBDA2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USBDA2016.daily, mean)

###CHANGING THE COLUMN NAMES TO SITE
USBDA2016.8day$Date<-USBDA2016.8day$`cut(Date, "8 days")`
USBDA2016.8day$GPP_site<-USBDA2016.8day$GPP_modeled
USBDA2016.8day$PAR_site<-USBDA2016.8day$PAR_Regression
USBDA2016.8day$VPD_site<-USBDA2016.8day$VPD_REddyProc
USBDA2016.8day$Tair_site<-USBDA2016.8day$Ta_REddyProc
USBDA2016.8day<-USBDA2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USBDA2016.8day$doy<-yday(USBDA2016.8day$Date)
USBDA2016.8day<-USBDA2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDA2016.8day$GPP_site<- formattable(USBDA2016.8day$GPP_site, digits = 2, format = "f")
USBDA2016.8day$PAR_site<- formattable(USBDA2016.8day$PAR_site, digits = 2, format = "f")
USBDA2016.8day$Tair_site<- formattable(USBDA2016.8day$Tair_site, digits = 2, format = "f")
USBDA2016.8day$VPD_site<- formattable(USBDA2016.8day$VPD_site, digits = 2, format = "f")
USBDA2016.8day$siteyear <- paste(substr(deparse(substitute(USBDA2016.8day)), 1, 9))




## Read the data
myCols<-as.character(read_excel(file_path,n_max = 1, col_names = FALSE, sheet =16))
USBDC2015<-read_excel(file_path, skip= 2, sheet =16, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USBDC2015$`Date Time`<-gsub("'","",USBDC2015$`Date Time`)
USBDC2015$datetime<-strptime(USBDC2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USBDC2015$Date<-as.Date(USBDC2015$datetime)
##ADD the DOY column
USBDC2015$DOY<- yday(USBDC2015$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-7")
USBDC2015<-dplyr::filter(USBDC2015, DOY>=MODISdoy)
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USBDC2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USBDC2015, mean)
#Second to daily
USBDC2015.daily$GPP_modeled<- (USBDC2015.daily$GPP_modeled)* 1e-6*86400*12.011
USBDC2015.daily$PAR_Regression<- (USBDC2015.daily$PAR_Regression)* 1e-6*86400
USBDC2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USBDC2015.daily, mean)
###CHANGING THE COLUMN NAMES TO SITE
USBDC2015.8day$Date<-USBDC2015.8day$`cut(Date, "8 days")`
USBDC2015.8day$GPP_site<-USBDC2015.8day$GPP_modeled
USBDC2015.8day$PAR_site<-USBDC2015.8day$PAR_Regression
USBDC2015.8day$VPD_site<-USBDC2015.8day$VPD_REddyProc
USBDC2015.8day$Tair_site<-USBDC2015.8day$Ta_REddyProc
USBDC2015.8day<-USBDC2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USBDC2015.8day$doy<-yday(USBDC2015.8day$Date)
USBDC2015.8day<-USBDC2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDC2015.8day$GPP_site<- formattable(USBDC2015.8day$GPP_site, digits = 2, format = "f")
USBDC2015.8day$PAR_site<- formattable(USBDC2015.8day$PAR_site, digits = 2, format = "f")
USBDC2015.8day$Tair_site<- formattable(USBDC2015.8day$Tair_site, digits = 2, format = "f")
USBDC2015.8day$VPD_site<- formattable(USBDC2015.8day$VPD_site, digits = 2, format = "f")
USBDC2015.8day$siteyear <- paste(substr(deparse(substitute(USBDC2015.8day)), 1, 9))
USBDC2015.8day




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
USBDC2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USBDC2016, mean)
#Second to daily
USBDC2016.daily$GPP_modeled<- ( USBDC2016.daily$GPP_modeled)* 1e-6*86400*12.011
USBDC2016.daily$PAR_Regression<- ( USBDC2016.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
USBDC2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USBDC2016.daily, mean)

###CHANGING THE COLUMN NAMES TO SITE
USBDC2016.8day$Date<-USBDC2016.8day$`cut(Date, "8 days")`
USBDC2016.8day$GPP_site<-USBDC2016.8day$GPP_modeled
USBDC2016.8day$PAR_site<-USBDC2016.8day$PAR_Regression
USBDC2016.8day$VPD_site<-USBDC2016.8day$VPD_REddyProc
USBDC2016.8day$Tair_site<-USBDC2016.8day$Ta_REddyProc
USBDC2016.8day<-USBDC2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USBDC2016.8day$doy<-yday(USBDC2016.8day$Date)
USBDC2016.8day<-USBDC2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDC2016.8day$GPP_site<- formattable(USBDC2016.8day$GPP_site, digits = 2, format = "f")
USBDC2016.8day$PAR_site<- formattable(USBDC2016.8day$PAR_site, digits = 2, format = "f")
USBDC2016.8day$Tair_site<- formattable(USBDC2016.8day$Tair_site, digits = 2, format = "f")
USBDC2016.8day$VPD_site<- formattable(USBDC2016.8day$VPD_site, digits = 2, format = "f")
USBDC2016.8day$siteyear <- paste(substr(deparse(substitute(USBDC2016.8day)), 1, 9))
USBDC2016.8day



myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =5))
USHRA2015<-read_excel(file_path, skip= 2, sheet =5, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USHRA2015$`Date Time`<-gsub("'","",USHRA2015$`Date Time`)
USHRA2015$datetime<-strptime(USHRA2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USHRA2015$Date<-as.Date(USHRA2015$datetime)
##ADD the DOY column
USHRA2015$DOY<- yday(USHRA2015$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-07")
USHRA2015<-dplyr::filter(USHRA2015, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USHRA2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRA2015, mean)
#Second to daily
USHRA2015.daily$GPP_modeled<- ( USHRA2015.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA2015.daily$PAR_Regression<- ( USHRA2015.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRA2015.daily$Date,  USHRA2015.daily$GPP_modeled)
USHRA2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRA2015.daily, mean)
plot( USHRA2015.8day$`cut(Date, "8 days")`,  USHRA2015.8day$GPP_modeled)
###CHANGING THE COLUMN NAMES TO SITE
USHRA2015.8day$Date<-USHRA2015.8day$`cut(Date, "8 days")`
USHRA2015.8day$GPP_site<-USHRA2015.8day$GPP_modeled
USHRA2015.8day$PAR_site<-USHRA2015.8day$PAR_Regression
USHRA2015.8day$VPD_site<-USHRA2015.8day$VPD_REddyProc
USHRA2015.8day$Tair_site<-USHRA2015.8day$Ta_REddyProc
USHRA2015.8day<-USHRA2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USHRA2015.8day$doy<-yday(USHRA2015.8day$Date)
USHRA2015.8day<-USHRA2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRA2015.8day
USHRA2015.8day$GPP_site<- formattable(USHRA2015.8day$GPP_site, digits = 2, format = "f")
USHRA2015.8day$PAR_site<- formattable(USHRA2015.8day$PAR_site, digits = 2, format = "f")
USHRA2015.8day$Tair_site<- formattable(USHRA2015.8day$Tair_site, digits = 2, format = "f")
USHRA2015.8day$VPD_site<- formattable(USHRA2015.8day$VPD_site, digits = 2, format = "f")
USHRA2015.8day$siteyear <- paste(substr(deparse(substitute(USHRA2015.8day)), 1, 9))
USHRA2015.8day




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
USHRA2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRA2016, mean)
#Second to daily
USHRA2016.daily$GPP_modeled<- ( USHRA2016.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA2016.daily$PAR_Regression<- ( USHRA2016.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRA2016.daily$Date,  USHRA2016.daily$GPP_modeled)
USHRA2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRA2016.daily, mean)
plot( USHRA2016.8day$`cut(Date, "8 days")`,  USHRA2016.8day$GPP_modeled)
###CHANGING THE COLUMN NAMES TO SITE
USHRA2016.8day$Date<-USHRA2016.8day$`cut(Date, "8 days")`
USHRA2016.8day$GPP_site<-USHRA2016.8day$GPP_modeled
USHRA2016.8day$PAR_site<-USHRA2016.8day$PAR_Regression
USHRA2016.8day$VPD_site<-USHRA2016.8day$VPD_REddyProc
USHRA2016.8day$Tair_site<-USHRA2016.8day$Ta_REddyProc
USHRA2016.8day<-USHRA2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USHRA2016.8day$doy<-yday(USHRA2016.8day$Date)
USHRA2016.8day<-USHRA2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRA2016.8day$siteyear <- paste(substr(deparse(substitute(USHRA2016.8day)), 1, 9))
USHRA2016.8day






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
USHRA2017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRA2017, mean)
#Second to daily
USHRA2017.daily$GPP_modeled<- ( USHRA2017.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA2017.daily$PAR_Regression<- ( USHRA2017.daily$PAR_Regression)* 1e-6*86400

## Scale to 8 day scale
USHRA2017.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRA2017.daily, mean)
USHRA2017.8day$Date<-USHRA2017.8day$`cut(Date, "8 days")`
USHRA2017.8day$GPP_site<-USHRA2017.8day$GPP_modeled
USHRA2017.8day$PAR_site<-USHRA2017.8day$PAR_Regression
USHRA2017.8day$VPD_site<-USHRA2017.8day$VPD_REddyProc
USHRA2017.8day$Tair_site<-USHRA2017.8day$Ta_REddyProc
USHRA2017.8day<-USHRA2017.8day%>%dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USHRA2017.8day$doy<-yday(USHRA2017.8day$Date)
USHRA2017.8day<-USHRA2017.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRA2017.8day$GPP_site<- formattable(USHRA2017.8day$GPP_site, digits = 2, format = "f")
USHRA2017.8day$PAR_site<- formattable(USHRA2017.8day$PAR_site, digits = 2, format = "f")
USHRA2017.8day$Tair_site<- formattable(USHRA2017.8day$Tair_site, digits = 2, format = "f")
USHRA2017.8day$VPD_site<- formattable(USHRA2017.8day$VPD_site, digits = 2, format = "f")
USHRA2017.8day$siteyear <- paste(substr(deparse(substitute(USHRA2017.8day)), 1, 9))
USHRA2017.8day




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
USHRC2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRC2015, mean)
#Second to daily
USHRC2015.daily$GPP_modeled<- ( USHRC2015.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC2015.daily$PAR_Regression<- ( USHRC2015.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRC2015.daily$Date,  USHRC2015.daily$GPP_modeled)
USHRC2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRC2015.daily, mean)
plot( USHRC2015.8day$`cut(Date, "8 days")`,  USHRC2015.8day$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE

USHRC2015.8day$Date<-USHRC2015.8day$`cut(Date, "8 days")`
USHRC2015.8day$GPP_site<-USHRC2015.8day$GPP_modeled
USHRC2015.8day$PAR_site<-USHRC2015.8day$PAR_Regression
USHRC2015.8day$VPD_site<-USHRC2015.8day$VPD_REddyProc
USHRC2015.8day$Tair_site<-USHRC2015.8day$Ta_REddyProc
USHRC2015.8day<-USHRC2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USHRC2015.8day$doy<-yday(USHRC2015.8day$Date)
USHRC2015.8day<-USHRC2015.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRC2015.8day
USHRC2015.8day$GPP_site<- formattable(USHRC2015.8day$GPP_site, digits = 2, format = "f")
USHRC2015.8day$PAR_site<- formattable(USHRC2015.8day$PAR_site, digits = 2, format = "f")
USHRC2015.8day$Tair_site<- formattable(USHRC2015.8day$Tair_site, digits = 2, format = "f")
USHRC2015.8day$VPD_site<- formattable(USHRC2015.8day$VPD_site, digits = 2, format = "f")
USHRC2015.8day$siteyear <- paste(substr(deparse(substitute(USHRC2015.8day)), 1, 9))
USHRC2015.8day




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
USHRC2016<-filter(USHRC2016, DOY>=MODISdoy)
##########################################################
## 8 day mean
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
USHRC2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRC2016, mean)
#Second to daily
USHRC2016.daily$GPP_modeled<- ( USHRC2016.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC2016.daily$PAR_Regression<- ( USHRC2016.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRC2016.daily$PAR_Regression,  USHRC2016.daily$GPP_modeled)
USHRC2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRC2016.daily, mean)
plot( USHRC2016.8day$`cut(Date, "8 days")`,  USHRC2016.8day$GPP_modeled)
###CHANGING THE COLUMN NAMES TO SITE
USHRC2016.8day$Date<-USHRC2016.8day$`cut(Date, "8 days")`
USHRC2016.8day$GPP_site<-USHRC2016.8day$GPP_modeled
USHRC2016.8day$PAR_site<-USHRC2016.8day$PAR_Regression
USHRC2016.8day$VPD_site<-USHRC2016.8day$VPD_REddyProc
USHRC2016.8day$Tair_site<-USHRC2016.8day$Ta_REddyProc
USHRC2016.8day<-USHRC2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USHRC2016.8day$doy<-yday(USHRC2016.8day$Date)
USHRC2016.8day<-USHRC2016.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRC2016.8day
USHRC2016.8day$GPP_site<- formattable(USHRC2016.8day$GPP_site, digits = 2, format = "f")
USHRC2016.8day$PAR_site<- formattable(USHRC2016.8day$PAR_site, digits = 2, format = "f")
USHRC2016.8day$Tair_site<- formattable(USHRC2016.8day$Tair_site, digits = 2, format = "f")
USHRC2016.8day$VPD_site<- formattable(USHRC2016.8day$VPD_site, digits = 2, format = "f")
USHRC2016.8day$siteyear <- paste(substr(deparse(substitute(USHRC2016.8day)), 1, 9))
USHRC2016.8day




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
USHRC2017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRC2017, mean)
#Second to daily
USHRC2017.daily$GPP_modeled<- ( USHRC2017.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC2017.daily$PAR_Regression<- ( USHRC2017.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot( USHRC2017.daily$Date,  USHRC2017.daily$GPP_modeled)
USHRC2017.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRC2017.daily, mean)
plot( USHRC2017.8day$`cut(Date, "8 days")`,  USHRC2017.8day$GPP_modeled)
###CHANGING THE COLUMN NAMES TO SITE
USHRC2017.8day$Date<-USHRC2017.8day$`cut(Date, "8 days")`
USHRC2017.8day$GPP_site<-USHRC2017.8day$GPP_modeled
USHRC2017.8day$PAR_site<-USHRC2017.8day$PAR_Regression
USHRC2017.8day$VPD_site<-USHRC2017.8day$VPD_REddyProc
USHRC2017.8day$Tair_site<-USHRC2017.8day$Ta_REddyProc
USHRC2017.8day<-USHRC2017.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USHRC2017.8day$doy<-yday(USHRC2017.8day$Date)
USHRC2017.8day<-USHRC2017.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRC2017.8day$GPP_site<- formattable(USHRC2017.8day$GPP_site, digits = 2, format = "f")
USHRC2017.8day$PAR_site<- formattable(USHRC2017.8day$PAR_site, digits = 2, format = "f")
USHRC2017.8day$Tair_site<- formattable(USHRC2017.8day$Tair_site, digits = 2, format = "f")
USHRC2017.8day$VPD_site<- formattable(USHRC2017.8day$VPD_site, digits = 2, format = "f")
USHRC2017.8day$siteyear <- paste(substr(deparse(substitute(USHRC2017.8day)), 1, 9))
USHRC2017.8day



# Reading the data removing the unit row
myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =8))
USOF12017<-read_excel(file_path, guess_max = 21474836,skip= 2, sheet =8, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USOF12017$`Date Time`<-gsub("'","",USOF12017$`Date Time`)
USOF12017$datetime<-strptime(USOF12017$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF12017$Date<-as.Date(USOF12017$datetime)
##ADD the DOY column
USOF12017$DOY<- yday(USOF12017$datetime)

###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2017-04-07")
USOF12017<-dplyr::filter(USOF12017, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
## Daily mean
USOF12017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF12017, mean)
#Second to daily
USOF12017.daily$GPP_modeled<- (USOF12017.daily$GPP_modeled)* 1e-6*86400*12.011
USOF12017.daily$PAR_Regression<- (USOF12017.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
USOF12017.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF12017.daily, mean)
USOF12017.8day$Date<-USOF12017.8day$`cut(Date, "8 days")`
USOF12017.8day$GPP_site<-USOF12017.8day$GPP_modeled
USOF12017.8day$PAR_site<-USOF12017.8day$PAR_Regression
USOF12017.8day$VPD_site<-USOF12017.8day$VPD_REddyProc
USOF12017.8day$Tair_site<-USOF12017.8day$Ta_REddyProc
USOF12017.8day$doy<-yday(USOF12017.8day$Date)
USOF12017.8day<-USOF12017.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF12017.8day$GPP_site<- formattable(USOF12017.8day$GPP_site, digits = 2, format = "f")
USOF12017.8day$PAR_site<- formattable(USOF12017.8day$PAR_site, digits = 2, format = "f")
USOF12017.8day$Tair_site<- formattable(USOF12017.8day$Tair_site, digits = 2, format = "f")
USOF12017.8day$VPD_site<- formattable(USOF12017.8day$VPD_site, digits = 2, format = "f")
USOF12017.8day$siteyear <- paste(substr(deparse(substitute(USOF12017.8day)), 1, 9))
USOF12017.8day



## USOF22017 is in the sheet 9 of RiceData_ResearchGroup03-10-2022
## Read the data
myCols<-as.character(read_excel(file_path, n_max = 1, col_names = FALSE, sheet =9))
USOF22017<-read_excel(file_path,  skip= 2, sheet =9, col_names = myCols)
## The qoutation around the datetime column and fixing the datetime 
USOF22017$`Date Time`<-gsub("'","",USOF22017$`Date Time`)
USOF22017$datetime<-strptime(USOF22017$`Date Time`, format='%d-%b-%Y %H:%M:%S')
###Get the date column from the dt
USOF22017$Date<-as.Date(USOF22017$datetime)
##ADD the DOY column
USOF22017$DOY<- yday(USOF22017$datetime)
###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-97
USOF22017<-dplyr::filter(USOF22017, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
## Daily mean
USOF22017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF22017, mean)
#Second to daily
USOF22017.daily$GPP_modeled<- (USOF22017.daily$GPP_modeled)* 1e-6*86400*12.011
USOF22017.daily$PAR_Regression<- (USOF22017.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF22017.daily$Date, USOF22017.daily$GPP_modeled)
USOF22017.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF22017.daily, mean)
plot(USOF22017.8day$`cut(Date, "8 days")`, USOF22017.8day$GPP_modeled)

###CHANGING THE COLUMNN NNAMES TO SITE
USOF22017.8day$Date<-USOF22017.8day$`cut(Date, "8 days")`
USOF22017.8day$GPP_site<-USOF22017.8day$GPP_modeled
USOF22017.8day$PAR_site<-USOF22017.8day$PAR_Regression
USOF22017.8day$VPD_site<-USOF22017.8day$VPD_REddyProc
USOF22017.8day$Tair_site<-USOF22017.8day$Ta_REddyProc
USOF22017.8day$doy<-yday(USOF22017.8day$Date)

USOF22017.8day<-USOF22017.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF22017.8day$GPP_site<- formattable(USOF22017.8day$GPP_site, digits = 2, format = "f")
USOF22017.8day$PAR_site<- formattable(USOF22017.8day$PAR_site, digits = 2, format = "f")
USOF22017.8day$Tair_site<- formattable(USOF22017.8day$Tair_site, digits = 2, format = "f")
USOF22017.8day$VPD_site<- formattable(USOF22017.8day$VPD_site, digits = 2, format = "f")
USOF22017.8day$siteyear <- paste(substr(deparse(substitute(USOF22017.8day)), 1, 9))
USOF22017.8day





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
USOF32017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF32017, mean)
#Second to daily
USOF32017.daily$GPP_modeled<- (USOF32017.daily$GPP_modeled)* 1e-6*86400*12.011
USOF32017.daily$PAR_Regression<- (USOF32017.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
USOF32017.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF32017.daily, mean)
###CHANGING THE COLUMNN NNAMES TO SITE

USOF32017.8day$Date<-USOF32017.8day$`cut(Date, "8 days")`
USOF32017.8day$GPP_site<-USOF32017.8day$GPP_modeled
USOF32017.8day$PAR_site<-USOF32017.8day$PAR_Regression
USOF32017.8day$VPD_site<-USOF32017.8day$VPD_REddyProc
USOF32017.8day$Tair_site<-USOF32017.8day$Ta_REddyProc
USOF32017.8day<-USOF32017.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
###CHANGING THE COLUMNN NNAMES TO SITE
USOF32017.8day$doy<-yday(USOF32017.8day$Date)
USOF32017.8day<-USOF32017.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF32017.8day$GPP_site<- formattable(USOF32017.8day$GPP_site, digits = 2, format = "f")
USOF32017.8day$PAR_site<- formattable(USOF32017.8day$PAR_site, digits = 2, format = "f")
USOF32017.8day$Tair_site<- formattable(USOF32017.8day$Tair_site, digits = 2, format = "f")
USOF32017.8day$VPD_site<- formattable(USOF32017.8day$VPD_site, digits = 2, format = "f")
USOF32017.8day$siteyear <- paste(substr(deparse(substitute(USOF32017.8day)), 1, 9))
USOF32017.8day






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
USOF42018<-filter(USOF42018, DOY>=MODISdoy)
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied
## Daily mean
USOF42018.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF42018, mean)
#Second to daily
USOF42018.daily$GPP_modeled<- (USOF42018.daily$GPP_modeled)* 1e-6*86400*12.011
USOF42018.daily$PAR_Regression<- (USOF42018.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF42018.daily$Date, USOF42018.daily$GPP_modeled)
USOF42018.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF42018.daily, mean)
plot(USOF42018.8day$`cut(Date, "8 days")`, USOF42018.8day$GPP_modeled)

###CHANGING THE COLUMNN NNAMES TO SITE
USOF42018.8day$Date<-USOF42018.8day$`cut(Date, "8 days")`
USOF42018.8day$GPP_site<-USOF42018.8day$GPP_modeled
USOF42018.8day$PAR_site<-USOF42018.8day$PAR_Regression
USOF42018.8day$VPD_site<-USOF42018.8day$VPD_REddyProc
USOF42018.8day$Tair_site<-USOF42018.8day$Ta_REddyProc

USOF42018.8day<-USOF42018.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
###CHANGING THE COLUMNN NNAMES TO SITE
USOF42018.8day$doy<-yday(USOF42018.8day$Date)
USOF42018.8day<-USOF42018.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF42018.8day
USOF42018.8day$GPP_site<- formattable(USOF42018.8day$GPP_site, digits = 2, format = "f")
USOF42018.8day$PAR_site<- formattable(USOF42018.8day$PAR_site, digits = 2, format = "f")
USOF42018.8day$Tair_site<- formattable(USOF42018.8day$Tair_site, digits = 2, format = "f")
USOF42018.8day$VPD_site<- formattable(USOF42018.8day$VPD_site, digits = 2, format = "f")
USOF42018.8day$siteyear <- paste(substr(deparse(substitute(USOF42018.8day)), 1, 9))
USOF42018.8day




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
USOF52018.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF52018, mean)
#Second to daily
USOF52018.daily$GPP_modeled<- (USOF52018.daily$GPP_modeled)* 1e-6*86400*12.011
USOF52018.daily$PAR_Regression<- (USOF52018.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF52018.daily$Date, USOF52018.daily$GPP_modeled)
USOF52018.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF52018.daily, mean)

###CHANGING THE COLUMNN NNAMES TO SITE
USOF52018.8day$Date<-USOF52018.8day$`cut(Date, "8 days")`
USOF52018.8day$GPP_site<-USOF52018.8day$GPP_modeled
USOF52018.8day$PAR_site<-USOF52018.8day$PAR_Regression
USOF52018.8day$VPD_site<-USOF52018.8day$VPD_REddyProc
USOF52018.8day$Tair_site<-USOF52018.8day$Ta_REddyProc
USOF52018.8day<-USOF52018.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site)

###CHANGING
USOF52018.8day$doy<-yday(USOF52018.8day$Date)
USOF52018.8day<-USOF52018.8day%>% dplyr::select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF52018.8day
USOF52018.8day$GPP_site<- formattable(USOF52018.8day$GPP_site, digits = 2, format = "f")
USOF52018.8day$PAR_site<- formattable(USOF52018.8day$PAR_site, digits = 2, format = "f")
USOF52018.8day$Tair_site<- formattable(USOF52018.8day$Tair_site, digits = 2, format = "f")
USOF52018.8day$VPD_site<- formattable(USOF52018.8day$VPD_site, digits = 2, format = "f")
USOF52018.8day$siteyear <- paste(substr(deparse(substitute(USOF52018.8day)), 1, 9))
USOF52018.8day



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
USOF62018.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF62018, mean)
#Second to daily
USOF62018.daily$GPP_modeled<- (USOF62018.daily$GPP_modeled)* 1e-6*86400*12.011
USOF62018.daily$PAR_Regression<- (USOF62018.daily$PAR_Regression)* 1e-6*86400
## Scale to 8 day scale
plot(USOF62018.daily$Date, USOF62018.daily$GPP_modeled)
USOF62018.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF62018.daily, mean)
plot(USOF62018.8day$`cut(Date, "8 days")`, USOF62018.8day$GPP_modeled)
###CHANGING THE COLUMNN NNAMES TO SITE
USOF62018.8day$Date<-USOF62018.8day$`cut(Date, "8 days")`
USOF62018.8day$GPP_site<-USOF62018.8day$GPP_modeled
USOF62018.8day$PAR_site<-USOF62018.8day$PAR_Regression
USOF62018.8day$VPD_site<-USOF62018.8day$VPD_REddyProc
USOF62018.8day$Tair_site<-USOF62018.8day$Ta_REddyProc
USOF62018.8day<-USOF62018.8day%>%dplyr:: select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
###CHANGING THE COLUMNN NNAMES TO SITE
USOF62018.8day$doy<-yday(USOF62018.8day$Date)
USOF62018.8day<-USOF62018.8day%>%dplyr:: select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF62018.8day$GPP_site<- formattable(USOF62018.8day$GPP_site, digits = 2, format = "f")
USOF62018.8day$PAR_site<- formattable(USOF62018.8day$PAR_site, digits = 2, format = "f")
USOF62018.8day$Tair_site<- formattable(USOF62018.8day$Tair_site, digits = 2, format = "f")
USOF62018.8day$VPD_site<- formattable(USOF62018.8day$VPD_site, digits = 2, format = "f")
USOF62018.8day$siteyear <- paste(substr(deparse(substitute(USOF62018.8day)), 1, 9))
USOF62018.8day

sitecombineddata <-rbind(USOF62018.8day, USOF52018.8day, USOF42018.8day, USOF32017.8day, USOF22017.8day, USOF12017.8day,
  USBDA2015.8day, USBDA2016.8day, USBDC2015.8day, USBDC2016.8day, USHRA2017.8day, USHRC2017.8day,
  USHRA2015.8day, USHRA2016.8day, USHRC2015.8day, USHRC2016.8day)

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
View(satellitecombined_data)
sitesatellitemergedDATA <- merge(sitecombineddata, satellitecombined_data, by = "siteyeardate")
sitesatellitemergedDATA

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


# Define the function to calculate GPP
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

