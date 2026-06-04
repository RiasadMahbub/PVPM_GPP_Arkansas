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
# Reading the data removing the unit row
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                   n_max = 1, col_names = FALSE, sheet =8))
USOF1<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", guess_max = 21474836,
                  skip= 2, sheet =8, col_names = myCols)
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


### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

## Daily mean
USOF1.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF1, mean)



#Second to daily
USOF1.daily$GPP_modeled<- (USOF1.daily$GPP_modeled)* 1e-6*86400*12.011
USOF1.daily$PAR_Regression<- (USOF1.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USOF1.daily$Date, USOF1.daily$GPP_modeled)


USOF1.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF1.daily, mean)
plot(USOF1.8day$`cut(Date, "8 days")`, USOF1.8day$GPP_modeled)



USOF1.8day$Date<-USOF1.8day$`cut(Date, "8 days")`
USOF1.8day$GPP_site<-USOF1.8day$GPP_modeled
USOF1.8day$PAR_site<-USOF1.8day$PAR_Regression
USOF1.8day$VPD_site<-USOF1.8day$VPD_REddyProc
USOF1.8day$Tair_site<-USOF1.8day$Ta_REddyProc
USOF1.8day$doy<-yday(USOF1.8day$Date)

USOF1.8day<-USOF1.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
library(formattable)


USOF1.8day$GPP_site<- formattable(USOF1.8day$GPP_site, digits = 2, format = "f")
USOF1.8day$PAR_site<- formattable(USOF1.8day$PAR_site, digits = 2, format = "f")
USOF1.8day$Tair_site<- formattable(USOF1.8day$Tair_site, digits = 2, format = "f")
USOF1.8day$VPD_site<- formattable(USOF1.8day$VPD_site, digits = 2, format = "f")

USOF1.8day[, 1:6] <- round(USOF1.8day[, 1:6], digits = 2)

USOF1.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USOF1VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USOF12017.csv")
#USOF1VI$Date<-as.Date(USOF1VI$Time, "%b %d, %Y")
USOF1VI$Tair_satellite<-((USOF1VI$Temp+USOF1VI$Tmax)/2)-273.16
USOF1VI$doy<-(USOF1VI$doy) + 1


USOF1VI8day<-merge(x = USOF1.8day, y = USOF1VI, by = "doy", all = TRUE)

USOF1LUEmax$doy<- USOF1LUEmax$DOY
USOF1VI8day<-merge(x = USOF1VI8day, y = USOF1LUEmax, by = "doy", all = TRUE)

USOF1VI8day$DOP = 91
USOF1VI8day$DOH = 249
USOF1VI8day$DAP = USOF1VI8day$doy - USOF1VI8day$DOP


View(USOF1VI8day)
a = -1972
b= -3537
c=  82.5211
d = 0.05764
x= USOF1VI8day$DAP
x
USOF1VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USOF1VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USOF1VI8day$DAP, USOF1VI8day$LUEmaxmodeled)
plot(USOF1VI8day$DAP, USOF1VI8day$Toptmodeled)

plot(USOF1VI8day$GDDcum, USOF1VI8day$LUEmax)
plot(USOF1VI8day$GDDcum, USOF1VI8day$Topt)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USOF1VI8day$Tair_satellite
Toptcal<-USOF1VI8day$Topt
Tsite<- USOF1VI8day$Tair_site
Toptmodeled<-USOF1VI8day$Toptmodeled
USOF1VI8day$Fapar <- (USOF1VI8day$EVI_SG - 0.1)*1.25
USOF1VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USOF1VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USOF1VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USOF1VI8day$DOP <- 91
USOF1VI8day$DAP<-USOF1VI8day$doy-USOF1VI8day$DOP






USOF1VI8day$Ws<- (1+USOF1VI8day$LSWI_SG)/((1+max(USOF1VI8day$LSWI_SG)))
USOF1VI8day$LUEvpm<- (USOF1VI8day$Tsvpm)*(USOF1VI8day$Ws)*(0.05)
USOF1VI8day$LUEpvpmcal<-(USOF1VI8day$Tspvpm_cal)*(USOF1VI8day$Ws)*(USOF1VI8day$LUEmax)
USOF1VI8day$LUEpvpmmodeled<- (USOF1VI8day$Tspvpm_modeled)*(USOF1VI8day$Ws)*(USOF1VI8day$LUEmaxmodeled)


USOF1VI8day$GPPvpm<- USOF1VI8day$LUEvpm*USOF1VI8day$Fapar*USOF1VI8day$PAR_site*12.011
USOF1VI8day$GPPpvpmcal<- USOF1VI8day$LUEmax*USOF1VI8day$Fapar*USOF1VI8day$PAR_site*12.011
USOF1VI8day$GPPpvpmmodel<- USOF1VI8day$LUEmaxmodeled*USOF1VI8day$Fapar*USOF1VI8day$PAR_site*12.011


plot(USOF1VI8day$GPP_site, USOF1VI8day$GPPvpm)
plot(USOF1VI8day$GPP_site, USOF1VI8day$GPPpvpmcal)
plot(USOF1VI8day$GPP_site, USOF1VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USOF1VI8day$GPP_site, USOF1VI8day$GPPvpm, na.rm=TRUE)
rmse(USOF1VI8day$GPP_site, USOF1VI8day$GPPpvpmcal)
rmse(USOF1VI8day$GPP_site, USOF1VI8day$GPPpvpmmodel)


ncol(USOF1VI8day)


##dropping the null values
USOF1VI8day<-USOF1VI8day[!is.na(USOF1VI8day$GPP_site),]
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
USOF1VI8day<-USOF1VI8day %>% mutate(across(where(is.numeric), ~ round(., 2)))

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
USOF1VI8day$GPPpvpmmodel<-specify_decimal(USOF1VI8day$GPPpvpmmodel, 3)

p<-ggplot(data=USOF1VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF1 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p

ggsave("USOF1PVPMcal.jpeg", width = 18, height = 7)
USOF1VI8day$GPPpvpmmodel<- as.numeric(USOF1VI8day$GPPpvpmmodel)
USOF1VI8day$GPPpvpmmodel<-(formatC(as.numeric(as.character(round(USOF1VI8day$GPPpvpmmodel, 2))), digits = 2, format = "f"))
USOF1VI8day$GPPpvpmmodel<- as.numeric(USOF1VI8day$GPPpvpmmodel)

USOF1VI8day$GPPpvpmmodel
format(USOF1VI8day)






##Convert all the columns to numeric
options(digits=19)
USOF1<-USOF1 %>%
  dplyr::mutate(across(everything(), as.numeric))
USOF1[1]
names(USOF1)[1] <-"TimeStamp"
names(USOF1)[1]


View(USOF1)
USOF1$TimeStamp<-as.POSIXct((USOF1$TimeStamp - 719529)*86400, origin = "1970-01-01", tz = "UTC")



## Drop the first 7 columns 
USOF1<-USOF1[, -1:-7]
## Selecting 4 columns of USOF1 [6 nee, 7 gpp, 8 reco]
USOF1<-USOF1[, c(1,6, 7,8,20,21,25, 23)]

## Replace all the -9999 values by NA (missing) labels
USOF1<-USOF1%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USOF1$time <- as.numeric(substr(USOF1$TIMESTAMP_END, 9, 12))

# formatting time stamp
USOF1$datetime <-ymd_hms(USOF1$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USOF1$SW_IN<-as.numeric(USOF1$SW_IN)
USOF1$GPP<-as.numeric(USOF1$GPP)
USOF1$RH<-as.numeric(USOF1$RH)
USOF1$TA<-as.numeric(USOF1$TA)
USOF1$time<-as.numeric(USOF1$time)
USOF1$FC_F<-as.numeric(USOF1$FC_F)
USOF1$RECO<-as.numeric(USOF1$RECO)

### 2017
#plantingdate: april1
## modis: mar 30; april07
####
##ADD doy to the dataframe

USOF1$DOY<- yday(USOF1$datetime)
## converting swin to par by multipying 2.02
USOF1$PAR<-USOF1$SW_IN*2.02

##ADD DAP
USOF1$DAP<-USOF1$DOY-91

### MODIS image nearest one starts at 137 day May 17
USOF1<-filter(USOF1, DOY>=137)
###2015
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
DOYlistway32015<-rep(0, 11)
LUEmax2015<-rep(0,10)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 137
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:10){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF1, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:10){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:10)){
  y<-dflist[[i]]$GPP
  x<-dflist[[i]]$PAR
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.03765,g= 31.03264))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}


LUEmax2015



avgsdevsub<-rep(0,10)
avgsdevadd<-rep(0,10)
for (i in (1:10)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}
plot((DOYlistway32015[c(-1, -11)]-91), as.numeric(LUEmax2015[(-1)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017",  ylim=c(0.01, 0.07))
 
  
arrows((DOYlistway32015-91), avgsdevsub,(DOYlistway32015-91), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")


dflist

y<-dflist[[1]]$GPP
x<-dflist[[1]]$PAR
modellist[[1]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.02,g= 2000))
modellist[1]
plot(dflist[[18]]$PAR_Regression, dflist[[18]]$GPP_modeled)
plot(USBDA_2015$PPFD_IN, USBDA_2015$GPP)

dflist



y<-USOF1$GPP
x<-USOF1$PAR
nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.05,g= 30))
LUEmax2015<-modellist[[i]]$m$getPars()[2]


###Light Response
USOF1_complete <- na.omit(USOF1)
USOF1_complete$NEE<-as.numeric(USOF1_complete$FC_F)
light.response(data = USOF1, NEE=USOF1$GPP_modeled, Reco=USOF1$PAR_Regression,PPFD=USOF1$PAR_Regression,PPFD_ref=2000)

### TOPT estimation
plot(dflist[[7]]$TA, dflist[[7]]$GPP)


topt<-rep(0, length(LUEmax2015))
for (i in 1: length(LUEmax2015)){
  topt[i]<-dflist[[i]]  %>% filter(quantile(GPP, 0.95)<GPP) %>% summarize(meanopttemp = mean(TA, na.rm = TRUE)-273.15)
}


## plot topt
plot((DOYlistway32015[( -11)]-91), topt,  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017")


arrows((DOYlistway32015-91), avgsdevsub,(DOYlistway32015-91), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")



### Scaling to daily scale 


dflist[[1]] 






















### Plotting assignmennt 1
## Plot timeseries of shortwave radiation
ggplot(USOF1, aes(x = datetime, y = SW_IN))+
  
  geom_point(aes(color = time), alpha = 0.5)+
  scale_color_viridis(option = "D", direction =-1)+
  xlab("Time")+
  ylab(bquote('Incoming Shortwave Radiation ('*'W'~ m^-2*')'))+
  
  theme(text = element_text(size=12))

ggsave("swintime.png", width = 8, height = 4)

## Plot the timeseries of Net ecosystem exchange
ggplot(USOF1, aes(x = datetime, y = FC_F))+
  
  geom_point(aes(color = time), alpha = 0.5)+
  scale_color_viridis(option = "D", direction =-1)+
  xlab("Time")+
  ylab(bquote('Net Ecosystem Exchange('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))+
  
  theme(text = element_text(size=12))

ggsave("NEEtime.png", width = 8, height = 4)

# Create the three different plots of NEE vs TA, RH and SWIN
FCRH <- ggplot(USOF1, aes(x=RH, y=FC_F)) +
  geom_point() + theme_bw()+
  xlab("Relative humidity (%)")+
  ylab(bquote('Net Ecosystem Exchange('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))

FCTA <- ggplot(USOF1, aes(x=TA, y=FC_F)) +
  geom_point() + theme_bw()+
  xlab("Air Temperature (K)")+
  ylab(bquote('Net Ecosystem Exchange('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))

FCSW <- ggplot(USOF1, aes(x=SW_IN, y=FC_F)) +
  geom_point() + theme_bw()+
  xlab('Incoming Shortwave Radiation ('*'W'~ m^-2*')')+
  ylab(bquote('Net Ecosystem Exchange('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')'))

## Create Flow timeseries plot that spans the grid by making one plot_grid
bottom_row <- plot_grid(FCRH, FCTA, FCSW, ncol = 1, labels = "AUTO")
plot_grid(FCRH, FCTA, FCSW, ncol = 3, labels = c("A", "B", "C"),
          rel_heights = c(1, 1, 1))
ggsave("Relationship.png", width = 12, height = 4)

#### Finding the percentage of gaps in the variables
library(dplyr)
USOF1%>% group_by(USOF1, group) %>% mutate(percent = value/sum(value))
Gappercentage<-as.data.frame(colMeans(is.na(USOF1))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USOF1)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USOF1)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))
