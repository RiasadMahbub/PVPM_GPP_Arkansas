### USBDA 
## 2015

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


## Read the data
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =14))
USBDA2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =14, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USBDA2015$`Date Time`<-gsub("'","",USBDA2015$`Date Time`)
USBDA2015$datetime<-strptime(USBDA2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USBDA2015$Date<-as.Date(USBDA2015$datetime)

##ADD the DOY column
USBDA2015$DOY<- yday(USBDA2015$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-7")

USBDA2015<-filter(USBDA2015, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USBDA2015$DOY)- min(USBDA2015$DOY))/8)

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
  dflist[[i]]<- subset(USBDA2015, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>40)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax

stderrorUSBDA2015<--rep(0, points)
rsquareUSBDA2015<--rep(0, points)
residualsumofsquareUSBDA2015<--rep(0, points)
rmseUSBDA2015<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.04377 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSBDA2015[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSBDA2015[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSBDA2015[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSBDA2015[i] <-  1-(residualsumofsquareUSBDA2015[i]/sum((y - mean(y))^2))
}
 


stderrorUSBDA2015



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USBDA2015 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USBDA2015, USBDA2015$NEE_modeled, Reco=USBDA2015$Reco_modeled,PPFD=USBDA2015$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[1]], dflist[[1]]$NEE_modeled, Reco=dflist[[1]]$Reco_modeled,PPFD=dflist[[1]]$PAR_Regression,PPFD_ref=2000)

par(mfrow = c(5, 5))
par(mar=c(1, 1, 1, 1))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}


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

toptvector
## plot topt
par(mfrow = c(1, 1))
par(mar=c(2, 2, 2, 2))
plot(DOYlistway32015[-17], toptvector,  xlab="DOY", cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     
     ylab="Tpot", main = "USOF1 2015 (95 percentile method)")
topt




##Create a dataframe
## drop the last 2 luemax values here and 
USBDA2015LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USBDA", "year" = 2015, "STD_Error" = stderrorUSBDA2015, "R-squared" = rsquareUSBDA2015, "RMSE" = rmseUSBDA2015, "Residual sum of square" = residualsumofsquareUSBDA2015, "Topt" = toptvector)
view(USBDA2015LUEmax)
View(USBDA2015)


#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USBDA2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USBDA2015, mean)


#Second to daily
USBDA2015.daily$GPP_modeled<- (USBDA2015.daily$GPP_modeled)* 1e-6*86400*12.011
USBDA2015.daily$PAR_Regression<- (USBDA2015.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USBDA2015.daily$Date, USBDA2015.daily$GPP_modeled)

USBDA2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USBDA2015.daily, mean)
plot(USBDA2015.8day$`cut(Date, "8 days")`, USBDA2015.8day$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE

USBDA2015.8day$Date<-USBDA2015.8day$`cut(Date, "8 days")`
USBDA2015.8day$GPP_site<-USBDA2015.8day$GPP_modeled
USBDA2015.8day$PAR_site<-USBDA2015.8day$PAR_Regression
USBDA2015.8day$VPD_site<-USBDA2015.8day$VPD_REddyProc
USBDA2015.8day$Tair_site<-USBDA2015.8day$Ta_REddyProc

USBDA2015.8day<-USBDA2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)
USBDA2015.8day



USBDA2015.8day$doy<-yday(USBDA2015.8day$Date)

USBDA2015.8day<-USBDA2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDA2015.8day


library(formattable)


USBDA2015.8day$GPP_site<- formattable(USBDA2015.8day$GPP_site, digits = 2, format = "f")
USBDA2015.8day$PAR_site<- formattable(USBDA2015.8day$PAR_site, digits = 2, format = "f")
USBDA2015.8day$Tair_site<- formattable(USBDA2015.8day$Tair_site, digits = 2, format = "f")
USBDA2015.8day$VPD_site<- formattable(USBDA2015.8day$VPD_site, digits = 2, format = "f")

USBDA2015.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USBDA2015VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USBDA2015.csv")
#USBDA2015VI$Date<-as.Date(USBDA2015VI$Time, "%b %d, %Y")
USBDA2015VI$Tair_satellite<-((USBDA2015VI$Temp+USBDA2015VI$Tmax)/2)-273.16
USBDA2015VI$doy<-(USBDA2015VI$doy)+1


USBDA2015VI8day<-merge(x = USBDA2015.8day, y = USBDA2015VI, by = "doy", all = TRUE)

USBDA2015LUEmax$doy<- USBDA2015LUEmax$DOY
USBDA2015VI8day<-merge(x = USBDA2015VI8day, y = USBDA2015LUEmax, by = "doy", all = TRUE)

USBDA2015VI8day$DOP = 92
USBDA2015VI8day$DAP = USBDA2015VI8day$doy - USBDA2015VI8day$DOP

a = -1972
b= -3537
c=  68.5211
d = 0.0764
x= USBDA2015VI8day$DAP
x
USBDA2015VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USBDA2015VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USBDA2015VI8day$DAP, USBDA2015VI8day$LUEmaxmodeled)
plot(USBDA2015VI8day$DAP, USBDA2015VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USBDA2015VI8day$Tair_satellite
Toptcal<-USBDA2015VI8day$Topt
Tsite<- USBDA2015VI8day$Tair_site
Toptmodeled<-USBDA2015VI8day$Toptmodeled
USBDA2015VI8day$Fapar <- (USBDA2015VI8day$EVI_SG - 0.1)*1.25
USBDA2015VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USBDA2015VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USBDA2015VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USBDA2015VI8day$DOP <- 92
USBDA2015VI8day$DAP<-USBDA2015VI8day$doy-USBDA2015VI8day$DOP
View(USBDA2015VI8day)





USBDA2015VI8day$Ws<- (1+USBDA2015VI8day$LSWI_SG)/((1+max(USBDA2015VI8day$LSWI_SG)))
USBDA2015VI8day$LUEvpm<- (USBDA2015VI8day$Tsvpm)*(USBDA2015VI8day$Ws)*(0.05)
USBDA2015VI8day$LUEpvpmcal<-(USBDA2015VI8day$Tspvpm_cal)*(USBDA2015VI8day$Ws)*(USBDA2015VI8day$LUEmax)
USBDA2015VI8day$LUEpvpmmodeled<- (USBDA2015VI8day$Tspvpm_modeled)*(USBDA2015VI8day$Ws)*(USBDA2015VI8day$LUEmaxmodeled)


USBDA2015VI8day$GPPvpm<- USBDA2015VI8day$LUEvpm*USBDA2015VI8day$Fapar*USBDA2015VI8day$PAR_site*12.011
USBDA2015VI8day$GPPpvpmcal<- USBDA2015VI8day$LUEmax*USBDA2015VI8day$Fapar*USBDA2015VI8day$PAR_site*12.011
USBDA2015VI8day$GPPpvpmmodel<- USBDA2015VI8day$LUEmaxmodeled*USBDA2015VI8day$Fapar*USBDA2015VI8day$PAR_site*12.011


plot(USBDA2015VI8day$GPP_site, USBDA2015VI8day$GPPvpm)
plot(USBDA2015VI8day$GPP_site, USBDA2015VI8day$GPPpvpmcal)
plot(USBDA2015VI8day$GPP_site, USBDA2015VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USBDA2015VI8day$GPP_site, USBDA2015VI8day$GPPvpm, na.rm=TRUE)
rmse(USBDA2015VI8day$GPP_site, USBDA2015VI8day$GPPpvpmcal)
rmse(USBDA2015VI8day$GPP_site, USBDA2015VI8day$GPPpvpmmodel)

(1+USBDA2015VI8day$LSWI_SG)/((1+max(USBDA2015VI8day$LSWI_SG)))

ncol(USBDA2015VI8day)
View(USOF6VI8day)

##dropping the null values
USBDA2015VI8day<-USBDA2015VI8day[!is.na(USBDA2015VI8day$GPP_site),]

p<-ggplot(data=USBDA2015VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2015 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDA2015VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USBDA2015VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2015 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDA2015PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USBDA2015VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2015 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDA2015PVPMmodeled.jpeg", width = 18, height = 7)

plot(USBDA2015VI8day$DAP,USBDA2015VI8day$LUEmax)
plot(USBDA2015VI8day$DAP,USBDA2015VI8day$GPPpvpmcal)














#################################################
USBDA_2015<-read_excel("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Stacener ZG-RunkleReba_07-2021.xlsx", sheet =2)



## Subsetting
## Dropping the first 8 rows
USBDA_2015<-USBDA_2015[-1:-7, ]

## Make the first row as the header row
names(USBDA_2015)<-USBDA_2015[1, ]
USBDA_2015<-USBDA_2015[-1, ]

## Drop the first 7 columns 
USBDA_2015<-USBDA_2015[, -1:-7]

## Selecting 4 columns of USOF1 which are time(1) GPP (7), Tair, SW_IN
USBDA_2015<-USBDA_2015[, c(1, 6, 7,8,20,21,25, 23)]

## Replace all the -9999 values by NA (missing) labels
USBDA_2015<-USBDA_2015%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USBDA_2015$time <- as.numeric(substr(USBDA_2015$TIMESTAMP_END, 9, 12))

# formatting time stamp
USBDA_2015$datetime <-ymd_hms(USBDA_2015$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USBDA_2015$SW_IN<-as.numeric(USBDA_2015$SW_IN)
USBDA_2015$FC_F<-as.numeric(USBDA_2015$FC_F)
USBDA_2015$RH<-as.numeric(USBDA_2015$RH)
USBDA_2015$TA<-as.numeric(USBDA_2015$TA)
USBDA_2015$time<-as.numeric(USBDA_2015$time)
USBDA_2015$FC_F<-as.numeric(USBDA_2015$FC_F)
USBDA_2015$RECO<-as.numeric(USBDA_2015$RECO)
USBDA_2015$GPP<-as.numeric(USBDA_2015$GPP)





### USBDA
## Date of planting: Mar 31-Apr 04
## Date of harvesting: July Aug 24-29

## MODIS image start from Mar 30 for 2015

### Select the required columns GPP, SWin, Tair
View(USBDA_2015)
USBDA_2015$PAR<-USBDA_2015$SW_IN*2.02

####
##ADD doy to the dataframe

USBDA_2015$DOY<- yday(USBDA_2015$datetime)
### MODIS image nearest one starts at 185 day july 4th
USBDA_2015_185<-filter(USBDA_2015, DOY>=185)



###2015
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
(max(USBDA_2015$DOY)-min(USBDA_2015$DOY))/8
DOYlistway32015<-rep(0, 7)
LUEmax2015<-rep(0,7)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 185
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:7){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USBDA_2015, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], SW_IN>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:7){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:7)){
  y<-dflist[[i]]$GPP
  x<-dflist[[i]]$SW_IN
  modellist[[i]]<-nlsLM((y) ~ (((a*(x)*g)/(a*(x)+ g))), start=list(a= 0.03143,g= 26.36339))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

light.response(data = USBDA_2015, NEE=USBDA_2015$FC_F, Reco=USBDA_2015$RECO,PPFD=USBDA_2015$PAR,PPFD_ref=2000)

LUEmax2015
plot(as.numeric(LUEmax2015))


plot(DOYlistway32015, as.numeric(LUEmax2015), xlab="DOY",
     ylab="LUEmax", main = "USBDA 2015")

DOYlistway32015

plot(dflist[[5]]$PPFD_IN, dflist[[5]]$GPP)

DOYlistway32015



plot(USBDA_2015$PPFD_IN, USBDA_2015$GPP)

###Aggregating the GPP data to daily mean 
#Splitting Date and Time (The date and time was together)
USBDA_2015<- USBDA_2015%>%
  separate(datetime, c("Date", "Time"), " ")

# #converting the date to date format
# USBDA_2015$Date<-as.Date(USBDA_2015$Date,
#                        format = "%Y/%m/%d")

#Converting date to year, month and day columns
USBDA_2015<-USBDA_2015 %>%
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date), 
                day = lubridate::day(Date))




#Our task is to take the various columns of date and time information and convert these to a vector of type *datetime*. Let's go step by step, we will start with a new character vector of date
date.char<-paste(USBDA_2015$year,USBDA_2015$month,USBDA_2015$day,sep = '-')


#Adding date as a separate column which would be useful later
USBDA_2015.char$date<-as.Date(date.char)


## Daily mean
USBDA_2015.daily <- aggregate(FC_F ~ Date, USBDA_2015, mean)
head(USBDA_2015.daily)
View(USBDA_2015.daily)

## Converting umol/m2/s to gC/m2/day
dfway316.daily$GPP<- (dfway316.daily$GPP)* 1e-6*86400*12.011
class(dfway316.daily$GPP)


#### Finding the percentage of gaps in the variables
library(dplyr)
# Gappercentage<-as.data.frame(colMeans(is.na(USBDA_2015))*100)
# Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
# rownames(Gappercentage) <- 1:nrow(Gappercentage)
# Gappercentage<-Gappercentage[c(2,3,4,5), ]
# 
# ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USBDA_2015)) * 100`, fill = newColName)) + 
#   geom_bar(stat = "identity") +
#   geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USBDA_2015)) * 100`, digits = 2))), vjust = 0)+
#   ylab("Percentages (%)")+
#   xlab("Variables")+
#   guides(fill=guide_legend(title="Variables"))+
#   theme(text = element_text(size=20))

