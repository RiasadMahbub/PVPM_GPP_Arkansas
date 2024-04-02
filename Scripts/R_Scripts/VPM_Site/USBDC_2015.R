#### US BdC 2015
#### 2015

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


## Read the data
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =16))
USBDCC2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                      skip= 2, sheet =16, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USBDCC2015$`Date Time`<-gsub("'","",USBDCC2015$`Date Time`)
USBDCC2015$datetime<-strptime(USBDCC2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USBDCC2015$Date<-as.Date(USBDCC2015$datetime)

##ADD the DOY column
USBDCC2015$DOY<- yday(USBDCC2015$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-7")

USBDCC2015<-filter(USBDCC2015, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USBDCC2015$DOY)- min(USBDCC2015$DOY))/8)

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
  dflist[[i]]<- subset(USBDCC2015, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>40)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSBDC2015<--rep(0, points)
rsquareUSBDC2015<--rep(0, points)
residualsumofsquareUSBDC2015<--rep(0, points)
rmseUSBDC2015<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.02989 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSBDC2015[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSBDC2015[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSBDC2015[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSBDC2015[i] <-  1-(residualsumofsquareUSBDC2015[i]/sum((y - mean(y))^2))
}






avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USBDCC2015 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USBDCC2015, USBDCC2015$NEE_modeled, Reco=USBDCC2015$Reco_modeled,PPFD=USBDCC2015$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[17]], dflist[[17]]$NEE_modeled, Reco=dflist[[17]]$Reco_modeled,PPFD=dflist[[17]]$PAR_Regression,PPFD_ref=2000)


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
USBDCC2015LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USBDC", "year" = 2015, "STD_Error" = stderrorUSBDC2015, "R-squared" = rsquareUSBDC2015, "RMSE" = rmseUSBDC2015, "Residual sum of square" = residualsumofsquareUSBDC2015, "Topt" = toptvector)
view(USBDCC2015LUEmax)


USBDCC2015LUEmax



#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USBDC2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USBDCC2015, mean)


#Second to daily
USBDC2015.daily$GPP_modeled<- (USBDC2015.daily$GPP_modeled)* 1e-6*86400*12.011
USBDC2015.daily$PAR_Regression<- (USBDC2015.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USBDC2015.daily$Date, USBDC2015.daily$GPP_modeled)

USBDC2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USBDC2015.daily, mean)
plot(USBDC2015.8day$`cut(Date, "8 days")`, USBDC2015.8day$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE

USBDC2015.8day$Date<-USBDC2015.8day$`cut(Date, "8 days")`
USBDC2015.8day$GPP_site<-USBDC2015.8day$GPP_modeled
USBDC2015.8day$PAR_site<-USBDC2015.8day$PAR_Regression
USBDC2015.8day$VPD_site<-USBDC2015.8day$VPD_REddyProc
USBDC2015.8day$Tair_site<-USBDC2015.8day$Ta_REddyProc

USBDC2015.8day<-USBDC2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)

###

USBDC2015.8day$doy<-yday(USBDC2015.8day$Date)

USBDC2015.8day<-USBDC2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDC2015.8day


library(formattable)


USBDC2015.8day$GPP_site<- formattable(USBDC2015.8day$GPP_site, digits = 2, format = "f")
USBDC2015.8day$PAR_site<- formattable(USBDC2015.8day$PAR_site, digits = 2, format = "f")
USBDC2015.8day$Tair_site<- formattable(USBDC2015.8day$Tair_site, digits = 2, format = "f")
USBDC2015.8day$VPD_site<- formattable(USBDC2015.8day$VPD_site, digits = 2, format = "f")

USBDC2015.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USBDC2015VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USBDC2015.csv")
#USBDC2015VI$Date<-as.Date(USBDC2015VI$Time, "%b %d, %Y")
USBDC2015VI$Tair_satellite<-((USBDC2015VI$Temp+USBDC2015VI$Tmax)/2)-273.16
USBDC2015VI$doy<-(USBDC2015VI$doy) + 1


USBDC2015VI8day<-merge(x = USBDC2015.8day, y = USBDC2015VI, by = "doy", all = TRUE)

USBDCC2015LUEmax$doy<- USBDCC2015LUEmax$DOY
USBDC2015VI8day<-merge(x = USBDC2015VI8day, y = USBDCC2015LUEmax, by = "doy", all = TRUE)

USBDC2015VI8day$DOP = 92
USBDC2015VI8day$DAP = USBDC2015VI8day$doy - USBDC2015VI8day$DOP

a = -1972
b= -3537
c=  82.5211
d = 0.05764
x= USBDC2015VI8day$DAP
x
USBDC2015VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USBDC2015VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USBDC2015VI8day$DAP, USBDC2015VI8day$LUEmaxmodeled)
plot(USBDC2015VI8day$DAP, USBDC2015VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USBDC2015VI8day$Tair_satellite
Toptcal<-USBDC2015VI8day$Topt
Tsite<- USBDC2015VI8day$Tair_site
Toptmodeled<-USBDC2015VI8day$Toptmodeled
USBDC2015VI8day$Fapar <- (USBDC2015VI8day$EVI_SG - 0.1)*1.25
USBDC2015VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USBDC2015VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USBDC2015VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USBDC2015VI8day$DOP <- 92
USBDC2015VI8day$DAP<-USBDC2015VI8day$doy-USBDC2015VI8day$DOP
View(USBDC2015VI8day)





USBDC2015VI8day$Ws<- (1+USBDC2015VI8day$LSWI_SG)/((1+max(USBDC2015VI8day$LSWI_SG)))
USBDC2015VI8day$LUEvpm<- (USBDC2015VI8day$Tsvpm)*(USBDC2015VI8day$Ws)*(0.05)
USBDC2015VI8day$LUEpvpmcal<-(USBDC2015VI8day$Tspvpm_cal)*(USBDC2015VI8day$Ws)*(USBDC2015VI8day$LUEmax)
USBDC2015VI8day$LUEpvpmmodeled<- (USBDC2015VI8day$Tspvpm_modeled)*(USBDC2015VI8day$Ws)*(USBDC2015VI8day$LUEmaxmodeled)


USBDC2015VI8day$GPPvpm<- USBDC2015VI8day$LUEvpm*USBDC2015VI8day$Fapar*USBDC2015VI8day$PAR_site*12.011
USBDC2015VI8day$GPPpvpmcal<- USBDC2015VI8day$LUEmax*USBDC2015VI8day$Fapar*USBDC2015VI8day$PAR_site*12.011
USBDC2015VI8day$GPPpvpmmodel<- USBDC2015VI8day$LUEmaxmodeled*USBDC2015VI8day$Fapar*USBDC2015VI8day$PAR_site*12.011


plot(USBDC2015VI8day$GPP_site, USBDC2015VI8day$GPPvpm)
plot(USBDC2015VI8day$GPP_site, USBDC2015VI8day$GPPpvpmcal)
plot(USBDC2015VI8day$GPP_site, USBDC2015VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USBDC2015VI8day$GPP_site, USBDC2015VI8day$GPPvpm, na.rm=TRUE)
rmse(USBDC2015VI8day$GPP_site, USBDC2015VI8day$GPPpvpmcal)
rmse(USBDC2015VI8day$GPP_site, USBDC2015VI8day$GPPpvpmmodel)



ncol(USBDC2015VI8day)


##dropping the null values
USBDC2015VI8day<-USBDC2015VI8day[!is.na(USBDC2015VI8day$GPP_site),]

p<-ggplot(data=USBDC2015VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDC2015 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDC2015VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USBDC2015VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDC2015 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDC2015PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USBDC2015VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDC2015 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDC2015PVPMmodeled.jpeg", width = 18, height = 7)














################################################
USBDC_2015<-read_excel("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Stacener ZG-RunkleReba_07-2021.xlsx", sheet =3)



## Subsetting
## Dropping the first 8 rows
USBDC_2015<-USBDC_2015[-1:-7, ]

## Make the first row as the header row
names(USBDC_2015)<-USBDC_2015[1, ]
USBDC_2015<-USBDC_2015[-1, ]

## Drop the first 7 columns 
USBDC_2015<-USBDC_2015[, -1:-7]
## Selecting 4 columns of USOF1
USBDC_2015<-USBDC_2015[, c(1, 6, 7,8,20,21,25, 23)]

## Replace all the -9999 values by NA (missing) labels
USBDC_2015<-USBDC_2015%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USBDC_2015$time <- as.numeric(substr(USBDC_2015$TIMESTAMP_END, 9, 12))

# formatting time stamp
USBDC_2015$datetime <-ymd_hms(USBDC_2015$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USBDC_2015$SW_IN<-as.numeric(USBDC_2015$SW_IN)
USBDC_2015$FC_F<-as.numeric(USBDC_2015$FC_F)
USBDC_2015$RH<-as.numeric(USBDC_2015$RH)
USBDC_2015$TA<-as.numeric(USBDC_2015$TA)
USBDC_2015$time<-as.numeric(USBDC_2015$time)
USBDC_2015$FC_F<-as.numeric(USBDC_2015$FC_F)
USBDC_2015$RECO<-as.numeric(USBDC_2015$RECO)
USBDC_2015$GPP<-as.numeric(USBDC_2015$GPP)

### 2015
#plantingdate: march 31-apr 04 
## harvesting date: sep 18-20
## modis: 2018 05, 09

USBDC_2015$DOY<- yday(USBDC_2015$datetime)
## converting swin to par by multipying 2.02
USBDC_2015$PAR<-USBDC_2015$SW_IN*2.02

(max(USBDC_2015$DOY)-min(USBDC_2015$DOY))/8
### MODIS image nearest one starts at 137 day May 17
USBDC_2015<-filter(USBDC_2015, DOY>=157)
###2015
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
DOYlistway32015<-rep(0, 10)
LUEmax2015<-rep(0,10)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 157
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:10){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USBDC_2015, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
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
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.03748,g= 26.69099))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

DOYlistway32015
LUEmax2015
plot(as.numeric(LUEmax2015))


light.response(data = USBDC_2015, NEE=USBDC_2015$FC_F, Reco=USBDC_2015$RECO,PPFD=USBDC_2015$PAR,PPFD_ref=2000)

plot(USBDC_2015$PAR, USBDC_2015$GPP)
plot(dflist[[4]]$PAR, dflist[[4]]$GPP)


avgsdevsub<-rep(0,10)
avgsdevadd<-rep(0,10)
for (i in (1:10)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot(DOYlistway32015, as.numeric(LUEmax2015), xlab="DOY",
     ylab="LUEmax", main = "USBDC 2015", ylim=c(0, 0.08))
  arrows(DOYlistway32015, avgsdevsub, DOYlistway32015, avgsdevadd, length=0.05, angle=90, code=3)
  abline(h=mean(LUEmax2015[-1]), col="blue")

nlsLM((dflist[[2]]$GPP) ~ (((a * (x) * g)/(a * (dflist[[2]]$PAR)+ g))), start=list(a= 0.03748,g= 26.69099))
summary(modellist[[1]]$m$getPars(stderr()))
  
summary(modellist[[1]])
str(modellist[[1]])


a<-summary(modellist[[1]])
modellist[[1]]$m$setVarying()[1,2]
a$parameters[1,2]

summary(modellist[[1]])$parameters[1,2]

Parameters

summary(modellist[[1]])

modellist[[1]]



#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USBDC_2015))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USBDC_2015)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USBDC_2015)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))
