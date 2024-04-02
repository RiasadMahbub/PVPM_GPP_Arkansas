### USOF6 
### 2018

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
                                n_max = 1, col_names = FALSE, sheet =13))
USOF6<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =13, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USOF6$`Date Time`<-gsub("'","",USOF6$`Date Time`)
USOF6$datetime<-strptime(USOF6$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF6$Date<-as.Date(USOF6$datetime)

##ADD the DOY column
USOF6$DOY<- yday(USOF6$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2018-04-15")

USOF6<-filter(USOF6, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF6$DOY)- min(USOF6$DOY))/8)

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
  dflist[[i]]<- subset(USOF6, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSOF62018<--rep(0, points)
rsquareUSOF62018<--rep(0, points)
residualsumofsquareUSOF62018<--rep(0, points)
rmseUSOF62018<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.04377 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF62018[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF62018[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF62018[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF62018[i] <-  1-(residualsumofsquareUSOF62018[i]/sum((y - mean(y))^2))
}




avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}

par(mfrow = c(5, 5))
par(mar=c(1, 1, 1, 1))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}
plot((DOYlistway32015[c(-1,-2,-3, -(points+1))]), as.numeric(LUEmax2015[c(-1,-2,-3)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF6 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USOF6, USOF6$NEE_modeled, Reco=USOF6$Reco_modeled,PPFD=USOF6$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[17]], dflist[[17]]$NEE_modeled, Reco=dflist[[17]]$Reco_modeled,PPFD=dflist[[17]]$PAR_Regression,PPFD_ref=2000)

plot(dflist[[17]]$PAR_Regression, dflist[[17]]$GPP_modeled)

dflist[[1]]


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
USOF6LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF6", "year" = 2018, "STD_Error" = stderrorUSOF62018, "R-squared" = rsquareUSOF62018, "RMSE" = rmseUSOF62018, "Residual sum of square" = residualsumofsquareUSOF62018,  "Topt" = toptvector)
view(USOF6LUEmax)


#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USOF6.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF6, mean)


#Second to daily
USOF6.daily$GPP_modeled<- (USOF6.daily$GPP_modeled)* 1e-6*86400*12.011
USOF6.daily$PAR_Regression<- (USOF6.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USOF6.daily$Date, USOF6.daily$GPP_modeled)

USOF6.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF6.daily, mean)
plot(USOF6.8day$`cut(Date, "8 days")`, USOF6.8day$GPP_modeled)




###CHANGING THE COLUMN NAMES TO SITE

###CHANGING THE COLUMNN NNAMES TO SITE

USOF6.8day$Date<-USOF6.8day$`cut(Date, "8 days")`
USOF6.8day$GPP_site<-USOF6.8day$GPP_modeled
USOF6.8day$PAR_site<-USOF6.8day$PAR_Regression
USOF6.8day$VPD_site<-USOF6.8day$VPD_REddyProc
USOF6.8day$Tair_site<-USOF6.8day$Ta_REddyProc

USOF6.8day<-USOF6.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)

###CHANGING THE COLUMNN NNAMES TO SITE

USOF6.8day$doy<-yday(USOF6.8day$Date)

USOF6.8day<-USOF6.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF6.8day


library(formattable)


USOF6.8day$GPP_site<- formattable(USOF6.8day$GPP_site, digits = 2, format = "f")
USOF6.8day$PAR_site<- formattable(USOF6.8day$PAR_site, digits = 2, format = "f")
USOF6.8day$Tair_site<- formattable(USOF6.8day$Tair_site, digits = 2, format = "f")
USOF6.8day$VPD_site<- formattable(USOF6.8day$VPD_site, digits = 2, format = "f")

USOF6.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USOF6VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USOF62018.csv")
#USOF6VI$Date<-as.Date(USOF6VI$Time, "%b %d, %Y")
USOF6VI$Tair_satellite<-((USOF6VI$Temp+USOF6VI$Tmax)/2)-273.16
USOF6VI$doy<-(USOF6VI$doy) +1


USOF6VI8day<-merge(x = USOF6.8day, y = USOF6VI, by = "doy", all = TRUE)

USOF6LUEmax$doy<- USOF6LUEmax$DOY
USOF6VI8day<-merge(x = USOF6VI8day, y = USOF6LUEmax, by = "doy", all = TRUE)

USOF6VI8day$DOP = 99
USOF6VI8day$DAP = USOF6VI8day$doy - USOF6VI8day$DOP

a = -1972
b= -3537
c=  70.5211
d = 0.0764
x= USOF6VI8day$DAP
x
USOF6VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USOF6VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USOF6VI8day$DAP, USOF6VI8day$LUEmaxmodeled)
plot(USOF6VI8day$DAP, USOF6VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USOF6VI8day$Tair_satellite
Toptcal<-USOF6VI8day$Topt
Tsite<- USOF6VI8day$Tair_site
Toptmodeled<-USOF6VI8day$Toptmodeled
USOF6VI8day$Fapar <- (USOF6VI8day$EVI_SG - 0.1)*1.25
USOF6VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USOF6VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USOF6VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USOF6VI8day$DOP <- 99
USOF6VI8day$DOH <- 241

USOF6VI8day$DAP<-USOF6VI8day$doy-USOF6VI8day$DOP
View(USOF6VI8day)





USOF6VI8day$Ws<- (1+USOF6VI8day$LSWI_SG)/((1+max(USOF6VI8day$LSWI_SG)))
USOF6VI8day$LUEvpm<- (USOF6VI8day$Tsvpm)*(USOF6VI8day$Ws)*(0.05)
USOF6VI8day$LUEpvpmcal<-(USOF6VI8day$Tspvpm_cal)*(USOF6VI8day$Ws)*(USOF6VI8day$LUEmax)
USOF6VI8day$LUEpvpmmodeled<- (USOF6VI8day$Tspvpm_modeled)*(USOF6VI8day$Ws)*(USOF6VI8day$LUEmaxmodeled)


USOF6VI8day$GPPvpm<- USOF6VI8day$LUEvpm*USOF6VI8day$Fapar*USOF6VI8day$PAR_site*12.011
USOF6VI8day$GPPpvpmcal<- USOF6VI8day$LUEmax*USOF6VI8day$Fapar*USOF6VI8day$PAR_site*12.011
USOF6VI8day$GPPpvpmmodel<- USOF6VI8day$LUEmaxmodeled*USOF6VI8day$Fapar*USOF6VI8day$PAR_site*12.011


plot(USOF6VI8day$GPP_site, USOF6VI8day$GPPvpm)
plot(USOF6VI8day$GPP_site, USOF6VI8day$GPPpvpmcal)
plot(USOF6VI8day$GPP_site, USOF6VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USOF6VI8day$GPP_site, USOF6VI8day$GPPvpm, na.rm=TRUE)
rmse(USOF6VI8day$GPP_site, USOF6VI8day$GPPpvpmcal)
rmse(USOF6VI8day$GPP_site, USOF6VI8day$GPPpvpmmodel)



ncol(USOF6VI8day)


##dropping the null values
USOF6VI8day<-USOF6VI8day[!is.na(USOF6VI8day$GPP_site),]

p<-ggplot(data=USOF6VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF6 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF6VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USOF6VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF6 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF6PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USOF6VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF6 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF6PVPMmodeled.jpeg", width = 18, height = 7)





















########################################################
USOF6<-read_excel("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Fairley-RunkleReba_07-2021.xlsx", sheet =7)


## Subsetting
## Dropping the first 8 rows
USOF6<-USOF6[-1:-7, ]

## Make the first row as the header row
names(USOF6)<-USOF6[1, ]
USOF6<-USOF6[-1, ]

## Drop the first 7 columns 
USOF6<-USOF6[, -1:-7]
## Selecting 4 columns of USOF1
USOF6<-USOF6[, c(1, 7,8,9,21,26, 24, 22)]

## Replace all the -9999 values by NA (missing) labels
USOF6<-USOF6%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USOF6$time <- as.numeric(substr(USOF6$TIMESTAMP_END, 9, 12))

# formatting time stamp
USOF6$datetime <-ymd_hms(USOF6$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USOF6$SW_IN<-as.numeric(USOF6$SW_IN)
USOF6$FC_F<-as.numeric(USOF6$FC_F)
USOF6$RH<-as.numeric(USOF6$RH)
USOF6$TA<-as.numeric(USOF6$TA)
USOF6$time<-as.numeric(USOF6$time)
USOF6$FC_F<-as.numeric(USOF6$FC_F)
USOF6$RECO<-as.numeric(USOF6$RECO)
USOF6$GPP<-as.numeric(USOF6$GPP)


### 2018
#plantingdate: april1 -
## harvesting date: sep 18-20
## modis: 2018 05, 09

USOF6$DOY<- yday(USOF6$datetime)
## converting swin to par by multipying 2.02
USOF6$PAR<-USOF6$SW_IN*2.02

### MODIS image nearest one starts at 137 day May 17
#USOF6<-filter(USOF6, DOY>=123)
###2015
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
(max(USOF6$DOY)-min(USOF6$DOY))/8

DOYlistway32015<-rep(0, 14)
LUEmax2015<-rep(0,14)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 123
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:14){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF6, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:14){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:14)){
  y<-dflist[[i]]$GPP
  x<-dflist[[i]]$PAR
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.03897,g= 36.35649))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

##bigleaf
for (i in (11)){
  modellist[[i]]<-light.response(data = dflist[[i]], NEE=dflist[[i]]$FC_F, Reco=dflist[[i]]$RECO,PPFD=dflist[[i]]$PAR,PPFD_ref=2000)
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}




DOYlistway32015
LUEmax2015
plot(as.numeric(LUEmax2015))

plot(DOYlistway32015, as.numeric(LUEmax2015), xlab="DOY",
     ylab="LUEmax", main = "USOF6 2018")


plot(dflist[[1]]$PAR, dflist[[1]]$GPP)

library(bigleaf)
light.response(data = USOF6, NEE=USOF6$FC_F, Reco=USOF6$RECO,PPFD=USOF6$PAR,PPFD_ref=2000)



#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USOF6))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USOF6)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USOF6)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))
view(USOF6)
