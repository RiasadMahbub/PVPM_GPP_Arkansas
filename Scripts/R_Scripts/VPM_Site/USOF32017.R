### USOF3
## 2018

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
library(latticeExtra)

## Read the data
### USOF3 is in the sheet 10
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_04-05-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =10))
USOF3<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_04-05-2022.xlsx", 
                  skip= 2, sheet =10, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USOF3$`Date Time`<-gsub("'","",USOF3$`Date Time`)
USOF3$datetime<-strptime(USOF3$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF3$Date<-as.Date(USOF3$datetime)

##ADD the DOY column
USOF3$DOY<- yday(USOF3$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-97

USOF3<-filter(USOF3, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF3$DOY)- min(USOF3$DOY))/8)

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
  dflist[[i]]<- subset(USOF3, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSOF32017<--rep(0, points)
rsquareUSOF32017<--rep(0, points)
residualsumofsquareUSOF32017<--rep(0, points)
rmseUSOF32017<--rep(0, points)
for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.06797 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF32017[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF32017[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF32017[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF32017[i] <-  1-(residualsumofsquareUSOF32017[i]/sum((y - mean(y))^2))
}








avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c(-1,-2,-3, -(points+1))]), as.numeric(LUEmax2015[c(-1,-2,-3)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF3 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

par(mfrow = c(5, 4))
par(mar=c(4, 4, 4, 4))
par(mar=c(1,1,1,1))
jpeg("USOF32017.jpeg", width = 10, height = 12, units = 'in', res = 900)
par(mfrow = c(5, 4))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}
dev.off()
par(mfrow = c(5, 4))
##Big leaf
light.response(data = USOF3, USOF3$NEE_modeled, Reco=USOF3$Reco_modeled,PPFD=USOF3$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[10]], dflist[[8]]$NEE_modeled, Reco=dflist[[8]]$Reco_modeled,PPFD=dflist[[8]]$PAR_Regression,PPFD_ref=2000)



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


##Create a dataframe
USOF3LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF3", "year" = 2017,  "STD_Error" = stderrorUSOF32017, "R-squared" = rsquareUSOF32017, "RMSE" = rmseUSOF32017, "Residual sum of square" = residualsumofsquareUSOF32017,   "Topt" = toptvector)
view(USOF3LUEmax)


USOF3LUEmax


### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

## Daily mean
USOF3.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF3, mean)
View(USOF3)


#Second to daily
USOF3.daily$GPP_modeled<- (USOF3.daily$GPP_modeled)* 1e-6*86400*12.011
USOF3.daily$PAR_Regression<- (USOF3.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USOF3.daily$Date, USOF3.daily$GPP_modeled)

USOF3.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF3.daily, mean)
plot(USOF3.8day$`cut(Date, "8 days")`, USOF3.8day$GPP_modeled)

###CHANGING THE COLUMNN NNAMES TO SITE

USOF3.8day$Date<-USOF3.8day$`cut(Date, "8 days")`
USOF3.8day$GPP_site<-USOF3.8day$GPP_modeled
USOF3.8day$PAR_site<-USOF3.8day$PAR_Regression
USOF3.8day$VPD_site<-USOF3.8day$VPD_REddyProc
USOF3.8day$Tair_site<-USOF3.8day$Ta_REddyProc

USOF3.8day<-USOF3.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)

###CHANGING THE COLUMNN NNAMES TO SITE

USOF3.8day$doy<-yday(USOF3.8day$Date)

USOF3.8day<-USOF3.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF3.8day


library(formattable)


USOF3.8day$GPP_site<- formattable(USOF3.8day$GPP_site, digits = 2, format = "f")
USOF3.8day$PAR_site<- formattable(USOF3.8day$PAR_site, digits = 2, format = "f")
USOF3.8day$Tair_site<- formattable(USOF3.8day$Tair_site, digits = 2, format = "f")
USOF3.8day$VPD_site<- formattable(USOF3.8day$VPD_site, digits = 2, format = "f")

USOF3.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USOF3VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USOF32017.csv")
#USOF3VI$Date<-as.Date(USOF3VI$Time, "%b %d, %Y")
USOF3VI$Tair_satellite<-((USOF3VI$Temp+USOF3VI$Tmax)/2)-273.16
USOF3VI$doy<-(USOF3VI$doy) + 1


USOF3VI8day<-merge(x = USOF3.8day, y = USOF3VI, by = "doy", all = TRUE)

USOF3LUEmax$doy<- USOF3LUEmax$DOY
USOF3VI8day<-merge(x = USOF3VI8day, y = USOF3LUEmax, by = "doy", all = TRUE)

USOF3VI8day$DOP = 91
USOF3VI8day$DOH = 260
USOF3VI8day$DAP = USOF3VI8day$doy - USOF3VI8day$DOP

a = -1972
b= -3537
c=  68.05
d = 0.0764
x= USOF3VI8day$DAP
x
USOF3VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USOF3VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USOF3VI8day$DAP, USOF3VI8day$LUEmaxmodeled)
plot(USOF3VI8day$DAP, USOF3VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USOF3VI8day$Tair_satellite
Toptcal<-USOF3VI8day$Topt
Tsite<- USOF3VI8day$Tair_site
Toptmodeled<-USOF3VI8day$Toptmodeled
USOF3VI8day$Fapar <- (USOF3VI8day$EVI_SG - 0.1)*1.25
USOF3VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USOF3VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USOF3VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USOF3VI8day$DOP <- 91
USOF3VI8day$DAP<-USOF3VI8day$doy-USOF3VI8day$DOP
View(USOF3VI8day)





USOF3VI8day$Ws<- (1+USOF3VI8day$LSWI_SG)/((1+max(USOF3VI8day$LSWI_SG)))
USOF3VI8day$LUEvpm<- (USOF3VI8day$Tsvpm)*(USOF3VI8day$Ws)*(0.05)
USOF3VI8day$LUEpvpmcal<-(USOF3VI8day$Tspvpm_cal)*(USOF3VI8day$Ws)*(USOF3VI8day$LUEmax)
USOF3VI8day$LUEpvpmmodeled<- (USOF3VI8day$Tspvpm_modeled)*(USOF3VI8day$Ws)*(USOF3VI8day$LUEmaxmodeled)


USOF3VI8day$GPPvpm<- USOF3VI8day$LUEvpm*USOF3VI8day$Fapar*USOF3VI8day$PAR_site*12.011
USOF3VI8day$GPPpvpmcal<- USOF3VI8day$LUEmax*USOF3VI8day$Fapar*USOF3VI8day$PAR_site*12.011
USOF3VI8day$GPPpvpmmodel<- USOF3VI8day$LUEmaxmodeled*USOF3VI8day$Fapar*USOF3VI8day$PAR_site*12.011


plot(USOF3VI8day$GPP_site, USOF3VI8day$GPPvpm)
plot(USOF3VI8day$GPP_site, USOF3VI8day$GPPpvpmcal)
plot(USOF3VI8day$GPP_site, USOF3VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USOF3VI8day$GPP_site, USOF3VI8day$GPPvpm, na.rm=TRUE)
rmse(USOF3VI8day$GPP_site, USOF3VI8day$GPPpvpmcal)
rmse(USOF3VI8day$GPP_site, USOF3VI8day$GPPpvpmmodel)



ncol(USOF3VI8day)


##dropping the null values
USOF3VI8day<-USOF3VI8day[!is.na(USOF3VI8day$GPP_site),]

p<-ggplot(data=USOF3VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF3 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF3VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USOF3VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF3 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF3PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USOF3VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF3 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF3PVPMmodeled.jpeg", width = 18, height = 7)










#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USOF3))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USOF3)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USOF3)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))
view(USOF3
     )





##












####################################
## Subsetting
## Dropping the first 8 rows
USOF3<-USOF3[-1:-7, ]

## Make the first row as the header row
names(USOF3)<-USOF3[1, ]
USOF3<-USOF3[-1, ]

## Drop the first 7 columns 
USOF3<-USOF3[, -1:-7]
## Selecting 4 columns of USOF3
USOF3<-USOF3[, c(1, 7,8,9,21,26, 24, 22)]

## Replace all the -9999 values by NA (missing) labels
USOF3<-USOF3%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USOF3$time <- as.numeric(substr(USOF3$TIMESTAMP_END, 9, 12))

# formatting time stamp
USOF3$datetime <-ymd_hms(USOF3$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype

USOF3$SW_IN<-as.numeric(USOF3$SW_IN)
USOF3$FC_F<-as.numeric(USOF3$FC_F)
USOF3$RH<-as.numeric(USOF3$RH)
USOF3$TA<-as.numeric(USOF3$TA)
USOF3$time<-as.numeric(USOF3$time)
USOF3$FC_F<-as.numeric(USOF3$FC_F)
USOF3$RECO<-as.numeric(USOF3$RECO)
USOF3$GPP<-as.numeric(USOF3$GPP)


###
#plantingdate: april1 -
## MODIS Image: DOY 193 2017-07-12
####
##ADD doy to the dataframe
USOF3$DOY<- yday(USOF3$datetime)
## converting swin to par by multipying 2.02
USOF3$PAR<-USOF3$SW_IN*2.02

### MODIS image nearest one starts at 137 day May 17
USOF3<-filter(USOF3, DOY>=193)


###
(max(USOF3$DOY)-min(USOF3$DOY))/8
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
DOYlistway32015<-rep(0, 6)
LUEmax2015<-rep(0,5)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 193
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:5){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF3, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:5){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:5)){
  y<-dflist[[i]]$GPP
  x<-dflist[[i]]$PAR
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.04692,g= 41.27126))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

DOYlistway32015
LUEmax2015
plot(as.numeric(LUEmax2015))


light.response(data = USOF3, NEE=USOF3$FC_F, Reco=USOF3$RECO,PPFD=USOF3$PAR,PPFD_ref=2000)


y<-dflist[[1]]$GPP
x<-dflist[[1]]$PAR
modellist[[1]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.06009,g= 43.27126))
modellist[1]
plot(dflist[[1]]$PAR, dflist[[1]]$GPP)
plot(USOF3$PAR, USOF3$GPP)

dflist

y<-USOF3$GPP
x<-USOF3$PAR
nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.05,g= 30))
LUEmax2015<-modellist[[i]]$m$getPars()[2]



USOF3_complete <- na.omit(USOF3)
USOF3_complete$NEE<-as.numeric(USOF3_complete$FC_F)

library(bigleaf)
light.response(data = USOF3, NEE=USOF3$FC_F, Reco=USOF3$RECO,PPFD=USOF3$PAR,PPFD_ref=2000)

### MODIFIED ARRHENIUS FUNCTION:


view(USOF3)

