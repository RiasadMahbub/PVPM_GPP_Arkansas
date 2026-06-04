### USOF5
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
library(minpack.lm)

## Read the data

myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =12))
USOF5<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =12, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USOF5$`Date Time`<-gsub("'","",USOF5$`Date Time`)
USOF5$datetime<-strptime(USOF5$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF5$Date<-as.Date(USOF5$datetime)

##ADD the DOY column
USOF5$DOY<- yday(USOF5$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2018-04-15")

USOF5<-filter(USOF5, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF5$DOY)- min(USOF5$DOY))/8)

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
  dflist[[i]]<- subset(USOF5, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSOF52018<--rep(0, points)
rsquareUSOF52018<--rep(0, points)
residualsumofsquareUSOF52018<--rep(0, points)
rmseUSOF52018<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.04308 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF52018[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF52018[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF52018[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF52018[i] <-  1-(residualsumofsquareUSOF52018[i]/sum((y - mean(y))^2))
}



stderrorUSOF5



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c(-1,-2,-3, -(points+1))]), as.numeric(LUEmax2015[c(-1,-2,-3)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF5 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USOF5, USOF5$NEE_modeled, Reco=USOF5$Reco_modeled,PPFD=USOF5$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[10]], dflist[[8]]$NEE_modeled, Reco=dflist[[8]]$Reco_modeled,PPFD=dflist[[8]]$PAR_Regression,PPFD_ref=2000)

dflist[[1]]



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
par(mar=c(2, 3, 2, 2))
plot(DOYlistway32015[-17], toptvector,  xlab="DOY", cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     
     ylab="Tpot", main = "USOF1 2015 (95 percentile method)")
topt



##Create a dataframe
USOF5LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF5", "year" = 2018, "STD_Error" = stderrorUSOF52018, "R-squared" = rsquareUSOF52018, "RMSE" = rmseUSOF52018, "Residual sum of square" = residualsumofsquareUSOF52018, "Topt" = toptvector)
view(USOF5LUEmax)


USOF5LUEmax


### Estimationn of topt for 



#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

## Daily mean
USOF5.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF5, mean)
View(USOF5)


#Second to daily
USOF5.daily$GPP_modeled<- (USOF5.daily$GPP_modeled)* 1e-6*86400*12.011
USOF5.daily$PAR_Regression<- (USOF5.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USOF5.daily$Date, USOF5.daily$GPP_modeled)

USOF5.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF5.daily, mean)
plot(USOF5.8day$`cut(Date, "8 days")`, USOF5.8day$GPP_modeled)





###CHANGING THE COLUMNN NNAMES TO SITE

USOF5.8day$Date<-USOF5.8day$`cut(Date, "8 days")`
USOF5.8day$GPP_site<-USOF5.8day$GPP_modeled
USOF5.8day$PAR_site<-USOF5.8day$PAR_Regression
USOF5.8day$VPD_site<-USOF5.8day$VPD_REddyProc
USOF5.8day$Tair_site<-USOF5.8day$Ta_REddyProc

USOF5.8day<-USOF5.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)

###CHANGING THE COLUMNN NNAMES TO SITE

USOF5.8day$doy<-yday(USOF5.8day$Date)

USOF5.8day<-USOF5.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF5.8day


library(formattable)


USOF5.8day$GPP_site<- formattable(USOF5.8day$GPP_site, digits = 2, format = "f")
USOF5.8day$PAR_site<- formattable(USOF5.8day$PAR_site, digits = 2, format = "f")
USOF5.8day$Tair_site<- formattable(USOF5.8day$Tair_site, digits = 2, format = "f")
USOF5.8day$VPD_site<- formattable(USOF5.8day$VPD_site, digits = 2, format = "f")

USOF5.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USOF5VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USOF52018.csv")
#USOF5VI$Date<-as.Date(USOF5VI$Time, "%b %d, %Y")
USOF5VI$Tair_satellite<-((USOF5VI$Temp+USOF5VI$Tmax)/2)-273.16
USOF5VI$doy<-(USOF5VI$doy)+1


USOF5VI8day<-merge(x = USOF5.8day, y = USOF5VI, by = "doy", all = TRUE)

USOF5LUEmax$doy<- USOF5LUEmax$DOY
USOF5VI8day<-merge(x = USOF5VI8day, y = USOF5LUEmax, by = "doy", all = TRUE)

USOF5VI8day$DOP = 99
USOF5VI8day$DAP = USOF5VI8day$doy - USOF5VI8day$DOP

a = -1972
b= -3537
c=  68.5211
d = 0.0764
x= USOF5VI8day$DAP
x
USOF5VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USOF5VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USOF5VI8day$DAP, USOF5VI8day$LUEmaxmodeled)
plot(USOF5VI8day$DAP, USOF5VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USOF5VI8day$Tair_satellite
Toptcal<-USOF5VI8day$Topt
Tsite<- USOF5VI8day$Tair_site
Toptmodeled<-USOF5VI8day$Toptmodeled
USOF5VI8day$Fapar <- (USOF5VI8day$EVI_SG - 0.1)*1.25
USOF5VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USOF5VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USOF5VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USOF5VI8day$DOP <- 99
USOF5VI8day$DOH <- 241
USOF5VI8day$DAP<-USOF5VI8day$doy-USOF5VI8day$DOP
View(USOF5VI8day)





USOF5VI8day$Ws<- (1+USOF5VI8day$LSWI_SG)/((1+max(USOF5VI8day$LSWI_SG)))
USOF5VI8day$LUEvpm<- (USOF5VI8day$Tsvpm)*(USOF5VI8day$Ws)*(0.05)
USOF5VI8day$LUEpvpmcal<-(USOF5VI8day$Tspvpm_cal)*(USOF5VI8day$Ws)*(USOF5VI8day$LUEmax)
USOF5VI8day$LUEpvpmmodeled<- (USOF5VI8day$Tspvpm_modeled)*(USOF5VI8day$Ws)*(USOF5VI8day$LUEmaxmodeled)


USOF5VI8day$GPPvpm<- USOF5VI8day$LUEvpm*USOF5VI8day$Fapar*USOF5VI8day$PAR_site*12.011
USOF5VI8day$GPPpvpmcal<- USOF5VI8day$LUEmax*USOF5VI8day$Fapar*USOF5VI8day$PAR_site*12.011
USOF5VI8day$GPPpvpmmodel<- USOF5VI8day$LUEmaxmodeled*USOF5VI8day$Fapar*USOF5VI8day$PAR_site*12.011


plot(USOF5VI8day$GPP_site, USOF5VI8day$GPPvpm)
plot(USOF5VI8day$GPP_site, USOF5VI8day$GPPpvpmcal)
plot(USOF5VI8day$GPP_site, USOF5VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USOF5VI8day$GPP_site, USOF5VI8day$GPPvpm, na.rm=TRUE)
rmse(USOF5VI8day$GPP_site, USOF5VI8day$GPPpvpmcal)
rmse(USOF5VI8day$GPP_site, USOF5VI8day$GPPpvpmmodel)



ncol(USOF5VI8day)


##dropping the null values
USOF5VI8day<-USOF5VI8day[!is.na(USOF5VI8day$GPP_site),]

p<-ggplot(data=USOF5VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF5 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF5VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USOF5VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF5 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF5PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USOF5VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF5 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF5PVPMmodeled.jpeg", width = 18, height = 7)






















########################################################
USOF5<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Fairley-RunkleReba_07-2021.xlsx", sheet =6)


## Subsetting
## Dropping the first 8 rows
USOF5<-USOF5[-1:-7, ]

## Make the first row as the header row
names(USOF5)<-USOF5[1, ]
USOF5<-USOF5[-1, ]

## Drop the first 7 columns 
USOF5<-USOF5[, -1:-7]
## Selecting 4 columns of USOF5
USOF5<-USOF5[, c(1, 7,8,9,21,26, 24, 22)]

## Replace all the -9999 values by NA (missing) labels
USOF5<-USOF5%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USOF5$time <- as.numeric(substr(USOF5$TIMESTAMP_END, 9, 12))

# formatting time stamp
USOF5$datetime <-ymd_hms(USOF5$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USOF5$SW_IN<-as.numeric(USOF5$SW_IN)
USOF5$FC_F<-as.numeric(USOF5$FC_F)
USOF5$RH<-as.numeric(USOF5$RH)
USOF5$TA<-as.numeric(USOF5$TA)
USOF5$time<-as.numeric(USOF5$time)
USOF5$FC_F<-as.numeric(USOF5$FC_F)
USOF5$RECO<-as.numeric(USOF5$RECO)
USOF5$GPP<-as.numeric(USOF5$GPP)



### 2018
#plantingdate: april1 -
## harvesting date: sep 18-20
## modis: 2018 05, 09


### 2018
#plantingdate: april1 -
## harvesting date: sep 18-20
## modis: 2018 06 13, 2018 06 18 0618 DOY= 169-258
####
USOF5$DOY<- yday(USOF5$datetime)
## converting swin to par by multipying 2.02
USOF5$PAR<-USOF5$SW_IN*2.02

### MODIS image nearest one starts at 137 day May 17
USOF5<-filter(USOF5, DOY>=123)
###2015
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
(max(USOF5$DOY)-min(USOF5$DOY))/8

DOYlistway32015<-rep(0, 11)
LUEmax2015<-rep(0,11)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 123
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:11){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF5, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:11){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:11)){
  y<-dflist[[i]]$GPP
  x<-dflist[[i]]$PAR
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.04471,g= 30))
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
     ylab="LUEmax", main = "USOF5 2018")



y<-dflist[[1]]$GPP
x<-dflist[[1]]$PAR
modellist[[1]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.04471,g= 1000))
modellist[1]
plot(dflist[[1]]$PAR, dflist[[2]]$GPP)
plot(USBDA_2015$PPFD_IN, USBDA_2015$GPP)

dflist

y<-USOF5$GPP
x<-USOF5$PAR
nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.05,g= 30))
LUEmax2015<-modellist[[i]]$m$getPars()[1]



USOF5_complete <- na.omit(USOF5)
USOF5_complete$NEE<-as.numeric(USOF5_complete$FC_F)

library(bigleaf)
modellist[[i]]light.response(data = dflist[[5]], NEE=dflist[[5]]$FC_F, Reco=dflist[[5]]$RECO,PPFD=dflist[[5]]$PAR,PPFD_ref=2000)

plot(USOF5$PAR, USOF5$GPP)





#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USOF5))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USOF5)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USOF5)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))
