#### USBDC
#### 2016


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


myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =15))
USBDA2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =15, col_names = myCols)


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

USBDA2016<-filter(USBDA2016, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USBDA2016$DOY)- min(USBDA2016$DOY))/8)

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
  dflist[[i]]<- subset(USBDA2016, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSBDA2016<--rep(0, points)
rsquareUSBDA2016<--rep(0, points)
residualsumofsquareUSBDA2016<--rep(0, points)
rmseUSBDA2016<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.0748 ,g= 27.1498))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSBDA2016[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSBDA2016[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSBDA2016[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSBDA2016[i] <-  1-(residualsumofsquareUSBDA2016[i]/sum((y - mean(y))^2))
}


stderrorUSBDA2016



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USBDA2016 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USBDA2016, USBDA2016$NEE_modeled, Reco=USBDA2016$Reco_modeled,PPFD=USBDA2016$PAR_Regression,PPFD_ref=2000)
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
USBDA2016LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USBDA", "year" = 2016, "STD_Error" = stderrorUSBDA2016, "R-squared" = rsquareUSBDA2016, "RMSE" = rmseUSBDA2016, "Residual sum of square" = residualsumofsquareUSBDA2016, "Topt"= toptvector)
view(USBDA2016LUEmax)


USBDA2016LUEmax
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USBDA2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USBDA2016, mean)


#Second to daily
USBDA2016.daily$GPP_modeled<- ( USBDA2016.daily$GPP_modeled)* 1e-6*86400*12.011
USBDA2016.daily$PAR_Regression<- ( USBDA2016.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot( USBDA2016.daily$Date,  USBDA2016.daily$GPP_modeled)

USBDA2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USBDA2016.daily, mean)
plot( USBDA2016.8day$`cut(Date, "8 days")`,  USBDA2016.8day$GPP_modeled)


###
###CHANGING THE COLUMN NAMES TO SITE

USBDA2016.8day$Date<-USBDA2016.8day$`cut(Date, "8 days")`
USBDA2016.8day$GPP_site<-USBDA2016.8day$GPP_modeled
USBDA2016.8day$PAR_site<-USBDA2016.8day$PAR_Regression
USBDA2016.8day$VPD_site<-USBDA2016.8day$VPD_REddyProc
USBDA2016.8day$Tair_site<-USBDA2016.8day$Ta_REddyProc

USBDA2016.8day<-USBDA2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)



USBDA2016.8day$doy<-yday(USBDA2016.8day$Date)

USBDA2016.8day<-USBDA2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDA2016.8day


library(formattable)


USBDA2016.8day$GPP_site<- formattable(USBDA2016.8day$GPP_site, digits = 2, format = "f")
USBDA2016.8day$PAR_site<- formattable(USBDA2016.8day$PAR_site, digits = 2, format = "f")
USBDA2016.8day$Tair_site<- formattable(USBDA2016.8day$Tair_site, digits = 2, format = "f")
USBDA2016.8day$VPD_site<- formattable(USBDA2016.8day$VPD_site, digits = 2, format = "f")

USBDA2016.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USBDA2016VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USBDA2016.csv")
#USBDA2016VI$Date<-as.Date(USBDA2016VI$Time, "%b %d, %Y")
USBDA2016VI$Tair_satellite<-((USBDA2016VI$Temp+USBDA2016VI$Tmax)/2)-273.16
USBDA2016VI$doy<-(USBDA2016VI$doy)+1 


USBDA2016VI8day<-merge(x = USBDA2016.8day, y = USBDA2016VI, by = "doy", all = TRUE)

USBDA2016LUEmax$doy<- USBDA2016LUEmax$DOY
USBDA2016VI8day<-merge(x = USBDA2016VI8day, y = USBDA2016LUEmax, by = "doy", all = TRUE)

USBDA2016VI8day$DOP = 82
USBDA2016VI8day$DAP = USBDA2016VI8day$doy - USBDA2016VI8day$DOP

a = -1972
b= -3537
c=  82.5211
d = 0.05764
x= USBDA2016VI8day$DAP
x
USBDA2016VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USBDA2016VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USBDA2016VI8day$DAP, USBDA2016VI8day$LUEmaxmodeled)
plot(USBDA2016VI8day$DAP, USBDA2016VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USBDA2016VI8day$Tair_satellite
Toptcal<-USBDA2016VI8day$Topt
Tsite<- USBDA2016VI8day$Tair_site
Toptmodeled<-USBDA2016VI8day$Toptmodeled
USBDA2016VI8day$Fapar <- (USBDA2016VI8day$EVI_SG - 0.1)*1.25
USBDA2016VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USBDA2016VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USBDA2016VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USBDA2016VI8day$DOP <- 82
USBDA2016VI8day$DAP<-USBDA2016VI8day$doy-USBDA2016VI8day$DOP






USBDA2016VI8day$Ws<- (1+USBDA2016VI8day$LSWI_SG)/((1+max(USBDA2016VI8day$LSWI_SG)))
USBDA2016VI8day$LUEvpm<- (USBDA2016VI8day$Tsvpm)*(USBDA2016VI8day$Ws)*(0.05)
USBDA2016VI8day$LUEpvpmcal<-(USBDA2016VI8day$Tspvpm_cal)*(USBDA2016VI8day$Ws)*(USBDA2016VI8day$LUEmax)
USBDA2016VI8day$LUEpvpmmodeled<- (USBDA2016VI8day$Tspvpm_modeled)*(USBDA2016VI8day$Ws)*(USBDA2016VI8day$LUEmaxmodeled)


USBDA2016VI8day$GPPvpm<- USBDA2016VI8day$LUEvpm*USBDA2016VI8day$Fapar*USBDA2016VI8day$PAR_site*12.011
USBDA2016VI8day$GPPpvpmcal<- USBDA2016VI8day$LUEmax*USBDA2016VI8day$Fapar*USBDA2016VI8day$PAR_site*12.011
USBDA2016VI8day$GPPpvpmmodel<- USBDA2016VI8day$LUEmaxmodeled*USBDA2016VI8day$Fapar*USBDA2016VI8day$PAR_site*12.011


plot(USBDA2016VI8day$GPP_site, USBDA2016VI8day$GPPvpm)
plot(USBDA2016VI8day$GPP_site, USBDA2016VI8day$GPPpvpmcal)
plot(USBDA2016VI8day$GPP_site, USBDA2016VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USBDA2016VI8day$GPP_site, USBDA2016VI8day$GPPvpm, na.rm=TRUE)
rmse(USBDA2016VI8day$GPP_site, USBDA2016VI8day$GPPpvpmcal)
rmse(USBDA2016VI8day$GPP_site, USBDA2016VI8day$GPPpvpmmodel)



ncol(USBDA2016VI8day)


##dropping the null values
USBDA2016VI8day<-USBDA2016VI8day[!is.na(USBDA2016VI8day$GPP_site),]

p<-ggplot(data=USBDA2016VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2016 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDA2016VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USBDA2016VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2016 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDA2016PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USBDA2016VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2016 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDA2016PVPMmodeled.jpeg", width = 18, height = 7)






##########################################################
USBDA_2016<-read_excel("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Stacener ZG-RunkleReba_07-2021.xlsx", sheet =4)


## Subsetting
## Dropping the first 8 rows
USBDA_2016<-USBDA_2016[-1:-7, ]

## Make the first row as the header row
names(USBDA_2016)<-USBDA_2016[1, ]
USBDA_2016<-USBDA_2016[-1, ]

## Drop the first 7 columns 
USBDA_2016<-USBDA_2016[, -1:-7]
## Selecting 4 columns of USOF1
USBDA_2016<-USBDA_2016[, c(1, 6, 7,8,20,21,25, 23)]

## Replace all the -9999 values by NA (missing) labels
USBDA_2016<-USBDA_2016%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USBDA_2016$time <- as.numeric(substr(USBDA_2016$TIMESTAMP_END, 9, 12))

# formatting time stamp
USBDA_2016$datetime <-ymd_hms(USBDA_2016$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USBDA_2016$SW_IN<-as.numeric(USBDA_2016$SW_IN)
USBDA_2016$FC_F<-as.numeric(USBDA_2016$FC_F)
USBDA_2016$RH<-as.numeric(USBDA_2016$RH)
USBDA_2016$TA<-as.numeric(USBDA_2016$TA)
USBDA_2016$time<-as.numeric(USBDA_2016$time)
USBDA_2016$FC_F<-as.numeric(USBDA_2016$FC_F)
USBDA_2016$RECO<-as.numeric(USBDA_2016$RECO)
USBDA_2016$GPP<-as.numeric(USBDA_2016$GPP)


#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USBDA_2016))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USBDA_2016)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USBDA_2016)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))

