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


#######
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =17))
USBDCC2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =17, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USBDCC2016$`Date Time`<-gsub("'","",USBDCC2016$`Date Time`)
USBDCC2016$datetime<-strptime(USBDCC2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USBDCC2016$Date<-as.Date(USBDCC2016$datetime)

##ADD the DOY column
USBDCC2016$DOY<- yday(USBDCC2016$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-03-29")

USBDCC2016<-filter(USBDCC2016, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USBDCC2016$DOY)- min(USBDCC2016$DOY))/8)

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
  dflist[[i]]<- subset(USBDCC2016, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>40)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSBDC2016<--rep(0, points)
rsquareUSBDC2016<--rep(0, points)
residualsumofsquareUSBDC2016<--rep(0, points)
rmseUSBDC2016<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.04173 ,g= 30.95749))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSBDC2016[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSBDC2016[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSBDC2016[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSBDC2016[i] <-  1-(residualsumofsquareUSBDC2016[i]/sum((y - mean(y))^2))
}



stderrorUSBDCC2016



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USBDC 2016")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USBDCC2016, USBDCC2016$NEE_modeled, Reco=USBDCC2016$Reco_modeled,PPFD=USBDCC2016$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[17]], dflist[[17]]$NEE_modeled, Reco=dflist[[17]]$Reco_modeled,PPFD=dflist[[17]]$PAR_Regression,PPFD_ref=2000)

par(mfrow = c(5, 4))
par(mar=c(4, 4, 4, 4))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5) 
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
USBDCC2016LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USBDC", "year" = 2016, "STD_Error" = stderrorUSBDC2016, "R-squared" = rsquareUSBDC2016, "RMSE" = rmseUSBDC2016, "Residual sum of square" = residualsumofsquareUSBDC2016, "Topt" = toptvector)
view(USBDCC2016LUEmax)


USBDCC2016LUEmax


#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USBDCC2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USBDCC2016, mean)


#Second to daily
USBDCC2016.daily$GPP_modeled<- ( USBDCC2016.daily$GPP_modeled)* 1e-6*86400*12.011
USBDCC2016.daily$PAR_Regression<- ( USBDCC2016.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot( USBDCC2016.daily$Date,  USBDCC2016.daily$GPP_modeled)

USBDCC2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USBDCC2016.daily, mean)
plot( USBDCC2016.8day$`cut(Date, "8 days")`,  USBDCC2016.8day$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE
USBDC2016.8day<--USBDCC2016.8day
USBDCC2016.8day$Date<-USBDCC2016.8day$`cut(Date, "8 days")`
USBDCC2016.8day$GPP_site<-USBDCC2016.8day$GPP_modeled
USBDCC2016.8day$PAR_site<-USBDCC2016.8day$PAR_Regression
USBDCC2016.8day$VPD_site<-USBDCC2016.8day$VPD_REddyProc
USBDCC2016.8day$Tair_site<-USBDCC2016.8day$Ta_REddyProc

USBDC2016.8day<-USBDC2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)


USBDCC2016.8day$doy<-yday(USBDCC2016.8day$Date)

USBDCC2016.8day<-USBDCC2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USBDCC2016.8day


library(formattable)


USBDCC2016.8day$GPP_site<- formattable(USBDCC2016.8day$GPP_site, digits = 2, format = "f")
USBDCC2016.8day$PAR_site<- formattable(USBDCC2016.8day$PAR_site, digits = 2, format = "f")
USBDCC2016.8day$Tair_site<- formattable(USBDCC2016.8day$Tair_site, digits = 2, format = "f")
USBDCC2016.8day$VPD_site<- formattable(USBDCC2016.8day$VPD_site, digits = 2, format = "f")

USBDCC2016.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USBDCC2016VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USBDC2016.csv")
#USBDCC2016VI$Date<-as.Date(USBDCC2016VI$Time, "%b %d, %Y")
USBDCC2016VI$Tair_satellite<-((USBDCC2016VI$Temp+USBDCC2016VI$Tmax)/2)-273.16
USBDCC2016VI$doy<-(USBDCC2016VI$doy) + 1


USBDCC2016VI8day<-merge(x = USBDCC2016.8day, y = USBDCC2016VI, by = "doy", all = TRUE)

USBDCC2016LUEmax$doy<- USBDCC2016LUEmax$DOY
USBDCC2016VI8day<-merge(x = USBDCC2016VI8day, y = USBDCC2016LUEmax, by = "doy", all = TRUE)

USBDCC2016VI8day$DOP = 82
USBDCC2016VI8day$DAP = USBDCC2016VI8day$doy - USBDCC2016VI8day$DOP

a = -1972
b= -3709
c=  68.5211
d = 0.0764
x= USBDCC2016VI8day$DAP
x
USBDCC2016VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USBDCC2016VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USBDCC2016VI8day$DAP, USBDCC2016VI8day$LUEmaxmodeled)
plot(USBDCC2016VI8day$DAP, USBDCC2016VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USBDCC2016VI8day$Tair_satellite
Toptcal<-USBDCC2016VI8day$Topt
Tsite<- USBDCC2016VI8day$Tair_site
Toptmodeled<-USBDCC2016VI8day$Toptmodeled
USBDCC2016VI8day$Fapar <- (USBDCC2016VI8day$EVI_SG - 0.1)*1.25
USBDCC2016VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USBDCC2016VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USBDCC2016VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USBDCC2016VI8day$DOP <- 82
USBDCC2016VI8day$DAP<-USBDCC2016VI8day$doy-USBDCC2016VI8day$DOP
View(USBDCC2016VI8day)





USBDCC2016VI8day$Ws<- (1+USBDCC2016VI8day$LSWI_SG)/((1+max(USBDCC2016VI8day$LSWI_SG)))
USBDCC2016VI8day$LUEvpm<- (USBDCC2016VI8day$Tsvpm)*(USBDCC2016VI8day$Ws)*(0.05)
USBDCC2016VI8day$LUEpvpmcal<-(USBDCC2016VI8day$Tspvpm_cal)*(USBDCC2016VI8day$Ws)*(USBDCC2016VI8day$LUEmax)
USBDCC2016VI8day$LUEpvpmmodeled<- (USBDCC2016VI8day$Tspvpm_modeled)*(USBDCC2016VI8day$Ws)*(USBDCC2016VI8day$LUEmaxmodeled)


USBDCC2016VI8day$GPPvpm<- USBDCC2016VI8day$LUEvpm*USBDCC2016VI8day$Fapar*USBDCC2016VI8day$PAR_site*12.011
USBDCC2016VI8day$GPPpvpmcal<- USBDCC2016VI8day$LUEmax*USBDCC2016VI8day$Fapar*USBDCC2016VI8day$PAR_site*12.011
USBDCC2016VI8day$GPPpvpmmodel<- USBDCC2016VI8day$LUEmaxmodeled*USBDCC2016VI8day$Fapar*USBDCC2016VI8day$PAR_site*12.011


plot(USBDCC2016VI8day$GPP_site, USBDCC2016VI8day$GPPvpm)
plot(USBDCC2016VI8day$GPP_site, USBDCC2016VI8day$GPPpvpmcal)
plot(USBDCC2016VI8day$GPP_site, USBDCC2016VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USBDCC2016VI8day$GPP_site, USBDCC2016VI8day$GPPvpm, na.rm=TRUE)
rmse(USBDCC2016VI8day$GPP_site, USBDCC2016VI8day$GPPpvpmcal)
rmse(USBDCC2016VI8day$GPP_site, USBDCC2016VI8day$GPPpvpmmodel)



ncol(USBDCC2016VI8day)


##dropping the null values
USBDCC2016VI8day<-USBDCC2016VI8day[!is.na(USBDCC2016VI8day$GPP_site),]

p<-ggplot(data=USBDCC2016VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDC 2016 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDCC2016VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USBDCC2016VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDC 2016 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDCC2016PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USBDCC2016VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDC 2016 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USBDCC2016PVPMmodeled.jpeg", width = 18, height = 7)

















###########################
USBDC_2016<-read_excel("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Stacener ZG-RunkleReba_07-2021.xlsx", skip =1, sheet =5)



## Subsetting
## Dropping the first 8 rows
USBDC_2016<-USBDC_2016[-1:-7, ]

## Make the first row as the header row
names(USBDC_2016)<-USBDC_2016[1, ]
USBDC_2016<-USBDC_2016[-1, ]

## Drop the first 7 columns 
USBDC_2016<-USBDC_2016[, -1:-7]
## Selecting 4 columns of USOF1
USBDC_2016<-USBDC_2016[, c(1, 6, 7,8,20,21,25, 23)]

## Replace all the -9999 values by NA (missing) labels
USBDC_2016<-USBDC_2016%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USBDC_2016$time <- as.numeric(substr(USBDC_2016$TIMESTAMP_END, 9, 12))

# formatting time stamp
USBDC_2016$datetime <-ymd_hms(USBDC_2016$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USBDC_2016$SW_IN<-as.numeric(USBDC_2016$SW_IN)
USBDC_2016$FC_F<-as.numeric(USBDC_2016$FC_F)
USBDC_2016$RH<-as.numeric(USBDC_2016$RH)
USBDC_2016$TA<-as.numeric(USBDC_2016$TA)
USBDC_2016$time<-as.numeric(USBDC_2016$time)
USBDC_2016$FC_F<-as.numeric(USBDC_2016$FC_F)
USBDC_2016$RECO<-as.numeric(USBDC_2016$RECO)
USBDC_2016$GPP<-as.numeric(USBDC_2016$GPP)




#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USBDC_2016))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USBDC_2016)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USBDC_2016)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))

view(USBDC_2016)
