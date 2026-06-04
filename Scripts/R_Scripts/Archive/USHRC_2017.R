## USHRC 2017

#PAGE 4


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


myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =15))
USHRC_2017<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =15, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USHRC_2017$`Date Time`<-gsub("'","",USHRC_2017$`Date Time`)
USHRC_2017$datetime<-strptime(USHRC_2017$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USHRC_2017$Date<-as.Date(USHRC_2017$datetime)

##ADD the DOY column
USHRC_2017$DOY<- yday(USHRC_2017$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2017-04-23")

USHRC_2017<-filter(USHRC_2017, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USHRC_2017$DOY)- min(USHRC_2017$DOY))/8)

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
  dflist[[i]]<- subset(USHRC_2017, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>40)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSHRC2017<--rep(0, points)
rsquareUSHRC2017<--rep(0, points)
residualsumofsquareUSHRC2017<--rep(0, points)
rmseUSHRC2017<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.035 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSHRC2017[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSHRC2017[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSHRC2017[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSHRC2017[i] <-  1-(residualsumofsquareUSHRC2017[i]/sum((y - mean(y))^2))
}

# for (i in (1:points)){
#   modellist[[i]]<-light.response(data = dflist[[i]], dflist[[i]]$NEE_modeled, Reco=dflist[[i]]$Reco_modeled,PPFD=dflist[[i]]$PAR_Regression,PPFD_ref=2000)
#   LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
#   stderrorUSHRC_2017[i]<-summary(modellist[[i]])$parameters[1,2]
# }
# 
# stderrorUSHRC_2017



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USHRC_2017 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USHRC_2017, USHRC_2017$NEE_modeled, Reco=USHRC_2017$Reco_modeled,PPFD=USHRC_2017$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[18]], dflist[[18]]$NEE_modeled, Reco=dflist[[18]]$Reco_modeled,PPFD=dflist[[18]]$PAR_Regression,PPFD_ref=2000)

plot(dflist[[17]]$PAR_Regression, dflist[[17]]$GPP_modeled)

par(mfrow = c(5,4))
par(mar=c(1, 1, 1, 1))

for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}


dim(dflist[[15]])

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
USHRC_2017LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USHRC", "year" = 2017, "STD_Error" = stderrorUSHRC2017, "R-squared" = rsquareUSHRC2017, "RMSE" = rmseUSHRC2017, "Residual sum of square" = residualsumofsquareUSHRC2017, "Topt"= toptvector)
view(USHRC_2017LUEmax)


USHRC_2017LUEmax





##########################################################
## 8 day mean


#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USHRC_2017.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRC_2017, mean)


#Second to daily
USHRC_2017.daily$GPP_modeled<- ( USHRC_2017.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC_2017.daily$PAR_Regression<- ( USHRC_2017.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot( USHRC_2017.daily$Date,  USHRC_2017.daily$GPP_modeled)

USHRC_2017.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRC_2017.daily, mean)
plot( USHRC_2017.8day$`cut(Date, "8 days")`,  USHRC_2017.8day$GPP_modeled)


###CHANGING THE COLUMN NAMES TO SITE

USHRC_2017.8day$Date<-USHRC_2017.8day$`cut(Date, "8 days")`
USHRC_2017.8day$GPP_site<-USHRC_2017.8day$GPP_modeled
USHRC_2017.8day$PAR_site<-USHRC_2017.8day$PAR_Regression
USHRC_2017.8day$VPD_site<-USHRC_2017.8day$VPD_REddyProc
USHRC_2017.8day$Tair_site<-USHRC_2017.8day$Ta_REddyProc

USHRC_2017.8day<-USHRC_2017.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)



USHRC_2017.8day$doy<-yday(USHRC_2017.8day$Date)

USHRC_2017.8day<-USHRC_2017.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRC_2017.8day


library(formattable)


USHRC_2017.8day$GPP_site<- formattable(USHRC_2017.8day$GPP_site, digits = 2, format = "f")
USHRC_2017.8day$PAR_site<- formattable(USHRC_2017.8day$PAR_site, digits = 2, format = "f")
USHRC_2017.8day$Tair_site<- formattable(USHRC_2017.8day$Tair_site, digits = 2, format = "f")
USHRC_2017.8day$VPD_site<- formattable(USHRC_2017.8day$VPD_site, digits = 2, format = "f")

USHRC_2017.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USHRC_2017VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/Way42017.csv")
#USHRC_2017VI$Date<-as.Date(USHRC_2017VI$Time, "%b %d, %Y")
USHRC_2017VI$Tair_satellite<-((USHRC_2017VI$Temp+USHRC_2017VI$Tmax)/2)-273.16
USHRC_2017VI$doy<-(USHRC_2017VI$doy) + 1


USHRC_2017VI8day<-merge(x = USHRC_2017.8day, y = USHRC_2017VI, by = "doy", all = TRUE)

USHRC_2017LUEmax$doy<- USHRC_2017LUEmax$DOY
USHRC_2017VI8day<-merge(x = USHRC_2017VI8day, y = USHRC_2017LUEmax, by = "doy", all = TRUE)

USHRC_2017VI8day$DOP = 100
USHRC_2017VI8day$DAP = USHRC_2017VI8day$doy - USHRC_2017VI8day$DOP

a = -1972
b= -3537
c=  68.5211
d = 0.0764
x= USHRC_2017VI8day$DAP
x
USHRC_2017VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USHRC_2017VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USHRC_2017VI8day$DAP, USHRC_2017VI8day$LUEmaxmodeled)
plot(USHRC_2017VI8day$DAP, USHRC_2017VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USHRC_2017VI8day$Tair_satellite
Toptcal<-USHRC_2017VI8day$Topt
Tsite<- USHRC_2017VI8day$Tair_site
Toptmodeled<-USHRC_2017VI8day$Toptmodeled
USHRC_2017VI8day$Fapar <- (USHRC_2017VI8day$EVI_SG - 0.1)*1.25
USHRC_2017VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USHRC_2017VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USHRC_2017VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USHRC_2017VI8day$DOP <- 100
USHRC_2017VI8day$DAP<-USHRC_2017VI8day$doy-USHRC_2017VI8day$DOP
View(USHRC_2017VI8day)





USHRC_2017VI8day$Ws<- (1+USHRC_2017VI8day$LSWI_SG)/((1+max(USHRC_2017VI8day$LSWI_SG)))
USHRC_2017VI8day$LUEvpm<- (USHRC_2017VI8day$Tsvpm)*(USHRC_2017VI8day$Ws)*(0.05)
USHRC_2017VI8day$LUEpvpmcal<-(USHRC_2017VI8day$Tspvpm_cal)*(USHRC_2017VI8day$Ws)*(USHRC_2017VI8day$LUEmax)
USHRC_2017VI8day$LUEpvpmmodeled<- (USHRC_2017VI8day$Tspvpm_modeled)*(USHRC_2017VI8day$Ws)*(USHRC_2017VI8day$LUEmaxmodeled)


USHRC_2017VI8day$GPPvpm<- USHRC_2017VI8day$LUEvpm*USHRC_2017VI8day$Fapar*USHRC_2017VI8day$PAR_site*12.011
USHRC_2017VI8day$GPPpvpmcal<- USHRC_2017VI8day$LUEmax*USHRC_2017VI8day$Fapar*USHRC_2017VI8day$PAR_site*12.011
USHRC_2017VI8day$GPPpvpmmodel<- USHRC_2017VI8day$LUEmaxmodeled*USHRC_2017VI8day$Fapar*USHRC_2017VI8day$PAR_site*12.011


plot(USHRC_2017VI8day$GPP_site, USHRC_2017VI8day$GPPvpm)
plot(USHRC_2017VI8day$GPP_site, USHRC_2017VI8day$GPPpvpmcal)
plot(USHRC_2017VI8day$GPP_site, USHRC_2017VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USHRC_2017VI8day$GPP_site, USHRC_2017VI8day$GPPvpm, na.rm=TRUE)
rmse(USHRC_2017VI8day$GPP_site, USHRC_2017VI8day$GPPpvpmcal)
rmse(USHRC_2017VI8day$GPP_site, USHRC_2017VI8day$GPPpvpmmodel)



ncol(USHRC_2017VI8day)


##dropping the null values
USHRC_2017VI8day<-USHRC_2017VI8day[!is.na(USHRC_2017VI8day$GPP_site),]

p<-ggplot(data=USHRC_2017VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRC_2017 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRC_2017VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USHRC_2017VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRC_2017 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRC_2017PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USHRC_2017VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRC_2017 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRC_2017PVPMmodeled.jpeg", width = 18, height = 7)



