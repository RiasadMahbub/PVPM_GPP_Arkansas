## USHRC 2015

#PAGE 2


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


myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =2))
USHRC_2015<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =2, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USHRC_2015$`Date Time`<-gsub("'","",USHRC_2015$`Date Time`)
USHRC_2015$datetime<-strptime(USHRC_2015$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USHRC_2015$Date<-as.Date(USHRC_2015$datetime)

##ADD the DOY column
USHRC_2015$DOY<- yday(USHRC_2015$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2015-04-15")

USHRC_2015<-filter(USHRC_2015, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USHRC_2015$DOY)- min(USHRC_2015$DOY))/8)

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
  dflist[[i]]<- subset(USHRC_2015, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax

stderrorUSHRC2015<--rep(0, points)
rsquareUSHRC2015<--rep(0, points)
residualsumofsquareUSHRC2015<--rep(0, points)
rmseUSHRC2015<--rep(0, points)


for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.08 ,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  pmaxUSHRC_2015[i]<-modellist[[i]]$m$getPars()[2]
  stderrorUSHRC2015[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSHRC2015[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSHRC2015[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSHRC2015[i] <-  1-(residualsumofsquareUSHRC2015[i]/sum((y - mean(y))^2))
}


# stderrorUSHRC_2015<--rep(0, points)
# for (i in (1:points)){
#   modellist[[i]]<-light.response(data = dflist[[i]], dflist[[i]]$NEE_modeled, Reco=dflist[[i]]$Reco_modeled,PPFD=dflist[[i]]$PAR_Regression,PPFD_ref=2000)
#   LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
#   stderrorUSHRC_2015[i]<-summary(modellist[[i]])$parameters[1,2]
# }


summary(modellist[[3]])$parameters[1,]





avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USHRC 2015")





par(mfrow = c(4, 5))
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

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")
text(USHRC_2015LUEmax$DOY, USHRC_2015LUEmax$LUEmax, labels = USHRC_2015LUEmax$STD_Error)
USHRC_2015LUEmax




##Create a dataframe
USHRC_2015LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USHRC", "year" = 2015, "STD_Error" = stderrorUSHRC2015, "R-squared" = rsquareUSHRC2015, "RMSE" = rmseUSHRC2015, "Residual sum of square" = residualsumofsquareUSHRC2015, "Topt"= toptvector )
view(USHRC_2015LUEmax)


USHRC_2015LUEmax





##########################################################
## 8 day mean

#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USHRC_2015.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRC_2015, mean)


#Second to daily
USHRC_2015.daily$GPP_modeled<- ( USHRC_2015.daily$GPP_modeled)* 1e-6*86400*12.011
USHRC_2015.daily$PAR_Regression<- ( USHRC_2015.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot( USHRC_2015.daily$Date,  USHRC_2015.daily$GPP_modeled)

USHRC_2015.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRC_2015.daily, mean)
plot( USHRC_2015.8day$`cut(Date, "8 days")`,  USHRC_2015.8day$GPP_modeled)

###CHANGING THE COLUMN NAMES TO SITE

USHRC_2015.8day$Date<-USHRC_2015.8day$`cut(Date, "8 days")`
USHRC_2015.8day$GPP_site<-USHRC_2015.8day$GPP_modeled
USHRC_2015.8day$PAR_site<-USHRC_2015.8day$PAR_Regression
USHRC_2015.8day$VPD_site<-USHRC_2015.8day$VPD_REddyProc
USHRC_2015.8day$Tair_site<-USHRC_2015.8day$Ta_REddyProc

USHRC_2015.8day<-USHRC_2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)




USHRC_2015.8day$doy<-yday(USHRC_2015.8day$Date)

USHRC_2015.8day<-USHRC_2015.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRC_2015.8day


library(formattable)


USHRC_2015.8day$GPP_site<- formattable(USHRC_2015.8day$GPP_site, digits = 2, format = "f")
USHRC_2015.8day$PAR_site<- formattable(USHRC_2015.8day$PAR_site, digits = 2, format = "f")
USHRC_2015.8day$Tair_site<- formattable(USHRC_2015.8day$Tair_site, digits = 2, format = "f")
USHRC_2015.8day$VPD_site<- formattable(USHRC_2015.8day$VPD_site, digits = 2, format = "f")

USHRC_2015.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USHRC_2015VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/Way32015.csv")
#USHRC_2015VI$Date<-as.Date(USHRC_2015VI$Time, "%b %d, %Y")
USHRC_2015VI$Tair_satellite<-((USHRC_2015VI$Temp+USHRC_2015VI$Tmax)/2)-273.16
USHRC_2015VI$doy<-(USHRC_2015VI$doy) + 1


USHRC_2015VI8day<-merge(x = USHRC_2015.8day, y = USHRC_2015VI, by = "doy", all = TRUE)

USHRC_2015LUEmax$doy<- USHRC_2015LUEmax$DOY
USHRC_2015VI8day<-merge(x = USHRC_2015VI8day, y = USHRC_2015LUEmax, by = "doy", all = TRUE)

USHRC_2015VI8day$DOP = 98
USHRC_2015VI8day$DAP = USHRC_2015VI8day$doy - USHRC_2015VI8day$DOP

a = -1972
b= -3537
c=68.5211
d = 0.0764
x= USHRC_2015VI8day$DAP
x
USHRC_2015VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USHRC_2015VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USHRC_2015VI8day$DAP, USHRC_2015VI8day$LUEmaxmodeled)
plot(USHRC_2015VI8day$DAP, USHRC_2015VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USHRC_2015VI8day$Tair_satellite
Toptcal<-USHRC_2015VI8day$Topt
Tsite<- USHRC_2015VI8day$Tair_site
Toptmodeled<-USHRC_2015VI8day$Toptmodeled
USHRC_2015VI8day$Fapar <- (USHRC_2015VI8day$EVI_SG - 0.1)*1.25
USHRC_2015VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USHRC_2015VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USHRC_2015VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USHRC_2015VI8day$DOP <- 98
USHRC_2015VI8day$DAP<-USHRC_2015VI8day$doy-USHRC_2015VI8day$DOP
View(USHRC_2015VI8day)





USHRC_2015VI8day$Ws<- (1+USHRC_2015VI8day$LSWI_SG)/((1+max(USHRC_2015VI8day$LSWI_SG)))
USHRC_2015VI8day$LUEvpm<- (USHRC_2015VI8day$Tsvpm)*(USHRC_2015VI8day$Ws)*(0.05)
USHRC_2015VI8day$LUEpvpmcal<-(USHRC_2015VI8day$Tspvpm_cal)*(USHRC_2015VI8day$Ws)*(USHRC_2015VI8day$LUEmax)
USHRC_2015VI8day$LUEpvpmmodeled<- (USHRC_2015VI8day$Tspvpm_modeled)*(USHRC_2015VI8day$Ws)*(USHRC_2015VI8day$LUEmaxmodeled)


USHRC_2015VI8day$GPPvpm<- USHRC_2015VI8day$LUEvpm*USHRC_2015VI8day$Fapar*USHRC_2015VI8day$PAR_site*12.011
USHRC_2015VI8day$GPPpvpmcal<- USHRC_2015VI8day$LUEmax*USHRC_2015VI8day$Fapar*USHRC_2015VI8day$PAR_site*12.011
USHRC_2015VI8day$GPPpvpmmodel<- USHRC_2015VI8day$LUEmaxmodeled*USHRC_2015VI8day$Fapar*USHRC_2015VI8day$PAR_site*12.011


plot(USHRC_2015VI8day$GPP_site, USHRC_2015VI8day$GPPvpm)
plot(USHRC_2015VI8day$GPP_site, USHRC_2015VI8day$GPPpvpmcal)
plot(USHRC_2015VI8day$GPP_site, USHRC_2015VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USHRC_2015VI8day$GPP_site, USHRC_2015VI8day$GPPvpm, na.rm=TRUE)
rmse(USHRC_2015VI8day$GPP_site, USHRC_2015VI8day$GPPpvpmcal)
rmse(USHRC_2015VI8day$GPP_site, USHRC_2015VI8day$GPPpvpmmodel)



ncol(USHRC_2015VI8day)


##dropping the null values
USHRC_2015VI8day<-USHRC_2015VI8day[!is.na(USHRC_2015VI8day$GPP_site),]

p<-ggplot(data=USHRC_2015VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRC_2015 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRC_2015VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USHRC_2015VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRC_2015 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRC_2015PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USHRC_2015VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRC_2015 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRC_2015PVPMmodeled.jpeg", width = 18, height = 7)







