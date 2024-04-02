#### USHRA 2016

####Page 6


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
                                n_max = 1, col_names = FALSE, sheet =6))
USHRA_2016<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                       skip= 2, sheet =6, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USHRA_2016$`Date Time`<-gsub("'","",USHRA_2016$`Date Time`)
USHRA_2016$datetime<-strptime(USHRA_2016$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USHRA_2016$Date<-as.Date(USHRA_2016$datetime)

##ADD the DOY column
USHRA_2016$DOY<- yday(USHRA_2016$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2016-04-30")

USHRA_2016<-filter(USHRA_2016, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USHRA_2016$DOY)- min(USHRA_2016$DOY))/8)

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
  dflist[[i]]<- subset(USHRA_2016, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax

stderrorUSHRA2016<--rep(0, points)
rsquareUSHRA2016<--rep(0, points)
residualsumofsquareUSHRA2016<--rep(0, points)
rmseUSHRA2016<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.0452, g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSHRA2016[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSHRA2016[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSHRA2016[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSHRA2016[i] <-  1-(residualsumofsquareUSHRA2016[i]/sum((y - mean(y))^2))
}


## Estimating using bigleaf package
# for (i in (1:points)){
#   modellist[[i]]<-light.response(data = dflist[[i]], dflist[[i]]$NEE_modeled, Reco=dflist[[i]]$Reco_modeled,PPFD=dflist[[i]]$PAR_Regression,PPFD_ref=2000)
#   LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
#   stderrorUSHRA_2016[i]<-summary(modellist[[i]])$parameters[1,2]
# }




avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c( -(points+1))]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USHRA_2016 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")

##Big leaf
light.response(data = USHRA_2016, USHRA_2016$NEE_modeled, Reco=USHRA_2016$Reco_modeled,PPFD=USHRA_2016$PAR_Regression,PPFD_ref=2000)
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
toptvector






##Create a dataframe
USHRA_2016LUEmax<-data.frame("DOY" = DOYlistway32015[ -c(2, points+1)], "LUEmax" = LUEmax2015[-2], "site" = "USHRA", "year" = 2016,"STD_Error" = stderrorUSHRA2016[-2], "R-squared" = rsquareUSHRA2016[-2], "RMSE" = rmseUSHRA2016[-2], "Residual sum of square" = residualsumofsquareUSHRA2016[-2], "Topt"= toptvector[-2])
view(USHRA_2016LUEmax)


USHRA_2016LUEmax





##########################################################

## Topt

##8 day mean
#####
### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

USHRA_2016.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date,  USHRA_2016, mean)


#Second to daily
USHRA_2016.daily$GPP_modeled<- ( USHRA_2016.daily$GPP_modeled)* 1e-6*86400*12.011
USHRA_2016.daily$PAR_Regression<- ( USHRA_2016.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot( USHRA_2016.daily$Date,  USHRA_2016.daily$GPP_modeled)

USHRA_2016.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"),  USHRA_2016.daily, mean)
plot( USHRA_2016.8day$`cut(Date, "8 days")`,  USHRA_2016.8day$GPP_modeled)


###CHANGING THE COLUMN NAMES TO SITE

USHRA_2016.8day$Date<-USHRA_2016.8day$`cut(Date, "8 days")`
USHRA_2016.8day$GPP_site<-USHRA_2016.8day$GPP_modeled
USHRA_2016.8day$PAR_site<-USHRA_2016.8day$PAR_Regression
USHRA_2016.8day$VPD_site<-USHRA_2016.8day$VPD_REddyProc
USHRA_2016.8day$Tair_site<-USHRA_2016.8day$Ta_REddyProc

USHRA_2016.8day<-USHRA_2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)




USHRA_2016.8day$doy<-yday(USHRA_2016.8day$Date)

USHRA_2016.8day<-USHRA_2016.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USHRA_2016.8day


library(formattable)


USHRA_2016.8day$GPP_site<- formattable(USHRA_2016.8day$GPP_site, digits = 2, format = "f")
USHRA_2016.8day$PAR_site<- formattable(USHRA_2016.8day$PAR_site, digits = 2, format = "f")
USHRA_2016.8day$Tair_site<- formattable(USHRA_2016.8day$Tair_site, digits = 2, format = "f")
USHRA_2016.8day$VPD_site<- formattable(USHRA_2016.8day$VPD_site, digits = 2, format = "f")

USHRA_2016.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USHRA_2016VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/Way42016.csv")
#USHRA_2016VI$Date<-as.Date(USHRA_2016VI$Time, "%b %d, %Y")
USHRA_2016VI$Tair_satellite<-((USHRA_2016VI$Temp+USHRA_2016VI$Tmax)/2)-273.16
USHRA_2016VI$doy<-(USHRA_2016VI$doy) + 1


USHRA_2016VI8day<-merge(x = USHRA_2016.8day, y = USHRA_2016VI, by = "doy", all = TRUE)

USHRA_2016LUEmax$doy<- USHRA_2016LUEmax$DOY
USHRA_2016VI8day<-merge(x = USHRA_2016VI8day, y = USHRA_2016LUEmax, by = "doy", all = TRUE)

USHRA_2016VI8day$DOP = 114
USHRA_2016VI8day$DAP = USHRA_2016VI8day$doy - USHRA_2016VI8day$DOP

a = -1972
b= -3537
c=  82.5211
d = 0.05764
x= USHRA_2016VI8day$DAP
x
USHRA_2016VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USHRA_2016VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USHRA_2016VI8day$DAP, USHRA_2016VI8day$LUEmaxmodeled)
plot(USHRA_2016VI8day$DAP, USHRA_2016VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USHRA_2016VI8day$Tair_satellite
Toptcal<-USHRA_2016VI8day$Topt
Tsite<- USHRA_2016VI8day$Tair_site
Toptmodeled<-USHRA_2016VI8day$Toptmodeled
USHRA_2016VI8day$Fapar <- (USHRA_2016VI8day$EVI_SG - 0.1)*1.25
USHRA_2016VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USHRA_2016VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USHRA_2016VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USHRA_2016VI8day$DOP <- 114
USHRA_2016VI8day$DAP<-USHRA_2016VI8day$doy-USHRA_2016VI8day$DOP
View(USHRA_2016VI8day)





USHRA_2016VI8day$Ws<- (1+USHRA_2016VI8day$LSWI_SG)/((1+max(USHRA_2016VI8day$LSWI_SG)))
USHRA_2016VI8day$LUEvpm<- (USHRA_2016VI8day$Tsvpm)*(USHRA_2016VI8day$Ws)*(0.05)
USHRA_2016VI8day$LUEpvpmcal<-(USHRA_2016VI8day$Tspvpm_cal)*(USHRA_2016VI8day$Ws)*(USHRA_2016VI8day$LUEmax)
USHRA_2016VI8day$LUEpvpmmodeled<- (USHRA_2016VI8day$Tspvpm_modeled)*(USHRA_2016VI8day$Ws)*(USHRA_2016VI8day$LUEmaxmodeled)


USHRA_2016VI8day$GPPvpm<- USHRA_2016VI8day$LUEvpm*USHRA_2016VI8day$Fapar*USHRA_2016VI8day$PAR_site*12.011
USHRA_2016VI8day$GPPpvpmcal<- USHRA_2016VI8day$LUEmax*USHRA_2016VI8day$Fapar*USHRA_2016VI8day$PAR_site*12.011
USHRA_2016VI8day$GPPpvpmmodel<- USHRA_2016VI8day$LUEmaxmodeled*USHRA_2016VI8day$Fapar*USHRA_2016VI8day$PAR_site*12.011


plot(USHRA_2016VI8day$GPP_site, USHRA_2016VI8day$GPPvpm)
plot(USHRA_2016VI8day$GPP_site, USHRA_2016VI8day$GPPpvpmcal)
plot(USHRA_2016VI8day$GPP_site, USHRA_2016VI8day$GPPpvpmmodel)

plot(USHRA_2016VI8day$DAP,USHRA_2016VI8day$GPPpvpmcal)


library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USHRA_2016VI8day$GPP_site, USHRA_2016VI8day$GPPvpm, na.rm=TRUE)
rmse(USHRA_2016VI8day$GPP_site, USHRA_2016VI8day$GPPpvpmcal)
rmse(USHRA_2016VI8day$GPP_site, USHRA_2016VI8day$GPPpvpmmodel)



ncol(USHRA_2016VI8day)


##dropping the null values
USHRA_2016VI8day<-USHRA_2016VI8day[!is.na(USHRA_2016VI8day$GPP_site),]

p<-ggplot(data=USHRA_2016VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRA_2016 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRA_2016VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USHRA_2016VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRA_2016 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRA_2016PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USHRA_2016VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USHRA_2016 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USHRA_2016PVPMmodeled.jpeg", width = 18, height = 7)



