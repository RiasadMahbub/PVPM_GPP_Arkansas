### USOF2
## 2017

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


## USOF2 is in the sheet 9 of RiceData_ResearchGroup03-10-2022
## Read the data
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =9))
USOF2<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx", 
                  skip= 2, sheet =9, col_names = myCols)

## The qoutation around the datetime column and fixing the datetime 
USOF2$`Date Time`<-gsub("'","",USOF2$`Date Time`)
USOF2$datetime<-strptime(USOF2$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF2$Date<-as.Date(USOF2$datetime)

##ADD the DOY column
USOF2$DOY<- yday(USOF2$datetime)

###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-97


USOF2<-filter(USOF2, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF2$DOY)- min(USOF2$DOY))/8)

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
  dflist[[i]]<- subset(USOF2, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.04124,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

## new code including rmse, std error and r2 and residual sum of square
stderrorUSOF22017<--rep(0, points)
rsquareUSOF22017<--rep(0, points)
residualsumofsquareUSOF22017<--rep(0, points)
rmseUSOF22017<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=  0.04124,g= 30))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF22017[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF22017[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF22017[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF22017[i] <-  1-(residualsumofsquareUSOF22017[i]/sum((y - mean(y))^2))
}



LUEmax2015



avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}
plot((DOYlistway32015[ -(points+1)]), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USOF1 2017")


arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")




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


## plot topt
par(mfrow = c(1, 1))
par(mar=c(2, 3, 2, 2))
plot(DOYlistway32015[-17], toptvector,  xlab="DOY", cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     
     ylab="Tpot", main = "USOF1 2015 (95 percentile method)")
topt


##Create a dataframe
USOF2LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF2", "year" = 2017, "STD_Error" = stderrorUSOF22017, "R-squared" = rsquareUSOF22017, "RMSE" = rmseUSOF22017, "Residual sum of square" = residualsumofsquareUSOF22017, "Topt" = toptvector)
USOF2LUEmax
##Big leaf
light.response(data = USOF2, USOF2$NEE_modeled, Reco=USOF2$Reco_modeled,PPFD=USOF2$PAR_Regression,PPFD_ref=2000)








### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

## Daily mean
USOF2.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF2, mean)



#Second to daily
USOF2.daily$GPP_modeled<- (USOF2.daily$GPP_modeled)* 1e-6*86400*12.011
USOF2.daily$PAR_Regression<- (USOF2.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USOF2.daily$Date, USOF2.daily$GPP_modeled)

USOF2.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF2.daily, mean)
plot(USOF2.8day$`cut(Date, "8 days")`, USOF2.8day$GPP_modeled)


###CHANGING THE COLUMNN NNAMES TO SITE
USOF2.8day$Date<-USOF2.8day$`cut(Date, "8 days")`
USOF2.8day$GPP_site<-USOF2.8day$GPP_modeled
USOF2.8day$PAR_site<-USOF2.8day$PAR_Regression
USOF2.8day$VPD_site<-USOF2.8day$VPD_REddyProc
USOF2.8day$Tair_site<-USOF2.8day$Ta_REddyProc
USOF2.8day$doy<-yday(USOF2.8day$Date)

USOF2.8day<-USOF2.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF2.8day


library(formattable)


USOF2.8day$GPP_site<- formattable(USOF2.8day$GPP_site, digits = 2, format = "f")
USOF2.8day$PAR_site<- formattable(USOF2.8day$PAR_site, digits = 2, format = "f")
USOF2.8day$Tair_site<- formattable(USOF2.8day$Tair_site, digits = 2, format = "f")
USOF2.8day$VPD_site<- formattable(USOF2.8day$VPD_site, digits = 2, format = "f")

USOF2.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USOF2VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USOF22017.csv")
#USOF2VI$Date<-as.Date(USOF2VI$Time, "%b %d, %Y")
USOF2VI$Tair_satellite<-((USOF2VI$Temp+USOF2VI$Tmax)/2)-273.16
USOF2VI$doy<-(USOF2VI$doy) + 1


USOF2VI8day<-merge(x = USOF2.8day, y = USOF2VI, by = "doy", all = TRUE)

USOF2LUEmax$doy<- USOF2LUEmax$DOY
USOF2VI8day<-merge(x = USOF2VI8day, y = USOF2LUEmax, by = "doy", all = TRUE)

USOF2VI8day$DOP = 91
USOF2VI8day$DOH = 249
USOF2VI8day$DAP = USOF2VI8day$doy - USOF2VI8day$DOP

a = -1972
b= -3537
c=  82.5211
d = 0.05764
x= USOF2VI8day$DAP
x
USOF2VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USOF2VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USOF2VI8day$DAP, USOF2VI8day$LUEmaxmodeled)
plot(USOF2VI8day$DAP, USOF2VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USOF2VI8day$Tair_satellite
Toptcal<-USOF2VI8day$Topt
Tsite<- USOF2VI8day$Tair_site
Toptmodeled<-USOF2VI8day$Toptmodeled
USOF2VI8day$Fapar <- (USOF2VI8day$EVI_SG - 0.1)*1.25
USOF2VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USOF2VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USOF2VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USOF2VI8day$DOP <- 91
USOF2VI8day$DAP<-USOF2VI8day$doy-USOF2VI8day$DOP
View(USOF2VI8day)





USOF2VI8day$Ws<- (1+USOF2VI8day$LSWI_SG)/((1+max(USOF2VI8day$LSWI_SG)))
USOF2VI8day$LUEvpm<- (USOF2VI8day$Tsvpm)*(USOF2VI8day$Ws)*(0.05)
USOF2VI8day$LUEpvpmcal<-(USOF2VI8day$Tspvpm_cal)*(USOF2VI8day$Ws)*(USOF2VI8day$LUEmax)
USOF2VI8day$LUEpvpmmodeled<- (USOF2VI8day$Tspvpm_modeled)*(USOF2VI8day$Ws)*(USOF2VI8day$LUEmaxmodeled)


USOF2VI8day$GPPvpm<- USOF2VI8day$LUEvpm*USOF2VI8day$Fapar*USOF2VI8day$PAR_site*12.011
USOF2VI8day$GPPpvpmcal<- USOF2VI8day$LUEmax*USOF2VI8day$Fapar*USOF2VI8day$PAR_site*12.011
USOF2VI8day$GPPpvpmmodel<- USOF2VI8day$LUEmaxmodeled*USOF2VI8day$Fapar*USOF2VI8day$PAR_site*12.011


plot(USOF2VI8day$GPP_site, USOF2VI8day$GPPvpm)
plot(USOF2VI8day$GPP_site, USOF2VI8day$GPPpvpmcal)
plot(USOF2VI8day$GPP_site, USOF2VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USOF2VI8day$GPP_site, USOF2VI8day$GPPvpm, na.rm=TRUE)
rmse(USOF2VI8day$GPP_site, USOF2VI8day$GPPpvpmcal)
rmse(USOF2VI8day$GPP_site, USOF2VI8day$GPPpvpmmodel)



ncol(USOF2VI8day)


##dropping the null values
USOF2VI8day<-USOF2VI8day[!is.na(USOF2VI8day$GPP_site),]

p<-ggplot(data=USOF2VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF2 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF2VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USOF2VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF2 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF2PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USOF2VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF2 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF2PVPMmodeled.jpeg", width = 18, height = 7)

#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USOF2))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USOF2)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USOF2)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))




#plantingdate: april1
## modis: 169 DOY, 169 june 18
####################################################################################################
#####################################################################################################








USOF2$DOY<- yday(USOF2$datetime)
## converting swin to par by multipying 2.02
USOF2$PAR<-USOF2$SW_IN*2.02

### MODIS image nearest one starts at 137 day May 17
USOF2<-filter(USOF2, DOY>=169)
### Datapoints
(max(USOF2$DOY)-min(USOF2$DOY))/8
##11


LUEmax<-rep(0,44)
##Creating a list of DOYlist
DOYlistway32015<-rep(0, 12)
LUEmax2015<-rep(0,11)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 164
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:11){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF2, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
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
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.04471,g= 44.86))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
}

DOYlistway32015
LUEmax2015
plot(as.numeric(LUEmax2015))





y<-dflist[[3]]$GPP
x<-dflist[[3]]$PAR
modellist[[3]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.04471,g= 44.86))
modellist[3]
plot(dflist[[8]]$PAR, dflist[[8]]$GPP)
plot(USOF2$PPFD_IN, USOF2$GPP)




avgsdevsub<-rep(0,11)
avgsdevadd<-rep(0,11)
for (i in (1:11)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}
plot((DOYlistway32015[(-12)]-91), as.numeric(LUEmax2015),  xlab="DOY",
     ylab="LUEmax", main = "USOF2 2017",  ylim=c(0.01, 0.09))


arrows((DOYlistway32015-91), avgsdevsub,(DOYlistway32015-91), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")


y<-USOF1$GPP
x<-USOF1$PAR
nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.05,g= 30))
LUEmax2015<-modellist[[i]]$m$getPars()[2]



USOF1_complete <- na.omit(USOF1)
USOF1_complete$NEE<-as.numeric(USOF1_complete$FC_F)
light.response(data = USOF2, NEE=USOF2$FC_F, Reco=USOF2$RECO,PPFD=USOF2$PAR,PPFD_ref=2000)



light.response(data = dflist[[1]], NEE=dflist[[1]]$FC_F, Reco=dflist[[1]]$RECO,PPFD=dflist[[1]]$PAR,PPFD_ref=2000)



