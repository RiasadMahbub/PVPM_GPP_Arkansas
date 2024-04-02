### USOF4 
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


## Read the data
myCols<-as.character(read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                                n_max = 1, col_names = FALSE, sheet =11))
USOF4<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/Rice Data_Research Group_03-10-2022.xlsx", 
                  skip= 2, sheet =11, col_names = myCols)


## The qoutation around the datetime column and fixing the datetime 
USOF4$`Date Time`<-gsub("'","",USOF4$`Date Time`)
USOF4$datetime<-strptime(USOF4$`Date Time`, format='%d-%b-%Y %H:%M:%S')

###Get the date column from the dt
USOF4$Date<-as.Date(USOF4$datetime)

##ADD the DOY column
USOF4$DOY<- yday(USOF4$datetime)


###MODIS first image on date
### For the new data the nearest date of MODIS is 2017_04_07 and 97
### MODIS image nearest one starts at 137 day May 17 for the original data
MODISdoy<-yday("2018-04-15")

USOF4<-filter(USOF4, DOY>=MODISdoy)

##Number of LUEmax points
points<-as.integer((max(USOF4$DOY)- min(USOF4$DOY))/8)

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
  dflist[[i]]<- subset(USOF4, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
  dflist[[i]]<-subset(dflist[[i]], PAR_Regression>20)
}

## Creating the list of model
modellist<-list()
for (i in 1:points){
  modellist[[i]]<-data.frame()
}

###LUEmax
stderrorUSOF42017<--rep(0, points)
rsquareUSOF42017<--rep(0, points)
residualsumofsquareUSOF42017<--rep(0, points)
rmseUSOF42017<--rep(0, points)

for (i in (1:points)){
  y<-dflist[[i]]$GPP_modeled
  x<-dflist[[i]]$PAR_Regression
  modellist[[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a=   0.05358 ,g= 27))
  LUEmax2015[i]<-modellist[[i]]$m$getPars()[1]
  stderrorUSOF42017[i]<-summary(modellist[[i]])$parameters[1,2]
  residualsumofsquareUSOF42017[i]<-sum((modellist[[i]]$m$resid())^2)
  rmseUSOF42017[i]<-sqrt(crossprod(modellist[[i]]$m$resid())/(length(modellist[[i]]$m$resid())))
  rsquareUSOF42017[i] <-  1-(residualsumofsquareUSOF42017[i]/sum((y - mean(y))^2))
}






avgsdevsub<-rep(0,points)
avgsdevadd<-rep(0,points)
for (i in (1:points)){
  avgsdevsub[i]<-(summary(modellist[[i]])$parameters[1,1]) - (summary(modellist[[i]])$parameters[1,2])
  avgsdevadd[i]<- (summary(modellist[[i]])$parameters[1,1]) + (summary(modellist[[i]])$parameters[1,2])
}


plot((DOYlistway32015[c(-1,-2,-3, -(points+1))]), as.numeric(LUEmax2015[c(-1,-2,-3)]),  xlab="DOY",
     ylab="LUEmax", main = "USOF4 2017")
LUEmax2015

arrows((DOYlistway32015), avgsdevsub,(DOYlistway32015), avgsdevadd, length=0.05, angle=90, code=3)
abline(h=mean(LUEmax2015[-1]), col="blue")


par(mfrow = c(5, 5))
par(mar=c(1, 1, 1, 1))
for (i in (1:points)){
  plot(dflist[[i]]$PAR_Regression, dflist[[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP")
  lines(sort(dflist[[i]]$PAR_Regression), fitted(modellist[[i]])[order(dflist[[i]]$PAR_Regression)], col='red', lwd=2) 
}
##Big leaf
light.response(data = USOF4, USOF4$NEE_modeled, Reco=USOF4$Reco_modeled,PPFD=USOF4$PAR_Regression,PPFD_ref=2000)
light.response(data = dflist[[10]], dflist[[8]]$NEE_modeled, Reco=dflist[[8]]$Reco_modeled,PPFD=dflist[[8]]$PAR_Regression,PPFD_ref=2000)

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

toptvector
## plot topt
par(mfrow = c(1, 1))
par(mar=c(2, 3, 2, 2))
plot(DOYlistway32015[-17], toptvector,  xlab="DOY", cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
     
     ylab="Tpot", main = "USOF1 2015 (95 percentile method)")
topt





##Create a dataframe
USOF4LUEmax<-data.frame("DOY" = DOYlistway32015[ -(points+1)], "LUEmax" = LUEmax2015, "site" = "USOF4", "year" = 2018, "STD_Error" = stderrorUSOF42017, "R-squared" = rsquareUSOF42017, "RMSE" = rmseUSOF42017, "Residual sum of square" = residualsumofsquareUSOF42017, "Topt" = toptvector)
view(USOF4LUEmax)


USOF4LUEmax










### Scale to daily daily scale
### Inorder to scale daile GPP and shortwave radiation needs to be multiplied

## Daily mean
USOF4.daily <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ Date, USOF4, mean)



#Second to daily
USOF4.daily$GPP_modeled<- (USOF4.daily$GPP_modeled)* 1e-6*86400*12.011
USOF4.daily$PAR_Regression<- (USOF4.daily$PAR_Regression)* 1e-6*86400


## Scale to 8 day scale
plot(USOF4.daily$Date, USOF4.daily$GPP_modeled)

USOF4.8day <- aggregate(cbind(GPP_modeled,PAR_Regression, Ta_REddyProc, VPD_REddyProc) ~ cut(Date, "8 days"), USOF4.daily, mean)
plot(USOF4.8day$`cut(Date, "8 days")`, USOF4.8day$GPP_modeled)




###CHANGING THE COLUMNN NNAMES TO SITE

USOF4.8day$Date<-USOF4.8day$`cut(Date, "8 days")`
USOF4.8day$GPP_site<-USOF4.8day$GPP_modeled
USOF4.8day$PAR_site<-USOF4.8day$PAR_Regression
USOF4.8day$VPD_site<-USOF4.8day$VPD_REddyProc
USOF4.8day$Tair_site<-USOF4.8day$Ta_REddyProc

USOF4.8day<-USOF4.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site)

###CHANGING THE COLUMNN NNAMES TO SITE

USOF4.8day$doy<-yday(USOF4.8day$Date)

USOF4.8day<-USOF4.8day%>% select(Date, GPP_site, PAR_site, VPD_site, Tair_site, doy)
USOF4.8day


library(formattable)


USOF4.8day$GPP_site<- formattable(USOF4.8day$GPP_site, digits = 2, format = "f")
USOF4.8day$PAR_site<- formattable(USOF4.8day$PAR_site, digits = 2, format = "f")
USOF4.8day$Tair_site<- formattable(USOF4.8day$Tair_site, digits = 2, format = "f")
USOF4.8day$VPD_site<- formattable(USOF4.8day$VPD_site, digits = 2, format = "f")

USOF4.8day

#### Read the satellite images
getwd()
options(digits=2)
getOption("digits")
USOF4VI<-read.csv("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/Code/VIdata/USOF42018.csv")
#USOF4VI$Date<-as.Date(USOF4VI$Time, "%b %d, %Y")
USOF4VI$Tair_satellite<-((USOF4VI$Temp+USOF4VI$Tmax)/2)-273.16
USOF4VI$doy<-(USOF4VI$doy)+1


USOF4VI8day<-merge(x = USOF4.8day, y = USOF4VI, by = "doy", all = TRUE)

USOF4LUEmax$doy<- USOF4LUEmax$DOY
USOF4VI8day<-merge(x = USOF4VI8day, y = USOF4LUEmax, by = "doy", all = TRUE)

USOF4VI8day$DOP = 99
USOF4VI8day$DOH = 241
USOF4VI8day$DAP = USOF4VI8day$doy - USOF4VI8day$DOP

a = -1972
b= -3537
c=  82.5211
d = 0.05764
x= USOF4VI8day$DAP
x
USOF4VI8day$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
USOF4VI8day$Toptmodeled<- 19.33+ (0.206*x) - (0.0007*(x^2)) -(1.619e-06*(x^3))

plot(USOF4VI8day$DAP, USOF4VI8day$LUEmaxmodeled)
plot(USOF4VI8day$DAP, USOF4VI8day$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- USOF4VI8day$Tair_satellite
Toptcal<-USOF4VI8day$Topt
Tsite<- USOF4VI8day$Tair_site
Toptmodeled<-USOF4VI8day$Toptmodeled
USOF4VI8day$Fapar <- (USOF4VI8day$EVI_SG - 0.1)*1.25
USOF4VI8day$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
USOF4VI8day$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
USOF4VI8day$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
USOF4VI8day$DOP <- 99
USOF4VI8day$DAP<-USOF4VI8day$doy-USOF4VI8day$DOP
View(USOF4VI8day)





USOF4VI8day$Ws<- (1+USOF4VI8day$LSWI_SG)/((1+max(USOF4VI8day$LSWI_SG)))
USOF4VI8day$LUEvpm<- (USOF4VI8day$Tsvpm)*(USOF4VI8day$Ws)*(0.05)
USOF4VI8day$LUEpvpmcal<-(USOF4VI8day$Tspvpm_cal)*(USOF4VI8day$Ws)*(USOF4VI8day$LUEmax)
USOF4VI8day$LUEpvpmmodeled<- (USOF4VI8day$Tspvpm_modeled)*(USOF4VI8day$Ws)*(USOF4VI8day$LUEmaxmodeled)


USOF4VI8day$GPPvpm<- USOF4VI8day$LUEvpm*USOF4VI8day$Fapar*USOF4VI8day$PAR_site*12.011
USOF4VI8day$GPPpvpmcal<- USOF4VI8day$LUEmax*USOF4VI8day$Fapar*USOF4VI8day$PAR_site*12.011
USOF4VI8day$GPPpvpmmodel<- USOF4VI8day$LUEmaxmodeled*USOF4VI8day$Fapar*USOF4VI8day$PAR_site*12.011


plot(USOF4VI8day$GPP_site, USOF4VI8day$GPPvpm)
plot(USOF4VI8day$GPP_site, USOF4VI8day$GPPpvpmcal)
plot(USOF4VI8day$GPP_site, USOF4VI8day$GPPpvpmmodel)

library(Metrics)
require(hydroGOF)
library(viridis)
library(ggpubr)
rmse(USOF4VI8day$GPP_site, USOF4VI8day$GPPvpm, na.rm=TRUE)
rmse(USOF4VI8day$GPP_site, USOF4VI8day$GPPpvpmcal)
rmse(USOF4VI8day$GPP_site, USOF4VI8day$GPPpvpmmodel)



ncol(USOF4VI8day)


##dropping the null values
USOF4VI8day<-USOF4VI8day[!is.na(USOF4VI8day$GPP_site),]

p<-ggplot(data=USOF4VI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF4 VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF4VPM.jpeg", width = 18, height = 7)


p<-ggplot(data=USOF4VI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF4 PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF4PVPMcal.jpeg", width = 18, height = 7)

p<-ggplot(data=USOF4VI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USOF4 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("USOF4PVPMmodeled.jpeg", width = 18, height = 7)






#### Finding the percentage of gaps in the variables
library(dplyr)
Gappercentage<-as.data.frame(colMeans(is.na(USOF4))*100)
Gappercentage <- cbind(newColName = rownames(Gappercentage), Gappercentage)
rownames(Gappercentage) <- 1:nrow(Gappercentage)
Gappercentage<-Gappercentage[c(2,3,4,5), ]

ggplot(Gappercentage, aes(x = newColName, y = `colMeans(is.na(USOF4)) * 100`, fill = newColName)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%0.2f", round(`colMeans(is.na(USOF4)) * 100`, digits = 2))), vjust = 0)+
  ylab("Percentages (%)")+
  xlab("Variables")+
  guides(fill=guide_legend(title="Variables"))+
  theme(text = element_text(size=20))




# Old code


################################
USOF4<-read_excel("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/co2_data-Fairley-RunkleReba_07-2021.xlsx", sheet =3)


## Subsetting
## Dropping the first 8 rows
USOF4<-USOF4[-1:-7, ]

## Make the first row as the header row
names(USOF4)<-USOF4[1, ]
USOF4<-USOF4[-1, ]

## Drop the first 7 columns 
USOF4<-USOF4[, -1:-7]
## Selecting 4 columns of USOF1
USOF4<-USOF4[, c(1, 6, 7,8,20,21,25, 23)]

## Replace all the -9999 values by NA (missing) labels
USOF4<-USOF4%>%mutate_all(~replace(., . == -9999, NA))

# make the time column
USOF4$time <- as.numeric(substr(USOF4$TIMESTAMP_END, 9, 12))

# formatting time stamp
USOF4$datetime <-ymd_hms(USOF4$TIMESTAMP_END, truncated = 1)

## Convert the columns into numeric datatype
USOF4$SW_IN<-as.numeric(USOF4$SW_IN)
USOF4$FC_F<-as.numeric(USOF4$FC_F)
USOF4$RH<-as.numeric(USOF4$RH)
USOF4$TA<-as.numeric(USOF4$TA)
USOF4$time<-as.numeric(USOF4$time)
USOF4$FC_F<-as.numeric(USOF4$FC_F)
USOF4$RECO<-as.numeric(USOF4$RECO)
USOF4$GPP<-as.numeric(USOF4$GPP)


### 2018
#plantingdate: april1 -
## harvesting date: sep 18-20
## modis: 2018 06 13, 2018 06 18 0618 DOY= 169-258
####
USOF4$DOY<- yday(USOF4$datetime)
## converting swin to par by multipying 2.02
USOF4$PAR<-USOF4$SW_IN*2.02

### MODIS image nearest one starts at 137 day May 17
USOF4<-filter(USOF4, DOY>=169)
###2015
## 16 datapoints
### 8 day LUEmax
###Creating a list of LUEmax 
## Starting DOY = 185
LUEmax<-rep(0,44)
##Creating a list of DOYlist
(max(USOF4$DOY)-min(USOF4$DOY))/8

DOYlistway32015<-rep(0, 11)
LUEmax2015<-rep(0,11)

### DOY 
for (i in 1:(length(DOYlistway32015)-1)){
  DOYlistway32015[1] = 169
  DOYlistway32015[i+1]=DOYlistway32015[i]+ 8
}

## Creating a list of empty dataframes
dflist<-list()
for (i in 1:11){
  dflist[[i]]<-data.frame()
}


###Storing the data in the empty dataframes
for (i in 1: length(dflist)){
  dflist[[i]]<- subset(USOF4, DOY>=DOYlistway32015[i] & DOY<DOYlistway32015[i+1])
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
     ylab="LUEmax", main = "USOF4 2018")



y<-dflist[[1]]$GPP
x<-dflist[[1]]$PAR
modellist[[1]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.04471,g= 1000))
modellist[1]
plot(dflist[[1]]$PAR, dflist[[2]]$GPP)
plot(USBDA_2015$PPFD_IN, USBDA_2015$GPP)

dflist

y<-USOF1$GPP
x<-USOF1$PAR
nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.05,g= 30))
LUEmax2015<-modellist[[i]]$m$getPars()[1]



USOF1_complete <- na.omit(USOF1)
USOF1_complete$NEE<-as.numeric(USOF1_complete$FC_F)

library(bigleaf)
modellist[[i]]light.response(data = dflist[[5]], NEE=dflist[[5]]$FC_F, Reco=dflist[[5]]$RECO,PPFD=dflist[[5]]$PAR,PPFD_ref=2000)

plot(USOF4$PAR, USOF4$GPP)


