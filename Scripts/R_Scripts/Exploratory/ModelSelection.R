library(tidyverse) 
library(minpack.lm)
library(viridis)
library(ggpubr)


## Adjusted R_squared, F test and AIC test to check which model is better

#### Model selection between CUMGPP and CUMGPP with LAI

x<-LUEmaxGDD$GDDcum
y<-LUEmaxGDD$GPP_site
stderror<-LUEmaxGDD$STD_Error
u<-LUEmaxGDD$PredictedLUmaxWM
ts<-LUEmaxGDD$Tspvpm_modeled
ws<-LUEmaxGDD$Ws
fpar<-LUEmaxGDD$Fapar
par<-LUEmaxGDD$PAR_site
evi<-LUEmaxGDD$EVI_SG
lai<-LUEmaxGDD$LAIscale
lapply(LUEmaxGDD, class)


a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02 



LUEmaxGDD
GPPminmodelGDDLAI<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011 + (int + coef* lai)), trace = TRUE, start = list(a = a, b= b, c=  c, d =d, int = int, coef = coef))
GPPminmodelGDD<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011 ), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))

GPPminmodelGDDLAI.summary<-summary(GPPminmodelGDDLAI)
names(GPPminmodelGDDLAI.summary)


### adj-R2 of the model
#define function to calculate adjusted R-squared
adj_r2 <- function(x) {
  return (1 - ((1-x$adj.r.squared)*(nobs(x)-1)/(nobs(x)-length(x$coefficients)-1)))
}

r.squared(GPPminmodelGDDLAI)

### AIC of the models
AIC(GPPminmodelGDDLAI)
AIC(GPPminmodelGDD)
### BIC of the models
BIC(GPPminmodelGDDLAI)
BIC(GPPminmodelGDD)

### Cp of the models

ols_mallows_cp(GPPminmodelGDDLAI)

## Cross validation
set.seed(1)
train<- sample(c(TRUE, FALSE), nrow(LUEmaxGDD), replace = TRUE)
test<- (!train)

## Training and testing of the models
LUEmaxGDDtrain <- LUEmaxGDD[train, ]
LUEmaxGDDtest<- LUEmaxGDD[test, ]

x<-LUEmaxGDDtest$GDDcum
y<-LUEmaxGDDtest$GPP_site
stderror<-LUEmaxGDDtest$STD_Error
u<-LUEmaxGDDtest$PredictedLUmaxWM
ts<-LUEmaxGDDtest$Tspvpm_modeled
ws<-LUEmaxGDDtest$Ws
fpar<-LUEmaxGDDtest$Fapar
par<-LUEmaxGDDtest$PAR_site
evi<-LUEmaxGDDtest$EVI_SG
lai<-LUEmaxGDDtest$LAIscale
lapply(LUEmaxGDDtest, class)



a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02

trainGPPminmodelGDDLAI<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011 + (int + coef* lai)), trace = TRUE, start = list(a = a, b= b, c=  c, d =d, int = int, coef = coef))
trainGPPminmodelGDD<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011 ), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))
trainGPPminmodelGDDLAI

a = coef(trainGPPminmodelGDDLAI)["a"]
b = coef(trainGPPminmodelGDDLAI)["b"]
c = coef(trainGPPminmodelGDDLAI)["c"]
d = coef(trainGPPminmodelGDDLAI)["d"]
int = trainGPPminmodelGDDLAI$m$getPars()[5]
coef = trainGPPminmodelGDDLAI$m$getPars()[6]
x= LUEmaxGDDtest$GDDcum
LUEmaxGDDtest$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
LUEmaxGDDtest$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDDtest$DAP)) - (0.0007*((LUEmaxGDDtest$DAP)^2)) -(1.619e-06*((LUEmaxGDDtest$DAP)^3))

plot(LUEmaxGDDtest$GDDcum, LUEmaxGDDtest$LUEmaxmodeled)
plot(LUEmaxGDDtest$DAP, LUEmaxGDDtest$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- LUEmaxGDDtest$Tair_satellite
Toptcal<-LUEmaxGDDtest$Topt
Tsite<- LUEmaxGDDtest$Tair_site
Toptmodeled<-LUEmaxGDDtest$Toptmodeled
LUEmaxGDDtest$Fapar <- (LUEmaxGDDtest$EVI_SG - 0.1)*1.25
LUEmaxGDDtest$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
LUEmaxGDDtest$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
LUEmaxGDDtest$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))






#LUEmaxGDDtest$Ws<- (1+LUEmaxGDDtest$LSWI_SG)/((1+max(LUEmaxGDDtest$LSWI_SG)))
LUEmaxGDDtest$LUEvpm<- (LUEmaxGDDtest$Tsvpm)*(LUEmaxGDDtest$Ws)*(0.05)
LUEmaxGDDtest$LUEpvpmcal<-(LUEmaxGDDtest$Tspvpm_cal)*(LUEmaxGDDtest$Ws)*(LUEmaxGDDtest$LUEmax)
LUEmaxGDDtest$LUEpvpmmodeled<- (LUEmaxGDDtest$Tspvpm_modeled)*(LUEmaxGDDtest$Ws)*(LUEmaxGDDtest$LUEmaxmodeled)


LUEmaxGDDtest$GPPvpm<- LUEmaxGDDtest$LUEvpm*LUEmaxGDDtest$Fapar*LUEmaxGDDtest$PAR_site*12.011
LUEmaxGDDtest$GPPpvpmcal<- LUEmaxGDDtest$LUEmax*LUEmaxGDDtest$Fapar*LUEmaxGDDtest$PAR_site*12.011
LUEmaxGDDtest$GPPpvpmmodel<- LUEmaxGDDtest$LUEpvpmmodeled*LUEmaxGDDtest$Fapar*LUEmaxGDDtest$PAR_site*12.011+ (int + coef* lai)
LUEmaxGDDtest$GPPpvpmmodelGDD<-((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts )*fpar*par*12.011+ (int + coef* lai))


library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel", "GPPpvpmmodelGDD")
LUEmaxGDDtest[cols.num] <- sapply(LUEmaxGDDtest[cols.num],as.numeric)
sapply(LUEmaxGDDtest, class)

rmse(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPpvpmcal)
rmse(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPpvpmmodelGDD)

mae(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPpvpmmodelGDD)

View(LUEmaxGDDtest)

residualsGDD <- (LUEmaxGDDtest$GPP_site - LUEmaxGDDtest$GPPpvpmmodel)
ressquar<-residualsGDD^2
sum(ressquar)


##
p<-ggplot(data=LUEmaxGDDtest, aes(x =GPP_site , y =GPPpvpmmodelGDD, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("ALLthesites PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p


##Cross validation on LUEmaxGDD 


LUEmaxGDDtrain <- LUEmaxGDD[train, ]

LUEmaxGDDtest<- LUEmaxGDD[test, ]

x<-LUEmaxGDDtest$GDDcum
y<-LUEmaxGDDtest$GPP_site
stderror<-LUEmaxGDDtest$STD_Error
u<-LUEmaxGDDtest$PredictedLUmaxWM
ts<-LUEmaxGDDtest$Tspvpm_modeled
ws<-LUEmaxGDDtest$Ws
fpar<-LUEmaxGDDtest$Fapar
par<-LUEmaxGDDtest$PAR_site
evi<-LUEmaxGDDtest$EVI_SG
lai<-LUEmaxGDDtest$LAIscale
lapply(LUEmaxGDDtest, class)
trainGPPminmodelGDD<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011 ), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))




a = trainGPPminmodelGDD$m$getPars()[1]
b =  trainGPPminmodelGDD$m$getPars()[2]
c =  trainGPPminmodelGDD$m$getPars()[3]
d =  trainGPPminmodelGDD$m$getPars()[4]
int = trainGPPminmodelGDD$m$getPars()[5]
coef = trainGPPminmodelGDD$m$getPars()[6]
x= LUEmaxGDDtest$GDDcum
LUEmaxGDDtest$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
LUEmaxGDDtest$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDDtest$DAP)) - (0.0007*((LUEmaxGDDtest$DAP)^2)) -(1.619e-06*((LUEmaxGDDtest$DAP)^3))

plot(LUEmaxGDDtest$GDDcum, LUEmaxGDDtest$LUEmaxmodeled)
plot(LUEmaxGDDtest$DAP, LUEmaxGDDtest$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- LUEmaxGDDtest$Tair_satellite
Toptcal<-LUEmaxGDDtest$Topt
Tsite<- LUEmaxGDDtest$Tair_site
Toptmodeled<-LUEmaxGDDtest$Toptmodeled
LUEmaxGDDtest$Fapar <- (LUEmaxGDDtest$EVI_SG - 0.1)*1.25
LUEmaxGDDtest$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
LUEmaxGDDtest$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
LUEmaxGDDtest$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))






#LUEmaxGDDtest$Ws<- (1+LUEmaxGDDtest$LSWI_SG)/((1+max(LUEmaxGDDtest$LSWI_SG)))
LUEmaxGDDtest$LUEvpm<- (LUEmaxGDDtest$Tsvpm)*(LUEmaxGDDtest$Ws)*(0.05)
LUEmaxGDDtest$LUEpvpmcal<-(LUEmaxGDDtest$Tspvpm_cal)*(LUEmaxGDDtest$Ws)*(LUEmaxGDDtest$LUEmax)
LUEmaxGDDtest$LUEpvpmmodeled<- (LUEmaxGDDtest$Tspvpm_modeled)*(LUEmaxGDDtest$Ws)*(LUEmaxGDDtest$LUEmaxmodeled)


LUEmaxGDDtest$GPPvpm<- LUEmaxGDDtest$LUEvpm*LUEmaxGDDtest$Fapar*LUEmaxGDDtest$PAR_site*12.011
LUEmaxGDDtest$GPPpvpmcal<- LUEmaxGDDtest$LUEmax*LUEmaxGDDtest$Fapar*LUEmaxGDDtest$PAR_site*12.011
LUEmaxGDDtest$GPPpvpmmodel<- LUEmaxGDDtest$LUEpvpmmodeled*LUEmaxGDDtest$Fapar*LUEmaxGDDtest$PAR_site*12.011
LUEmaxGDDtest$GPPpvpmmodelGDD<-((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts )*fpar*par*12.011)


library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel", "GPPpvpmmodelGDD")
LUEmaxGDDtest[cols.num] <- sapply(LUEmaxGDDtest[cols.num],as.numeric)
sapply(LUEmaxGDDtest, class)

rmse(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPpvpmcal)
rmse(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPpvpmmodel)

mae(LUEmaxGDDtest$GPP_site, LUEmaxGDDtest$GPPpvpmmodel)

View(LUEmaxGDDtest)

residualsGDD <- (LUEmaxGDDtest$GPP_site - LUEmaxGDDtest$GPPpvpmmodel)
ressquar<-residualsGDD^2
sum(ressquar)


##
p<-ggplot(data=LUEmaxGDDtest, aes(x =GPP_site , y =GPPpvpmmodelGDD, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("ALLthesites PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p

## How much percentage it is improving 

LUEmaxGDDtest$GPPpvpmmodel

AIC

