### LUEmax GDD all the sites

##

LUEmaxGDD<-do.call("rbind", list(USOF1VI8day, USOF2VI8day, USOF3VI8day, USOF4VI8day, USOF5VI8day, USOF6VI8day, USBDA2015VI8day, USBDC2015VI8day, USBDCC2016VI8day, USBDA2016VI8day, USHRA_2015VI8day, USHRA_2016VI8day, USHRA_2017VI8day, USHRC_2015VI8day, USHRC_2016VI8day, USHRC_2017VI8day))
nrow(USOF1VI8day) + nrow ( USOF2VI8day) + nrow ( USOF3VI8day) + nrow ( USOF4VI8day) + nrow ( USOF5VI8day) + nrow ( USOF6VI8day) + nrow ( USBDA2015VI8day) + nrow ( USBDC2015VI8day) + nrow ( USBDCC2016VI8day) + nrow ( USBDA2016VI8day) + nrow ( USHRA_2015VI8day) + nrow ( USHRA_2016VI8day) + nrow ( USHRA_2017VI8day) + nrow ( USHRC_2015VI8day) + nrow ( USHRC_2016VI8day) + nrow ( USHRC_2017VI8day)


library(tidyverse) 
library(minpack.lm)
library(viridis)
library(ggpubr)


plot(LUEmaxGDD$GDDcum, LUEmaxGDD$LUEpvpmcal, xlab = "Cumulative GDD", ylab = "LUEmax")
plot(LUEmaxGDD$DAP, LUEmaxGDD$LUEpvpmcal, xlab = "Days after planting", ylab = "LUEmax")
plot(LUEmaxGDD$LAIscale, LUEmaxGDD$LUEpvpmcal, xlab = "Days after planting", ylab = "LUEmax")
plot(LUEmaxGDD$LAIscale, LUEmaxGDD$LUEmaxmodeled)




###Plot LUEmax LAI
ggplot(data=LUEmaxGDD, aes(LAIscale,LUEpvpmcal, colour = site, shape= as.factor(year))) +
  geom_point(size =3)+
  stat_smooth((aes(group = 1)))+
  xlab("Leaf area index")+
  ylab("LUEmax")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)
ggsave("LUEmaxLAI.jpeg", dpi = 300)


ggplot(LUEmaxGDD, aes(x=DAP, y = LUEmax)) +
  geom_point()+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)

ggplot(LUEmaxGDD, aes(x=DAP, y = LAIscale)) +
  geom_point()+
  theme(text = element_text(size = 30))+
  ylab("LAI")+
  theme_bw(base_size= 25)


LUEmaxGDD <- subset(LUEmaxGDD, select = -c(LSWI, EVI))
nrow(LUEmaxGDD)
LUEmaxGDD <- na.omit(LUEmaxGDD) 
nrow(LUEmaxGDD)

###Apply Modified Arrhenius Function
x<-LUEmaxGDD$GDDcum
z<-LUEmaxGDD$Topt
y<-LUEmaxGDD$LUEmax
stderror<-LUEmaxGDD$STD_Error
u<-LUEmaxGDD$PredictedLUmaxWM
lapply(LUEmaxGDD, class)

nlsLM(y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), start = list(a = 1551, b= 1807, c=  95, d =0.05891),control = nls.lm.control(maxiter = 1000))
WeightedMAmodel<-nlsLM(y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), start = list(a = 9687, b =20180, c = 984.5, d = 0.0622), weights = 1/(stderror^2))
summary(WeightedMAmodel)

WeightedMAmodel


a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02 


a = 1.046e+04                
b  = 2.693e+04 
c = 7.596e+02 
d = 7.957e-02 

a = -1665
b= -3537
c=  69
d = 0.071

a  =    8.65e+02   
b   =  2.03e+03 
c  = 8.70e+01 
d = 6.09e-02 

x= LUEmaxGDD$GDDcum
x
LUEmaxGDD$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
LUEmaxGDD$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDD$DAP)) - (0.0007*((LUEmaxGDD$DAP)^2)) -(1.619e-06*((LUEmaxGDD$DAP)^3))

plot(LUEmaxGDD$GDDcum, LUEmaxGDD$LUEmaxmodeled)
plot(LUEmaxGDD$GDDcum, LUEmaxGDD$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- LUEmaxGDD$Tair_satellite
Toptcal<-LUEmaxGDD$Topt
Tsite<- LUEmaxGDD$Tair_site
Toptmodeled<-LUEmaxGDD$Toptmodeled
LUEmaxGDD$Fapar <- (LUEmaxGDD$EVI_SG - 0.1)*1.25
LUEmaxGDD$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
LUEmaxGDD$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
LUEmaxGDD$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
LUEmaxGDD$DOP <- 99
LUEmaxGDD$DAP<-LUEmaxGDD$doy-LUEmaxGDD$DOP
View(LUEmaxGDD)





LUEmaxGDD$Ws<- (1+LUEmaxGDD$LSWI_SG)/((1+max(LUEmaxGDD$LSWI_SG)))
LUEmaxGDD$LUEvpm<- (LUEmaxGDD$Tsvpm)*(LUEmaxGDD$Ws)*(0.05)
LUEmaxGDD$LUEpvpmcal<-(LUEmaxGDD$Tspvpm_cal)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmax)
LUEmaxGDD$LUEpvpmmodeled<- (LUEmaxGDD$Tspvpm_modeled)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmaxmodeled)


LUEmaxGDD$GPPvpm<- LUEmaxGDD$LUEvpm*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmcal<- LUEmaxGDD$LUEmax*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodel<- LUEmaxGDD$LUEmaxmodeled*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011


plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodel)

library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel")
LUEmaxGDD[cols.num] <- sapply(LUEmaxGDD[cols.num],as.numeric)
sapply(LUEmaxGDD, class)

rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodel)

rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
mae(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodel)


LUEmaxGDD<-LUEmaxGDD[!is.na(LUEmaxGDD$GPP_site),]


ggplot(data=LUEmaxGDD, aes(DAP,LUEpvpmcal)) +
  geom_point() + 
  stat_smooth(method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), 
              method.args = list(start = list( a = a , b= b , c=  c , d = d)),
              se = FALSE)+
  xlab("Days after Planting")+
  ylab("LUEmax")+
  theme(text = element_text(size = 30))


View(LUEmaxGDD)


nrow(LUEmaxGDD)


library(reshape2)
separate_DF<-LUEmaxGDD%>% select(GPPvpm, GPPpvpmmodel, GPP_site, DAP)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP"),
                variable.name = "Model",
                value.name = "GPP")

ggplot(df_long,aes(x=DAP,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =2)+geom_smooth()+
  xlab(bquote('Days after Planting'))+
  ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  theme(text = element_text(size = 20))







coef(GPPminmodel)
confint(GPPminmodel)
deviance(GPPminmodel)
df.residual(GPPminmodel)
fitted(GPPminmodel)
formula(GPPminmodel)
logLik(GPPminmodel)
predict(GPPminmodel)
print(GPPminmodel)
profile(GPPminmodel)
residuals(GPPminmodel)
summary(GPPminmodel)
update(GPPminmodel)
vcov(GPPminmodel)
weights(GPPminmodel)


plot(LUEmaxGDD$GPPpvpmmodel, fitted(GPPminmodel))


p<-ggplot(data=LUEmaxGDD, aes(x =DAP , y =Ws, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) 

p

View(LUEmaxGDD)








### DAP
### Minimize GPP 

x<-LUEmaxGDD$DAP
y<-LUEmaxGDD$GPP_site
stderror<-LUEmaxGDD$STD_Error
u<-LUEmaxGDD$PredictedLUmaxWM
ts<-LUEmaxGDD$Tspvpm_modeled
ws<-LUEmaxGDD$Ws
fpar<-LUEmaxGDD$Fapar
par<-LUEmaxGDD$PAR_site
lapply(LUEmaxGDD, class)


a  = -1.06e+03             
b   = -2.86e+03  
c  = 6.29e+01  
d = 0.07



GPPminmodel<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011+ (int + coef* lai)), trace = TRUE, start = list(a = a, b= b, c=  c, d =d, int = int, coef = coef))
GPPminmodel

a = coef(GPPminmodel)["a"]
b = coef(GPPminmodel)["b"]
c = coef(GPPminmodel)["c"]
d = coef(GPPminmodel)["d"]




ggplot(data=LUEmaxGDD, aes(DAP,LUEpvpmcal)) +
  geom_point() + 
  stat_smooth(method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), 
              method.args = list(start = list( a = a , b= b , c=  c , d = d)),
              se = FALSE)+
  xlab("Days after planting")+
  ylab("LUEmax")+
  theme(text = element_text(size = 30))






coef(GPPminmodel)["a"]*3

x= LUEmaxGDD$DAP
x
LUEmaxGDD$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
LUEmaxGDD$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDD$DAP)) - (0.0007*((LUEmaxGDD$DAP)^2)) -(1.619e-06*((LUEmaxGDD$DAP)^3))

plot(LUEmaxGDD$GDDcum, LUEmaxGDD$LUEmaxmodeled)
plot(LUEmaxGDD$DAP, LUEmaxGDD$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- LUEmaxGDD$Tair_satellite
Toptcal<-LUEmaxGDD$Topt
Tsite<- LUEmaxGDD$Tair_site
Toptmodeled<-LUEmaxGDD$Toptmodeled
LUEmaxGDD$Fapar <- (LUEmaxGDD$EVI_SG - 0.1)*1.25
LUEmaxGDD$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
LUEmaxGDD$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
LUEmaxGDD$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))






#LUEmaxGDD$Ws<- (1+LUEmaxGDD$LSWI_SG)/((1+max(LUEmaxGDD$LSWI_SG)))
LUEmaxGDD$LUEvpm<- (LUEmaxGDD$Tsvpm)*(LUEmaxGDD$Ws)*(0.05)
LUEmaxGDD$LUEpvpmcal<-(LUEmaxGDD$Tspvpm_cal)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmax)
LUEmaxGDD$LUEpvpmmodeled<- (LUEmaxGDD$Tspvpm_modeled)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmaxmodeled)


LUEmaxGDD$GPPvpm<- LUEmaxGDD$LUEvpm*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmcal<- LUEmaxGDD$LUEmax*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodel<- LUEmaxGDD$LUEpvpmmodeled*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodelDAP<-((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts )*fpar*par*12.011)

plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
plot(LUEmaxGDD$GPPpvpmmodelDAP, LUEmaxGDD$GPPpvpmmodel)
plot(LUEmaxGDD$GPPpvpmmodel, fitted(GPPminmodel))
LUEmaxfrommodel<-(((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))))))
plot((LUEmaxGDD$GPPpvpmmodel), (LUEmaxfrommodel*ts*ws*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011))

plot(ts, LUEmaxGDD$Tspvpm_modeled)
plot(fpar, LUEmaxGDD$Fapar)
plot(y, LUEmaxGDD$GPP_site)
plot(ws, LUEmaxGDD$Ws)
plot(par, LUEmaxGDD$PAR_site)
plot((LUEmaxGDD$LUEmaxmodeled), ((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))))))

library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel")
LUEmaxGDD[cols.num] <- sapply(LUEmaxGDD[cols.num],as.numeric)
sapply(LUEmaxGDD, class)

rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodel)

mae(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodel)

View(LUEmaxGDD)

residuals <- (LUEmaxGDD$GPP_site - LUEmaxGDD$GPPpvpmmodel)
ressquar<-residuals^2
sum(ressquar)

p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
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

nrow(LUEmaxGDD)


library(reshape2)
separate_DF<-LUEmaxGDD%>% select(GPPvpm, GPPpvpmmodel, GPP_site, DAP)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP"),
                variable.name = "Model",
                value.name = "GPP")

ggplot(df_long,aes(x=DAP,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =2)+geom_smooth()+
  xlab(bquote('Days after Planting'))+
  ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  theme(text = element_text(size = 20))




coef(GPPminmodel)
confint(GPPminmodel)
deviance(GPPminmodel)
df.residual(GPPminmodel)
fitted(GPPminmodel)
formula(GPPminmodel)
logLik(GPPminmodel)
predict(GPPminmodel)
print(GPPminmodel)
profile(GPPminmodel)
residuals(GPPminmodel)
summary(GPPminmodel)
update(GPPminmodel)
vcov(GPPminmodel)
weights(GPPminmodel)


plot(LUEmaxGDD$GPPpvpmmodel, fitted(GPPminmodel))


p<-ggplot(data=LUEmaxGDD, aes(x =DAP , y =Ws, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) 

p

View(LUEmaxGDD)

y<-LUEmaxGDD$GPP_site
x<- LUEmaxGDD$LAIscale

class(fpar)

### LAI implementation of non linear regression 
## Based on the relationship between LAI and LUEmax we see a 
LAImodel<-nlsLM(y~((Vm*x/(K+x))* ws* ts *fpar*par*12.011), trace = TRUE, start = list(K =6 , Vm = 12))

LAImodel

Vm = coef(LAImodel)["Vm"]
K = coef(LAImodel)["K"]


LUEmaxGDD$GPPpvpmmodelLAI<- (Vm*x/(K+x))* ws* ts *fpar*par*12.011
cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel")
LUEmaxGDD[cols.num] <- sapply(LUEmaxGDD[cols.num],as.numeric)
sapply(LUEmaxGDD, class)
LUEmaxGDD$GPPpvpmmodelLAI<-as.numeric(LUEmaxGDD$GPPpvpmmodelLAI)
plot(LUEmaxGDD$GPPpvpmmodelLAI, fitted(LAImodel))
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelLAI)
mae(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelLAI)
residuals <- (LUEmaxGDD$GPP_site - LUEmaxGDD$GPPpvpmmodelLAI)
ressquar<-residuals^2
sum(ressquar)

View(LUEmaxGDD)

LUEmaxGDD$GPPpvpmLAIavg <- (LUEmaxGDD$GPPpvpmmodel+LUEmaxGDD$GPPpvpmmodelLAI)/2
LUEmaxGDD$GPPpvpmLAIavg<-as.numeric(LUEmaxGDD$GPPpvpmLAIavg)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodel)


LUEmaxGDD$DAPLAI <- LUEmaxGDD$DAP * LUEmaxGDD$LAIscale
LUEmaxGDD$CumGDDLAI <- LUEmaxGDD$GDDcum * LUEmaxGDD$LAIscale

plot(LUEmaxGDD$DAPLAI, LUEmaxGDD$LUEpvpmcal)
plot(LUEmaxGDD$LAIscale ,residuals)
residuals

LAIDAPrelation <- lm(residuals~LUEmaxGDD$LAIscale)

int = coef(LAIDAPrelation)[1]
coef =  coef(LAIDAPrelation)[2]



LUEmaxGDD$GPPpvpmmodeldaplaiadj<-LUEmaxGDD$GPPpvpmmodel + int + coef* LUEmaxGDD$LAIscale
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodeldaplaiadj)




plot(LUEmaxGDD$LAIscale ,residualsGDD)
residualsGDD

LAIGDDrelation <- lm(residualsGDD~LUEmaxGDD$LAIscale)

int = coef(LAIDAPrelation)[1]
coef =  coef(LAIDAPrelation)[2]



LUEmaxGDD$GPPpvpmmodelgddlaiadj<-LUEmaxGDD$GPPpvpmmodelGDD + int + coef* LUEmaxGDD$LAIscale
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelgddlaiadj)


p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPpvpmmodelgddlaiadj, col = doy))+
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


LUEmaxGDD$site <- factor(LUEmaxGDD$site)
p<-ggplot(data=LUEmaxGDD, aes(x =EVI_SG , y= LAIscale, col = doy, shape=site)) +
  scale_shape_manual(values = 0:10)+
  geom_point(size =5) +
  xlab(bquote('EVI'))+
  ylab(bquote('Leaf Area Index'))
p

### LUEmax graph for the ameriflux conference
ggplot(data=LUEmaxGDD, aes(x,y)) +
  geom_point() + 
  stat_smooth(method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), 
              method.args = list(start = list( a = -759.2884 , b= -2626.255 , w=  81.4211 , x = 0.05673379)),
              se = FALSE)+
  xlab("Days after planting")+
  ylab("LUEmax")+
  theme(text = element_text(size = 30))



#### GPPmodel using GPPcum no lAI

x<-LUEmaxGDD$GDDcum
y<-LUEmaxGDD$GPP_site
stderror<-LUEmaxGDD$STD_Error
u<-LUEmaxGDD$PredictedLUmaxWM
ts<-LUEmaxGDD$Tspvpm_modeled
ws<-LUEmaxGDD$Ws
fpar<-LUEmaxGDD$Fapar
par<-LUEmaxGDD$PAR_site
evi<-LUEmaxGDD$EVI_SG

lapply(LUEmaxGDD, class)

a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02 


GPPminmodel<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))* ws* ts *fpar*par*12.011 ), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))

GPPminmodel




library(hrbrthemes)
library(broom)
library(viridis)
library(kinfitr)
library(nls.multstart)
library(nlme)
library(brms)
library(nlshelper)


a = GPPminmodel$m$getPars()[1]
b = GPPminmodel$m$getPars()[2]
c = GPPminmodel$m$getPars()[3]
d = GPPminmodel$m$getPars()[4]

a
b
c
d

LUEmaxmodelparameterscsv<-list(a =a, b = b, c = c, d =d)
LUEmaxmodelparameterscsv<-as.data.frame(LUEmaxmodelparameterscsv)

write.csv(LUEmaxmodelparameterscsv,"/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Desktop_Laptop_exchange/LUEmaxParameters/LUEmaxmodelparameters.csv", row.names = FALSE)
LUEmaxGDD$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))
LUEmaxGDD$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDD$DAP)) - (0.0007*((LUEmaxGDD$DAP)^2)) -(1.619e-06*((LUEmaxGDD$DAP)^3))
LUEmaxGDD$Toptmodeled<- 19.23+ (0.02011*(LUEmaxGDD$GDDcum))  -(9.693e-06*((LUEmaxGDD$GDDcum)^2)) +(1.032e-09*((LUEmaxGDD$GDDcum)^3))


plot(LUEmaxGDD$GDDcum, LUEmaxGDD$LUEmaxmodeled)
plot(LUEmaxGDD$GDDcum, LUEmaxGDD$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- LUEmaxGDD$Tair_satellite
Toptcal<-LUEmaxGDD$Topt
Tsite<- LUEmaxGDD$Tair_site
Toptmodeled<-LUEmaxGDD$Toptmodeled
LUEmaxGDD$Fapar <- (LUEmaxGDD$EVI_SG - 0.1)*1.25
LUEmaxGDD$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
LUEmaxGDD$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
LUEmaxGDD$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))
ts<-LUEmaxGDD$Tspvpm_modeled





#LUEmaxGDD$Ws<- (1+LUEmaxGDD$LSWI_SG)/((1+max(LUEmaxGDD$LSWI_SG)))
LUEmaxGDD$LUEvpm<- (LUEmaxGDD$Tsvpm)*(LUEmaxGDD$Ws)*(0.05)
LUEmaxGDD$LUEpvpmcal<-(LUEmaxGDD$Tspvpm_cal)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmax)
LUEmaxGDD$LUEpvpmmodeled<- (LUEmaxGDD$Tspvpm_modeled)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmaxmodeled)


LUEmaxGDD$GPPvpm<- LUEmaxGDD$LUEvpm*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmcal<- LUEmaxGDD$LUEmax*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodel<- LUEmaxGDD$LUEpvpmmodeled*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodelGDD<-((((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))* ws* ts )*fpar*par*12.011)

plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelGDD)
plot( LUEmaxGDD$GPPpvpmmodel, fitted(GPPminmodel))
LUEmaxfrommodel<-(((((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))))))
plot((LUEmaxGDD$GPPpvpmmodel), (LUEmaxfrommodel*ts*ws*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011))

plot(ts, LUEmaxGDD$Tspvpm_modeled)
plot(fpar, LUEmaxGDD$Fapar)
plot(y, LUEmaxGDD$GPP_site)
plot(ws, LUEmaxGDD$Ws)
plot(par, LUEmaxGDD$PAR_site)
plot((LUEmaxGDD$LUEmaxmodeled), ((((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))))))

library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel", "GPPpvpmmodelGDD")
LUEmaxGDD[cols.num] <- sapply(LUEmaxGDD[cols.num],as.numeric)
sapply(LUEmaxGDD, class)

rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelGDD)

mae(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelGDD)

View(LUEmaxGDD)

residualsGDD <- (LUEmaxGDD$GPP_site - LUEmaxGDD$GPPpvpmmodel)
ressquar<-residualsGDD^2
sum(ressquar)

p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPpvpmmodelGDD, col = doy))+
  
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =8) +

  coord_fixed(ratio = 0.6) +
  xlim(-2, 27)+
  ylim(-2, 27)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  
 # ggtitle("ALLthesites PVPM modeled incorporating LAI")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 15)+
  labs(col="DOY")+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 30))+
  theme(plot.margin = unit(c(-2,0,-2,0), "cm")) # ("left", "right", "bottom", "top")


p

ggsave("ALLthesites PVPM modeled.jpeg", width = 18, height = 12, dpi = 600)

### LUEmax graph for the ameriflux conference


z <-LUEmaxGDD$LUEpvpmcal
LUEmaxGDD$year<- as.factor(LUEmaxGDD$year)
ggplot(data=LUEmaxGDD, aes(x,z, col = site, shape=year)) +
  geom_point(size = 5) + 
  stat_smooth(aes(group = 1), method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), 
              method.args = list(start = list( a = GPPminmodel$m$getPars()[1] , b= GPPminmodel$m$getPars()[2] , c=  GPPminmodel$m$getPars()[3] , d = GPPminmodel$m$getPars()[4])),
              se = FALSE)+
  xlab("Cumulative Growing Degree Days (\u00B0C)")+
  ylab((bquote(''*LUE[max]*'(mol'*~CO[2]~ '/mol PPFD'*')')))+
  theme_classic()+
  theme(text = element_text(size = 25))
ggsave("LUEmax.jpeg", dpi = 300)


p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPvpm, col = doy))+
  
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =8) +
  
  coord_fixed(ratio = 0.6) +
  xlim(-2, 27)+
  ylim(-2, 27)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  
  # ggtitle("ALLthesites PVPM modeled incorporating LAI")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 15)+
  labs(col="DOY")+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 30))+
  theme(plot.margin = unit(c(-2,0,-2,0), "cm")) # ("left", "right", "bottom", "top")


p

ggsave("ALLthesites VPM modeled.jpeg", width = 18, height = 12, dpi = 600)

##conferennce
p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPvpm, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(bquote('GPP VPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation(label.x = -2, label.y = 28, size = 15)+
  stat_cor(aes(label = ..rr.label..), label.x = -2, label.y = 24, size = 15)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 40))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))



p  
ggsave(p, filename = "VPMallsites.png", width = 12, height =9)

p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPpvpmmodelGDD, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation(label.x = -2, label.y = 28, size = 15)+
  stat_cor(aes(label = ..rr.label..), label.x = -2, label.y = 24, size = 15)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 40))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))



p  
ggsave(p, filename = "PVPMallsites.png", width = 12, height =9)

### Modeled Data

LUEmaxGDD
###Read the modeled data

# Join the data based on the date and 

## Make a new dataframe for 2:2 graph of PVPM and VPM









####using LAI in the model
### Minimize GPP 

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
int =   2.358e+00  
coef = -8.853e-02 

GPPminmodel<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts *fpar*par*12.011 + (int + coef* lai)), trace = TRUE, start = list(a = a, b= b, c=  c, d =d, int = int, coef = coef))

GPPminmodel



library(hrbrthemes)
library(broom)
library(viridis)
library(kinfitr)
library(nls.multstart)
library(nlme)
library(brms)
library(nlshelper)


a = GPPminmodel$m$getPars()[1]
b = GPPminmodel$m$getPars()[2]
c = GPPminmodel$m$getPars()[3]
d = GPPminmodel$m$getPars()[4]
int = GPPminmodel$m$getPars()[5]
coef = GPPminmodel$m$getPars()[6]




LUEmaxGDD$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))
LUEmaxGDD$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDD$DAP)) - (0.0007*((LUEmaxGDD$DAP)^2)) -(1.619e-06*((LUEmaxGDD$DAP)^3))

plot(LUEmaxGDD$GDDcum, LUEmaxGDD$LUEmaxmodeled)
plot(LUEmaxGDD$DAP, LUEmaxGDD$Toptmodeled)


## Fapar
Tmax = 48
Tmin = -1
Tsatellite<- LUEmaxGDD$Tair_satellite
Toptcal<-LUEmaxGDD$Topt
Tsite<- LUEmaxGDD$Tair_site
Toptmodeled<-LUEmaxGDD$Toptmodeled
LUEmaxGDD$Fapar <- (LUEmaxGDD$EVI_SG - 0.1)*1.25
LUEmaxGDD$Tspvpm_cal<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-Toptcal)^2))
LUEmaxGDD$Tsvpm<- ((Tsite- Tmax) * (Tsite-Tmin))/ (((Tsite- Tmax)*(Tsite-Tmin))-((Tsite-30)^2))
LUEmaxGDD$Tspvpm_modeled<- ((Tsatellite- Tmax) * (Tsatellite-Tmin))/ (((Tsatellite- Tmax)*(Tsatellite-Tmin))-((Tsatellite-Toptmodeled)^2))






#LUEmaxGDD$Ws<- (1+LUEmaxGDD$LSWI_SG)/((1+max(LUEmaxGDD$LSWI_SG)))
LUEmaxGDD$LUEvpm<- (LUEmaxGDD$Tsvpm)*(LUEmaxGDD$Ws)*(0.05)
LUEmaxGDD$LUEpvpmcal<-(LUEmaxGDD$Tspvpm_cal)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmax)
LUEmaxGDD$LUEpvpmmodeled<- (LUEmaxGDD$Tspvpm_modeled)*(LUEmaxGDD$Ws)*(LUEmaxGDD$LUEmaxmodeled)


LUEmaxGDD$GPPvpm<- LUEmaxGDD$LUEvpm*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmcal<- LUEmaxGDD$LUEmax*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodel<- LUEmaxGDD$LUEpvpmmodeled*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011
LUEmaxGDD$GPPpvpmmodelGDD<-((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c))))))))* ws* ts )*fpar*par*12.011+ (int + coef* lai))

plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
plot(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelGDD)
plot( LUEmaxGDD$GPPpvpmmodel, fitted(GPPminmodel))
LUEmaxfrommodel<-(((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))))))
plot((LUEmaxGDD$GPPpvpmmodel), (LUEmaxfrommodel*ts*ws*LUEmaxGDD$Fapar*LUEmaxGDD$PAR_site*12.011))

plot(ts, LUEmaxGDD$Tspvpm_modeled)
plot(fpar, LUEmaxGDD$Fapar)
plot(y, LUEmaxGDD$GPP_site)
plot(ws, LUEmaxGDD$Ws)
plot(par, LUEmaxGDD$PAR_site)
plot((LUEmaxGDD$LUEmaxmodeled), ((((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))))))

library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "GPPpvpmcal", "GPPpvpmmodel", "GPPpvpmmodelGDD")
LUEmaxGDD[cols.num] <- sapply(LUEmaxGDD[cols.num],as.numeric)
sapply(LUEmaxGDD, class)

rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPvpm, na.rm=TRUE)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmcal)
rmse(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelGDD)

mae(LUEmaxGDD$GPP_site, LUEmaxGDD$GPPpvpmmodelGDD)

View(LUEmaxGDD)

residualsGDD <- (LUEmaxGDD$GPP_site - LUEmaxGDD$GPPpvpmmodel)
ressquar<-residualsGDD^2
sum(ressquar)

p<-ggplot(data=LUEmaxGDD, aes(x =GPP_site , y =GPPpvpmmodelGDD, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("ALLthesites PVPM modeled incorporating LAI")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p

ggsave("ALLthesites PVPM modeled incorporating LAI.jpeg", dpi = 300)


### Topt using Cum gdd
x<- LUEmaxGDD$GDDcum
y<-LUEmaxGDD$Topt
fit1<-lm(y ~ x + I(x^2) + I(x^3))            
plot(x,y, xlab = "Days after planting", ylab = "Topt (deg Celsius)", cex.lab=1.5, cex.axis=1.5) 
#use model to get predicted values
pred <- predict(fit1)
ix <- sort(x, index.return=T)$ix

#add polynomial curve to plot
lines(x[ix], pred[ix], col='red', lwd=2)

fit1

pred

LUEmaxGDD$Toptmodeled<- 19.23+ (0.02011*(LUEmaxGDD$GDDcum))  -(9.693e-06*((LUEmaxGDD$GDDcum)^2)) +(1.032e-09*((LUEmaxGDD$GDDcum)^3))
LUEmaxGDD$Toptmodeled


c = fit1$coefficients[1]
x1 = fit1$coefficients[2]
x2= fit1$coefficients[3]
x3 = fit1$coefficients[4]

c
x1
x2
x3

Toptparams<-list(c =c, x1 = x1, x2 = x2, x3 =x3)
Toptparams<-as.data.frame(Toptparams)

write.csv(Toptparams,"/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Desktop_Laptop_exchange/LUEmaxParameters/Toptparams.csv", row.names = FALSE)



## Data wrangling taking the mean of the all sites
LUEmaxTopte<-do.call("rbind", list(USOF1LUEmax, USOF2LUEmax, USOF3LUEmax, USOF4LUEmax, USOF5LUEmax, USOF6LUEmax, USBDA2015LUEmax, USBDCC2015LUEmax, USBDCC2016LUEmax, USBDA2016LUEmax, USHRA_2015LUEmax, USHRA_2016LUEmax, USHRA_2017LUEmax, USHRC_2015LUEmax, USHRC_2016LUEmax, USHRC_2017LUEmax))

LUEmaxTopte$siteyear<-paste(LUEmaxTopte$site, LUEmaxTopte$year)

LUEmaxoptsite<-LUEmaxTopte %>%
  group_by(site) %>%
  summarise(across(STD_Error:Residual.sum.of.square , mean), n = n())

LUEmaxoptsiteyear<-LUEmaxTopte %>%
  group_by(siteyear) %>%
  summarise(across(STD_Error:Residual.sum.of.square , mean), n = n())


LUEmaxoptyear<-LUEmaxTopte %>%
  group_by(year) %>%
  summarise(across(STD_Error:Residual.sum.of.square , mean), n = n())

write.csv(LUEmaxoptsiteyear,"/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Desktop_Laptop_exchange/LUEmaxParameters/LUEmaxoptsiteyear.csv", row.names = FALSE)
write.csv(LUEmaxoptsite,"/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Desktop_Laptop_exchange/LUEmaxParameters/LUEmaxoptsite.csv", row.names = FALSE)
write.csv(LUEmaxoptyear,"/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Desktop_Laptop_exchange/LUEmaxParameters/LUEmaxoptyear.csv", row.names = FALSE)


## r2 rolling window
plot(LUEmaxGDD$LUEmaxmodeled)
