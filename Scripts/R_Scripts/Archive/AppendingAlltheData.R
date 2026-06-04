###APPEND all the scripts
library(tidyverse)
library(minpack.lm)
library(viridis)

LUEmaxTopte<-do.call("rbind", list(USOF1LUEmax, USOF2LUEmax, USOF3LUEmax, USOF4LUEmax, USOF5LUEmax, USOF6LUEmax, USBDA2015LUEmax, USBDCC2015LUEmax, USBDCC2016LUEmax, USBDA2016LUEmax, USHRA_2015LUEmax, USHRA_2016LUEmax, USHRA_2017LUEmax, USHRC_2015LUEmax, USHRC_2016LUEmax, USHRC_2017LUEmax))
LUEmaxTopte

par(mfrow = c(2, 1))
par(mar=c(2, 2, 2, 2))
plot(LUEmaxTopte$DOY,LUEmaxTopte$LUEmax)
plot(LUEmaxTopte$DOY,LUEmaxTopte$Topt)




LUEmaxTopte


## Drop the second value from USHRA2016


### Add the DOP to this dataframe
LUEmaxTopte<-LUEmaxTopte %>% mutate(DOP = case_when(
  site =="USOF1" & year == 2017 ~ 91,
  site =="USOF2" & year == 2017 ~ 91,
  site =="USOF3" & year == 2017 ~ 91,
  site =="USOF4" & year == 2018 ~ 99,
  site =="USOF5" & year == 2018 ~ 99,
  site =="USOF6" & year == 2018 ~ 99,
  site =="USBDA2015" & year == 2015 ~ 92,
  site =="USBDCC2015" & year == 2015 ~ 92,
  site =="USBDCC2016" & year == 2016 ~ 82,
  site =="USBDA2016" & year == 2016 ~ 82,
  site =="USHRA_2017" & year == 2017 ~ 100,
  site =="USHRC_2015" & year == 2015 ~ 98,
  site =="USHRC_2016" & year == 2016 ~ 114,
  site =="USHRC_2017" & year == 2017 ~ 100,
  site =="USHRA_2015" & year == 2015 ~ 97,
  site =="USHRA_2016" & year == 2016 ~ 114,
  

  TRUE ~ as.numeric(NA)))


LUEmaxTopte$DAP <- LUEmaxTopte$DOY - LUEmaxTopte$DOP

ggplot(data =LUEmaxTopte, aes(x = DAP, y = Topt))+
  geom_point(aes(col = as.factor(site)), size =4)+
  geom_smooth(method = lm)+
  theme(text = element_text(size = 30))


## SUbset LUEmaxTopte based on filter 0.1 values

LUEmaxTopte <- subset(LUEmaxTopte, LUEmax < 0.1) 
View(LUEma)
###Apply Modified Arrhenius Function
x<-LUEmaxTopte$DAP
z<-LUEmaxTopte$Topt
y<-LUEmaxTopte$LUEmax
stderror<-LUEmaxTopte$STD_Error
u<-LUEmaxTopte$PredictedLUmaxWM

nlsLM(y~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), start = list(a = 158, b =-3985, c = 75, d = 0.07), weights = 1/(stderror^2))
WeightedMAmodel<-nlsLM(y~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), start = list(a = 158, b =-3985, c = 75, d = 0.07), weights = 1/(stderror^2))
summary(WeightedMAmodel)

WeightedMAmodel

### some sites have years attached with it. We have to skip those
LUEmaxTopte$site_short<-substr(LUEmaxTopte$site, 1,6)
LUEmaxTopte$site_short<-as.factor(LUEmaxTopte$site_short)
LUEmaxTopte$year<-as.factor(LUEmaxTopte$year)

a = -1665
b= -3537
c=  68.5211
d = 0.071
ggplot(data=LUEmaxTopte, aes(x,y, shape=year, ymin = y-stderror, ymax = y+stderror)) +
  geom_point() + 
  stat_smooth(aes(group = 1), method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), 
              method.args = list(start = list( a = -1665, b= -3537, c=  68.5211, d = 0.071)),
              se = FALSE)+
  xlab("Days after planting")+
  ylab("LUEmax")+
  theme(text = element_text(size = 30))+
  geom_pointrange()+
  theme_bw(base_size= 25)
ggsave("LUEmaxoptimizedparamters.jpeg", dpi = 300)



GPPalldataVI8dayPARAMS







e<- WeightedMAmodel$m$getPars()[1]
f<- WeightedMAmodel$m$getPars()[2]
g<- WeightedMAmodel$m$getPars()[3]
h<- WeightedMAmodel$m$getPars()[4]


ggplot(data=LUEmaxTopte, aes(x,y)) +
  geom_point() + 
  stat_smooth(method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*8.14*c)))/(b-(a*(1-exp((b*(x-c))/(x*8.14*c)))))))), 
              method.args = list(start = list( a = -759.2884 , b= -2626.255 , w=  81.4211 , x = 0.05673379)),
              se = FALSE)+
  xlab("Days after planting")+
  ylab("LUEmax")+
  theme(text = element_text(size = 30))


DAPdf<-as.data.frame(LUEmaxTopte$LUEmax)
PredictedLUmaxWM<-predict(WeightedModel, DAPdf)
LUEmaxTopte$PredictedLUmaxWM<-((h*((f*exp((e*(x-g))/(x*8.14*g)))/(f-(e*(1-exp((f*(x-g))/(x*8.14*g))))))))
k<-LUEmaxTopte$PredictedLUmaxWM
  
ggplot(LUEmaxTopte, aes(x = x))+
  geom_point(aes(y = y))+
  geom_line(y=k)

plot(GPPalldataVI8day$doy, GPPalldataVI8day$GPPpvpmmodel, xlab = "DOY", ylab = "GPP")
plot(GPPalldataVI8day$DAP, GPPalldataVI8day$GPPpvpmmodel, xlab = "DAP", ylab = "GPP")



ggplot(LUEmaxTopte, aes(x = x, y = y, ymin = y- stderror, ymax = y+stderror)) +
  geom_point() +
  geom_errorbarh(height=.2)


# Initialize ggplot with data
f <- ggplot(
  LUEmaxTopte, 
  aes(x = x, y = y, ymin = y-stderror, ymax = y+stderror, col = as.factor(site))
)
f
f + geom_pointrange()


x<-LUEmaxTopte$DAP
z<-LUEmaxTopte$Topt
y<-LUEmaxTopte$LUEmax
stderror<-LUEmaxTopte$STD_Error
u<-LUEmaxTopte$PredictedLUmaxWM

nlsLM(z~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), start = list(a = 158, b =-3985, c = 75, d = 35))


### LUEmaxTopte
ggplot(data=LUEmaxTopte, aes(x,Topt)) +
  geom_point()+
  stat_smooth()+
  xlab("Days after planting")

library(drc)
library(aomisc)
model<- nls(z~NLS.beta(x,b,d,xb,xo, xc))
summary(model)


fit1<-lm(z ~ x + I(x^2) + I(x^3))            
plot(x,z, xlab = "Days after planting", ylab = "Topt (deg Celsius)", cex.lab=1.5, cex.axis=1.5) 
#use model to get predicted values
pred <- predict(fit1)
ix <- sort(x, index.return=T)$ix

#add polynomial curve to plot
lines(x[ix], pred[ix], col='red', lwd=2)
  
fit1




### All data together
GPPalldataVI8day<-do.call("rbind", list(USOF1VI8day, USOF2VI8day, USOF3VI8day, USOF4VI8day, USOF5VI8day, USOF6VI8day, USBDA2015VI8day, USBDC2015VI8day, USBDCC2016VI8day, USBDA2016VI8day, USHRA_2015VI8day, USHRA_2016VI8day, USHRA_2017VI8day, USHRC_2015VI8day, USHRC_2016VI8day, USHRC_2017VI8day))
GPPalldata

GPPalldataVI8day<-GPPalldataVI8day[!is.na(GPPalldataVI8day$GPP_site),]

p<-ggplot(data=GPPalldataVI8day, aes(x =GPP_site , y =GPPvpm, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 25)+
  ylim(-5, 25)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP VPM ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("All sites VPM")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("GPPALLDATAVI8DAYVPM.jpeg", width = 14, height = 7)


p<-ggplot(data=GPPalldataVI8day, aes(x =GPP_site , y =GPPpvpmcal, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM calculated ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("All sites PVPM calculated")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("GPPALLDATAVI8DAYPVPMcal.jpeg", width = 14, height = 7)

p<-ggplot(data=GPPalldataVI8day, aes(x =GPP_site , y =GPPpvpmmodel, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("All sites PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p
ggsave("GPPALLDATAVI8DAYPVPMmodeled.jpeg", width = 14, height = 7)
GPPalldataVI8day$Ws

p<-ggplot(data=GPPalldataVI8day, aes(x =DAP , y =Ws, col = doy))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  coord_fixed(ratio = 0.5) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  #xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  #ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("All sites PVPM modeled")#+
  #geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  #scale_color_viridis(option = "D", direction=-1)+
  #stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         #size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  #theme(text = element_text(size = 20))
p
ggsave("GPPALLDATAVI8DAYPVPMmodeled.jpeg", width = 14, height = 7)

plot(GPPalldataVI8day$DAP, GPPalldataVI8day$Ws)
plot(GPPalldataVI8day$DAP, GPPalldataVI8day$Tspvpm_cal)
plot(GPPalldataVI8day$DAP, GPPalldataVI8day$LUEmaxmodeled)
library(Metrics)
rmse(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPvpm, na.rm=TRUE)
rmse(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPpvpmcal, na.rm=TRUE)
rmse(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPpvpmmodel, na.rm=TRUE)

GPPalldataVI8day$GPP_site<-as.numeric(GPPalldataVI8day$GPP_site)
GPPalldataVI8day$GPPvpm<-as.numeric(GPPalldataVI8day$GPPvpm)
GPPalldataVI8day$GPPpvpmcal<-as.numeric(GPPalldataVI8day$GPPpvpmcal)
GPPalldataVI8day$GPPpvpmmodel<-as.numeric(GPPalldataVI8day$GPPpvpmmodel)

mae(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPvpm, na.rm=TRUE)
mae(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPpvpmcal, na.rm=TRUE)
mae(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPpvpmmodel, na.rm=TRUE)

bias(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPvpm)
bias(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPpvpmcal)
bias(GPPalldataVI8day$GPP_site, GPPalldataVI8day$GPPpvpmmodel)

GPPalldataVI8day$da

library(ggplot2)
library(reshape2)
meltdf <- melt(GPPalldataVI8day, id.vars=c("GPPvpm", "GPPpvpmcal", "GPPpvpmmodel"))
ggplot(meltdf,aes(x=Time,y=value,colour=variable,group=variable)) + geom_line()

view(meltdf)

separate_DF <- GPPalldataVI8day %>% separate(DAP, c("GPPvpm", "GPPpvpmcal"))
View(separate_DF)

separate_DF<-GPPalldataVI8day%>% select(GPPvpm, GPPpvpmcal, GPPpvpmmodel, GPP_site, DAP)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP"),
                variable.name = "Model",
                value.name = "GPP")

ggplot(df_long,aes(x=DAP,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =2)+geom_smooth()+
  xlab(bquote('Days after Planting'))+
  ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  theme(text = element_text(size = 20))

ggsave("GPPinterannual.jpeg", width = 14, height = 7)
GPPalldataVI8day$site

GPPalldataVI8day$Tsvpm
ggplot(GPPalldataVI8day,aes(x=DAP,y=Ws,colour=site, stroke = 1)) + geom_line(size =1)+ # geom_smooth()+
  xlab(bquote('Days after Planting'))+
  ylab(bquote('Water Scalar (Ws)'))+
  theme(text = element_text(size = 20))

ggplot(GPPalldataVI8day,aes(x=DAP,y=Tsvpm,colour=site, stroke = 1)) + geom_line(size =0.5)+ # geom_smooth()+
  xlab(bquote('Days after Planting'))+
  ylab(bquote('Temperature Scalar (Ts)'))+
  theme(text = element_text(size = 20))



View(GPPalldataVI8day)



USBDA2015VI8day$EVI_interpolated
p<-ggplot(data=GPPalldataVI8day, aes(x =EVI_SG , y =LUEmaxmodeled, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  geom_point(size =5) +
  #coord_fixed(ratio = 0.5) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('GPP EC ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(bquote('GPP PVPM modeled ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ggtitle("USBDA2015 PVPM modeled")+
  geom_smooth(method=lm, se = FALSE)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1)+
  stat_regline_equation( aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")), 
                         size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel4df)), size =5)+
  theme(text = element_text(size = 20))
p

library(plot3D)
scatter3D(GPPalldataVI8day$DOY, GPPalldataVI8day$Temp, GPPalldataVI8day$LUEmaxmodeled)

library("SciViews")

scatter3D(GPPalldataVI8day$EVI_SG, GPPalldataVI8day$DOY, GPPalldataVI8day$LUEmaxmodeled, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
plot(GPPalldataVI8day$EVI_SG, log10(GPPalldataVI8day$LUEmaxmodeled), col =GPPalldataVI8day$doy )
dflisttopt<-dflist

dflisttopt<-dflist
dflisttopt[[i]]<-dflist[[i]]




