library(minpack.lm)
library(tidyverse)

df_luemaxgdd_modeled


View(df_luemaxgdd_modeled)
df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USBDA = "US-BDA")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USBDC = "US-BDC")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USHRC = "US-HRC")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USHRA = "US-HRA")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USOF1 = "US-OF1")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USOF2 = "US-OF2")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USOF3 = "US-OF3")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USOF4 = "US-OF4")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USOF5 = "US-OF5")

df_luemaxgdd_modeled$sitenew <- recode(df_luemaxgdd_modeled$sitenew ,
                                       USOF6 = "US-OF6")
ggplot(data=df_luemaxgdd_modeled, aes(Tsvpm, GPP_site, colour = sitenew, shape= as.factor(year))) +
  geom_point(size =3)+
  #stat_smooth((aes(group = 1)))+
  xlab("Ts")+
  ylab("GPP-EC")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)

ggplot(data=df_luemaxgdd_modeled, aes(Ws.y, GPP_site, colour = sitenew, shape= as.factor(year))) +
  geom_point(size =3)+
  #stat_smooth((aes(group = 1)))+
  xlab("Ws")+
  ylab("GPP-EC")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)


df_luemaxgdd_modeled$siteyear <-  paste(df_luemaxgdd_modeled$site.x, df_luemaxgdd_modeled$year, sep = "")
df_luemaxgdd_modeled$siteyear

# Use the aggregate function to calculate the max value of each group
max_values <- aggregate(LSWI_SG.x ~ siteyear, data = df_luemaxgdd_modeled, FUN = max)
max_values

# Merge the max values back into the original dataframe
df_luemaxgdd_modeled <- merge(df_luemaxgdd_modeled, max_values, by = "siteyear", suffixes = c("", "_max"))

df_luemaxgdd_modeled$Ws_growing<-(1+df_luemaxgdd_modeled$LSWI_SG.x)/(1+df_luemaxgdd_modeled$LSWI_SG.x_max)


plot(df_luemaxgdd_modeled$doy.x, df_luemaxgdd_modeled$Ws_growing)


df_luemaxgdd_modeled$gpppvpmwsgrowing<-(df_luemaxgdd_modeled$Ws_growing)*(df_luemaxgdd_modeled$Tspvpm_modeled)*(df_luemaxgdd_modeled$LUEmaxmodeled)*(df_luemaxgdd_modeled$Fapar) *(df_luemaxgdd_modeled$PAR_site)*12.011
df_luemaxgdd_modeled$gpppvpmwsgrowingsat<-(df_luemaxgdd_modeled$Ws_growing)*(df_luemaxgdd_modeled$Tspvpm_modeled)*(df_luemaxgdd_modeled$lu)*(df_luemaxgdd_modeled$Fapar) *(df_luemaxgdd_modeled$par)*12.011

plot(df_luemaxgdd_modeled$gpppvpmwsgrowing, df_luemaxgdd_modeled$GPP_site)
df_luemaxgdd_modeled$gpppvpmwsgrowing



GPPsatellitePVPMwsgrowing<-ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =gpppvpmwsgrowing, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(100, 250))+
  stat_regline_equation(label.x = -4, label.y = 28, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -4.5, label.y = 24, size = 18)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  annotate("text", x = 0.75, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 18, fontface = 'italic') +
  annotate("text", x = 0.45, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 18, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
GPPsatellitePVPMwsgrowing



x<-df_luemaxgdd_modeled$GDDcum
y<-df_luemaxgdd_modeled$GPP_site
stderror<-LUEmaxGDD$STD_Error
u<-LUEmaxGDD$PredictedLUmaxWM
ts<-df_luemaxgdd_modeled$Tspvpm_modeled
ws<-df_luemaxgdd_modeled$Ws_growing
fpar<-df_luemaxgdd_modeled$Fapar
par<-df_luemaxgdd_modeled$PAR_site
evi<-df_luemaxgdd_modeled$EVI_SG

lapply(LUEmaxGDD, class)

a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02 


GPPminmodelwsgrow<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))* ws* ts *fpar*par*12.011 ), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))

a = GPPminmodelwsgrow$m$getPars()[1]
b = GPPminmodelwsgrow$m$getPars()[2]
c = GPPminmodelwsgrow$m$getPars()[3]
d = GPPminmodelwsgrow$m$getPars()[4]

df_luemaxgdd_modeled$LUEmaxmodeled<-((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))

mean(df_luemaxgdd_modeled$Ws_growing)
mean(df_luemaxgdd_modeled$Tspvpm_modeled)

min(df_luemaxgdd_modeled$Ws_growing)
min(df_luemaxgdd_modeled$Tspvpm_modeled)

max(df_luemaxgdd_modeled$Ws_growing)
max(df_luemaxgdd_modeled$Tspvpm_modeled)

sqrt(var(df_luemaxgdd_modeled$Ws_growing))
sqrt(var(df_luemaxgdd_modeled$Tspvpm_modeled))



# Use the group_by and summarize functions in dplyr to calculate the mean value of each group
df_luemaxgdd_modeled %>%
  group_by(siteyear) %>%
  summarize(mean_value1 = mean(LUEpvpmcal))

# Print the resulting dataframe
print(mean_values)

mean(c(0.023,0.045,0.055))
mean(c(0.028,0.037,0.0490))

mean(c(9.37, 10.30, 10.42))
mean(c(9.18, 10.47, 9.92))


df_luemaxgdd_modeled$newgpp<- df_luemaxgdd_modeled$gpppvpmwsgrowingsat/ (df_luemaxgdd_modeled$Ws_growing)
df_luemaxgdd_modeled$newgpp<- df_luemaxgdd_modeled$newgpp/ (df_luemaxgdd_modeled$Tspvpm)




ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =gpppvpmwsgrowingsat, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(100, 250))+
  stat_regline_equation(label.x = -4, label.y = 28, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -4.5, label.y = 24, size = 18)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))


df_luemaxgdd_modeled$luemaxbacktsws<-(df_luemaxgdd_modeled$LUEpvpmcal)/(df_luemaxgdd_modeled$Ws_growing)
df_luemaxgdd_modeled$luemaxbacktsws<-(df_luemaxgdd_modeled$luemaxbacktsws)/(df_luemaxgdd_modeled$Tspvpm_modeled)

ggplot(data=df_luemaxgdd_modeled, aes(GDDcum,LUEpvpmcal)) +
  geom_point(size = 5) 

a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02 

x<-df_luemaxgdd_modeled$GDDcum
y<-df_luemaxgdd_modeled$luemaxbacktsws
stderror<-df_luemaxgdd_modeled$STD_Error
u<-df_luemaxgdd_modeled$PredictedLUmaxWM
ts<-df_luemaxgdd_modeled$Tspvpm_modeled
ws<-df_luemaxgdd_modeled$Ws_growing
fpar<-df_luemaxgdd_modeled$Fapar
par<-df_luemaxgdd_modeled$PAR_site
evi<-df_luemaxgdd_modeled$EVI_SG
lai<-df_luemaxgdd_modeled$LAIscale
lapply(df_luemaxgdd_modeled, class)

GPPminmodel<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))
WeightedMAmodel<-nlsLM(y~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), start = list(a = 158, b =-3985, c = 75, d = 0.07), weights = 1/(stderror^2))




a = GPPminmodel$m$getPars()[1]
b = GPPminmodel$m$getPars()[2]
c=  GPPminmodel$m$getPars()[3]
d = GPPminmodel$m$getPars()[4]

a = -1665
b= -3537
c=  68.5211
d = 0.071


x<-df_luemaxgdd_modeled$DAP.y
z <-df_luemaxgdd_modeled$luemaxbacktsws

ggplot(data=df_luemaxgdd_modeled, aes(x,z, col = site.x)) +
  geom_point(size = 5) 
df_luemaxgdd_modeled$gpppvpmwsgrowingsatnew<-(((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))*fpar*par*12.011*ts*ws )

df_luemaxgdd_modeled$gpppvpmwsgrowingsatnew

ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =gpppvpmwsgrowingsatnew, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(100, 250))+
  stat_regline_equation(label.x = -4, label.y = 28, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -4.5, label.y = 24, size = 18)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  annotate("text", x = 0.75, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 18, fontface = 'italic') +
  annotate("text", x = 0.45, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 18, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

GPPsatellitePVPMwsgrowing
x<-df_luemaxgdd_modeled$GDDnzh
df_luemaxgdd_modeled$GPPsatellitegrownew<-(((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))*df_luemaxgdd_modeled$FAPAR_sg*df_luemaxgdd_modeled$par*12.011 )


df_luemaxgdd_modeled$luemaxbackcalculated<- ((df_luemaxgdd_modeled$GPP_site) /((df_luemaxgdd_modeled$FAPAR_sg)*(df_luemaxgdd_modeled$Tspvpm_modeled) *(df_luemaxgdd_modeled$PAR_site)*(df_luemaxgdd_modeled$Ws_growing) *12.011))
z<-df_luemaxgdd_modeled2$luemaxbackcalculated
x<-df_luemaxgdd_modeled2$GDDnzh
ggplot(data=df_luemaxgdd_modeled2, aes(x,z, col = site.x)) +
  geom_point(size = 5) 

plot(df_luemaxgdd_modeled$luemaxbackcalculated, df_luemaxgdd_modeled$LUEmaxmodeled)

df_luemaxgdd_modeled$luemaxbackcalculated2<-(df_luemaxgdd_modeled$luemaxbackcalculated+df_luemaxgdd_modeled$LUEpvpmcal)/2

df_luemaxgdd_modeled2<-df_luemaxgdd_modeled%>%filter(luemaxbackcalculated2<0.5)

x<-df_luemaxgdd_modeled$DAP.x
y<-df_luemaxgdd_modeled$LUEpvpmcal
library(minpack.lm)
library(minpack.lm)

# Calculate weights based on inverse standard errors
weights <- df_luemaxgdd_modeled$STD_Error

# Check for missing or infinite weights
missing_weights <- is.na(weights) | !is.finite(weights)

# Invert and square non-missing and finite weights
weights[!missing_weights] <- 1 / weights[!missing_weights]^2

model <- nlsLM(y ~ a * (exp(x *(- b)) * (1 - exp(x * (-c)))), 
               start = list(a = 0.06, b = 0.016, c = 0.26),
               control = nls.lm.control(maxiter = 100))

a = model$m$getPars()[1]
b = model$m$getPars()[2]
c= model$m$getPars()[3]

df_luemaxgdd_modeled$luemaxmodeledhyperbola <-(a * (exp(-x * b)) * (1 - exp(-x * c)))
plot(df_luemaxgdd_modeled$GDDcum, df_luemaxgdd_modeled$luemaxbacktsws)

df_luemaxgdd_modeled$gpppvpmwsgrowingnewfunnc<-(df_luemaxgdd_modeled$luemaxmodeledhyperbola*fpar*par*12.011*ts*ws )


df_luemaxgdd_modeled$residualsitevpmpercent<-abs((df_luemaxgdd_modeled$GPPvpm -df_luemaxgdd_modeled$GPP_site)/df_luemaxgdd_modeled$GPP_site)*100

df_luemaxgdd_modeled$residualspatialvpmpercent<-abs((df_luemaxgdd_modeled$gpp - df_luemaxgdd_modeled$GPP_site)/df_luemaxgdd_modeled$GPP_site)*100



df_luemaxgdd_modeled$residualsitevpm<-df_luemaxgdd_modeled$gpp -df_luemaxgdd_modeled$GPP_site

df_luemaxgdd_modeled$residualspatialvpm<-df_luemaxgdd_modeled$GPPvpm - df_luemaxgdd_modeled$GPP_site
par(mfrow=c(2,2), mar=c(5, 5, 1, 1), oma=c(1, 1, 1, 1))

# Set font size parameters
par(cex.lab=1.5, cex.main=1.5, cex.axis=1.2)

site <- "site"
plot(df_luemaxgdd_modeled$DAP.x, df_luemaxgdd_modeled$residualsitevpm,
     xlab = "", ylab = bquote("Residuals from VPM"[.(site)]))
text(5, max(df_luemaxgdd_modeled$residualsitevpm)- 0.3, "A", cex=1.5)

site <- "site"
plot(df_luemaxgdd_modeled$DAP.x, df_luemaxgdd_modeled$residualsitevpmpercent,
     xlab = "", ylab = bquote("Percent Residuals from VPM"[.(site)]))
text(5, max(df_luemaxgdd_modeled$residualsitevpmpercent)- 0.3, "B", cex=1.5)

site <- "spatial"
plot(df_luemaxgdd_modeled$DAP.x, df_luemaxgdd_modeled$residualspatialvpm,
     xlab = "Days after planting", ylab = bquote("Residuals from VPM"[.(site)]))
text(5, max(df_luemaxgdd_modeled$residualspatialvpm)- 0.3, "C", cex=1.5)

site <- "spatial"
plot(df_luemaxgdd_modeled$DAP.x, df_luemaxgdd_modeled$residualspatialvpmpercent,
     xlab = "Days after planting", ylab = bquote("Percent Residuals from VPM"[.(site)]))
text(5, max(df_luemaxgdd_modeled$residualspatialvpmpercent)- 0.3, "D", cex=1.5)

# Load the plotrix package
# Load the plotrix package

# Reset to default settings
par()

library(chron)
library(lattice)
library(ggplot2)
library(plotrix)
library(graphics)
#Engr. Haroon Haider
#haroonhaider32@gmai.com
#+923027501600
ref <-as.numeric(df_luemaxgdd_modeled$GPP_site)
vpmspatial <-as.numeric(df_luemaxgdd_modeled$gpp)
vpmsite <-as.numeric(df_luemaxgdd_modeled$GPPvpm)
pvpmsite<-as.numeric(df_luemaxgdd_modeled$GPPpvpmmodel)
pvpmspatial<-as.numeric(df_luemaxgdd_modeled$gpppvpm)

taylor.diagram(ref,vpmspatial,add=FALSE,col="black",pch=19,pos.cor=TRUE,
               xlab="Standard Deviation",ylab="",main="Taylor Diagram",
               show.gamma=TRUE,ngamma=6,gamma.col=5,sd.arcs=8,
               ref.sd=TRUE,sd.method="sample",grad.corr.lines=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99),
               pcex=1,cex.axis=1,normalize=FALSE,mar=c(2,8,2,8),)
taylor.diagram(ref,vpmsite,add=TRUE,col="black",pch=8,pcex=1, normalize=FALSE)
taylor.diagram(ref,pvpmsite,add=TRUE,col="black",pch=8,pcex=1, normalize=FALSE)
taylor.diagram(ref,pvpmspatial,add=TRUE,col="black",pch=8,pcex=1, normalize=FALSE)
legend(11,12,cex=1,pt.cex=0.5,legend=c("VPMsite","VPMspatial"),pch=c(19,8,17,15,9),col=c(1,1,1,1,1))


sqrt(var(vpmsite))
sqrt(var(vpmspatial))
sqrt(var(ref))

sqrt(var(ref))-sqrt(var(ref))
sqrt(var(ref))-sqrt(var(vpmsite))
sqrt(var(ref))-sqrt(var(vpmspatial))

# Calculate RMSE between ref and each variable
sqrt(mse(ref, vpmspatial))
sqrt(mse(ref, vpmsite))
sqrt(mse(ref, pvpmsite))
sqrt(mse(ref, pvpmspatial))



ref <- as.numeric(df_luemaxgdd_modeled$GPP_site)
vpmspatial <- as.numeric(df_luemaxgdd_modeled$gpp)
vpmsite <- as.numeric(df_luemaxgdd_modeled$GPPvpm)
pvpmsite <- as.numeric(df_luemaxgdd_modeled$GPPpvpmmodel)
pvpmspatial <- as.numeric(df_luemaxgdd_modeled$gpppvpm)

library(Hmisc)

# Calculate RMSE between ref and each variable
rmse_vpmspatial <- sqrt(mse(ref, vpmspatial))
rmse_vpmsite <- sqrt(mse(ref, vpmsite))
rmse_pvpmsite <- sqrt(mse(ref, pvpmsite))
rmse_pvpmspatial <- sqrt(mse(ref, pvpmspatial))

# Print out RMSE values
cat("RMSE (ref, vpmspatial):", rmse_vpmspatial, "\n")
cat("RMSE (ref, vpmsite):", rmse_vpmsite, "\n")
cat("RMSE (ref, pvpmsite):", rmse_pvpmsite, "\n")
cat("RMSE (ref, pvpmspatial):", rmse_pvpmspatial, "\n")

# Plot Taylor diagram
taylor.diagram(ref, vpmspatial, add = FALSE, col = "black", pch = 19, pos.cor = TRUE,
               xlab = "Standard Deviation", ylab = "", main = "Taylor Diagram",
               show.gamma = TRUE, ngamma = 8, gamma.col = 8, sd.arcs = 8,
               ref.sd = TRUE, sd.method = "sample", grad.corr.lines = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99),
               pcex = 1, cex.axis = 1, normalize = FALSE, mar = c(2, 8, 2, 8))
taylor.diagram(ref, vpmsite, add = TRUE, col = "black", pch = 8, pcex = 1, normalize = FALSE)
taylor.diagram(ref, pvpmsite, add = TRUE, col = "black", pch = 3, pcex = 1, normalize = FALSE)
taylor.diagram(ref, pvpmspatial, add = TRUE, col = "black", pch = 2, pcex = 1, normalize = FALSE)
legend(11, 12, cex = 1, pt.cex = 0.5, legend = c("VPMsite", "VPMspatial"), pch = c(19, 8, 17, 15, 9), col = c(1, 1, 1, 1, 1))






# create two sample data sets
x <- pvpmspatial
y <- ref

# calculate the mean of each data set
mean_x <- mean(x)
mean_y <- mean(y)

# calculate the centered data sets
x_centered <- x - mean_x
y_centered <- y - mean_y

# calculate the CRMSD
crmsd <- sqrt(mean((x_centered - y_centered)^2))

# print the result
print(crmsd)


