library(tidyverse) 
library(minpack.lm)
library(viridis)
library(ggpubr)

LUEmaxGDD<-do.call("rbind", list(USOF1VI8day, USOF2VI8day, USOF3VI8day, USOF4VI8day, USOF5VI8day, USOF6VI8day, USBDA2015VI8day, USBDC2015VI8day, USBDCC2016VI8day, USBDA2016VI8day, USHRA_2015VI8day, USHRA_2016VI8day, USHRA_2017VI8day, USHRC_2015VI8day, USHRC_2016VI8day, USHRC_2017VI8day))
nrow(USOF1VI8day) + nrow ( USOF2VI8day) + nrow ( USOF3VI8day) + nrow ( USOF4VI8day) + nrow ( USOF5VI8day) + nrow ( USOF6VI8day) + nrow ( USBDA2015VI8day) + nrow ( USBDC2015VI8day) + nrow ( USBDCC2016VI8day) + nrow ( USBDA2016VI8day) + nrow ( USHRA_2015VI8day) + nrow ( USHRA_2016VI8day) + nrow ( USHRA_2017VI8day) + nrow ( USHRC_2015VI8day) + nrow ( USHRC_2016VI8day) + nrow ( USHRC_2017VI8day)
LUEmaxGDD

LUEmaxGDD<-LUEmaxGDD[!is.na(LUEmaxGDD$GPP_site),]
nrow(LUEmaxGDD)
#### Model selection between CUMGPP and CUMGPP with LAI

a = 9.687e+03
b= 2.018e+04
c=  9.845e+02 
d = 6.220e-02 

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

class(GPPminmodel$m$getPars()[1])



## Drop na values based on luemaxcal
LUEmaxGDD<-LUEmaxGDD %>% drop_na(LUEpvpmcal)

LUEmaxGDD
GPPminmodel
GPPminmodel<-nlsLM(y~(((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))* ws* ts *fpar*par*12.011 ), trace = TRUE, start = list(a = a, b= b, c=  c, d =d))
a = GPPminmodel$m$getPars()[1]
b = GPPminmodel$m$getPars()[2]
c=  GPPminmodel$m$getPars()[3]
d = GPPminmodel$m$getPars()[4]

x<-LUEmaxGDD$GDDcum
z <-LUEmaxGDD$LUEpvpmcal
w<-LUEmaxGDD$LUEmaxmodeled
LUEmaxGDD$year<- as.factor(LUEmaxGDD$year)
ggplot(data=LUEmaxGDD, aes(x,z, col = site, shape=year)) +
  geom_point(size = 5) + 
  stat_smooth(aes(group = 1), method="nls.lm", formula=y~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), 
              method.args = list(start = list( a = GPPminmodel$m$getPars()[1] , b= GPPminmodel$m$getPars()[2] , c=  GPPminmodel$m$getPars()[3] , d = GPPminmodel$m$getPars()[4])),
              se = FALSE)+
  xlab("Cumulative Growing Degree Days (\u00B0C)")+
  ylab((bquote(''*LUE[max]*'(mol'*~CO[2]~ '/mol PPFD'*')')))+
  theme_classic()+
  theme(text = element_text(size = 25))

##rename usbda to us-bda
View(LUEmaxGDD)
LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USBDA = "US-BDA")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USBDC = "US-BDC")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USHRC = "US-HRC")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USHRA = "US-HRA")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USOF1 = "US-OF1")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USOF2 = "US-OF2")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USOF3 = "US-OF3")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USOF4 = "US-OF4")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USOF5 = "US-OF5")

LUEmaxGDD$site <- recode(LUEmaxGDD$site ,
                         USOF6 = "US-OF6")


plot_rsquared <- paste0(
  "R^2 == ",
  0.40 %>% round(2))
### rather than calling the numbers from the model variable I put the numbers in the model solved the issue.
ggplot(data=LUEmaxGDD, aes(x,z, col = site, shape=year)) +
  geom_point(size = 5) + 
  stat_smooth(aes(group = 1), method="nls", formula=y~((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c)))))))), 
              method.args = list(start = list( a = a , b= b, c=  c , d =d)),
              se = FALSE)+
  xlab("Cumulative Growing Degree Days (\u00B0C)")+
  ylab((bquote(''*epsilon[0]*~'(mol'*~CO[2]~mol^-1~'PPFD'*')')))+
  annotate(geom = "text", x = 172, y = 0.09, label = plot_rsquared, size = 7, parse = TRUE)+
  annotate(geom = "text", x = 235, y = 0.085, label = "RMSE = 0.02", size = 7)+
  annotate(geom = "text", x = 237.5, y = 0.080, label = "PBIAS = 21.1", size = 7)+

  theme_classic()+
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1, "cm")) +
  theme(legend.title.align = 0.5)+
  labs(color = "Site", shape= "Year")+
  theme(text = element_text(size = 25))
#ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/LUEmaxconference.png", dpi = 300, width = 8, height = 8)
ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/LUEmax.png", dpi = 300, width = 16, height = 10)

LUEmaxGDD
plot(LUEmaxGDD$DAP, LUEmaxGDD$LUEmaxmodeled)





# install.packages("devtools")
devtools::install_github("PecanProject/rpecanapi")
library(Peca)
solarMJ2ppfd(0.5)
sola
package?PEcAn.data.atmosphere
library(devtools)
##install_github("pecanproject/pecan/modules/data.atmosphere")


##Topt graph
LUEmaxGDD$Toptmodeled<- 19.33+ (0.206*(LUEmaxGDD$GDDcum)) - (0.0007*((LUEmaxGDD$GDDcum)^2)) -(1.619e-06*((LUEmaxGDD$GDDcum)^3))
ggplot(data=LUEmaxGDD, aes(GDDcum,Topt, col = site))+
  geom_point(size = 5) + 
  stat_smooth(aes(group = 1), method="nls", formula=y~a+ (b*(x)) - (c*((x)^2)) -(d*((x)^3)), 
              method.args = list(start = list( a = 19.33 , b= 0.206, c=  0.0007 , d =1.619e-06)),
              se = FALSE)+
  xlab("Cumulative Growing Degree Days (\u00B0C)")+
  ylab((bquote(''*T[opt]*~'(\u00B0C)')))+
  theme_classic()+
  theme(legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1, "cm")) +
  theme(legend.title.align = 0.5)+
  labs(color = "Site")+
  theme(text = element_text(size = 25))
ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/Topt.png", dpi = 300, width = 16, height = 10)
LUEmaxGDD$Tspvpm_cal
LUEmaxGDD$Topt

LUEmaxGDD

US-BDA





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

gddvalues <- data.frame(speed = c(500, 600, 700, 800, 900, 1000))
GPPminmodel
max(predict(GPPminmodel, newdata = gddvalues))
max(LUEmaxGDD$LUEpvpmmodeled)


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

library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)
## Calculate the rmse
rmse(LUEmaxGDD$LUEpvpmcal, LUEmaxGDD$LUEmaxmodeled)
rmse(LUEmaxGDD$LUEmaxmodeled, LUEmaxGDD$LUEpvpmcal)
## Calculate the bias
pbias(LUEmaxGDD$LUEpvpmcal, LUEmaxGDD$LUEmaxmodeled)
pbias(LUEmaxGDD$LUEmaxmodeled, LUEmaxGDD$LUEpvpmcal)
## Calculate the r2
rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(LUEmaxGDD$LUEpvpmcal, LUEmaxGDD$LUEmaxmodeled)

LUEmaxGDD$LUEmaxmodeledpvpmtofindmaxval<-((d*((b*exp((a*(x-c))/(x*c)))/(b-(a*(1-exp((b*(x-c))/(x*c))))))))

max(LUEmaxGDD$LUEmaxmodeledpvpmtofindmaxval)
x
LUEmaxGDD$LUEmaxmodeledpvpmtofindmaxval

### Uncertainity test for the Topt
topty <- LUEmaxGDD$Topt
GDDcumx <- LUEmaxGDD$GDDcum
mod <- lm(topty ~ 19.33+ (0.206*(GDDcumx)) - (0.0007*((GDDcumx)^2)) -(1.619e-06*((GDDcumx^3))))

model <- lm(topty ~ GDDcumx + I(GDDcumx^2) + I(GDDcumx^3))
summary(model)
plot(GDDcumx, fitted(model))

library(nlstools)
confint2(model)

confint2(GPPminmodel)
summary(GPPminmodel)

library(dyn)
lag <- stats:::lag # this line is only needed if dplyr is loaded
plot(LUEmaxGDD$GDDcum, LUEmaxGDD$LUEmaxmodeled)

out <- rollapplyr(LUEmaxGDD, 2, function(x) {
  fm <- dyn$lm(LUEmaxGDD$LUEmaxmodeled ~LUEmaxGDD$GDDcum, zoo(x))
  s <- summary(fm)
  c(coef(fm), r2 = s$r.squared, a.r2 = s$adj.r.squared, e = resid(fm))
}, by.column = FALSE, fill = NA)
View(out)


library(dyn)
lag <- stats:::lag # this line is only needed if dplyr is loaded

out <- rollapplyr(anscombe,200, function(x) {
  fm <- dyn$lm(y1 ~ x1, zoo(x))
  s <- summary(fm)
  c(coef(fm), r2 = s$r.squared, a.r2 = s$adj.r.squared, e = resid(fm))
}, by.column = FALSE, fill = NA)
out


anscombe$LUEmaxmodeled<-LUEmaxGDD$LUEmaxmodeled 
anscombe$GDDcum<-LUEmaxGDD$GDDcum 

merge(df1, df2, by="year", all = T)
class(anscombe)
typeof(anscombe$x1)

library(zoo)

rollapply(rolldata, width=3, function(x) cor(x[,2],x[,3]), by.column=FALSE)


LUEmaxGDD[,18]
LUEmaxGDD[,29]

class(LUEmaxGDD[,18])
class(LUEmaxGDD[,29])

rolldata<- as.data.frame(LUEmaxGDD$GDDcum)
rolldata$GDDcum<-rolldata$`LUEmaxGDD$GDDcum`
rolldata$LUEmaxmodeled<-LUEmaxGDD$LUEmaxmodeled
zz <- rolldata |> aggregate(rolldata$`LUEmaxGDD$GDDcum`)
  
rolldata120<-rolldata[1:20, ]
rolldata2 <- rolldata[order(rolldata$GDDcum),]
View(rolldata2)
rollapply(rolldata, width=3, function(x) cor(x[,2],x[,3]), by.column=FALSE)

sort(rolldata2, index.return = TRUE)

rollapply(rolldata2, 8, function(x) x[3,] / x[4,] , by.column=FALSE)

rolldata2[,2]

