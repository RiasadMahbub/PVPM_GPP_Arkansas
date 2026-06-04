#BIOL asssignment 2
library(tidyverse)
library(SciViews)
library(ggplot2)

### Create an array of z 1-100 m as an increment of 1 cm
z <- seq(0.01, 10, 0.01)
length(z)

H1m <- rep(1, length(z))
H2m <- rep(2, length(z))
H3m <- rep(3, length(z))


###Create the Dataframe
u_ustar_H1m<-1/(0.4)*ln(z/z0_H1m)
u_ustar_H2m<-1/(0.4)*ln(z/z0_H2m)
u_ustar_H3m<-1/(0.4)*ln(z/z0_H3m)


class.df<- data.frame(z, u_ustar_H1m, 
                      u_ustar_H2m, u_ustar_H3m)
long_DF <- class.df %>% gather(Height, u_ustar, u_ustar_H1m:u_ustar_H3m)

long_DF[long_DF < 0] <- NA

library(dplyr)

long_DF <- long_DF %>% 
  mutate(Height = case_when(long_DF$Height == "u_ustar_H1m" ~ "1m", # both tests: group A
                            long_DF$Height == "u_ustar_H2m" ~ "2m", # one test: group B
                            long_DF$Height == "u_ustar_H3m" ~ "3m" # neither test: group C
  ))


ggplot(long_DF, aes(x = u_ustar, y = z, color = Height)) +
  geom_line(aes(size =2))+
  theme(text = element_text(size=20))+
  ylab("Z(m)")+
  xlab(bquote(u/u^"*"))+
  scale_size(guide = "none")

ggsave("ustar1.png")


###lnz
##
long_DF$ln_z<-ln(long_DF$z)




library(dplyr)




ggplot(long_DF, aes(x = u_ustar, y = ln_z, color = Height)) +
  geom_line(aes(size =2))+
  theme(text = element_text(size=20))+
  ylab("ln(Z)")+
  xlab(bquote(u/u^"*"))+
  scale_size(guide = "none")

ggsave("ustar2.png")



long_DF['H'] <- NA
long_DF <- long_DF %>% 
  mutate(H = case_when(long_DF$Height == "1m" ~ 1, # both tests: group A
                       long_DF$Height == "2m" ~ 2, # one test: group B
                       long_DF$Height == "3m" ~ 3 # neither test: group C
  ))



long_DF$dH<-long_DF$H*(2/3)
long_DF$z0<-long_DF$H*(0.15)
long_DF$ln_z_dH<-ln(long_DF$z - long_DF$dH)
long_DF$ln_z_dH_z0<-ln((long_DF$z - long_DF$dH)/long_DF$z0)





ggplot(long_DF, aes(x = u_ustar, y = ln_z_dH, color = Height)) +
  geom_line(aes(size = 0.0002))+
  theme(text = element_text(size=20))+
  ylab(bquote(ln(z-d[H])))+
  xlab(bquote(u/u^"*"))+
  scale_size(guide = "none")
ggsave("ustar3.png")


ggplot(long_DF, aes(x = u_ustar, y = ln_z_dH_z0, color = Height)) +
  geom_line(aes(size = 0.0002))+
  theme(text = element_text(size=20))+
  ylab(bquote(ln(z-d[H])/z[0]))+
  xlab(bquote(u/u^"*"))+
  scale_size(guide = "none")
ggsave("ustar4.png")


#####
###2
####


## Read the csv file
headers = read.csv("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/AMF_US-HRA_BASE_HH_3-5.csv", skip = 2, header = F, nrows = 1, as.is = T)
df = read.csv("/Users/riasadbinmahbub/Box/Courses/Spring2022/BENG5963/ECData_otherRiceSites/AMF_US-HRA_BASE_HH_3-5.csv", skip = 2, header = T)
colnames(df)= headers

view(headers)
view(df)

library(lubridate)
df<-df%>%mutate_all(~replace(., . == -9999, NA))
df$datetime <-ymd_hms(df$TIMESTAMP_START, truncated = 1)
df




### Calculate H using the equation
## HZhang
# rho,air density = 1.225 kg/m^3
## Cp = 1004.67 Jkg^-1K-1
##ga =

## u = wind speed

df$g_a<- (df$WS/((df$USTAR)^2)+ (6.2*((df$USTAR)^(-2/3))))^-1




## Estimate Ts
df$T_surface<-((df$LW_OUT)/((0.970*1*(5.67* 10^-8))))^(1/4)
df$T_surface


df$TA_1_1_1<-df$TA_1_1_1+273.15
##Estimate Ts

df$g_a<- (df$WS/((df$USTAR)^2)+ (6.2*((df$USTAR)^(2/3))))
df$H_formula<-1.225*1004.67*df$g_a*((df$T_surface)-(df$TA_1_1_1))

plot(df$TA_1_1_1, df$T_surface)
plot(df$H_formula, df$H)

view(df)

df$DOY<-yday(df$datetime)
df$DOY
df_null_h<-  df %>% drop_na(H)

ggplot(df, aes(H, H_formula)) +
  geom_point(aes(color = DOY), size = 2)+
  expand_limits(x=c(-100,300), y=c(0, 500))+
  scale_y_continuous(name="Sensible heat flux predicted (W/m^-2)", limits=c(-100, 500))+
  scale_x_continuous(name="Sensible heat flux measured (W/m^-2)", limits=c(-100, 500))
  
library(ggpubr)
    
  ggscatter(df_null_h, x = "H", y = "H_formula", add = "reg.line") +
    geom_point(aes(color = DOY), size = 2, alpha = 0.15)+
  stat_cor(
    aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
    label.x = 300,
    label.y = 100
  )+
    scale_y_continuous(name="Sensible heat flux predicted (W/m^-2)", limits=c(-100, 500))+
    scale_x_continuous(name="Sensible heat flux measured (W/m^-2)", limits=c(-100, 500))+
    theme(legend.position = "bottom")
ggsave("H_measuredvsHpredicted.jpeg")  



library(ggplot2)
library(reshape2)
df_H<-df_null_h
meltdf <- melt(df_null_h,id="datetime")
target <- c("H", "H_formula")
df_H<-filter(meltdf, variable ==target)

filter(starwars, hair_color == "none", eye_color == "black")
df_H_2015 <- subset(df_H, datetime> "2015-01-01" & datetime < "2015-07-05")
df_H_2016 <- subset(df_H, datetime> "2016-01-01" & datetime < "2016-07-05")
df_H_2017 <- subset(df_H, datetime> "2017-01-01" & datetime < "2017-07-05")


ggplot(df_H_2015,aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()+
  xlab("Time")+
  ylab("Sensible Heat Flux (W/m^2)")+
  theme(text = element_text(size=10))+
  theme(legend.position = "bottom")

ggsave("interannnua2015.jpeg")
ggplot(df_H_2016,aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()+
xlab("Time")+
  ylab("Sensible Heat Flux (W/m^2)")+
  theme(text = element_text(size=10))+
  theme(legend.position = "bottom")

ggsave("interannnua2016.jpeg")
ggplot(df_H_2017,aes(x=datetime,y=value,colour=variable,group=variable)) + geom_line()+
  xlab("Time")+
  ylab("Sensible Heat Flux (W/m^2)")+
  theme(text = element_text(size=10))+
  theme(legend.position = "bottom")

ggsave("interannnua2017.jpeg")
library(Metrics)
sqrt( sum( (df_null_h$H_formula- df_null_h$H)^2 , na.rm = TRUE ) / nrow(df_null_h) )
( sum( abs(df_null_h$H_formula- df_null_h$H) , na.rm = TRUE ) / nrow(df_null_h) )

