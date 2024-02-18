### Making a time series of yield data and GPP data
library(tidyverse)
library(scales)
### Read the csv file
datayield20082020<-read.csv("C:/Users/rbmahbub/Documents/Data/YieldData/ArkansasCounty2008_2020lb_acre_areayield.csv")
View(datayield20082020)

##Remove the comma in the value column
datayield20082020$Value <- as.numeric(gsub(",","",datayield20082020$Value))

##Aggregate by year

datayield20082020_aggregateyear <- datayield20082020 %>% group_by(Year) %>% 
  summarise(meanyield=mean(Value),
            .groups = 'drop')
datayield20082020_aggregateyear

plot(datayield20082020_aggregateyear$Year, datayield20082020_aggregateyear$meanyield)
datayield20082020_aggregateyear$yieldgm<-(datayield20082020_aggregateyear$meanyield)*0.112085

# Add the regression line
ggplot(datayield20082020_aggregateyear, aes(x=Year, y=yieldgm)) + 
  geom_point()+
  geom_smooth(method=lm)+
  labs(x='Year',y=expression(Mean ~Yield(g~m^{-2}~season^{-1})))+
  scale_x_continuous(breaks= pretty_breaks())



datayield20082020

###
#  allrasterscumulativestack

### path of the images
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/"

setwd(path)


rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/", pattern='.tif$', 
                                 all.files=TRUE, full.names=FALSE)
allrasterscumulative <- lapply(rastlistcumulative, raster)

st_ext <- extent(-94.86209 , -89.14432 , 32.77952 , 36.67372 )
allrasterscumulativeextent <- vector("list", 13)
for(i in 1:13){
  allrasterscumulativeextent[[i]] = setExtent(allrasterscumulative[[i]], st_ext)
}

for(i in 1:13){
  allrasterscumulativeextent[[i]] = resample(allrasterscumulative[[i]], allrasterscumulative[[1]])
}
allrasterscumulativeextent

allrasterscumulativestack<-stack(allrasterscumulativeextent)



allrasterscumulativestack
allrasterscumulative
plot(allrasterscumulativestack)

allrasterscumulativestack
allrasterscumulativestack_mean <-cellStats(allrasterscumulativestack, 'mean', na.rm = T)
allrasterscumulativestack_meandf<-as.data.frame(allrasterscumulativestack_mean)
allrasterscumulativestack_meandf$meancumgpp8dayagg<-allrasterscumulativestack_meandf$allrasterscumulativestack_mean*8
allrasterscumulativestack_meandf$year<-seq(2008, 2020, by =1)
library(Kendall)
MannKendall(allrasterscumulativestack_meandf)


plot(allrasterscumulativestack_meandf$year, allrasterscumulativestack_meandf$meancumgpp8dayagg)
plot(as.data.frame(diff_ndvi_mean)$diff_ndvi_mean*8,datayield20082020_aggregateyear$yieldgm*.5, col=(datayield20082020_aggregateyear$Year) )
result = cor(allrasterscumulativestack_meandf$year, allrasterscumulativestack_meandf$meancumgpp8dayagg, method = "kendall")
result
meanRallrasterscumulativeextent




## To add the error bars we will calculate the standard deviation and add it to the data frame at the end
allrasterscumulativestack_sd <-cellStats(allrasterscumulativestack, 'sd', na.rm = T)
allrasterscumulativestack_sddf<-as.data.frame(allrasterscumulativestack_sd)
allrasterscumulativestack_sddf$sdcumgpp8dayagg<-allrasterscumulativestack_sddf$allrasterscumulativestack_sd*8
allrasterscumulativestack_meansddf<- cbind(allrasterscumulativestack_meandf, allrasterscumulativestack_sddf)

library(ggpubr)
library(ggtext)
library(ggplot2)
ggscatter(allrasterscumulativestack_meansddf, x = "year", y = "meancumgpp8dayagg", add = "reg.line", size =7) +
  stat_cor(label.x = 2016, label.y = 400, size =8) +
  stat_regline_equation(label.x = 2016, label.y = 500, size =8)+
  scale_x_continuous(breaks = seq(2008, 2020, by = 2))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  geom_errorbar(aes(ymin=meancumgpp8dayagg-sdcumgpp8dayagg , ymax=meancumgpp8dayagg+sdcumgpp8dayagg ), width=.2,
                position=position_dodge(90))+ 
  xlab("Year")+
  font("xy.text", size = 24)+
  font("xlab", size = 24)+
  font("ylab", size = 24)+
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, season^-1,")")))

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/yearwisespatialGPP.png",width = 14, height = 8 )

summary(allrasterscumulativestack[[1]], na.rm = T)
mean(c(1,2,3, NA), na.rm = T)

allrasterscumulativestack_meansddf
mean(values(allrasterscumulativestack[[1]]), na.rm = T)
cellStats(allrasterscumulativestack[[1]], 'mean', na.rm = T)

allrasterscumulativestack_meansddf$meancumgpp8dayagg[1]
allrasterscumulativestack_meansddf$meancumgpp8dayagg[13]
(allrasterscumulativestack_meansddf$meancumgpp8dayagg[13]-allrasterscumulativestack_meansddf$meancumgpp8dayagg[1])/allrasterscumulativestack_meansddf$meancumgpp8dayagg[1]

write.csv(allrasterscumulativestack_meansddf, "cumulativeGPP.csv")
getwd()

yieldarkansas2008<-6660
yieldarkansas2020<-7410

(yieldarkansas2020-yieldarkansas2008)/(yieldarkansas2008)


