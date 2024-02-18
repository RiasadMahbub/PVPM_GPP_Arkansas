
###
#  allrasterscumulativestack

### path of the images
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/"

setwd(path)


rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/", pattern='.tif$', 
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


ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMyearwisespatialGPP.png",width = 14, height = 8 )


allrasterscumulativestack_meansddf

mean(allrasterscumulativestack_meansddf$meancumgpp8dayagg)
sqrt(var(allrasterscumulativestack_meansddf$meancumgpp8dayagg))
minrow(allrasterscumulativestack_meansddf$meancumgpp8dayagg)

allrasterscumulativestack_meansddf %>%
  arrange(desc(meancumgpp8dayagg)) %>%
  head(3)

allrasterscumulativestack_meansddf %>%
  arrange(meancumgpp8dayagg) %>%
  head(3)



### Plot ts vs gpp
# Define the file path
file_path <- "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/Temperature"

# Construct the full file path including the file name
file_name <- "Ts.csv"
full_file_path <- file.path(file_path, file_name)

# Read the CSV file into a variable named 'ts'
ts <- read.csv(full_file_path)

# Join the data frames based on the common column
merged_df <- merge(allrasterscumulativestack_meansddf, ts, by = "year", all.x = TRUE, all.y = TRUE)
merged_df$meancumgpp8dayagg.x
ggscatter(merged_df, x = "ts", y = "meancumgpp8dayagg.x", add = "reg.line", size =12) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = 15, label.y = 1700, size =12) +
  stat_regline_equation(label.x = 15, label.y = 1600, size =12)+
  scale_x_continuous(breaks = seq(15, 20, by = 1), limits = c(15, 20)) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  xlab("Mean Temperature (°C)")+
  font("xy.text", size = 24)+
  font("xlab", size = 24)+
  font("ylab", size = 24)+
  ylab(expression(paste("Gross Primary Productivity (g C ", m^-2, year^-1,")")))


ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/TEMPERATUREGPPinterannual.png",width = 14, height = 8 )
allrasterscumulativestack_meansddf$ts

merged_df$ts
