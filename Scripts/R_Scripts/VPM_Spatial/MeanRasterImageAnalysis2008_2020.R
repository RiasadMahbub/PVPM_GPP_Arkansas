##Plot the mean graphs here
### Plootinng Mean Cumulative
##3 This code makes the cumulative 2008-2020 and gam regression graph

###Load all the required libraries
library(raster)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(sf)
library(terra)
library(lubridate)
library(maptools)  ## For wrld_simpl
library(sp)
library(rgdal)
library(cowplot)
library(ggsn)
library(ggpubr)
library(sf)
library(gridExtra)
library(grid)
library(rasterVis)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(RColorBrewer)
library(magrittr)
### path of the images
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/"
setwd(path)

rastlistcumulative <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativePVPM/CumulativePVPM/", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)
allrasterscumulative <- lapply(rastlistcumulative, raster)


## Clip the raster images
plot(allrasterscumulative[[10]])

alignExtent(allrasterscumulative, object, snap='near')
same_as_r1 <- sapply(allrasterscumulative, function(x) extent(x) == extent(allrasterscumulative[[1]]))
## Create one raster image taking the mean

extent(allrasterscumulative[[1]])
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

meancumulative <- calc(allrasterscumulativestack, fun = mean)
plot(meancumulative)

meancumulativeNArmtrue <- calc(allrasterscumulativestack, fun = mean, na.rm=TRUE)
plot(meancumulativeNArmtrue)

sumcumulative <- calc(allrasterscumulativestack, fun = sum)
plot(sumcumulative)
## Check the mean of google earth engine if that takes account of mean NA values





#



##Plot multiple raster
## Multiple raster stack file = allrasterscumulativestack

r_stack_df <- as.data.frame(allrasterscumulativestack, xy = TRUE) %>% 
  tidyr::pivot_longer(cols = !c(x, y), 
                      names_to = 'variable', 
                      values_to = 'value')

ggplot() + 
  geom_raster(data = r_stack_df, 
              aes(x = x, y = y, fill = value), alpha = 0.2) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal() + 
  theme_minimal() 


### Create mean raster data


## multiply by 8
cumulativerasdf$layer<-cumulativerasdf$layer*8

## Plotting cumulative old technique
plot(meancumulative)
plot(cumulativerasdf)
nameColor <- bquote(atop(Mean~Cumulative ~GPP~(2008-2020)~(g~C~m^-2~season^-1)~"  "))

cumulativerasdf <- as.data.frame(meancumulativeNArmtrue,xy=TRUE)%>%drop_na()

my_breaks <- c(0, 800, 1000, 1200, 1600, 2000, 2400)

##applydegree north with function with x axis label
nwbrks <- seq(31,36,1)
nwlbls <- unlist(lapply(nwbrks, function(x) paste(x, "°N")))


cumulativemap<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=layer),data=cumulativerasdf)+
  #scale_fill_viridis_c( limits = c(0, 300), option = "turbo", breaks = my_breaks, nameColor, direction = -1, oob = scales::squish)+
  scale_fill_viridis(rescaler = function(x, to = c(0, 0.8), from = NULL) {
    ifelse(x<100, 
           scales::rescale(x,
                           to = c(0,0.2),
                           from = c(min(x, na.rm = TRUE), 500)),
           ifelse(x>1600, 
                  scales::rescale(x,
                                  to = c(0.8, 0.8),
                                  from = c(0.8, 1)), 
                  ifelse(x>100 & x<1600, 
                         scales::rescale(x,
                                         to = to,
                                         from = c(min(x, na.rm = TRUE), 1600)), 0.8)))
         
         },limits = c(0, 2400), option = "turbo", breaks = my_breaks, nameColor, direction = -1) +
  labs(x='Longitude',y='Latitude', color = nameColor)+
  scale_y_continuous(breaks = seq(34, 36, by=1))+
  cowplot::theme_cowplot(font_size = 24)+
  theme(legend.key.width=unit(4,"cm"), legend.spacing.x = unit(2, 'cm'))+
  theme(axis.text = element_text(size = 25))  +
  
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=5, height=0.02,
           transform = TRUE, model = "WGS84")

cumulativegam<-  ggplot(cumulativerasdf, aes(x=y, y=layer)) +
  geom_smooth(level = 0.687)+labs(x='Latitude',y=expression(Mean~Cumulative ~GPP~(g~C~m^{-2}~season^{-1})))+
  scale_x_continuous(breaks = nwbrks, labels = nwlbls, expand = c(0, 0)) +
  #scale_x_continuous(breaks = seq(33, 36, by=1))+
  
  coord_flip()+
  cowplot::theme_cowplot(font_size = 23)+
  theme(axis.text = element_text(size = 25))  

cumulativearranged<-ggarrange(cumulativemap, cumulativegam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(1.6,1),
                   common.legend = TRUE, legend = "bottom")
cumulativearranged

ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/cumulativearranged.png", plot=cumulativearranged, height=10, width=22, units="in", dpi=150)


hist( cumulativerasdf$layer, xlab = "GPP")

##CUMULATIVE WITH ERROR BAR
cumulativerasdf$lat <- cumulativerasdf$y
cumulativerasdf$lat<-format(round(cumulativerasdf$lat, 1), nsmall = 1)
cumulativerasdf$lat<- as.character(cumulativerasdf$lat)

Data_summary <- aggregate(layer ~ lat, cumulativerasdf,       # Create summary data
                          function(x) c(mean = mean(x),
                                        se = sd(x) / sqrt(length(x))))
data_summary <- data.frame(group = Data_summary[ , 1], Data_summary$layer)
# Print summary data

data_summary <-data.frame(unclass((data_summary)), check.names = FALSE, stringsAsFactors = FALSE)

base_r_barplot <- line(data_summary$mean~group,  # Draw and store Base R barplot
                       data_summary)
arrows(x0 = base_r_barplot,                           # Add error bars
       y0 = data_summary$mean + data_summary$se,
       y1 = data_summary$mean - data_summary$se,
       angle = 90,
       code = 3,
       length = 0.1)

data_summary$group<-as.numeric(data_summary$group)

data_summary$group<-as.integer(data_summary$group)
data_summary$group<-as.character(data_summary$group)
cumulativegamerror<- ggplot(data_summary, aes(x=group, y=mean)) +
  geom_point()+labs(x='Latitude',y=expression(Mean~Cumulative ~GPP(g~C~m^{-2}~season^{-1})))+
  scale_x_continuous(breaks = nwbrks, labels = nwlbls, expand = c(0, 0)) +
  geom_errorbar( aes(x=group, ymin=mean-se, ymax=mean+se))+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

cumulativegamerror
cumulativearranged<-ggarrange(cumulativemap, cumulativegamerror, labels = c("A", "B"),font.label = list(size = 35) , widths = c(2,1),
                              common.legend = TRUE, legend = "bottom")
cumulativearranged

ggsave("cumulativearranged.png", plot=cumulativearranged, height=10, width=18, units="in", dpi=150)


cumulativerasdf

plt <- function(sigma = 1, base = exp(1)) {
  trans_new(
    "pseudo_log",
    function(x) asinh(x / (2 * sigma)) / log(base),
    function(x) 2 * sigma * sinh(x * log(base)),
    breaks = log_breaks(n=5, base=base)
  )
}

max(cumulativerasdf$layer)
min(cumulativerasdf$layer)




### The above code does for the cumulative graph but now we will plot the mean graphs

### path of the images
pathmean = "C:/Users/rbmahbub/Documents/Data/GeospatialData/MeanPVPM/MeanPVPM/"
rastlistmean <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/MeanPVPM/MeanPVPM/", pattern='.tif$', 
                                 all.files=TRUE, full.names=FALSE)
allrastersmean <- lapply(rastlistmean, raster)
allrastersmean

for(i in 1:13){
  allrastersmeanextent[[i]] = resample(allrastersmean[[i]], allrastersmean[[1]])
}

allrastersmeanstack<-stack(allrastersmeanextent)

meanmean <- calc(allrastersmeanstack, fun = mean)
plot(meanmean)

meanmeanNArmtrue <- calc(allrastersmeanstack, fun = mean, na.rm=TRUE)
plot(meanmeanNArmtrue)

summean <- calc(allrastersmeanstack, fun = sum)
plot(summean)

calc(randomvar, fun = mean, na.rm=TRUE)

randomvar<-raster(ncols=3, nrows=1)
View(randomvar)
as.data.frame(randomvar)


r <- raster(matrix(sample(1:255, 100, replace = T), ncol = 2))
r
randomvar
calc(r, fun = mean, na.rm=TRUE)



## Plotting mean old technique
plot(meanmean)

nameColor <- bquote(atop(Mean ~GPP~(2008-2020)~(g~C~m^-2~season^-1)~"  "))

meanrasdf <- as.data.frame(meanmeanNArmtrue,xy=TRUE)%>%drop_na()
my_breaks <- c(1, 2, 10, 15, 25)

viridis_names <-c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")

library(ggpubr)

meanmap<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=layer),data=meanrasdf)+
  scale_fill_viridis_c( limits = c(5, 15), option = "cividis", breaks = my_breaks, nameColor, direction = 1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(5,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 40, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

meangam<-  ggplot(meanrasdf, aes(x=y, y=layer)) +
  geom_smooth()+labs(x='Latitude',y=expression(Mean ~GPP~(2008-2020)~(g~C~m^{-2}~day^{-1})))+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

meanarranged<-ggarrange(meanmap, meangam, labels = c("A", "B"),font.label = list(size = 35) , widths = c(2,1),
                        common.legend = TRUE, legend = "bottom")
meanarranged

ggsave("meanarranged.png", plot=meanarranged, height=10, width=18, units="in", dpi=150)




### standard error bar

## make a new column only one decimal places
cumulativerasdf$lat <- cumulativerasdf$y
cumulativerasdf$lat<-format(round(cumulativerasdf$lat, 2), nsmall = 2)
cumulativerasdf$lat<- as.character(cumulativerasdf$lat)

Data_summary <- aggregate(layer ~ lat, cumulativerasdf,       # Create summary data
                          function(x) c(mean = mean(x),
                                        se = sd(x) / sqrt(length(x))))
data_summary <- data.frame(group = Data_summary[ , 1], Data_summary$layer)
data_summary$group<-as.numeric(data_summary$group)
data_summary$group<- format(round(data_summary$group, 1), nsmall = 1)
# Print summary data

data_summary <-data.frame(unclass((data_summary)), check.names = FALSE, stringsAsFactors = FALSE)
format(round(x, 2), nsmall = 2)

datasummarylat<-data_summary %>%
  group_by(group) %>%
  summarize(mean_size = mean(mean, na.rm = TRUE), mean_se = mean(se))


write.csv(datasummarylat, "datasummarylat.csv")
datasummarylat
