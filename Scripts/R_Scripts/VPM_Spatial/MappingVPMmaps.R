##Conventional VPM 
##2020

library(raster)
library(cowplot)
library(tidyverse)
library(ggsn)
library(ggpubr)
library(sf)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(grid)

grey <- raster("C:/Users/rbmahbub/Downloads/ImagetoR.tif")
rgb <- brick(grey)

plot(rgb)
plot(grey)
arkansasshp <- st_read(
  "C:/Users/rbmahbub/Downloads/tl_2016_05_cousub/tl_2016_05_cousub.shp")

arkansasshp
View(arkansasshp)
plot(arkansasshp)
plot(grey)
plotRGB(rgb)
glimpse(grey)
names(grey)
labels(grey)
# Manipulating data
rasdf <- as.data.frame(grey,xy=TRUE)%>%drop_na()
head(rasdf)

map<-ggplot()+
  geom_raster(aes(x=x,y=y,fill=arkansasRice2020),data=rasdf)+
  geom_sf(fill='transparent',data=ME)+
  scale_fill_viridis_c('GPP (gC/m2/day)',direction = -1)+
  
  coord_sf(expand=c(0,0))+
  labs(x='Longitude',y='Latitude',
       title="Arkansas Rice Gross Primary Productivity 2020",
       subtitle='Conventional VPM model',
       caption='Source: WorldClim, 2020')+
       
  cowplot::theme_cowplot()+
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = 'dashed',
                                        size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill=NA,color = 'black'),
        panel.ontop = TRUE)
  
  
map
mean(grey)
view(rasdf)
mean(rasdf$arkansasRice2020)


###Latitudinal curve
# expand_limits() increases the y range to include the value 0
dp<-  ggplot(rasdf, aes(x=y, y=arkansasRice2020)) +
  geom_smooth()+
  coord_flip()+
  cowplot::theme_cowplot()



ggarrange(map, dp, labels = c("A", "B"),
          common.legend = TRUE, legend = "bottom")




my_breaks <- c(0, 6, 8, 12)


nameColor <- bquote(atop(Mean ~GPP~phantom(),
                         (gCm^-2~day^-1)))

###2020 
nameColor <- bquote(atop(Mean ~GPP(g~C~m^-2~day^-1)~"  "))
library(ggplot2)

map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=arkansasRice2020),data=rasdf)+
  scale_fill_viridis_c(limits = c(4, 9), option = "turbo",  nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 35)+
  theme(legend.key.width=unit(6,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdf, aes(x=y, y=arkansasRice2020)) +
  geom_smooth()+labs(x='Latitude',y=expression(Mean ~GPP(g~C~m^{-2}~day^{-1})))+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 30)

p2_legend <- get_legend(map)
gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
          common.legend = TRUE, legend = "bottom")

##conference figure
gpp2020<-ggarrange(map, dp, labels = c("A", "B"),font.label = list(size = 35) , widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("gpp2020_conferenceposter.png", plot=gpp2020, height=10, width=18, units="in", dpi=150)
ggsave("gpp2020.png")
ggarrange(map, 
          dp, nrow =1)
grid.arrange(arrangeGrob(map + theme(legend.position="none"), 
                         dp + theme(legend.position="none"), nrow=1), 
             p2_legend, 
             nrow=2,heights=c(10, 1))

figure <- ggarrange(map + rremove("ylab") + rremove("xlab"), dp + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    labels = NULL,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))



figure




#############################################
### crop yield data
view(arkansasshp)

plot(arkansasshp)
typeof(arkansasshp)



county <- aggregate(arkansasshp["COUNTYFP"], by = list(diss = arkansasshp$COUNTYFP), 
                    FUN = function(x)x[1], do_union = TRUE)

plot(county)



##Subsetting one county by ansi code
FAULKNER = subset(county, COUNTYFP=="045")
plot(FAULKNER)
## COUTNY ANSI = CONTYFP


##County with one variable
countyonevariable <- county[ ,c(2,3)]
plot(countyonevariable)
view(countyonevariable)

###Merging yield and county
yield<-read.csv("yield2020.csv")
view(yield)

library(stringr)
yield$COUNTYFP<-str_pad(yield$COUNTYFP, 3, pad = "0")

yield$Yield<-as.numeric(gsub(",", "", yield$Yield))


m <- merge(countyonevariable, yield, by='COUNTYFP', all.x=TRUE)
plot(m)
view(m)
## Replace NA with fixed value 7310
m[is.na(m)] <- 7310
view(m)

myield<-m[ ,c(3,4)]
plot(myield)






##raster conversion
library(raster)
library(rgdal)
summary(myield$geometry)
r <- raster(extent(myield))
projection(r) <- proj4string(myield)

r10 <- rasterize(myield, field="Yield", r)

plot(r10)

rasdfyield <- as.data.frame(r10,xy=TRUE)%>%drop_na()


myield$Yieldgm<-(myield$Yield)*(0.112085)
rasdfyield$Yieldgm<-(rasdfyield$layer)*(0.112085)


view(rasdfyield)

my_breaks <- c(0, 6, 8, 12)
map<-ggplot() +
  geom_sf(data = Yieldgm, aes(fill = Yield), colour = NA) 

nameColor <- bquote(atop(Rice ~yield~(g~m^-2)~"  "))

map<-ggplot()+
  geom_sf(data = myield, aes(fill = Yieldgm), colour = NA)+
  geom_sf(fill='transparent',data=countyonevariable)+
  scale_fill_viridis_c(limits = c(600, 950), option = "magma",  nameColor, direction = -1, na.value="white")+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 40)+
  theme(legend.key.width=unit(6,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

map


dp<-  ggplot(rasdfyield, aes(x=y, y=Yieldgm)) +
  geom_smooth(level=0.90)+labs(x='Latitude',y=expression(Rice~yield~(g ~m^{-2})))+
  scale_y_continuous( breaks = seq(600, 900, by = 50))+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 40)

dp

p2_legend <- get_legend(map)
yield2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

yield2020

##conference figure
yield2020<-ggarrange(map, dp, labels = c("A", "B"),font.label = list(size = 35) , widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

yield2020
ggsave("yield2020_conferencepostergm.png", plot=yield2020, height=10, width=18, units="in", dpi=150)









mergedR<-merge(county, rasdf)
mergedR
view(mergedR)




###Mapping cumulative GPP in the growing season

rastercumugpp2020 <- raster("D:/Data/VPMModel/GeospatialAnalysis/VPM_Spatial_Data/Cumulative/cumulativearkansasRice2020.tif")
rasdfcumugpp2020 <- as.data.frame(rastercumugpp2020,xy=TRUE)%>%drop_na()
plot(rastercumugpp2020)
view(rasdfcumugpp2020)

nameColor <- bquote(atop(Mean ~GPP(gCm^-2~season^-1)~"  "))
map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=cumulativearkansasRice2020),data=rasdfcumugpp2020)+
  scale_fill_viridis_c(limits = c(100, 200),nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(4,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdfcumugpp2020, aes(x=y, y=cumulativearkansasRice2020)) +
  geom_smooth()+labs(x='Latitude',y='GPP')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

p2_legend <- get_legend(map)
gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("CumulativeGPP2020.png", width =12, height = 6)

## Plotting Forcing variables
## Mean
## 2020

# Mean Temperature
rastermeanmeantemp2020 <- raster("D:/Data/VPMModel/GeospatialAnalysis/VPM_Spatial_Data/ForcingVariables/Mean/2020/Meanmeantemp2020.tif")
rasdfmeanmeantemp2020  <- as.data.frame(rastermeanmeantemp2020,xy=TRUE)%>%drop_na()
plot(rastermeanmeantemp2020)
view(rasdfmeanmeantemp2020)

nameColor <- bquote(atop(Mean ~Temperature(Kelvin)~"  "))
map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=Meanmeantemp2020),data=rasdfmeanmeantemp2020)+
  scale_fill_viridis_c(limits = c(295, 298),nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(4,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdfmeanmeantemp2020, aes(x=y, y=Meanmeantemp2020)) +
  geom_smooth()+labs(x='Latitude',y='Mean Temperature')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("TemperatureMean2020.png", width =12, height = 6)

# Max Temperature
rasterMeanmaxtempp2020 <- raster("D:/Data/VPMModel/GeospatialAnalysis/VPM_Spatial_Data/ForcingVariables/Mean/2020/Meanmaxtempp2020.tif")
rasdfMeanmaxtempp2020  <- as.data.frame(rasterMeanmaxtempp2020,xy=TRUE)%>%drop_na()
plot(rasterMeanmaxtempp2020)
view(rasdfMeanmaxtempp2020)

nameColor <- bquote(atop(Maximum ~Temperature(Kelvin)~"  "))
map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=Meanmaxtempp2020),data=rasdfMeanmaxtempp2020)+
  scale_fill_viridis_c(limits = c(296, 301),nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(4,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdfMeanmaxtempp2020, aes(x=y, y=Meanmaxtempp2020)) +
  geom_smooth()+labs(x='Latitude',y='Maximum Temperature')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("TemperatureMax2020.png", width =12, height = 6)


# PAR
rasterMeanpar_2020 <- raster("D:/Data/VPMModel/GeospatialAnalysis/VPM_Spatial_Data/ForcingVariables/Mean/2020/Meanpar_2020.tif")
rasdfMeanpar_2020  <- as.data.frame(rasterMeanpar_2020,xy=TRUE)%>%drop_na()
plot(rasterMeanpar_2020)
view(rasdfMeanpar_2020)

nameColor <- bquote(atop(Mean ~PAR~"  "))
map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=Meanpar_2020),data=rasdfMeanpar_2020)+
  scale_fill_viridis_c(limits = c(41, 45),nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(4,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdfMeanpar_2020, aes(x=y, y=Meanpar_2020)) +
  geom_smooth()+labs(x='Latitude',y='PAR')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("Meanpar_2020.png", width =12, height = 6)


# EVI
rasterMeanEVI_SG_2020 <- raster("D:/Data/VPMModel/GeospatialAnalysis/VPM_Spatial_Data/ForcingVariables/Mean/2020/MeanEVI_SG_2020.tif")
rasdfMeanEVI_SG_2020  <- as.data.frame(rasterMeanEVI_SG_2020,xy=TRUE)%>%drop_na()
plot(rasterMeanEVI_SG_2020)
view(rasdfMeanEVI_SG_2020)

nameColor <- bquote(atop(Mean ~EVI~"  "))
map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=MeanEVI_SG_2020),data=rasdfMeanEVI_SG_2020)+
  scale_fill_viridis_c(limits = c(0.32, 0.5),nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(4,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdfMeanEVI_SG_2020, aes(x=y, y=MeanEVI_SG_2020)) +
  geom_smooth()+labs(x='Latitude',y='EVI')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("MeanEVI_2020.png", width =12, height = 6)



# LSWI
rasterMeanLSWI_SG_2020 <- raster("D:/Data/VPMModel/GeospatialAnalysis/VPM_Spatial_Data/ForcingVariables/Mean/2020/MeanLSWI_SG_2020.tif")
rasdfMeanLSWI_SG_2020  <- as.data.frame(rasterMeanLSWI_SG_2020,xy=TRUE)%>%drop_na()
plot(rasterMeanLSWI_SG_2020)
view(rasdfMeanLSWI_SG_2020)

nameColor <- bquote(atop(Mean ~LSWI~"  "))
map<-ggplot()+
  geom_sf(fill='transparent',data=ME)+
  geom_raster(aes(x=x,y=y,fill=MeanLSWI_SG_2020),data=rasdfMeanLSWI_SG_2020)+
  scale_fill_viridis_c(limits = c(0.1, 0.4),nameColor, direction = -1)+
  labs(x='Longitude',y='Latitude', color = nameColor)+
  cowplot::theme_cowplot(font_size = 20)+
  theme(legend.key.width=unit(4,"cm"))+
  north(arkansasshp) +
  scalebar(arkansasshp, dist = 50, dist_unit = "km",st.size=3, height=0.02,
           transform = TRUE, model = "WGS84")

dp<-  ggplot(rasdfMeanLSWI_SG_2020, aes(x=y, y=MeanLSWI_SG_2020)) +
  geom_smooth()+labs(x='Latitude',y='LSWI')+
  coord_flip()+
  cowplot::theme_cowplot(font_size = 20)

gpp2020<-ggarrange(map, dp, labels = c("A", "B"), widths = c(2,1),
                   common.legend = TRUE, legend = "bottom")

gpp2020
ggsave("MeanLSWI_2020.png", width =12, height = 6)


rasdfMeanLSWI_SG_2020
rasdfMeanEVI_SG_2020
rasdfMeanpar_2020
rasdfmeanmeantemp2020
rasdfcumugpp2020

nrow(rasdfcumugpp2020) == nrow(rasdfMeanpar_2020)


mergedcolumndf<-rasdfMeanLSWI_SG_2020 %>% 
  bind_cols(rasdfMeanEVI_SG_2020)%>% 
  bind_cols(rasdfMeanpar_2020)%>% 
  bind_cols(rasdfmeanmeantemp2020)%>% 
  bind_cols(rasdfcumugpp2020)



plot(mergedcolumndf$cumulativearkansasRice2020, mergedcolumndf$MeanLSWI_SG_2020)
plot(mergedcolumndf$cumulativearkansasRice2020, mergedcolumndf$MeanEVI_SG_2020)
plot(mergedcolumndf$cumulativearkansasRice2020, mergedcolumndf$Meanpar_2020)
plot(mergedcolumndf$cumulativearkansasRice2020, mergedcolumndf$Meanmeantemp2020)
plot(mergedcolumndf$cumulativearkansasRice2020, mergedcolumndf$y...2)







gpplswi<-ggplot(data=mergedcolumndf, aes(x =MeanLSWI_SG_2020 , y =cumulativearkansasRice2020))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =5) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean LSWI'))+
  ylab(bquote('Mean Cumulative GPP('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  stat_regline_equation(label.x = -0.2, label.y = 200, size = 12)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = -0.2, label.y = 230, size = 12)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 30))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
    
gppevi<-ggplot(data=mergedcolumndf, aes(x =MeanEVI_SG_2020 , y =cumulativearkansasRice2020))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =5) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean EVI'))+
  ylab(bquote('Mean Cumulative GPP('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  stat_regline_equation(label.x = -0.1, label.y = 200, size = 12)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = -0.1, label.y = 230, size = 12)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 30))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+    
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))



gpptemp<-ggplot(data=mergedcolumndf, aes(x =Meanmeantemp2020 , y =cumulativearkansasRice2020))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =5) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean Temperature (K)'))+
  ylab(bquote('Mean Cumulative GPP('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  stat_regline_equation(label.x = 293, label.y = 10, size = 10)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = 293, label.y = 40, size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 25))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+    
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
gpptemp


gpppar<-ggplot(data=mergedcolumndf, aes(x =Meanpar_2020 , y =cumulativearkansasRice2020))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =5) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('photosynthetic active radiation'))+
  ylab(bquote('Mean Cumulative GPP('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  stat_regline_equation(label.x = 43.35, label.y = -10, size = 8)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = 43.01, label.y = -30, size = 8)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 25))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+    
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
gpppar

library(cowplot)
plot_grid(gppevi, gpplswi,gpptemp, gpppar, labels = "AUTO")



ggsave("C:/Users/rbmahbub/Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/factosgpp.png",width = 34, height = 20)




####Spaital join of yield 
#county = countyonevariable
#myield = yield shapefile
# rastercumugpp2020 = cumulative gpp raster
ricegppcounty = aggregate(x = rastercumugpp2020, by = myield, FUN = mean)


ricegppcounty <- raster::extract(rastercumugpp2020, myieldshptransformed, fun = mean)
ricegppcountydf = data.frame(ricegppcounty)
print(ricegppcountydf)
plot(ricegppcounty)


view(ricegppcountydf)


library(raster)
library(maptools)
require(maptools)
library(terra)
typeof(myield)


myieldshp<-(as(myield, 'Spatial'))

rastercumugpp2020 <- rast("arkansasRice2008PVPMcumulative.tif")
myieldcrstransformed<-st_transform(myield, crs(rastercumugpp2020))
class(myieldcrstransformed)

mtranformed<-st_transform(m, crs(rastercumugpp2020))
view(mtranformed)

z = raster::zonal(rastercumugpp2020, myieldshptransformed, fun = "mean")
library(proj4)
library(terra)


myieldshptransformed <- spTransform(myieldshp, crs(rastercumugpp2020))

crs(myieldshptransformed)
crs(rastercumugpp2020)


elevation = terra::extract(rastercumugpp2020, vect(mtranformed))
zion_points = cbind(elevation, rastercumugpp2020)

view(elevation)




agg_mean = aggregate(elevation,by=list(elevation$ID),FUN=mean, na.rm=TRUE)
agg_mean

##Merge two dataframe
m$ID<-1:75

agg_mean<-as.data.frame(agg_mean)


agg_mean$ID<-str_pad(agg_mean$ID, 3, pad = "0")
agg_mean$COUNTYFP<-agg_mean$ID



gppricecounty <- merge(m, agg_mean, by='ID', all.x=TRUE)

agg_mean
m
view(gppricecounty)
plot(gppricecounty$gpp_sum, gppricecounty$Yield)


ggplot(gppricecounty, aes(gpp_sum, Yield)) +
  geom_point() +
  stat_smooth(method = lm)









gppricecountydf<- as.data.frame(gppricecounty)
view(gppricecountydf)


gppricecountydfnaomit<-gppricecountydf %>% drop_na(Yield)
view(gppricecountydfnaomit)
plot(gppricecountydfnaomit$gpppvpm_sum, gppricecountydfnaomit$Yield)


library(ggpmisc)
my.formula <- yieldgm ~gpp_sum_8day


##Converting the yield to g/m2
gppricecountydfnaomit$yieldgm<-(gppricecountydfnaomit$Yield)*0.112085

##Converting the modis to 8 day
gppricecountydfnaomit$gpp_sum_8day<-(gppricecountydfnaomit$gpp_sum)*8


library(dplyr)
df.summary <- gppricecountydfnaomit %>%
  summarise(
    sd = sd(yieldgm, na.rm = TRUE),
    yieldgm = mean(yieldgm)
  )
df.summary


gppricecountydfnaomit$ymin<- gppricecountydfnaomit$yieldgm -df.summary$sd
gppricecountydfnaomit$ymax<- gppricecountydfnaomit$yieldgm +df.summary$sd


df.summary$sd

library(ggpubr)
# Custom formatting fucntion
format_pval <- function(pval){
  pval <- scales::pvalue(pval, accuracy= 0.0001, add_p = TRUE)
  gsub(pattern = "(=|<)", replacement = " \\1 ", x = pval)
}

suppressPackageStartupMessages(library(ggpubr))
ggplot(gppricecountydfnaomit, aes(gpp_sum_8day, yieldgm, ymin = ymin, ymax = ymax)) +
  geom_point(size =4) +
  geom_errorbar(width = 5) +
  labs(x=expression(GPP~(g~C~m^{-2}~season^{-1})), y = Rice~ yield~(g~m^{-2}))+
  stat_regline_equation(label.x =1000, label.y = 950, size = 5)+
  stat_cor(aes(label = ..rr.label..), label.x =1000, label.y = 925, size = 5)+
  stat_cor(aes(label = paste(format_pval(..p..), sep = "*`,`~")),
           label.x = 925
  )+
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  geom_smooth(method = "lm", se = FALSE)


ggsave("riceyield.png",  height=10, width=10, units="in", dpi=150)
