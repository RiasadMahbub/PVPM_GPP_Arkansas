##Processing the Modeled data
library(readr)
library(lubridate)
library(cowplot)
library(patchwork)
library(tidyverse)
library(viridis)
library(ggpubr)

LUEmaxGDD<-do.call("rbind", list(USOF1VI8day, USOF2VI8day, USOF3VI8day, USOF4VI8day, USOF5VI8day, USOF6VI8day, USBDA2015VI8day, USBDC2015VI8day, USBDCC2016VI8day, USBDA2016VI8day, USHRA_2015VI8day, USHRA_2016VI8day, USHRA_2017VI8day, USHRC_2015VI8day, USHRC_2016VI8day, USHRC_2017VI8day))
nrow(USOF1VI8day) + nrow ( USOF2VI8day) + nrow ( USOF3VI8day) + nrow ( USOF4VI8day) + nrow ( USOF5VI8day) + nrow ( USOF6VI8day) + nrow ( USBDA2015VI8day) + nrow ( USBDC2015VI8day) + nrow ( USBDCC2016VI8day) + nrow ( USBDA2016VI8day) + nrow ( USHRA_2015VI8day) + nrow ( USHRA_2016VI8day) + nrow ( USHRA_2017VI8day) + nrow ( USHRC_2015VI8day) + nrow ( USHRC_2016VI8day) + nrow ( USHRC_2017VI8day)
nrow(LUEmaxGDD)


list_csv_files <- list.files(path = "/Users/riasadbinmahbub/CodeRepository/R/GapfillingOtherRiceSites/ModeledPVPMVPMdata")
setwd("/Users/riasadbinmahbub/CodeRepository/R/GapfillingOtherRiceSites/ModeledPVPMVPMdata")
dfmodeled <- readr::read_csv(list_csv_files, id = "file_name")

LUEmaxGDD['siteyear'] <- NA
LUEmaxGDD

LUEmaxGDD$sitedt<-paste(LUEmaxGDD$site, LUEmaxGDD$Date,  sep="_")
LUEmaxGDD$sitedt

dfmodeled$Date <- ifelse(dfmodeled$file_name == "USHRC2015Modeled_Satellite.csv", as.Date(dfmodeled$Time, format="%d-%b-%y"), mdy(dfmodeled$Time))
dfmodeled$Date <-as.Date(dfmodeled$Date, origin='1970-01-01')
dfmodeled$sitedt<-paste(dfmodeled$site, dfmodeled$Date,  sep="_")

dfmodeled$site <- gsub('[AntonApt]','',address_str)

dfmodeled$siteinitial<-str_sub(dfmodeled$file_name, end = 3)
dfmodeled$site <- ifelse(dfmodeled$siteinitial == "Way", str_sub(dfmodeled$file_name, end = 4), str_sub(dfmodeled$file_name, end = 5))
dfmodeled$sitedt<-paste(dfmodeled$site, dfmodeled$Date,  sep="_")


###### left join in R using merge() function 
df_luemaxgdd_modeled = LUEmaxGDD %>% left_join(dfmodeled,by="sitedt")
df_luemaxgdd_modeled<-merge(x=LUEmaxGDD,y=dfmodeled,by="sitedt",all.x=FALSE, all.y=FALSE)
df_luemaxgdd_modeled

##Create RMSE and MAE metrics
library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)

cols.num <- c("GPP_site","GPPvpm", "gpp", "GPPpvpmmodel", "gpppvpm")
df_luemaxgdd_modeled[cols.num] <- sapply(df_luemaxgdd_modeled[cols.num],as.numeric)
sapply(LUEmaxGDD, class)

GPPsiteVPMrmse<-rmse(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPvpm, na.rm=TRUE)
GPPsitePVPMrmse <-rmse(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPpvpmmodel)
GPPsatelliteVPMrmse<-rmse(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpp)
GPPsatellitePVPMrmse<-rmse(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpppvpm)

GPPsiteVPMmae<-mae(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPvpm, na.rm=TRUE)
GPPsitePVPMmae<-mae(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPpvpmmodel)
GPPsatelliteVPMmae <-mae(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpp)
GPPsatellitePVPMmae<-mae(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpppvpm)




GPPsitePVPM<-ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =GPPpvpmmodel, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[site],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(100, 250))+
  stat_regline_equation(label.x = -4, label.y = 28, size = 19)+
  stat_cor(aes(label = ..rr.label..), label.x = -4.5, label.y = 24, size = 19)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  annotate("text", x = 20, y = 2, label = sprintf("RMSE: %0.2f", GPPsitePVPMrmse), size = 19, fontface = 'italic') +
  annotate("text", x = 0.4, y = 15, label = sprintf("MAE:  %0.2f",GPPsitePVPMmae), size = 19, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))


GPPsiteVPM<-ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =GPPvpm, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~VPM[site],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(100, 250))+
  stat_regline_equation(label.x = -4, label.y = 28, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -4.5, label.y = 24, size = 18)+
  annotate("text", x = 0.75, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsiteVPMrmse), size = 18, fontface = 'italic') +
  annotate("text", x = 0.45, y = 15, label = sprintf("MAE:  %0.2f",GPPsiteVPMmae), size = 18, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

GPPsatelliteVPM<-ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =gpp, col = DOY))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~VPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(100, 250))+
  stat_regline_equation(label.x = -4, label.y = 28, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -4.5, label.y = 24, size = 18)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  annotate("text", x = 0.75, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatelliteVPMrmse), size = 18, fontface = 'italic') +
  annotate("text", x = 0.45, y = 15, label = sprintf("MAE:  %0.2f",GPPsatelliteVPMmae), size = 18, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

GPPsatellitePVPM<-ggplot(data=df_luemaxgdd_modeled, aes(x =GPP_site , y =gpppvpm, col = DOY))+
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

plotgrd2<-plot_grid(GPPsiteVPM, GPPsitePVPM, GPPsatelliteVPM, GPPsatellitePVPM, labels=c("A", "B", "C", "D"), ncol = 2, nrow = 2)

#https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots

combined <-  GPPsiteVPM +GPPsitePVPM + GPPsatelliteVPM + GPPsatellitePVPM & theme(legend.position = "right")
combined<-combined + plot_layout(guides = "collect")+plot_annotation(tag_levels = 'A')
combined
ggsave(combined, filename = "/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/FourModelsconferennce.png", width = 36, height =20)

##VPM
combined <-  GPPsiteVPM  + GPPsatelliteVPM  & theme(legend.position = "right")
combinedvpm<-combined + plot_layout(guides = "collect")+plot_annotation(tag_levels = 'A')
combinedvpm
ggsave(combinedvpm, filename = "/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/VPMModelsconferennce.png", width = 36, height =20)


#ggsave(plotgrd2, filename = "FourModelsplotgrd2.png", width = 48, height =24)



# arrange the three plots in a single row
prow <- plot_grid( GPPsiteVPM + theme(legend.position="none"),
                   GPPsitePVPM + theme(legend.position="none"),
                   GPPsatelliteVPM + theme(legend.position="none"),
                   GPPsatellitePVPM + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("A", "B", "C"),
                   hjust = -1,
                   nrow = 2
)

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
legend_b <- get_legend(GPPsiteVPM + theme(legend.box.margin = margin(0, 0, 0, 12)))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2), labels = "AUTO")
p

ggsave(p, filename = "FourModelscowplot.png", width = 48, height =24)


library(reshape2)
library(cowplot)
library(tidyverse)
separate_DF<-df_luemaxgdd_modeled%>% select(GPP_site, GPPpvpmmodel,gpppvpm, GPPvpm, gpp, DAP.x)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP.x"),
                variable.name = "Model",
                value.name = "GPP")


### Change the labels name
plt<-ggplot(df_long,aes(x=DAP.x,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =3)+geom_smooth(alpha=0.15, size = 2)+
  scale_shape_manual(name = "Model",labels = c("EC", bquote(PVPM[site]), bquote(PVPM[spatial]),bquote(VPM[site]),  bquote(VPM[spatial])), values=c(16, 17, 18, 19, 20))+
  scale_color_manual(name = "Model",labels = c("EC", bquote(PVPM[site]), bquote(PVPM[spatial]),bquote(VPM[site]),  bquote(VPM[spatial])), values=c("#E7B800","#00AFBB", "#1b97de","#f581ad","#b94e76"))+
  scale_x_continuous(breaks = seq(0, 150, by = 20))+
  scale_y_continuous(breaks = seq(0, 30, by = 5))+
  xlab(bquote('Days after Planting (DAP)'))+
  ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  labs(color=NULL)+
  theme_classic()+
  
  theme(legend.position = c(0.1, 0.8),
        legend.title=element_blank(),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"),
        legend.title.align = 0.5)+


  
  theme(text = element_text(size = 25))
plt
ggsave(("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/temporalgpp2.png"), plot = plt, width = 16, height = 8, dpi = 300)


###VPM thesis

library(reshape2)
library(cowplot)
library(tidyverse)
separate_DF<-df_luemaxgdd_modeled%>% select(GPP_site, GPPvpm, gpp, DAP.x)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP.x"),
                variable.name = "Model",
                value.name = "GPP")

# Use spread to convert back to the original wide format
# Filter rows where Model is "GPP_site"
filtered_df1 <- df_long %>%
  filter(Model == "GPP_site")
filtered_df2 <- df_long %>%
  filter(Model == "GPPvpm")
filtered_df3 <- df_long %>%
  filter(Model == "gpp")

colnames(filtered_df1)
colnames(filtered_df2)

### Change the labels name
plt<-ggplot(df_long,aes(x=DAP.x,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =3)+geom_smooth(alpha=0.15, size = 2)+
  scale_shape_manual(name = "Model",labels = c("EC", bquote(VPM[site]),  bquote(VPM[spatial])), values=c(16, 17, 18, 19, 20))+
  scale_color_manual(name = "Model",labels = c("EC", bquote(VPM[site]),  bquote(VPM[spatial])), values=c("#E7B800","#00AFBB", "#1b97de","#f581ad","#b94e76"))+
  scale_x_continuous(breaks = seq(0, 150, by = 20))+
  scale_y_continuous(breaks = seq(0, 30, by = 5))+
  xlab(bquote('Days after Planting (DAP)'))+
  ylab(bquote('GPP ( '*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  labs(color=NULL)+
  theme_classic()+
  
  theme(legend.position = c(0.1, 0.8),
        legend.title=element_blank(),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1.5, "cm"),
        legend.title.align = 0.5)+
  
  
  
  theme(text = element_text(size = 25))
plt
ggsave(("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/vpmtemporalgpp2.png"), plot = plt, width = 16, height = 8, dpi = 300)

# Specify the path for saving the CSV file
output_path <- "/Users/riasadbinmahbub/CodeRepository/R/GapfillingOtherRiceSites/ModeledPVPMVPMdata"

# Save the dataframe as a CSV file
write.csv(dfmodeled, file.path(output_path, "df_luemaxgdd_modeled.csv"), row.names = FALSE)




### This data is from computer VPM spatial project YieldPP_COuntymaps_yearwise data
gppcounty<-read.csv("gppricecounty.csv")

View(gppcounty)
gppcounty$Year<- as.integer(gppcounty$Year)
##Linearregression predicted yield 
gppcounty$predictedyield<- 570+ 0.18*gppcounty$gpp_sum_8day
rmseyield<-rmse(gppcounty$yieldgm, gppcounty$predictedyield, na.rm=TRUE)
rmseyield

lb1 <- paste("RMSE == rmseyield", round(runif(1),4))
ggplot(data=gppcounty, aes(x =gpp_sum_8day , y =yieldgm, col = Year))+
  #geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =6) +
  #coord_fixed(ratio = 1) +
  #xlim(-5, 30)+
  #ylim(-5, 30)+
  xlab(bquote('Mean Cumulative GPP ('*g~ 'C'~ m^-2~season^-1*')'))+
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')'))+
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+
  scale_y_continuous(limits = c(500,1000))+
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2008, 2012, 2016, 2020), labels = c("2008","2012", "2016", "2020"))+
  
  stat_regline_equation(label.x = 1450, label.y = 580, size = 10)+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001, label.x = 1450, label.y = 625, size = 10)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 30))+ annotate(
    geom = "text",
    x = 1575,
    y = 520,
    label = bquote("RMSE == " * 56 * " g  m"^"-2" ~ season^-1),
    parse = TRUE,
    size = 10
  ) +
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(axis.ticks.length = unit(.25, "cm"))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

gppcountyyieldplot
ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/gppcountyyieldplot.png", width = 40, height = 20, units = "cm")



gppcountyyieldplot
rmse(gppcounty$gpp_sum_8day, gppcounty$yieldgm, na.rm=TRUE)

ggplot(data=gppcounty, aes(x =gpp_sum_8day , y =yieldgm, col = Year))+
  geom_point()+
  scale_color_continuous(high = "#132B43", low = "#56B1F7", breaks = c(2010, 2015, 2020), labels = c("2010", "2015", "2020"))+

  geom_smooth(method=lm, se = FALSE, size =5)+
  #facet_wrap(~year)+

  stat_regline_equation(label.x = 1400, label.y = 600, size = 15)+
  stat_cor(aes(label = ..rr.label..), label.x = 1500, label.y = 650, size = 15)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  #annotate("text", x = 1, y = 19.5, label = sprintf("RMSE: %0.2f", GPPsatellitePVPMrmse), size = 15, fontface = 'italic') +
  #annotate("text", x = 0.7, y = 15, label = sprintf("MAE:  %0.2f",GPPsatellitePVPMmae), size = 15, fontface = 'italic') +
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#net change
ggplot(data = gppcounty, aes(x = gpp_sum_8day, y = yieldgm, col = Year)) +
  geom_point(size = 6) +
  xlab(bquote('Mean Cumulative GPP ('*g~ C~ m^-2~season^-1*')')) +
  ylab(bquote('Reported Yield ('*g~ m^-2~season^-1*')')) +
  geom_smooth(method = lm, se = FALSE, size = 5) +
  scale_y_continuous(limits = c(500, 1000)) +
  scale_color_continuous(high = "#132B43", low = "#56B1F7",
                         breaks = c(2008, 2012, 2016, 2020), labels = c("2008", "2012", "2016", "2020")) +
  stat_regline_equation(label.x = 1450, label.y = 580, size = 10) +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~','~")), p.accuracy = 0.001,
           label.x = 1450, label.y = 625, size = 10) +
  theme_classic() +
  theme(text = element_text(size = 30)) +
  # Corrected line: use bquote for consistent formatting and units
  annotate(geom = "text", x = 1575, y = 520,
           label = bquote('RMSE = ', 56.88~g~m^-2~season^-1), size = 10, parse = TRUE) +
  theme(legend.key.size = unit(2, 'cm')) +
  theme(axis.line = element_text(size = 1.7)) +
  theme(axis.ticks.length = unit(.25, "cm")) +
  theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm"))


df_luemaxgdd_modeled$sitenew<-df_luemaxgdd_modeled$site.x

### FACET wrap function
### FACET wrap r2 and rmse

##rename usbda to us-bda
View(LUEmaxGDD)
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




library(caret)
library(tidyverse)
gpppvpmsitermser2 <- df_luemaxgdd_modeled %>% 
  group_by(sitenew) %>% 
  summarise(Rsq = R2(GPP_site, GPPpvpmmodel),
            RMSE = RMSE(GPP_site, GPPpvpmmodel)) %>% 
  mutate_if(is.numeric, round, digits=2)
# Here we create our annotations data frame.
df.annotationssitePVPM <- data.frame()
# Rsq
df.annotationssitePVPM <- rbind(df.annotationssitePVPM ,
                        cbind(as.character(gpppvpmsitermser2$sitenew),
                              paste("Rsq", gpppvpmsitermser2$Rsq,
                                    sep = " = ")))
# RMSE
df.annotationssitePVPM <- rbind(df.annotationssitePVPM ,
                        cbind(as.character(gpppvpmsitermser2$sitenew),
                              paste("RMSE", gpppvpmsitermser2$RMSE,
                                    sep = " = ")))

# This here is important, especially naming the first column
colnames(df.annotationssitePVPM ) <- c("sitenew", "label")
vertical_adjustment = ifelse(grepl("Rsq",df.annotationssitePVPM$label),1.5,3)

pvpmsitewxrap<- ggplot(data=df_luemaxgdd_modeled, aes(x = GPP_site, y = GPPpvpmmodel)) +
  geom_point(color="blue",alpha = 1/3) + 
  facet_wrap(sitenew ~ ., scales="free") +
  geom_smooth(method=lm, fill="black", formula = y ~ x) +
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[site],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ 
  theme_bw() +
  xlim(-5, 30)+
  ylim(-5, 30)+
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
pvpmsitewrap + geom_text(data=df.annotationssitePVPM,aes(x=-Inf,y=+Inf,label=label),
                         hjust = -0.0, vjust = vertical_adjustment, size=4)


ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/FacewrapPVPMsite.png", width = 20, height = 15, units = "cm")




##gpppvpm
gpppvpmsatellitermser2 <- df_luemaxgdd_modeled %>% 
  group_by(sitenew) %>% 
  summarise(Rsq = R2(GPP_site, gpppvpm),
            RMSE = RMSE(GPP_site, gpppvpm)) %>% 
  mutate_if(is.numeric, round, digits=2)
# Here we create our annotations data frame.
df.annotationsatellitePVPM <- data.frame()
# Rsq
df.annotationsatellitePVPM <- rbind(df.annotationsatellitePVPM ,
                                    cbind(as.character(gpppvpmsatellitermser2 $sitenew),
                                          paste("Rsq", gpppvpmsatellitermser2 $Rsq,
                                                sep = " = ")))

# RMSE
df.annotationsatellitePVPM <- rbind(df.annotationsatellitePVPM ,
                                    cbind(as.character(gpppvpmsatellitermser2 $sitenew),
                                          paste("RMSE", gpppvpmsatellitermser2 $RMSE,
                                                sep = " = ")))

# This here is important, especially naming the first column
# Species
colnames(df.annotationsatellitePVPM ) <- c("sitenew", "label")

df.annotationsatellitePVPM
vertical_adjustment = ifelse(grepl("Rsq",df.annotationsatellitePVPM$label),1.5,3)





pvpmsatellitewrap<- ggplot(data=df_luemaxgdd_modeled, aes(x = GPP_site, y = gpppvpm)) +
  geom_point(color="blue",alpha = 1/3) + 
  facet_wrap(sitenew ~ ., scales="free") +
  geom_smooth(method=lm, fill="black", formula = y ~ x) +
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
pvpmsatellitewrap + geom_text(data=df.annotationsatellitePVPM,aes(x=-Inf,y=+Inf,label=label),
                              hjust = -0.1, vjust = vertical_adjustment, size=3.5)



ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/FacewrapPVPMsatellite.png", width = 20, height = 15, units = "cm")
df_luemaxgdd_modeled


## mAKE THE GRAPH OF PVPM
plot(df_luemaxgdd_modeled$doy.x, df_luemaxgdd_modeled$EVI_SG.x)



###Site and yearwise LUEmax
# luemax modeled=LUEmaxmodeled
# luemax calculated = LUEpvpmcal
luemaxmodeledyrgraph<-df_luemaxgdd_modeled$LUEmaxmodeled
luemaxcalyrgraph<- df_luemaxgdd_modeled$LUEpvpmcal
###invalid graphics state
## run: dev.off()


plot(df_luemaxgdd_modeled$LUEmaxmodeled, df_luemaxgdd_modeled$LUEpvpmcal)

luemaxpvpm<- ggplot(data=df_luemaxgdd_modeled, aes(x = luemaxcalyrgraph, y = luemaxmodeledyrgraph)) +
  geom_point(color="blue",alpha = 1/3) + 
  facet_wrap(sitenew ~ ., scales="free") +
  geom_smooth(method=lm, fill="black", formula = y ~ x) +
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*')'))+
  ylab(expression(paste(GPP~PVPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ 
  theme_bw() +
  geom_abline(intercept = 0, slope = 1, size =1, col ="red", linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
luemaxpvpm + geom_text(data=df.annotationsatellitePVPM,aes(x=-Inf,y=+Inf,label=label),
                              hjust = -0.1, vjust = vertical_adjustment, size=3.5)

##3 summarise based on the max values
maxsitevalues<-df_luemaxgdd_modeled %>% 
  group_by(site.x)%>%
  summarise((max = max(GPP_site)))

mean(maxsitevalues$`(max = max(GPP_site))`) 

library(Fgmutils)


### Bias and mse
get_mse <- function(truth, estimate){
  mean((estimate- truth)^2)
}
get_bias <-function(truth, estimate){
  mean(estimate)-truth
}

get_var<-function(truth, estimate){
  mean((estimate-mean(estimate))^2)
}

mean(get_bias(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPpvpmmodelGDD))^2
mean(get_bias(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpppvpm))^2
mean(get_bias(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpp))^2
mean(get_bias(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPvpm))^2

mean(get_var(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPpvpmmodelGDD))^2
mean(get_var(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpppvpm))^2
mean(get_var(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$gpp))^2
mean(get_var(df_luemaxgdd_modeled$GPP_site, df_luemaxgdd_modeled$GPPvpm))^2
df_luemaxgdd_modeled

hist(df_luemaxgdd_modeled$Ws.x)
hist(df_luemaxgdd_modeled$ts, col = df_luemaxgdd_modeled$doy.x)

plot(df_luemaxgdd_modeled$doy.x, df_luemaxgdd_modeled$LSWI.x)
plot(df_luemaxgdd_modeled$doy.x, df_luemaxgdd_modeled$Ws.y)

plot(df_luemaxgdd_modeled$doy.x, df_luemaxgdd_modeled$Tspvpm_modeled, xlab = "DOY", ylab="Ts")

##plot ts after group meeting

LUEmaxGDD<-LUEmaxGDD[!is.na(LUEmaxGDD$site),]

ggplot(data=LUEmaxGDD, aes(doy, Tsvpm, colour = site, shape= as.factor(year))) +
  geom_point(size =3)+
  stat_smooth((aes(group = 1)))+
  xlab("DOY")+
  ylab("Ts")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)

ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/Ts.png", dpi = 300, width = 16, height = 8)

ggplot(data=LUEmaxGDD, aes(doy, Ws, colour = site, shape= as.factor(year))) +
  geom_point(size =3)+
  stat_smooth((aes(group = 1)))+
  xlab("DOY")+
  ylab("Ws")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)
ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/Ws.png", dpi = 300, width = 16, height = 8)

ggplot(data=LUEmaxGDD, aes(Tsvpm, GPP_site, colour = site, shape= as.factor(year))) +
  geom_point(size =3)+
  #stat_smooth((aes(group = 1)))+
  xlab("Ts")+
  ylab("GPP-EC")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)

ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/TsGPP.png", dpi = 300, width = 16, height = 8)

View(LUEmaxGDD%>%filter(site == "USHRA"))


ggplot(data=LUEmaxGDD, aes(Ws, GPP_site, colour = site, shape= as.factor(year))) +
  geom_point(size =3)+
  #stat_smooth((aes(group = 1)))+
  xlab("Ws")+
  ylab("GPP-EC")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)

ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/WsGPP.png", dpi = 300, width = 16, height = 8)

ggplot(data=LUEmaxGDD, aes(LUEmaxmodeled, GPP_site, colour = site, shape= as.factor(year))) +
  geom_point(size =3)+
  #stat_smooth((aes(group = 1)))+
  xlab("LUEmax")+
  ylab("GPP-EC")+
  theme(text = element_text(size = 30))+
  theme_bw(base_size= 25)

ggsave("/Users/riasadbinmahbub/Library/CloudStorage/Box-Box/Research/ManuscriptFile/Optimum Air Temperature/Figure/LUEmaxmodeledGPP.png", dpi = 300, width = 16, height = 8)

hist(LUEmaxGDD$Ws)


library(sensemakr)

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
LUEmaxGDD$LUEmaxmodeled

GPPlinearmodel<- lm(GPP_site~Tspvpm_modeled+ Ws+ Fapar + PAR_site+ LUEmaxmodeled , LUEmaxGDD)
calc.relimp(GPPlinearmodel, type ="lmg", data =LUEmaxGDD )

GPPminmodelGDD


# Bootstrap Measures of Relative Importance (100 samples) 
boot <- boot.relimp(GPPlinearmodel, b = 100, type = "lmg", rank = TRUE, rela = FALSE)
plot(booteval.relimp(boot,sort=TRUE)) # plot result


# Load the sensitivity package
library(sensitivity)

# Perform global sensitivity analysis using the FAST method
sa <- fast99(GPPminmodel, ranges, n = 1000, keep.input = TRUE)

# Plot the sensitivity indices
barplot(sa$S1, names.arg = colnames(sa$S1), ylab = "Sensitivity index")

library(hydroGOF)                                                                   
pbias(df_luemaxgdd_modeled$gpppvpm, df_luemaxgdd_modeled$GPP_site)
pbias(df_luemaxgdd_modeled$GPPpvpmmodelGDD, df_luemaxgdd_modeled$GPP_site)


### bias
get_bias = function(estimate, truth) {
  mean(estimate) - truth
}
LUEmaxGDD$vpmbias<-get_bias(LUEmaxGDD$GPPvpm, LUEmaxGDD$GPP_site)

plot(LUEmaxGDD$doy, LUEmaxGDD$vpmbias)
