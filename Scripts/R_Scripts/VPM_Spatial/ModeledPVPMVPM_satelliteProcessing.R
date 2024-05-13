##Processing the Modeled data
library(readr)
library(lubridate)
library(cowplot)
library(patchwork)
library(tidyverse)
library(viridis)
library(ggpubr)
##Create RMSE and MAE metrics
library(Metrics)
detach("package:Metrics", unload=TRUE)
require(hydroGOF)
library(viridis)
library(ggpubr)
library(ggplot2)

GPPsiteVPMrmse<-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0_06512_31_79)
#GPPsitePVPMrmse <-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPpvpmmodel)
GPPsatelliteVPMrmse<-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpp)
#PPsatellitePVPMrmse<-Metrics::rmse(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpppvpm)
GPPsiteVPMmae<-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0_06512_31_79)
#GPPsitePVPMmae<-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPpvpmmodel)
GPPsatelliteVPMmae <-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpp)
#GPPsatellitePVPMmae<-Metrics::mae(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpppvpm)
GPPsiteVPMbias<-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPP_0_06512_31_79)
#GPPsitePVPMbias<-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$GPPpvpmmodel)
GPPsatelliteVPMbias <-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpp)
#GPPsatellitePVPMbias<-Metrics::bias(sitesatellitemergedDATA$GPP_site, sitesatellitemergedDATA$gpppvpm)
GPPsiteVPM<-ggplot(data=sitesatellitemergedDATA, aes(x =GPP_site , y =GPP_0_06512_31_79, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(expression(paste(GPP~VPM[site],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  stat_regline_equation(label.x = -5, label.y = 22, size = 18)+
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 20, size = 18)+
  ggplot2::annotate("text", x = -0.5, y = 28, label = sprintf("RMSE: %0.2f", GPPsiteVPMrmse), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 24, label = sprintf("MAE:  %0.2f",GPPsiteVPMmae), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 26, label = sprintf("Bias: %0.2f", GPPsiteVPMbias), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = 8.5, y = 28, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 24, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 26, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

GPPsatelliteVPM<-ggplot(data=sitesatellitemergedDATA, aes(x =GPP_site , y =gpp, col = DAP))+
  geom_abline(intercept = 0, slope = 1, size =5, col ="red", linetype="dashed")+
  geom_point(size =10) +
  #coord_fixed(ratio = 1) +
  xlim(-5, 30)+
  ylim(-5, 30)+
  xlab(bquote('GPP EC ('*g~ 'C'~ m^-2~day^-1*') 8-day mean'))+
  ylab(expression(paste(GPP~VPM[spatial],~'('*g~ 'C'~ m^-2~day^-1*')' )))+ # '( '*g~ 'C'~ m^-2~day^-1*')'
  #ggtitle("Way 3 Pixel 3")+
  geom_smooth(method=lm, se = FALSE, size = 5)+
  #facet_wrap(~year)+
  scale_color_viridis(option = "D", direction=-1, limits = c(0, 170))+
  stat_regline_equation(label.x = -5, label.y = 23, size = 16)+
  stat_cor(aes(label = ..rr.label..), label.x = -5, label.y = 21, size = 16)+
  #geom_text(aes(label=LUE),hjust=0, vjust=0)+
  ggplot2::annotate("text", x = -0.5, y = 29, label = sprintf("RMSE: %0.2f", GPPsatelliteVPMrmse), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 25, label = sprintf("MAE:  %0.2f",GPPsatelliteVPMmae), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = -1.5, y = 27, label = sprintf("Bias: %0.2f", GPPsatelliteVPMbias), size = 16, fontface = 'italic') +
  ggplot2::annotate("text", x = 8.5, y = 29, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 25, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  ggplot2::annotate("text", x = 7.5, y = 27, label = expression(~''*g~'C'~m^{-2}~day^{-1}*''),size = 16, parse = TRUE)+
  #annotate("text", x = 0, y = 30, label = paste("Number of observations: ", nrow(way3pixel3df)), size =5)+
  theme_classic()+
  theme(text = element_text(size = 48))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(axis.line=element_line(size=1.7))+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

#https://stackoverflow.com/questions/13649473/add-a-common-legend-for-combined-ggplots
##VPM
combined <-  GPPsiteVPM  + GPPsatelliteVPM  & theme(legend.position = "right")
combinedvpm<-combined + plot_layout(guides = "collect")+plot_annotation(tag_levels = 'A')
combinedvpm
ggsave(combinedvpm, filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/VPMModelsscatterplot.png", width = 40, height = 20)


# Assuming you have calculated the metrics

cat("GPPsiteVPMrmse:", GPPsiteVPMrmse, "\n")
cat("GPPsatelliteVPMrmse:", GPPsatelliteVPMrmse, "\n")
cat("GPPsiteVPMmae:", GPPsiteVPMmae, "\n")
cat("GPPsatelliteVPMmae:", GPPsatelliteVPMmae, "\n")
cat("GPPsiteVPMbias:", GPPsiteVPMbias, "\n")
cat("GPPsatelliteVPMbias:", GPPsatelliteVPMbias, "\n")


###Max minimum values of the result sections:
max(sitesatellitemergedDATA$GPPvpmsite)
max(sitesatellitemergedDATA$gpp)


###VPM thesis
library(reshape2)
library(cowplot)
library(tidyverse)
separate_DF<-sitesatellitemergedDATA%>% select(GPP_site, GPP_0_06512_31_79, gpp, DAP)
df_long <- melt(data = separate_DF, 
                id.vars = c("DAP"),
                variable.name = "Model",
                value.name = "GPP")

# Use spread to convert back to the original wide format
# Filter rows where Model is "GPP_site"
filtered_df1 <- df_long %>%
  filter(Model == "GPP_site")
filtered_df2 <- df_long %>%
  filter(Model == "GPPvpmsite")
filtered_df3 <- df_long %>%
  filter(Model == "gpp")

colnames(filtered_df1)
colnames(filtered_df2)

### Change the labels name
plt<-ggplot(df_long,aes(x=DAP,y=GPP,colour=Model,shape=Model, stroke = 1)) + geom_point(size =3)+geom_smooth(alpha=0.15, size = 2)+
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
ggsave(filename = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/vpmtemporalgpp2.png", plot = plt, width = 16, height = 8, dpi = 300)


####RESIDUAL PLOTS
sitesatellitemergedDATA$residualsitevpm<-sitesatellitemergedDATA$GPP_site- sitesatellitemergedDATA$gpp
sitesatellitemergedDATA$residualsatellitevpm<-sitesatellitemergedDATA$GPP_site- sitesatellitemergedDATA$GPP_0_06512_31_79

sitesatellitemergedDATA$residualsitevpm
sitesatellitemergedDATA$residualsatellitevpm

# Create ggplot objects for each set of data
plot1 <- ggplot(sitesatellitemergedDATA, aes(x = DAP)) +
  geom_point(aes(y = residualsitevpm),  size = 2) +
  labs(x = NULL, y = expression("Residuals from VPM"[site]*" (g C m"^{-2}*" day"^{-1}*")")) +
  theme_bw(base_size = 19)+
  scale_x_continuous(breaks = seq(0, 150, by = 25))+
  annotate("text", x = 150, y = 14, label = "A", vjust = 1, hjust = 1, size = 17, fontface = "bold")

plot2 <- ggplot(sitesatellitemergedDATA, aes(x = DAP)) +
  geom_point(aes(y = residualsatellitevpm), size = 2) +
  labs(x = "Days after planting (day)", y = expression("Residuals from VPM"[satellite]*" (g C m"^{-2}*" day"^{-1}*")")) +
  theme_bw(base_size = 19)+
  scale_x_continuous(breaks = seq(0, 150, by = 25))+
  annotate("text", x = 150, y = 14, label = "B", vjust = 1, hjust = 1, size = 17, fontface = "bold")

# Combine the plots
combined_plot <- plot1 + plot2 + plot_layout(nrow = 2) 
# Save the combined plot as an image
ggsave(file = "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/residuals_graph.png",
       plot = combined_plot,
       width = 16, height = 12, units = "in", dpi = 300)


#############################################
#############UNNECESSARY######################
#################################################

# If necessary, convert data to numeric
library(openair)
library(tidyverse)
df_long
GPP_site <- as.numeric(GPP_site)
VPMsite <- as.numeric(VPMsite)
VPMspatial <- as.numeric(VPMspatial)

# Create tailor diagram
TaylorDiagram(df_long, 
              obs = "GPP_site", 
              mod = "Model", 
              group = "GPP")

# Add legend after creating the plot
legend("bottomright", legend = c("VPMsite", "VPMspatial"),
       col = c("black", "blue"), pch = c(16, 17), bty = "n")


library(plotrix)


# Subset the data for GPP_site and GPPpvpmmodel
observed <- subset(df_long, Model == "GPP_site")$GPP
modeled <- subset(df_long, Model == "GPPpvpmmodel")$GPP

# Create Taylor Diagram
taylor.diagram(observed, modeled, col = "black", pch = 16, main = "Taylor Diagram of Model Performances")



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







