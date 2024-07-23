

###Fixing the plot
for (var_name in names(sheet_cols)) {  
  points[[var_name]]<-as.integer((max(merged_data_frames[[var_name]]$DOY)- min(merged_data_frames[[var_name]] $DOY))/8) ## how many 8 day data points will be there
  DOYlist[[var_name]]<-rep(0, points[[var_name]]+1) ##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
  LUEmax[[var_name]]<-rep(0,points[[var_name]]) ## this list will store the luemax values
  lightresponsegs[[var_name]]<-rep(0,points[[var_name]])## this list will store the luemax values from bigleaf analysis
  
  ## creating some dataframes that will store the data based on 8 day windows
  for (i in 1:(length(DOYlist[[var_name]])-1)){
    DOYlist[[var_name]][1] = MODISdoy[[var_name]]
    DOYlist[[var_name]][i+1]=DOYlist[[var_name]][i]+ 8
  }
  
  ## Creating a list of empty dataframes
  for (i in 1:points[[var_name]]){
    dflist[[var_name]][[i]]<-data.frame()
  }
  ###Storing the data in the empty dataframes
  for (i in 1: length(dflist[[var_name]])){
    dflist[[var_name]][[i]]<- subset(merged_data_frames[[var_name]], DOY>=DOYlist[[var_name]][i] & DOY<DOYlist[[var_name]][i+1])
    
    dflist[[var_name]][[i]]<-subset(dflist[[var_name]][[i]], PAR_Regression>20)
    percentile_95 <- quantile(dflist[[var_name]][[i]]$GPP_modeled, 0.90)
    #dflist[[var_name]][[i]] <- subset(dflist[[var_name]][[i]], GPP_modeled > percentile_95) ## subsetting GPP above 95th percentile
    #dflist[[var_name]][[i]]$APAR <-dflist[[var_name]][[i]]$FAPAR* dflist[[var_name]][[i]]$PAR_Regression ## calculate the APAR
  }
  
  for (i in 1:points[[var_name]]){
    modellist[[var_name]][[i]]<-data.frame()
    lightresponsegs_modellist[[var_name]][[i]]<-data.frame()
  }
  
  stderror[[var_name]]<-rep(0, points[[var_name]])
  rsquared[[var_name]]<-rep(0, points[[var_name]])
  rss[[var_name]]<-rep(0, points[[var_name]])
  rmse[[var_name]]<-rep(0, points[[var_name]])
  ### Read the files of Site Calibration 
  ### PAR to APAR
  
  
  for (i in (1:points[[var_name]])){
    y<-dflist[[var_name]][[i]]$GPP_modeled
    x<-dflist[[var_name]][[i]]$APAR  ## changed to APAR, before it was PAR
    modellist[[var_name]][[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.03 ,g= 25),   ## what should the parameters value
                                      lower = c(a = 0, g = 0),  # Set lower bound for 'a'
                                      upper = c(a = 100, g =200) )
    
    LUEmax[[var_name]][[i]]<-modellist[[var_name]][[i]]$m$getPars()[1]
    stderror[[var_name]][i]<-summary(modellist[[var_name]][[i]])$parameters[1,2]
    rss[[var_name]][i]<-sum((modellist[[var_name]][[i]]$m$resid())^2)
    rmse[[var_name]][i]<-sqrt(crossprod(modellist[[var_name]][[i]]$m$resid())/(length(modellist[[var_name]][[i]]$m$resid())))
    rsquared[[var_name]][i] <-  1-(rss[[var_name]][i]/sum((y - mean(y))^2))
  }
}

LUEmax
plot_directory <- "/Users/riasadbinmahbub/RProgramming/GapfillingOtherRiceSites/GapfillingOtherRiceSites/Figure/LightResponseCurve/GPP_APAR_NLSM/GGplotExport"
for (var_name in names(sheet_cols)) {
  plots <- lapply(1:points[[var_name]], function(i) {
    ggplot(data = dflist[[var_name]][[i]], aes(x = APAR, y = GPP_modeled, color = DOY)) +
      geom_point() +
      geom_line(aes(x = sort(APAR), y = fitted(modellist[[var_name]][[i]])[order(APAR)]), col = 'red', size = 2) +
      labs(x = "PAR", y = "GPP") +
      theme_minimal()
  })
  
  # Save plots in a single file
  ggsave(file.path(plot_directory, paste0(var_name, "_light_response_plot.png")), 
         grid.arrange(grobs = plots, ncol = 5, nrow = 4))
}

ggplot(data = dflist$USOF1_2017[[2]], aes(x = APAR, y = GPP_modeled, color = DOY)) +
  geom_point()+ 
  geom_line(aes(x = sort(dflist$USOF1_2017[[2]]$APAR), y = fitted(modellistGPP$USOF1_2017[[2]])[order(dflist$USOF1_2017[[2]]$APAR)]), col = 'red', size = 2) 
  labs(x = "PAR", y = "GPP") +
  theme_minimal()

  # Example assuming dflist and modellistGPP are lists with the expected structure
  plot(dflist$USHRA_2015[[3]]$APAR, dflist$USHRA_2015[[3]]$GPP_modeled, xlab = "APAR", ylab = "GPP")
  lines(sort(dflist$USHRA_2015[[3]]$APAR), fitted(modellistGPP$USHRA_2015[[3]])[order(dflist$USHRA_2015[[3]]$APAR)], col='red', lwd=2) 
  lines(sort(dflist[[var_name]][[i]]$APAR), fitted(modellistGPP[[var_name]][[i]])[order(dflist[[var_name]][[i]]$APAR)], col='red', lwd=2) 
  
  plot(fitted(modellistGPP$USHRA_2015[[3]]), fitted(modellistGPP$USHRA_2015[[3]])[order(dflist$USHRA_2015[[3]]$APAR)])
  
  ###NEE plot
  plot(dflist$USHRA_2015[[9]]$APAR, dflist$USHRA_2015[[9]]$NEE_modeled, xlab = "APAR", ylab = "NEE")
  lines(sort(dflist$USHRA_2015[[9]]$APAR), fitted(modellistNEE$USHRA_2015[[9]])[order(dflist$USHRA_2015[[9]]$APAR)], col='red', lwd=2) 
  
  
  plot(dflist$USHRA_2015[[10]]$APAR, dflist$USHRA_2015[[10]]$NEE_modeled, xlab = "APAR", ylab = "NEE")
  lines(sort(dflist$USHRA_2015[[10]]$APAR), dflist$USHRA_2015[[10]]$GPP_predicted_modellistNEE[order(dflist$USHRA_2015[[10]]$APAR)])
  
  plot(dflist$USHRA_2015[[10]]$APAR, dflist$USHRA_2015[[10]]$GPP_modeled, xlab = "APAR", ylab = "NEE")
  lines(sort(dflist$USHRA_2015[[10]]$APAR), dflist$USHRA_2015[[10]]$GPP_predicted_modellistGPP[order(dflist$USHRA_2015[[10]]$APAR)])
  
plot_directory <- "/Users/riasadbinmahbub/RProgramming/GapfillingOtherRiceSites/GapfillingOtherRiceSites/Figure/LightResponseCurve/GPP_APAR_NLSM"
png(file.path(plot_directory, paste0(var_name, "_light_response_plot.png")), width = 800, height = 800)
for (var_name in names(sheet_cols)) {
  png(file.path(plot_directory, paste0(var_name, "_light_response_plot.png")), width = 800, height = 800)
  par(mfrow = c(5, 4))
  par(mar=c(2, 2, 2, 2))
  for (i in (1:points[[var_name]])){
  plot(dflist[[var_name]][[i]]$APAR, dflist[[var_name]][[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP", col=dflist[[var_name]][[i]]$DOY)
  lines(sort(dflist[[var_name]][[i]]$APAR), fitted(modellistGPP[[var_name]][[i]])[order(dflist[[var_name]][[i]]$APAR)], col='red', lwd=2) 
  }
  dev.off() 
}
names(sheet_cols)


stdflist[[var_name]][[1]]
LUEmax
stderr()
dflist$USOF1_2017[[1]]$GPP_modeled <- as.numeric(dflist$USOF1_2017[[1]]$GPP_modeled)
dflist$USOF1_2017[[1]]%>%group_by(DOY) %>% summarise(mean(GPP_modeled))
dflist$USOF1_2017[[2]]%>%group_by(DOY) %>% summarise(mean(GPP_modeled))
dflist$USOF1_2017[[3]]%>%group_by(DOY) %>% summarise(mean(GPP_modeled))
View(dflist$USOF1_2017[[3]])


# Assuming your data frame is named df
result <- dflist$USOF1_2017[[2]] %>% 
  group_by(DOY) %>% 
  summarise(mean_GPP = mean(GPP_modeled))

# Find the row with the maximum mean_GPP value
max_row <- result[which.max(result$mean_GPP), ]

# Print the row with the maximum mean_GPP value
max_row$DOY





###Fixing the plot
for (var_name in names(sheet_cols)) {  
  points[[var_name]]<-as.integer((max(merged_data_frames[[var_name]]$DOY)- min(merged_data_frames[[var_name]] $DOY))/64) ## how many 8 day data points will be there
  DOYlist[[var_name]]<-rep(0, points[[var_name]]+1) ##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
  LUEmax[[var_name]]<-rep(0,points[[var_name]]) ## this list will store the luemax values
  lightresponsegs[[var_name]]<-rep(0,points[[var_name]])## this list will store the luemax values from bigleaf analysis
  
  ## creating some dataframes that will store the data based on 8 day windows
  for (i in 1:(length(DOYlist[[var_name]])-1)){
    DOYlist[[var_name]][1] = MODISdoy[[var_name]]
    DOYlist[[var_name]][i+1]=DOYlist[[var_name]][i]+ 64
  }
  
  ## Creating a list of empty dataframes
  for (i in 1:points[[var_name]]){
    dflist[[var_name]][[i]]<-data.frame()
  }
  ###Storing the data in the empty dataframes
  for (i in 1: length(dflist[[var_name]])){
    dflist[[var_name]][[i]]<- subset(merged_data_frames[[var_name]], DOY>=DOYlist[[var_name]][i] & DOY<DOYlist[[var_name]][i+1])
    
    dflist[[var_name]][[i]]<-subset(dflist[[var_name]][[i]], PAR_Regression>20)
    result <- dflist[[var_name]][[i]] %>% 
      group_by(DOY) %>% 
      summarise(mean_GPP = mean(GPP_modeled))
    max_row <- result[which.max(result$mean_GPP), ]
    #percentile_95 <- quantile(dflist[[var_name]][[i]]$GPP_modeled, 0.90)
    dflist[[var_name]][[i]] <- subset(dflist[[var_name]][[i]], DOY == max_row$DOY) ## subsetting GPP above 95th percentile
    #dflist[[var_name]][[i]]$APAR <-dflist[[var_name]][[i]]$FAPAR* dflist[[var_name]][[i]]$PAR_Regression ## calculate the APAR
  }
  
  for (i in 1:points[[var_name]]){
    modellist[[var_name]][[i]]<-data.frame()
    lightresponsegs_modellist[[var_name]][[i]]<-data.frame()
  }
  
  stderror[[var_name]]<-rep(0, points[[var_name]])
  rsquared[[var_name]]<-rep(0, points[[var_name]])
  rss[[var_name]]<-rep(0, points[[var_name]])
  rmse[[var_name]]<-rep(0, points[[var_name]])
  ### Read the files of Site Calibration 
  ### PAR to APAR
  
  
  for (i in (1:points[[var_name]])){
    y<-dflist[[var_name]][[i]]$GPP_modeled
    x<-dflist[[var_name]][[i]]$APAR  ## changed to APAR, before it was PAR
    modellist[[var_name]][[i]]<-nlsLM((y) ~ (((a * (x) * g)/(a * (x)+ g))), start=list(a= 0.03 ,g= 25),   ## what should the parameters value
                                      lower = c(a = 0, g = 0),  # Set lower bound for 'a'
                                      upper = c(a = 100, g =200) )
    
    LUEmax[[var_name]][[i]]<-modellist[[var_name]][[i]]$m$getPars()[1]
    stderror[[var_name]][i]<-summary(modellist[[var_name]][[i]])$parameters[1,2]
    rss[[var_name]][i]<-sum((modellist[[var_name]][[i]]$m$resid())^2)
    rmse[[var_name]][i]<-sqrt(crossprod(modellist[[var_name]][[i]]$m$resid())/(length(modellist[[var_name]][[i]]$m$resid())))
    rsquared[[var_name]][i] <-  1-(rss[[var_name]][i]/sum((y - mean(y))^2))
  }
}
LUEmax

##nonfiltered dataset
###Fixing the plot
for (var_name in names(sheet_cols)) {  
  points[[var_name]]<-as.integer((max(merged_data_frames[[var_name]]$DOY)- min(merged_data_frames[[var_name]] $DOY))/8) ## how many 8 day data points will be there
  DOYlist[[var_name]]<-rep(0, points[[var_name]]+1) ##Creating a list of DOYlist (DOYlist is 1 greater than LUEMax length)
  LUEmax[[var_name]]<-rep(0,points[[var_name]]) ## this list will store the luemax values
  lightresponsegs[[var_name]]<-rep(0,points[[var_name]])## this list will store the luemax values from bigleaf analysis
  
  ## creating some dataframes that will store the data based on 8 day windows
  for (i in 1:(length(DOYlist[[var_name]])-1)){
    DOYlist[[var_name]][1] = MODISdoy[[var_name]]
    DOYlist[[var_name]][i+1]=DOYlist[[var_name]][i]+ 8
  }
  
  ## Creating a list of empty dataframes
  for (i in 1:points[[var_name]]){
    dflist[[var_name]][[i]]<-data.frame()
  }
  ###Storing the data in the empty dataframes
  for (i in 1: length(dflist[[var_name]])){
    dflist[[var_name]][[i]]<- subset(merged_data_frames[[var_name]], DOY>=DOYlist[[var_name]][i] & DOY<DOYlist[[var_name]][i+1])
    
    dflist[[var_name]][[i]]<-subset(dflist[[var_name]][[i]], PAR_Regression>20)
    #result <- dflist[[var_name]][[i]] %>% 
      #group_by(DOY) %>% 
      #summarise(mean_GPP = mean(GPP_modeled))
    #max_row <- result[which.max(result$mean_GPP), ]
    #percentile_95 <- quantile(dflist[[var_name]][[i]]$GPP_modeled, 0.90)
    #dflist[[var_name]][[i]] <- subset(dflist[[var_name]][[i]], DOY == max_row$DOY) ## subsetting GPP above 95th percentile
    #dflist[[var_name]][[i]]$APAR <-dflist[[var_name]][[i]]$FAPAR* dflist[[var_name]][[i]]$PAR_Regression ## calculate the APAR
  }
}



###Plotting LUEmax
for (var_name in names(sheet_cols)) {
  par(mfrow = c(5, 4))
  par(mar=c(2, 2, 2, 2))
  for (i in (1:points[[var_name]])){
    plot(dflist[[var_name]][[i]]$APAR, dflist[[var_name]][[i]]$GPP_modeled, xlab = "PAR", ylab = "GPP", col=dflist[[var_name]][[i]]$DOY)
    lines(sort(dflist[[var_name]][[i]]$APAR), fitted(modellist[[var_name]][[i]])[order(dflist[[var_name]][[i]]$APAR)], col='red', lwd=2) 
  }
}

LUEmax
rss

plot(LUEmax$USBDC_2015)
