### calculate all the VPM images using one script

## Load the libraries
library(raster)
library(ggplot2)
library(tidyverse)
library(sf)
library(lubridate)
library(maptools)  ## For wrld_simpl
library(rgdal)
library(terra)
library(sp)
library(rgdal)

###Create the shapefile path 
### Read the files in the path

### path of the images
path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/"
year = 2008
pathlist <- vector("list", 13)
for (i in 1:13) {
  pathlist[i] = paste(path, year, "VPM", sep = "")
  year = year+1
} 

### rastlist 
rastlist <- vector("list", 13)
for (i in 1:13) {
  rastlist[[i]] <- list.files(path = pathlist[[i]], pattern='.tif$', 
                              all.files=TRUE, full.names=FALSE)
}

## stacklist
#stacked <-stack(rastlist)

stacked <- vector("list", 13)
for (i in 1:13) {
  setwd(pathlist[[i]])
  stacked[[i]] <- stack(rastlist[[i]])
}

## Create the shapefile paths
pathshp1 = "C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapfile/ArkansasRiceClipped/"
setwd(pathshp1)
year = 2008
pathshp2 = "/RepojectedAreaGreaterthan5000/"
pathshp3 = "ArkansasRice"
pathshp4 = "_Shapefile_corrected.shp"
pathshplist <- vector("list", 13)
for (i in 1:13) {
  pathshplist[i] = paste(pathshp1, year,pathshp2, pathshp3, year, pathshp4,sep = "")
  year = year+1
} 


arkansasriceshp<-vector("list", 13)
### Read the shapefile paths
#arkansasshp2018 <- readOGR("C:/Users/rbmahbub/Documents/Data/GeospatialData/Shapfile/ArkansasRiceClipped/2018/FixedGeometry/ricearkansasshapefile2009FixGeoricearkansasshapefile2018.shp")


shapefilelist <- vector("list", 13)
for (i in 1:13) {
  shapefilelist[[i]] <- readOGR(pathshplist[[i]])
}

shapefilelist


## Transform the shapfiles
shapefilelisttransformed<-vector("list", 13)
for (i in 1:13) {
  shapefilelisttransformed[[i]] <- spTransform(shapefilelist[[i]], crs(stacked[[i]]))
}



## Crop the shapefiles
rastlistcropped <- vector("list", 13) 
for (i in 1:13) {
  rastlistcropped[[i]] <- raster::crop(stacked[[i]], shapefilelisttransformed[[i]])
}


## Mask the shapefiles
rastlistmasked <- vector("list", 13) 
for (i in 1:13) {
  rastlistmasked[[i]] <- raster::mask(rastlistcropped[[i]], shapefilelisttransformed[[i]])
}
##Summary
stackedsummarygrowing <- vector("list", 13) 
for (i in 1:13) {
  stackedsummarygrowing[[i]] <- summary(getValues(rastlistmasked[[i]]))
}

### Mean rows
stackedsummarygrowingdf<-vector("list", 13)
for (i in 1:13) {
  stackedsummarygrowingdf[[i]] <- as.data.frame(stackedsummarygrowing[[i]])
}

for(i in 1:13) {
  for(j in 1:nrow(stackedsummarygrowingdf[[1]])) {
    stackedsummarygrowingdf[[i]]$Meanrow[j]<-substr(stackedsummarygrowingdf[[i]][j,3], 1,4)
  }
}

## Filter the mean rows
stackedsummarygrowingdfmean<-vector("list", 13)
for (i in 1:13) {
  stackedsummarygrowingdfmean[[i]] <- stackedsummarygrowingdf[[i]]%>% filter(Meanrow == "Mean")
}
## Get the values of the mean rows
## Get the numeric Mean values
for (i in 1:13) {
  for (j in 1:length(stackedsummarygrowingdfmean[[i]]$Var1)) {
    stackedsummarygrowingdfmean[[i]]$MeanGPP[j]<-as.numeric(substr(stackedsummarygrowingdfmean[[i]][j,3], 9,20))
  } 
  
}
## Add date 
rastlistdf<-vector("list", 13)
for (i in 1:13) {
  rastlistdf[[i]] <- as.data.frame(rastlist[[i]])
}
for (i in 1:13) {
  for (j in 1:length(rastlistdf[[i]]$rastlist)) {
    rastlistdf[[i]]$Date[j]<-((substr(rastlistdf[[i]][j,], 1,10)))
  }
}
### Bind the 
df_list<-bind_rows(stackedsummarygrowingdfmean, .id = "column_label")
df_list_date<-bind_rows(rastlistdf, .id = "column_label")
## cHART OF NA
stackedsummarygrowingdfNA<-vector("list", 13)
for (i in 1:13) {
  stackedsummarygrowingdfNA[[i]] <- stackedsummarygrowingdf[[i]]%>% filter(Meanrow == "NA's")
}
## Get the numeric na values
for (i in 1:13) {
  for (j in 1:length(stackedsummarygrowingdfNA[[i]]$Var1)) {
    stackedsummarygrowingdfNA[[i]]$nullvalue[j]<-as.numeric(substr(stackedsummarygrowingdfNA[[i]][j,3], 9,20))
  } 
}
df_listNA<-bind_rows(stackedsummarygrowingdfNA, .id = "column_label")
df_listNA<-df_listNA[, 6]
df_merge <- data.frame(df_list,df_list_date, df_listNA)

##Percentage of Null values
nullvaluespercentage<-vector("list", 13)
for (i in 1:13) {
  maxval<-ncell(rastlistmasked[[i]])-min(stackedsummarygrowingdfNA[[i]]$nullvalue)
  nullvaluespercentage[[i]] <- ((((ncell(rastlistmasked[[i]]))- (stackedsummarygrowingdfNA[[i]]$nullvalue))/maxval)*100)
}

nullvaluespercentage_df<-list.rbind(nullvaluespercentage, .id = "column_label")
nullvaluespercentagedf <- do.call(rbind, nullvaluespercentage)

library(purrr)
nullvaluespercentagedf<-unlist(nullvaluespercentage, recursive = FALSE)
nullvaluespercentage_df<-as.data.frame(nullvaluespercentagedf)

df_merge <- data.frame(df_list,df_list_date, df_listNA, nullvaluespercentage_df)

df_merge$Date<-ymd(df_merge$Date)
df_merge$DOY<-yday(df_merge$Date)
df_merge$MeanGPP<-as.numeric(df_merge$MeanGPP)
df_merge$Year <- as.numeric(format(df_merge$Date, "%Y"))
df_merge$Year <- as.character(df_merge$Year)
df_merge_filtered$Year <- as.character(df_merge_filtered$Year)
df_merge[which.max(df_merge$MeanGPP),]
df_merge_filtered<-df_merge%>%filter(MeanGPP<30)
###if statement to get rid of the gpp that are in the beginning and end of the year
df_merge$MeanGPPfiltered <- df_merge$MeanGPP
df_merge <- df_merge %>% 
  mutate(MeanGPPfiltered = ifelse(nullvaluespercentagedf<3,0,MeanGPPfiltered))
df_merge_filtered<-df_merge%>%filter(MeanGPP<30)
df_merge_growing<-df_merge%>%filter(MeanGPPfiltered>0)

## convert all the zeroes to null values
df_merge[df_merge == 0] <- NA


##save the csvFile so that we dont have to run it again
write.csv(df_merge, "SpatialInterannaluGPP.csv")

# Scatter plot by group
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))
colorTP<-c("#65000b","#a50F15","#EF3B2C","#FC9272","#FCBBA1","#fac2e3","#001499","#0a2bff","#9acfe4")
colorPlot<-c("#40271f","#5e4839","#947f70","#c49f68","#bfb3aa","#ebddc7","#235978","#45877f","#aed4ca")
meanGPPgraph<-ggplot(df_merge, aes(x = DOY, y = MeanGPPfiltered , color = Year, alpha = Year)) +
  geom_line(alpha = 0.8, size = 2)+
  
  scale_color_manual(values = mycolors)+
  
  ylab(expression(Mean~GPP~of~rice~'in'~Arkansas~(g~C~m^{"-2"}~day^{"-1"})))+
  xlab("DOY")+ theme_classic(base_size = 25)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits=c(70, 280))

meanGPPgraph
ggsave("meanGPPgraph.png", width = 20, height = 8)

df_merge$Year<-as.character(df_merge$Year)
meanGPPgraph<-ggplot(df_merge, aes(x = DOY, y = MeanGPPfiltered , color = Year, alpha = Year)) +
  geom_line(alpha = 0.75, size = 2)+
  
  scale_color_viridis(discrete = TRUE, option = "rocket", direction = -1) +
  
  ylab(expression(Mean~GPP~of~rice~'in'~Arkansas~(g~C~m^{"-2"}~day^{"-1"})))+
  xlab("Day of the year (DOY)")+ theme_classic(base_size = 25)+
  theme(legend.position = c(0.17, 0.45))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits=c(70, 280))
meanGPPgraph
ggsave("meanGPPgraph.png", width = 20, height = 8)
##max value of each year
mean((df_merge%>%group_by(Year)%>%summarise(maxvalue = (max(MeanGPPfiltered))))$maxvalue)


## Facet wrap by year
ggplot(df_merge, aes(x = DOY, y = MeanGPPfiltered )) +
  geom_line(alpha = 0.8, size = 2)+
  ylab(expression(Mean~GPP~of~rice~'in'~Arkansas~(g~C~m^{"-2"}~day^{"-1"})))+
  xlab("DOY")+ theme_classic(base_size = 25)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits=c(0, 365))+
  facet_grid(~Year)

n <- 1e3

approxData <- data.frame(
  with(df_merge, 
       approx(DOY, MeanGPPfiltered, xout = seq(1, n, by = 10), method = "linear")
  ),
  method = "approx()"
)

library(ggplot2)
library(ggpmisc)
library(lubridate)
ggplot(subset(df_merge_filtered,Year %in% c( "2011"))) + 
  geom_line(aes(DOY, MeanGPP,  colour=Year,size = 1))+
  ylab(expression(Mean~GPP~of~rice~'in'~Arkansas~(g~C~m^{"-2"}~day^{"-1"})))+
  xlab("DOY")+ theme_classic(base_size = 25)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 15), limits=c(0, 365))



##Violin plot
p<-ggplot(df_merge_growing, aes(x=Year, y=MeanGPPfiltered, color=Year)) +
  geom_violin(trim=FALSE)

p<-ggplot(df_merge_growing, aes(x=Year, y=MeanGPPfiltered, fill=Year)) +
  geom_violin(trim=FALSE)
p
p+scale_fill_brewer(palette="Darjeeling1")

df_merge_growing$MeanGPPfiltered <0
## Aggregate mean by columns
# Specify data column
df_merge_filtered_acrossyear<-aggregate(x= df_merge$MeanGPPfiltered,     
                                        
                                        # Specify group indicator
                                        by = list(df_merge$Year),      
                                        
                                        # Specify function (i.e. mean)
                                        FUN = mean)



df_merge_filtered_acrossyearsum<-aggregate(x= df_merge$MeanGPPfiltered,     
                                           
                                           # Specify group indicator
                                           by = list(df_merge$Year),      
                                           
                                           # Specify function (i.e. mean)
                                           FUN = sum)


library(ggpmisc)
library(ggplot2)
library(ggbreak) 
library(patchwork)
library(ggpubr)
df_merge_filtered_acrossyear$Group.1<-as.character(df_merge_filtered_acrossyear$Group.1)
ggplot(df_merge_filtered_acrossyearsum, aes(x =  Group.1 , y =  x)) +
  geom_point(size = 6)+
  ylab(expression(Mean~GPP~of~rice~'in'~Arkansas~(g~C~m^{"-2"}~day^{"-1"})))+
  xlab("Year")+
  
  theme_classic(base_size = 25)


plot(rastlistmasked[[4]], 45, stretch = 'lin')
ncell(rastlistmasked[[4]])
ncell(rastlistmasked[[9]])

1054752-(ncell(rastlistmasked[[4]]))


(ncell(rastlistmasked[[4]]))-1036628
23/18147


(ncell(rastlistmasked[[4]]))- stackedsummarygrowingdfNA[[4]]$nullvalue
plot((((ncell(rastlistmasked[[4]]))- stackedsummarygrowingdfNA[[4]]$nullvalue)/18199 )*100, ,
     ylab="Percentage of Pixels", xlab="Index")

stackedsummarygrowingdfNA

max(stackedsummarygrowingdfNA[[4]]$nullvalue)-min(stackedsummarygrowingdfNA[[4]]$nullvalue)





nullvaluespercentage<-vector("list", 13)
for (i in 1:13) {
  maxval<-ncell(rastlistmasked[[i]])-min(stackedsummarygrowingdfNA[[i]]$nullvalue)
  nullvaluespercentage[[i]] <- ((((ncell(rastlistmasked[[i]]))- (stackedsummarygrowingdfNA[[i]]$nullvalue))/maxval)*100)
}
plot(nullvaluespercentage[[4]])

for (i in 1:13) {
  for (j in 1:length(rastlistdf[[i]]$rastlist)) {
    rastlistdf[[i]]$Date[j]<-((substr(rastlistdf[[i]][j,], 1,10)))
  }
}