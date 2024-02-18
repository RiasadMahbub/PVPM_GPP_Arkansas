### This script is to find the mean of GPP of all the images in an year

## Date: 10_9_2020 (m_d_year)

## Load the libraries
library(raster)
library(ggplot2)
library(tidyverse)
library(sf)
library(terra)
library(lubridate)


### Read the files
#first import all files in a single folder as a list 
## YEAR: 2020
rastlist <- list.files(path = "C:/Users/rbmahbub/Documents/Data/GeospatialData/PVPMclippedByShapefile/2020", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

stacked <-stack(rastlist)
## The general summary function calculates the mean of GPP ignoring NA values
## WHich can be attributed as the growing season mean where NA values signify there  is no growing season for that pixel
stackedsummarygrowing<-summary(getValues(stacked))
stackedsummarygrowingdf<-as.data.frame(stackedsummarygrowing)
stackedsummarygrowingdf$meanrow<- NA
for (i in 1:length(stackedsummarygrowing)) {
  stackedsummarygrowingdf$Meanrow[i]<-substr(stackedsummarygrowingdf[i,3], 1,4)
} 

## select the rows with mean
stackedsummarygrowingdfmean<-stackedsummarygrowingdf%>% filter(Meanrow == "Mean")

## Get the numeric Mean values
for (i in 1:length(stackedsummarygrowingdfmean$Var1)) {
  stackedsummarygrowingdfmean$MeanGPP[i]<-as.numeric(substr(stackedsummarygrowingdfmean[i,3], 9,20))
} 



### Add the date
rastlistdf<-as.data.frame(rastlist)
for (i in 1:length(rastlistdf$rastlist)) {
  rastlistdf$Date[i]<-((substr(rastlistdf[i,], 4,13)))
} 
rastlistdf$Date<-ymd(rastlistdf$Date)

for (i in 1:length(stackedsummarygrowingdfmean$Var1)) {
  stackedsummarygrowingdfmean$Date[i]<-rastlistdf$Date[i]
} 

View(stackedsummarygrowingdfmean)
stackedsummarygrowingdfmean$Date<-rastlistdf$Date


plot(stackedsummarygrowingdfmean$Date, stackedsummarygrowingdfmean$MeanGPP)
stackedsummarygrowingdfmean$MeanGPP<-as.numeric(stackedsummarygrowingdfmean$MeanGPP)
ggplot(stackedsummarygrowingdfmean, aes(x=Date, y=MeanGPP)) +
  geom_point()




####YEAR####
### the whole year where it caculates the NA as 0
stacked[is.na(stacked)] <- 0
stackedsummaryyear<-summary(getValues(stacked))
stackedsummaryyeardf<-as.data.frame(stackedsummaryyear)
stackedsummaryyeardf$meanrow<- NA
for (i in 1:length(stackedsummaryyear)) {
  stackedsummaryyeardf$Meanrow[i]<-substr(stackedsummaryyeardf[i,3], 1,4)
} 

## select the rows with mean
stackedsummaryyeardfmean<-stackedsummaryyeardf%>% filter(Meanrow == "Mean")

## Get the numeric Mean values
for (i in 1:length(stackedsummaryyeardfmean$Var1)) {
  stackedsummaryyeardfmean$MeanGPP[i]<-as.numeric(substr(stackedsummaryyeardfmean[i,3], 9,20))
} 



### Add the date
rastlistdf<-as.data.frame(rastlist)
for (i in 1:length(rastlistdf$rastlist)) {
  rastlistdf$Date[i]<-((substr(rastlistdf[i,], 4,13)))
} 
rastlistdf$Date<-ymd(rastlistdf$Date)

for (i in 1:length(stackedsummaryyeardfmean$Var1)) {
  stackedsummaryyeardfmean$Date[i]<-rastlistdf$Date[i]
} 

View(stackedsummaryyeardfmean)
stackedsummaryyeardfmean$Date<-rastlistdf$Date


plot(stackedsummaryyeardfmean$Date, stackedsummaryyeardfmean$MeanGPP)
stackedsummaryyeardfmean$MeanGPP<-as.numeric(stackedsummaryyeardfmean$MeanGPP)
ggplot(stackedsummaryyeardfmean, aes(x=Date, y=MeanGPP)) +
  geom_point()

stacked

