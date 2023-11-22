---
title: "CodingBluePrint-PVPMWork"
author: "Riasad Bin Mahbub"
date: '2023-11-21'
output:
  pdf_document: default
  html_document: default
---
```{r}

```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown
The first step involves site calibration. For site calibration we will require 

1. Site Flux data that are derived from 16 site years data
2. MODIS derived EVI_SG to calculate FPAR and LSWI to calculate water scalar. 
Derivation of LUEmax require PAR be multiplied with FPAR values
So the dataframes need to joined and the field names have to be same
FPAR values have to downscaled to 30 min resolution
NEEmax values have to derived from the negative perspective
3. At the site calibration we will fit the model to find the right parameters for 



This the blueprint of the code for the PVPM Model. The following lines will give the workflow steps and the relevant code to implement it. 
The whole code runs on a for loop and the following steps are followed


1. Read the column headers **as.character** 

2. Read the data skipping the first two rows **read_excel** 

3. Define the filepath 

4. sheet_cols, why? (page number in the excel sheet)

5. data_frames list contain the list of the dataframes and each list is based on the name of the dataframe

5. Make a list of of DOY where the MODIS DOY matches with the ec DOY

6. Create empty list of dataframes (data_frames, points, DOYlist, LUEmax, dflist, modellist, stderror, rsquared, rss, rmse, avgsdevsub, avgsdevadd, lightresponsegs)

7. The datetime column is formatted using **gsub, strptime, as.Date, filter, yday** so that it can be joined later with the MODIS derived satellite data which will be used to calibrate the model.


```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
