### Script flow: This is how the scripts will be run

1. Run the SiteScaleAnalysis_DataReadingMerging.R and Function.R script to find the calibrated values and use them in the google earth engine code. 
1. Scripts run in google earth engine data: Export the images in relevant folders, The satellite VPM calculation is done in the script but if you want. Even though we run the script using the final topt and luemax. But the function script in R tells us the right parameter values. 
#Run and export scripts from 2015-2018 for site scale data export

Data required for google earth engine:
1. Rice shapefile after rice fields filtering out: Remembering how they were filtered out
Name of the directory: 
2. Rice sh

### Do not copy paste the whole script for 2015 -2018, because they have the site csv file export details.
Copy paste the code before the part of site-calibrated information. 
Google drive folder:Satellite-SiteCalibrationCSV 

The region of geometry comes from the 2020 year
Sitecalibration factors: LUEmax and Topt 
Site Scale analysis
// calibrated luemax = 0.72 (0.06*12.011 =0.072)
// calibrated topt = 32

## Yield data and area data were derived from the USDA NASS stats
Directory: 


## Run and export the scripts from 2008-2020 for spatial data export
The data are exported as the 8-day mean cumulative so each script is mulitplying it by 8-day we might remove it later so keep an eye on this.
Desktop Directory: 
In R, forward slashes (/) are preferred over backslashes "(\)" for representing file paths. This convention ensures consistency across different operating systems, as forward slashes work universally. While modern versions of R for Windows accept both forward slashes and backslashes (\), using forward slashes is a best practice.

Cumulative VPM directory: C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/

Scripts and R projects:"C:\Users\rbmahbub\Documents\RProjects\VPM_Spatial"
Figure Exported: "C:\Users\rbmahbub\Documents\RProjects\VPM_Spatial\Figure"
and
Figure Exported R version: "C:/Users/rbmahbub/Documents/RProjects/VPM_Spatial/Figure/"
1. SiteScaleAnalysis_DataReadingMerging.R [Dataframe sitesatellitemergedDATA]
2. Function.R: [Dataframe sitesatellitemergedDATA]
3. ModeledPVPMVPM_satelliteProcessing.R dataframe sitesatellitemergedDATA
4. AllDriverinOneScript-Projecting500.R:Spatial correlation here. The exported images from different directories are read here. 
    Setting directory, stacking raster, finding the mean
    Files need: Crop frequency, EVI, 
5. Drivers_EVI_T_Precipitation_LSWI.R
6. VPMMeanRasterImageAnalysis2008_2020.R
7. VPMYieldGPP_CountyMaps_Yearwise.R
8. VPM_InterannualGraph.R


#Graphs in the figure:
1. QGIS map
Files directory:
Saved Filed directory:
Functions used:
Tutorial used:

2. Figure 2: Temporal pattern: ModeledPVPMVPM_satelliteProcessing.R 
Calibration analysis: Function.R (script)
3. Figure 3: Scatterplot comparison: ModeledPVPMVPM_satelliteProcessing.R (script)
4. Figure 4: Spatial plot of the GPP: 95 percentile data has been used here: VPMMeanRasterImageAnalysis2008_2020.R (script)
5. Figure 5: Figure 5: Inter-seasonal variability of mean cumulative GPP: VPM_InterannualGraph.R (script)
6. Figure 6: County-scale Relationship between GPP and yield: VPMYieldGPP_CountyMaps_Yearwise.R (script)
7. Figure 7: 
8. Figure 8: 
Supplementary figure:
Figure S1: VPM performance across different Ɛ0 and Topt values. Function.R (script)
Figure S2: Residual plots of the models VPMsite (A) and VPMspatial (B). 
Figure S3: Scatterplot explaining the relationship between EVImean (A), LSWImean (B), PARmean (C), and Tmean (D) of each rice growing pixels for 13 years. 
Figure S4: Yearwise trend of state-scale rice yield of Arkansas.
Figure S5: Relationship of GPPcum and yield across different area values.  

Supplementary table 1:Supplementary table 1: Year-Wise Mean yield, change in yield, percentage of change and correlation (R, R2) of the relationship between mean yield against GPPcum
Supplementary table 2. Relationship between mean annual EVI and annual cumulative GPP across 13 years
Supplementary table 3. Relationship between annual mean LSWI and annual mean cumulative GPP across 13 years
Supplementary table 4. Relationship between annual mean air temperature and annual mean cumulative GPP across 13 years
Supplementary table 5. Relationship between annual mean PAR and annual mean cumulative GPP across 13 years



This code snippet sets the working directory to a specified path containing raster data files. 
Then, it lists all files with the ".tif" extension in the specified directory. 
Finally, it reads each raster file into R as a list using lapply.


### text for the paper 
VPM_InterannualGraph.R (script) : has the minimum values of GPP For the the years and the max values of GPP for the years
Ts and Tmean for the year where Arkansas has faced drought. 


### Cross validation design
16 site-years n = 16
Function.R script
dataframe required: sitesatellitemergedDATA
 

folds what are we predicting?
We are trying to find the value of LUEmax and Topt the RMSE, MSE, bias are the lowest?

We do not know the true LUEmax Topt for the whole rice growing region. However, we can look into the simulated data if the RMSE, MSE and bias is reduced in the test too. 

To find the ideal site calibrated Ɛgand Topt values we did site calibration of Ɛgand Topt values using 16 site-year data. The site calibrated Ɛgand Topt values were derived when the error rate of GPP prediction is the lowest based on RMSE, MAE, and bias in the VPM. We use different combinations of Ɛgvalues (0.03-0.08 mol CO2 mol‑1 PPFD) and Topt values (28-35 ºC). To understand the level of overfitting, we ran k-fold cross validation and checked the Ɛgand Topt values in each run of training and testing set. The site years were grouped in 8 folds consisting of two site years in the testing set and 14 site years in training set. This configuration of 14 site years in the training set and 2 site years in the testing set provided 120 iterations of the k-fold validation. From each run we find the Ɛgand Topt values that gives the lowest error of GPP prediction. The mean of the training and testing sets of the 120-ensemble run of Ɛgwere 0.062± 0.0036 mol CO2 mol‑1 PPFD and 0.065± 0.0085 mol CO2 mol‑1 PPFD respectively. The mean of the training and testing sets of the 120-ensemble run of Topt were 29.61 ± 2.62 ºCand 31.98± 2.69 ºCrespectively. Since there has been not much difference of the Ɛgand Topt values in training and testing set, we take the mean of the ensemble run of the training set as the site-calibrated Ɛgand Topt values for the model which are 0.062 mol CO2 mol‑1 PPFD respectively and 29.61 ºC.  


