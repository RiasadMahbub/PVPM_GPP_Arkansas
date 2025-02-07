# Script flow: This is how the scripts will be run
1. Run google earth engine scripts 2015-2018 to obtain the EVI and LSWI values. The EVI will be used to derive FPAR, and LSWI will be used to derive Ws. in 
Google drive folder:Satellite-SiteCalibrationCSV [all the excel sheets will be exported here]
Directory of the satellite data in local directory: "C:/Users/rbmahbub/Box/Research/Data/VPM/SatelliteDataForSiteAnalysis"
Bring 

2. Run the SiteScaleAnalysis_DataReadingMerging.R and Function.R script to find the calibrated values and use them in the google earth engine code. In the beginning you will have these values. 
Directory of the site scale EC data: "C:/Users/rbmahbub/Box/Courses/Spring2022/BENG5963 ECData_otherRiceSites/Rice Data_Research Group_05-23-2022.xlsx"
 Function.R script:
 1. Four functions to calculate Ts, GPP, calculate_metrics, and calculate_metrics_validation
 2. Run the K-fold cross validation 
 3. Run all sites in single run and plot them. 
 4. Plot the of the cumulative GPP, 
 5. Show the cumulative GPP using site-calibrated values are better than GPP using 0.05 and 30 deg Topt
    
<!-- Insitu calibrated LUEmax value: 0.06512821
Insitu calibrated Topt value: 31.79615  --> This mistake happened due to the misplacement of OF6 and OF5 in gee

Insitu calibrated LUEmax value: 0.061
Insitu calibrated Topt value: 30.671 



3. Scripts run in google earth engine data: Export the images in relevant folders, The satellite VPM calculation is done in the script but if you want. Even though we run the script using the final topt and luemax. But the function script in R tells us the right parameter values. 
#Run and export scripts from 2015-2018 for site scale data export
Data required for google earth engine:
4. Rice shapefile after rice fields filtering out: Remembering how they were filtered out
Name of the directory: 
5. Do not copy paste the whole script for 2015 -2018 (Copy paste the code before the part of site-calibrated information for the scripts 2015-2018), because they have the site csv file export details.
Also from copying 2020 you will copy paste the import geometry part too. The region of geometry comes from the 2020 year

The latest run is based on luemax 0.05 and 30 deg celsius. Run the scripts 
Sitecalibration factors: LUEmax and Topt 
Site Scale analysis
// calibrated luemax = 0.732671 (0.061*12.011 =0.732671)
// calibrated topt = 30.671  deg celsius
 var luemax = 0.732671
 var topt = 30.671

 biome specific luemax = 0.60055 (0.05*12.011)
 biome specific topt = 30


Exported Directory VPM images in google drive: CumulativeVPM
Exported Images of other features: VPMDriver
Cumulative VPM directory: C:/Users/rbmahbub/Documents/Data/GeospatialData/CumulativeVPM/CumulativeVPM/
After exporting the images the Images are clipped based on 50% threshold pixel values using Distributioncheck50percentricepixelversusfullricepixel script
So the directory folder of the raster images are changed 


##Modeling script:
Varibles = sitesatellitemergedDATA (SiteScaleAnalysis_DataReadingMerging.R script)
1. Plot scatterplot
2. Plot Timeseries performance of VPM spatial and site
3. Plot residual plots
4. VPM performance based on seasonal scale

4. Project the rasters of EVI, LSWI and temp based on the projection of VPM


Export the csv files of the relation between GPP and LSWI, PD, Temp, PAR.
projected_cumulativeEVIdflist will used later 



gppricecounty dataframe
direcotory: C:/Users/rbmahbub/Documents/Data/YieldData/Harvested_ACre_Rice_Arkansas.csv
stateyield-conversion factor - aggregate - yearwise correlation - mann kendall test of the yield - gppcountyyieldplot- increase rate table 
Area harvested for each county - box plot of the county GPP relationship - GPP yield county map - State yield timeseries graph and store the mankendall test again - top five counties and top lowest counties

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
    Finding the growing season and non-growing season GPP
    satellitecombined_data dataframe has the information of satellite information
    sitesatellitemergedDATAGPPng

2. Function.R: [Dataframe sitesatellitemergedDATA]
    Topt <- mean(training_df$Topt)
    LUEmax_0_06512<-mean(training_df$LUEmax)
    VPM site: sitesatellitemergedDATA$GPP_0_06512_31_79
    VPM spatial: gpp
3. ModeledPVPMVPM_satelliteProcessing.R dataframe sitesatellitemergedDATA
    Plot the scatter plot
    plot the GPP~Yield relationship
    Yearly performance
    Seasonal Performance
    Mixed Pixel effect: 
        The raster image is from 2012 which has all the image can calculates the percentage with weight function
    Yield data were obtained from Ben's mail and file was created 16siteyearYield. The common column was siteyear.x



4. AllDriverinOneScript-Projecting500.R:Spatial correlation here. 
    The exported images from different directories are read here. 
    Setting directory, stacking raster, finding the mean
    Files need: Crop frequency, EVI, 
    
    Define the directory path containing the raster files
    List all .tif files in the specified directory
    Read the shapefile for a region of interest (ME)

5. Drivers_EVI_T_Precipitation_LSWI.R
    1. read the directories of each EVI, LSWI, PAR, Temperature, PD, Cropping frequency, and vpm images
    2. list the files
    3. create a vector list and store the information in the vector list
    4. 
        Plot EVI, LSWI, Temp, PD, PAR vs GPP across multiple years. 
        Figure export across different years: 
        Tables export of all the years:
        Driver figure of all the year: 


    DataFrames Creation: Initialize empty DataFrames to store merged data.
    Adding Year Information: Add a year column to each data frame in the projected lists.
    Column Name Modification: Remove trailing year digits from the column names to ensure proper binding.
        # Remove trailing four digits from column names
    names(projected_cumulativeEVIdflistyear[[1]]) <- sub("\\d{4}$", "", names(projected_cumulativeEVIdflistyear[[1]]))
    Row Binding: Bind rows from each year's data into the initialized DataFrames.
    Column Combination: Combine the columns into a single DataFrame.
    File Operations: Save the combined DataFrame to a CSV file and read it back.
    Data Cleaning: Remove EVI values greater than 1 and omit NA values.
    Plotting: Plot the distribution of the coverage fraction using ggplot2 with real numbers on the y-axis.

6. VPMMeanRasterImageAnalysis2008_2020.R
    Exports the filtered raster images at the end
    Regionbased analysis

7. VPMYieldGPP_CountyMaps_Yearwise.R
    Read the raster files
    Yield gm conversion by multiplying ti by 0.112085
    

    Plot the relationship between raster and yield

    Do the automate process for the other directories of rasters


8. VPM_InterannualGraph.R
9. Distributioncheck50percentricepixelversusfullricepixel
    Checks the distribution of GPP in filtered raster of complete GPP and fractioned GPP
    Load the library
    read the raster files and stack them read the shapefile
    Convert the raster files to rast objects
    convert the raster files to dataframes and multiply it by 8

    Read the raster files with 50% roi

    cumulative_raster_df_coverage= 50% coverage
    cumulative_raster_df_vpm = all pixels

    Add source and rename columns and combine them 

    From the 2012 image find the coverage percentage of the pixels on the 




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
8. Figure 8: AllDriverinOneScript-Projecting500.R
Supplementary figure:
Figure S1: VPM performance across different Ɛ0 and Topt values. Function.R (script)
Figure S2: Residual plots of the models VPMsite (A) and VPMspatial (B).  ModeledPVPMVPM_satelliteProcessing.R (script)
Figure S3: Scatterplot explaining the relationship between EVImean (A), LSWImean (B), PARmean (C), and Tmean (D) of each rice growing pixels for 13 years. 
Figure S4: Yearwise trend of state-scale rice yield of Arkansas. VPMYieldGPP_CountyMaps_Yearwise.R (script)
Figure S5: Relationship of GPPcum and yield across different area values. VPMYieldGPP_CountyMaps_Yearwise.R (script)


Supplementary table 1:Supplementary table 1: Year-Wise Mean yield, change in yield, percentage of change and correlation (R, R2) of the relationship between mean yield against GPPcum
VPMYieldGPP_CountyMaps_Yearwise.R (script)


Supplementary table 2. Relationship between mean annual EVI and annual cumulative GPP across 13 years Drivers_EVI_T_Precipitation_LSWI.R (script)
Supplementary table 3. Relationship between annual mean LSWI and annual mean cumulative GPP across 13 years Drivers_EVI_T_Precipitation_LSWI.R (script)
Supplementary table 4. Relationship between annual mean air temperature and annual mean cumulative GPP across 13 years Drivers_EVI_T_Precipitation_LSWI.R (script)
Supplementary table 5. Relationship between annual mean PAR and annual mean cumulative GPP across 13 years Drivers_EVI_T_Precipitation_LSWI.R (script)
growth_rate.csv


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


### Checking how much pixels are rice or not
#MODIS IMAGES: C:\Users\rbmahbub\Documents\Data\GeospatialData\CumulativeVPM\CumulativeVPM
#Rice shapefile: 



This was checked by opening 2012 image on GEE and QGIS, we matched the cumulative GPP to find the right pixel and found the coordinates of those pixels
Points needs to be to find the pixel that represents the field
USOF1 = Point (-90.05202, 35.73629)
USOF3 = Point 

var  OF5  =ee.Geometry.Point([-90.0403,35.7333])
var  OF6  =ee.Geometry.Point([-90.0406,35.7297])