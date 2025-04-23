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
The R scripts used for the manuscript "Magnitude, drivers, and patterns of gross primary productivity of rice in Arkansas using a calibrated vegetation photosynthesis model" are located in the following folder:
PVPM_GPP_Arkansas/Scripts/R_Scripts/VPM_Spatial/

The google earth engine script to export EVI, LSWI, temperature and PAR data can be obtained from this folder 
PVPM_GPP_Arkansas/Scripts/GEE_Scripts/VPM/YearWiseVPMStateScale

This repository contains the full workflow for modeling Gross Primary Productivity (GPP) using the Vegetation Photosynthesis Model (VPM) across Arkansas's rice-growing regions. The analysis combines satellite remote sensing, environmental drivers, and in-situ eddy covariance calibration to assess spatial and temporal GPP patterns and their relationship with rice yield.

1. Satellite Data Preparation (Google Earth Engine)

    Years: 2015–2018 (Site-Scale) and 2008–2020 (Spatial)

    Indices: EVI (to derive FPAR) and LSWI (to derive water stress)

    Post-processing: Filter raster images using a 50% rice-pixel threshold.

2. Site Calibration (R Scripts)

    Run SiteScaleAnalysis_DataReadingMerging.R and Function.R

    Calibrate LUEmax and Topt based on EC tower data

        Final site-calibrated values:

            LUEmax = 0.06038462

            Topt = 30.02308 °C

    Validate site model performance against EC-observed GPP

    Compare against biome-default VPM parameters

3. VPM Modeling in Earth Engine

    Use calibrated parameters in the 2015–2018 GEE scripts

    Export final VPM GPP images to CumulativeVPM and VPMDriver

4. Model Evaluation (R)

    Scripts: ModeledPVPMVPM_satelliteProcessing.R, VPMYieldGPP_CountyMaps_Yearwise.R

    Analyze:

        GPP vs Yield (correlation, Mann-Kendall trend test)

        Mixed pixel effect

        County-level boxplots and spatial maps

    Yield data from: Harvested_ACre_Rice_Arkansas.csv

5. Driver Analysis

    Script: Drivers_EVI_T_Precipitation_LSWI.R

    Input Variables: EVI, LSWI, Temp, PAR, Cropping Frequency, PD

    Outputs:

        Multi-year trends

        Driver vs GPP plots

        Summary tables

6. Raster Handling and Export

    Project rasters (EVI, LSWI, Temp) to match VPM spatial resolution

    Script: AllDriverinOneScript-Projecting500.R

    Outputs stored for downstream driver correlation analysis

7. Advanced Analysis

    Year-by-year regional raster processing (VPMMeanRasterImageAnalysis2008_2020.R)

    Interannual trends (VPM_InterannualGraph.R)

    Distribution check between filtered vs full GPP (Distributioncheck50percentricepixelversusfullricepixel)

