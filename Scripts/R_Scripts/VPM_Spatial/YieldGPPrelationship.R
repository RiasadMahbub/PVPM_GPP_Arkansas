#### Spatial map of yield

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




arkansasshp <- st_read(
  "C:/Users/rbmahbub/Downloads/tl_2016_05_cousub/tl_2016_05_cousub.shp")

plot(arkansasshp)
