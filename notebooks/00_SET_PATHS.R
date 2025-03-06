# ================================================================
# Script Name: set_path.R
# Purpose: Creates the locals, globals and installs packages
# Input Dataset: NA
# Output Dataset: NA
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

# Clean environment
rm(list = ls())




## Package loading
# Note: Needs IT to install Rtools40 on computer first; then, install:
# renv::install("github::rspatial/terra")





# List of required packages
packages <- c(
  "dplyr", "data.table", "zoo", "foreign", "haven", "sf", "rgeos", "tidyverse", 
  "paletteer", "raster", "ggplot2", "exactextractr", "readxl", "colorspace", 
  "ncdf4", "foreach", "doParallel", "readr", "sp", "scales", "gridExtra", 
  "geojsonio", "lwgeom", "classInt", "RColorBrewer", "purrr", "parallel", 
  "terra", "openxlsx", "leaflet", "blackmarbler", "geodata", "lubridate", 
  "tmap", "osrm", "janitor", "readstata13", "ggnewscale", "survey", "hrbrthemes", 
  "countrycode", "wbstats", "rpart", "rpart.plot", "caret", "ipred", "rsample", 
  "rattle", "fastDummies", "car", "stargazer", "plm", "nlme", "nortest", "MASS", 
  "codebook", "expss", "Polychrome", "ggthemes", "ggh4x", "gt", "tidytext", 
  "cowplot", "ggpubr", "patchwork", "fuzzyjoin", "reticulate"
)

# Install and load packages
new_packs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packs)) install.packages(new_packs)
lapply(packages, require, character.only = TRUE)

# Ensure specific version of ggplot2
remotes::install_version("ggplot2", version = "3.4.4", repos = "http://cran.us.r-project.org")






## Directory setting [Need to change directory settings to setup the locals]
if (Sys.getenv('USERNAME') == "wb569257") {
  mena_path <- "M:/MENA"
  global_file_path <- "M:/GEOGlobal"
  mena_file_path <- "M:/MENA/GEO"
  onedrive_dir <- "C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/MENAPOV GEO/Projects/vulnerability"
  
  ### RAW DATASETS
  raw_replication <- file.path(onedrive_dir, "REPLICATION_RAW_DATA")
  
  
  # PM2.5
  mena_file_path_air_pol <- file.path(raw_replication,"PM2_5")
  
  #FLOOD
  mena_file_path_flood <- file.path(raw_replication,"FLOOD")
  
  
  #HEAT STRESS
  mena_file_path_heat <-  file.path(raw_replication,"HEAT_STRESS")
  
  
  #RWI
  mena_file_path_rwi <- file.path(raw_replication, "RWI")
  
  
  #GMD Database
  data_hh_survey <- file.path(file.path(raw_replication, "HH_SURVEYS","raw"))
  
  
  ### FINAL DATASET
  final_replication <- file.path(onedrive_dir, "REPLICATION_FINAL_DATA")
  
  ### OUTPUT
  graphs <- file.path(onedrive_dir, "output", "Figures", "graphs")
  tables <- file.path(onedrive_dir, "output", "Figures", "tables")
  maps <- file.path(onedrive_dir, "output", "Figures", "maps", "MENA")
}
