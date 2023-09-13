# MENA POV

# Master R Script

# Organization
# * Filepaths: To make code work, change paths in "Root file path"
# * Parameters: Parameters used throughout scripts
# * Libraries: Load R packages and user defined functions

# FILEPATHS ====================================================================
#### Root
github_dir  <- "C:/Users/wb569257/OneDrive - WBG/Documents/GitHub"
#local_dir  <- "//MENAPOV/menapov"
local_dir   <- "M:"



#### Data
lbn_file_path <- file.path(local_dir,"LBN","GEO")
lbn_onedrive_dir <- file.path("C:/Users/wb569257/OneDrive - WBG/lbn_geospatial_analysis")

yem_file_path <- file.path(local_dir,"YEM","GEO")

jor_file_path <- file.path(local_dir,"JOR","GEO")
jor_onedrive_dir <- file.path("C:/Users/wb569257/OneDrive - WBG/Jordan")

mor_file_path <- file.path(local_dir, "MOR", "GEO")
mor_onedrive_dir <- file.path("C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/Projects/mor_earthquake_analysis")

mena_file_path <- file.path(local_dir,"MENA","GEO")

#### Overleaf
tables_dir  <- "PATH"
figures_dir <- "PATH"


# PARAMETERS ===================================================================
UTM_JOR <- '+init=epsg:3394'
UTM_LBN <- '+init=epsg:32637'
UTM_YEM <- '+init=epsg:5836'
N_CORES <- 2

# LIBRARIES ====================================================================
load_package_safely <- function(package_name){
  if (!requireNamespace(package_name, quietly = TRUE)) {
    message(package_name, " is not available.")
  } else {
    suppressWarnings(suppressMessages(library(package_name, character.only = TRUE)))
  }
}

packages <- c("sp", "purrr", "raster","readr","dplyr","parallel","pbmcapply",
               "rgdal","rgeos","geosphere","sf","broom","gdistance","data.table",
               "ggpubr","reshape","doBy","readstata13","haven","ggmap","gtools",
               "readxl","ggrepel", "plm","stargazer","xml2","mapsapi","leaflet",
               "XML", "tmap","lubridate", "hrbrthemes", "tidyr","stringr","lfe",
               "devtools", "usethis","viridisLite","viridis","jtools","osrm",
               "formattable","geojsonio","DataCombine","nngeo","qwraps2","pastecs",
               "htmlwidgets","car","visreg","fuzzyjoin","leaflet.extras",
               "maptools","mapview","lmtest","multiwayvcov","ggplot2","sjlabelled",
               "dtplyr","expss","h3jsr", "spdplyr", "janitor",
               "tidygraph", "sparklyr", "styler","remotes",
               "h3","arrow","fs","jsonlite", "geodata", "osrm", 
               "stplanr", "exactextractr", "rjson", "patchwork", "ggmap",
               "progress",'haven','dplyr', 'reshape2', 'tidyverse', 
               'pracma','lubridate', 'scales', 'stringr', 'tidymodels', 
               'flextable', 'rsample', 'hdm', 'pROC', 'glmnet',
               'tibble',"xlsx","GGally","xtable","pls","ggcorrplot","caret","sandwich", "KRLS",
               "classInt", "RColorBrewer", "splines","ggeffects","ggsn", "ggspatial")

for (pkg in packages) {
  load_package_safely(pkg)
}

