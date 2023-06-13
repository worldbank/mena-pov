# MENA POV

# Master R Script

# Organization
# * Filepaths: To make code work, change paths in "Root file path"
# * Parameters: Parameters used throughout scripts
# * Libraries: Load R packages and user defined functions

# FILEPATHS ====================================================================
#### Root
github_dir  <- "C:/Users/wb569257/OneDrive - WBG/Documents/GitHub"
local_dir  <- "//MENAPOV/menapov"





#### Data
lbn_file_path <- file.path(local_dir,"LBN","GEO")
yem_file_path <- file.path(local_dir,"YEM","GEO")
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
if(F){
  library(pacman)
  remotes::install_github("crazycapivara/h3-r")
}

pacman::p_load("sp", "purrr", "raster","readr","dplyr","parallel","pbmcapply",
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
               'tibble',"xlsx")

# ##installing velox
# install_github("hunzikp/velox")
# library(velox)
# 
# ##installing h3 for R
# remotes::install_github("crazycapivara/h3-r")

# install.packages("devtools")
#devtools::install_github("ramarty/blackmarbler")
#library(blackmarbler)



