# MENA POV

# Master R Script

# Organization
# * Filepaths: To make code work, change paths in "Root file path"
# * Parameters: Parameters used throughout scripts
# * Libraries: Load R packages and user defined functions

# FILEPATHS ====================================================================
#### Root
if(Sys.info()[["user"]] == "chitrab") {
  github_dir  <- "C:/WBG/Github/mena-pov"
  local_dir <- ""
}



#### Data


#### Overleaf
tables_dir  <- "PATH"
figures_dir <- "PATH"


# PARAMETERS ===================================================================
UTM_JOR <- '+init=epsg:3394'
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
               "stplanr", "exactextractr")

# install.packages("devtools")
# devtools::install_github("ramarty/blackmarbler")
library(blackmarbler)

## Importing functions
source("https://raw.githubusercontent.com/ramarty/fast-functions/master/R/functions_in_chunks.R")
source("https://raw.githubusercontent.com/ramarty/rgeos_chunks/master/R/rgeos_chunks.R")

