##Nighttime Lights Map

library(purrr)
library(furrr)
library(stringr)
library(rhdf5)
library(raster)
library(dplyr)
library(sf)
library(lubridate)
library(exactextractr)

source("https://raw.githubusercontent.com/ramarty/blackmarbler/main/R/blackmarbler.R")

# Load Data ---------------------------------------------------------------
viirs_2012_2022 <- raster(file.path(lbn_file_path,
                                    "Nighttime_Lights",
                                    "raw",
                                    "annual",
                                    "lebanon_viirs_corrected_annual_median_2014_2021_avg_rad.tif"))
                                    
                                                          


                                