# Merge DMSP, VIIRS and BlackMarble at the Cadester Level


# Load Data ---------------------------------------------------------------
#Black Marble
setwd("C:/Users/wb569257/OneDrive - WBG/lbn_geospatial_analysis/DataLab/cadaster/individual-monthly")
rds_list <- list.files(path = "C:/Users/wb569257/OneDrive - WBG/lbn_geospatial_analysis/DataLab/cadaster/individual-monthly", pattern='.Rds$', 
                       all.files=TRUE, full.names=FALSE)
#import all raster files in folder using lapply
allrds <- lapply(rds_list, readRDS)
#append the dataset
bm_ntl <- do.call(rbind,allrds)



#VIIRS
viirs_ntl <- readRDS("M:/LBN/GEO/Nighttime_Lights/final/cas_pop_ntl_pop2000_2020.Rds")


#DMSP
dmsp_ntl <- readRDS("M:/LBN/GEO/Nighttime_Lights/final/ntl_dmsp_2011_cadester.Rds")

#rename variables before merging
dmsp_ntl <- dmsp_ntl %>%
  rename(dmsp_ntl_mean = ntl_mean,
         dmsp_ntl_median = ntl_median)


# Merge NTL dataset -------------------------------------------------------
ACS_CODE_1 <- unique(viirs_ntl$ACS_CODE_1)

years <- 2000:2023

months <- 1:12


# Creating a data frame using crossing from tidyr to create all combinations
dataset <- tidyr::crossing(ACS_CODE_1 = ACS_CODE_1, year = years, month = months)


#Merge
dataset <- left_join(dataset,viirs_ntl, by = c("ACS_CODE_1", "year","month"))
dataset <- left_join(dataset,dmsp_ntl, by = c("ACS_CODE_1", "year","month"))
