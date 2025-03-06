# ================================================================
# Script Name: 05_ESTIMATE_TOTAL_POOR_EXPOSED_MENA
# Purpose: Estimate the total number of poor in the region, and those that are exposed
# Input Dataset: 
# Output Dataset: 
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication,"grid_10km_poor_215.Rds"))
grid_people <- readRDS(file.path(final_replication,"grid_10km_people.Rds"))

                       

# Total Poor in MENA ------------------------------------------------------

tot_poor <- grid_poor %>%
  dplyr::summarise(tot_poor_215 = sum(poor215_count_rwi_dup, na.rm = T))

names(grid_poor)
                       
                      