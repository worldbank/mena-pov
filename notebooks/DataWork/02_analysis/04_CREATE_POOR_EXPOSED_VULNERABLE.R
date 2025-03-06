# ================================================================
# Script Name: 04_CREATE_EXPOSED_POOR_VULNERABLE
# Purpose: Create maps of the exposed poor for Egypt
# Input Dataset:grid_10km_poor_215_685.Rds,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication, "grid_10km_poor_215_685.Rds"))
grid_sf_vul <- readRDS(file.path(final_replication, "grid_vul_10km_final.Rds"))


names(grid_sf_vul)
# Merge poor grid and vul grid --------------------------------------------
grid_sf_vul_sub <- grid_sf_vul %>%
  dplyr::select(c(grid_id,vul_share)) %>%
  st_drop_geometry()




gsap_grid_merged_vul <- grid_poor %>%
  left_join(grid_sf_vul_sub, by = c("grid_id"))



names(gsap_grid_merged_vul)

# Poor, vulnerable and exposed --------------------------------------------
# Note: regional threshold of vulnerability is 0.42 -- looking at the density plot of vulnerability share
# and choosing the peak
poor_vul_exposed <- gsap_grid_merged_vul %>%
  dplyr::mutate(exposed = ifelse(upper_bound >=32|grid_pop_flood_expected>0|
                                   annual_mean2_pm25>=15|drought_freq>20,1,0),
                poor215_vul042_exposed = ifelse(exposed>0 & vul_share>0.42 & poor215_rwi_dup>0,1,0),
                poor365_vul042_exposed = ifelse(exposed>0 & vul_share>0.42 & poor365_rwi_dup>0,1,0),
                poor685_vul042_exposed = ifelse(exposed>0 & vul_share>0.42 & poor365_rwi_dup>0,1,0))








# Export ------------------------------------------------------------------

saveRDS(poor_vul_exposed,file.path(final_replication,"grid_10km_poor_vul_exposed.Rds"))


