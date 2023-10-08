# MERGE


# Load Data ---------------------------------------------------------------
pop <- readRDS(file.path(mena_file_path,
                          "Population",
                          "final",
                          "grid_pop_10km.Rds"))
  
rwi <- readRDS(file.path(mena_file_path,
                 "RWI",
                 "final",
                 "grid_rwi_10km.Rds"))
  
heat_stress <- readRDS(file.path(mena_file_path,
                                 "Hazards",
                                 "TEMP",
                                 "final",
                                 "grid_wbgt_10km.Rds"))




# Combine and clean the vars ----------------------------------------------

merged_grid <- cbind(pop,rwi,heat_stress)

merged_grid <- merged_grid %>%
  select(OBJECTID,ADM0_CODE,ADM1_CODE,ADM2_CODE,ISO_A2,WB_ADM1_CO,
         WB_ADM0_CO,WB_ADM0_NA,WB_ADM1_NA,WB_ADM2_CO,WB_ADM2_NA,
         ID_ADM,grid_id,pop_count,pop_density,rwi_dza,rwi_dji,rwi_egy,
         rwi_jor,rwi_lbn,rwi_lby,rwi_mar,rwi_tun,rwi_all,temp_rp_100y,
         temp_rp_20y,temp_rp_5y,upper_bound)







# Export ------------------------------------------------------------------
saveRDS(merged_grid,file.path(mena_file_path,"Allsources","grid_10km_all.Rds"))

st_write(merged_grid, file.path(mena_file_path, "Allsources","grid_10km_all.shp"),append = F)



