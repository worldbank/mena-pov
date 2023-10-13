options(scipen=999)


# Load Data ---------------------------------------------------------------
grid_10km <- readRDS(file.path(mena_file_path,"Allsources","grid_10km_all.Rds"))


# Import crosswalk prepared by Sandra in ArcGIS
rwi_crosswalk <- st_read(file.path(mena_file_path,"RWICrosswalk", "grid_10km_all_gsap_union.shp")) %>% 
  filter(region == "MNA" & grid_id !=0) %>%
  select(grid_id,id,surveyd,region,code,g_cd2_n)




# Add the poverty rate data from the excel sheet, since the shapefiles did not include it
gsap_excel <- read_excel(file.path(local_dir,"MENA","Poverty","global-subnational-poverty-gsap-2019-data.xlsx")) %>%
  filter(region == "MNA")


# MENA boundary
boundary <- st_read(file.path(mena_file_path, "Boundaries","raw", "MENA_ADM0.shp"))
mena_adm2 <- st_read(file.path(mena_file_path, "Boundaries","raw", "MENA_ADM2.shp"))
mena_adm0 <- st_read(file.path(mena_file_path, "Boundaries","raw", "MENA_Country.shp"))


# Step 1: Merge with GSAP data --------------------------------------------
rwi_crosswalk_merged <- left_join(rwi_crosswalk,gsap_excel, by = c("g_cd2_n"="geo_code2_new"))
rwi_crosswalk_merged <- rwi_crosswalk_merged %>% 
  select(grid_id,id,g_cd2_n,poor215_ln,poor365_ln,poor685_ln)


# drop geometry from grid to avoid using st_join
grid_rwi_df <- grid_10km %>% st_drop_geometry()



# merge GSAP/Our data crosswalk with the grid
gsap_grid_merged <- rwi_crosswalk_merged %>% 
  left_join(grid_rwi_df, by = c("grid_id"))



# Step 2: Calculate the area for each grid and keep the max ---------------

# compute the area of each grid cell
gsap_grid_merged$area_km2 <- as.numeric(st_area(gsap_grid_merged) / 1000000, na.rm = T )

#sort the area by grid id within a polygon 
gsap_grid_merged_filtered <- gsap_grid_merged %>%
  group_by(grid_id,g_cd2_n) %>%
  arrange(desc(area_km2)) %>%
  filter(area_km2 == max(area_km2, na.rm = T))


# Step 3: Make RWI positive -----------------------------------------------


gsap_grid_merged_filtered$rwi_positive <- gsap_grid_merged_filtered$rwi_all+1.5



# Step 4: Create population variable for GSAP polygon ---------------------

gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  group_by(g_cd2_n) %>%
  mutate(tot_pop_gsap = sum(pop_count, na.rm = T),
         tot_grid_cells = n_distinct(grid_id, na.rm = T)) %>%
  ungroup()



# Step 5: RWI Population Mean Weighted ------------------------------------

gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  group_by(g_cd2_n) %>%
  mutate(rwi_pop_mean_gsap = sum(rwi_positive * pop_count, na.rm = TRUE) / sum(pop_count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rwi_wt = rwi_positive/rwi_pop_mean_gsap, na.rm = T)
         



# Step 6: Calculate RWI poverty rate --------------------------------------

gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  mutate(pov_rate_rwi = ifelse(!is.na(rwi_positive),rwi_wt*poor215_ln,poor215_ln),
         tot_poor_gsap = (poor215_ln*pop_count),
         tot_poor_rwi = (pov_rate_rwi*pop_count))


# Step 7: Check consistency with GSAP -------------------------------------

gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  group_by(g_cd2_n) %>%
  mutate(tot_poor_rwi_gsap = sum(tot_poor_rwi, na.rm = T)/tot_pop_gsap, na.rm = T) %>%
  ungroup()




# Step 8: Calculate different indicators ----------------------------------

# poor affected by heat stress
gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  mutate(severe_heat_stress = ifelse(upper_bound >=32,1,0),
         heat_affected_poor = severe_heat_stress*tot_poor_rwi)




