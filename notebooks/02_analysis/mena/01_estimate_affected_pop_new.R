
# Load Data ---------------------------------------------------------------
grid_10km <- readRDS(file.path(mena_file_path,"Allsources","grid_10km_all.Rds"))


# Import crosswalk prepared by Sandra in ArcGIS
rwi_crosswalk <- st_read(file.path(mena_file_path,"RWICrosswalk", "grid_10km_all_gsap_union.shp")) %>% 
  filter(region == "MNA" & grid_id !=0) %>%
  select(grid_id,id,surveyd,region,code,g_cd2_n)




# Add the poverty rate data from the excel sheet, since the shapefiles did not include it
gsap_excel <- read_excel(file.path(local_dir,"MENA","Poverty","global-subnational-poverty-gsap-2019-data.xlsx")) %>%
  filter(region == "MNA")




# Subset data -------------------------------------------------------------
grid_rwi <- grid_10km %>%
  select(grid_id,WB_ADM1_NA,WB_ADM1_CO,WB_ADM0_NA,rwi_all,pop_count,pop_density)


# Step 1: Merge with GSAP data --------------------------------------------
rwi_crosswalk_merged <- left_join(rwi_crosswalk,gsap_excel, by = c("g_cd2_n"="geo_code2_new"))
rwi_crosswalk_merged <- rwi_crosswalk_merged %>% 
  select(grid_id,id,g_cd2_n,poor215_ln,poor365_ln,poor685_ln)


# drop geometry from grid to avoid using st_join
grid_rwi_df <- grid_rwi %>% st_drop_geometry()



# merge GSAP/Our data crosswalk with the grid
gsap_grid_merged <- rwi_crosswalk_merged %>% 
  left_join(grid_rwi_df, by = c("grid_id"))



# Step 2: Calculate the area for each grid and keep the max ---------------

# compute the area of each grid cell
gsap_grid_merged$area <- as.numeric(st_area(gsap_grid_merged) / 1000000 )

#sort the area by grid id within a polygon 
gsap_grid_merged_filtered <- gsap_grid_merged %>%
  group_by(grid_id,g_cd2_n) %>%
  arrange(desc(area)) %>%
  filter(area == max(area))


# Step 3: Make RWI positive -----------------------------------------------


gsap_grid_merged_filtered$rwi_positive <- gsap_grid_merged_filtered$rwi_all+1.5



# Step 4: Create population variable for GSAP polygon ---------------------

gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  group_by(g_cd2_n) %>%
  mutate(tot_pop_gsap = sum(pop_count)) %>%
  ungroup()



# Step 5: Calculate number of poor GSAP -----------------------------------

gsap_grid_merged_filtered$poor_gsap <- (gsap_grid_merged_filtered$poor215_ln)*(gsap_grid_merged_filtered$tot_pop_gsap)




# Step 6: Re-weight the number of poor using RWI ---------------------------
gsap_grid_merged_filtered <- gsap_grid_merged_filtered %>%
  group_by(g_cd2_n) %>%
  mutate(rwi_tot = sum(rwi_positive),
         ) %>%
  ungroup() %>%

  


