# ================================================================
# Script Name: 01_ESTIMATE_POOR_RWI
# Purpose: Use RWI and reweight it to have estimates for poor at 5km level
# Input Dataset: merged_dataset_10km.Rds, grid_10km_all_gsap_union.shp
# Output Dataset: grid_rwi_gsap_final,Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================


###########################################################
# 1. Load Data ------------------------------------------------------------
###########################################################

# Load 10km grid dataset
grid_10km <- readRDS(file.path(final_replication, "merged_dataset_10km.Rds"))

# Load GSAP crosswalk shapefile and filter relevant regions
rwi_crosswalk <- st_read(file.path(raw_replication, "GSAP", "grid_10km_all_gsap_union.shp")) %>% 
  filter((region == "MNA" | code == "ISR") & grid_id != 0) %>% 
  # Select necessary columns
  dplyr::select(grid_id, id, surveyd, region, code, g_cd2_n) 
# This crosswalk was prepared in ArcGIS

# Load poverty rate data from Excel (not included in shapefile)
gsap_excel <- read_excel(file.path(raw_replication, "GSAP", "global-subnational-poverty-gsap-2019-data.xlsx")) %>%
  filter(region == "MNA" | code == "ISR") %>%
  dplyr::select(-region, -code)

# Merge GSAP crosswalk with poverty data
rwi_crosswalk_merged <- left_join(rwi_crosswalk, gsap_excel, by = c("g_cd2_n" = "geo_code2_new")) %>%
  dplyr::select(grid_id, id, g_cd2_n, baseyear, survname, poor215_ln, poor365_ln, poor685_ln)

# Remove geometry from grid dataset
grid_rwi_df <- grid_10km %>% st_drop_geometry()

# Merge GSAP crosswalk with 10km grid
gsap_grid_merged <- left_join(rwi_crosswalk_merged, grid_rwi_df, by = "grid_id")

###########################################################
# 2. Calculate the area of the grid cells
###########################################################

# Compute grid cell areas in square kilometers
gsap_grid_merged <- gsap_grid_merged %>%
  dplyr::mutate(original_area_km2_dup = as.numeric(st_area(geometry) / 1e6))

# Identify duplicate grid IDs
gsap_grid_merged$duplicates <- ifelse(
  duplicated(gsap_grid_merged$grid_id) | duplicated(gsap_grid_merged$grid_id, fromLast = TRUE), 1, 0)

# Sum of duplicate entries
sum(gsap_grid_merged$duplicates)  # Due to non-square cell splitting

# Compute total area per grid ID
gsap_grid_merged <- gsap_grid_merged %>%
  group_by(grid_id) %>%
  dplyr::mutate(original_area_km2 = sum(original_area_km2_dup, na.rm = TRUE)) %>%
  ungroup()

# Compute weight of each duplicate grid cell
gsap_grid_merged$weight_dup_grid_id <- gsap_grid_merged$original_area_km2_dup / gsap_grid_merged$original_area_km2

###########################################################
# 3. Correct the population estimate for non-square cells
###########################################################

gsap_grid_merged$pop_count_dup_grid_id <- gsap_grid_merged$pop_count * gsap_grid_merged$weight_dup_grid_id

###########################################################
# 4. Calculate "weighted RWI" poverty rates
###########################################################

# Shift RWI values to avoid negative numbers (higher value means poorer)
gsap_grid_merged$rwi_pos_dup <- -gsap_grid_merged$rwi_all + 1.5

# Handle cases where population is zero
gsap_grid_merged$rwi_pos2_dup <- gsap_grid_merged$rwi_pos_dup
gsap_grid_merged$rwi_pos2_dup[is.na(gsap_grid_merged$rwi_pos_dup) & gsap_grid_merged$pop_count_dup_grid_id == 0] <- 1

# Create population count variable for missing RWI cases
gsap_grid_merged$pop_count2_dup <- gsap_grid_merged$pop_count_dup_grid_id
gsap_grid_merged$pop_count2_dup[is.na(gsap_grid_merged$rwi_pos2_dup)] <- NaN

# Compute weighted RWI-population product
gsap_grid_merged$rwi_pop_dup <- gsap_grid_merged$rwi_pos2_dup * gsap_grid_merged$pop_count_dup_grid_id

# Aggregate data within GSAP polygons
gsap_grid_merged <- gsap_grid_merged %>%
  group_by(g_cd2_n) %>%
  mutate(
    poor215_gsap_dup = mean(poor215_ln),
    poor365_gsap_dup = mean(poor365_ln),
    poor685_gsap_dup = mean(poor685_ln),
    rwi_pop_gsap_dup = sum(rwi_pop_dup, na.rm = TRUE),
    population_gsap_rwi_dup = sum(pop_count2_dup, na.rm = TRUE),
    population_gsap_dup = sum(pop_count_dup_grid_id)
  ) %>%
  ungroup()

# Equal to RWI_mean=(rwi1*pop1+rwi2*pop2)/(pop1+pop2)
gsap_grid_merged$rwi_weight_pop_gsap_dup<-gsap_grid_merged$rwi_pop_gsap_dup/gsap_grid_merged$population_gsap_rwi_dup

# RWI weights: rwi/RWI_mean such that the sum in the above expression =1
gsap_grid_merged$rwi_weight_dup<-gsap_grid_merged$rwi_pos2_dup/gsap_grid_merged$rwi_weight_pop_gsap_dup


# New poverty rates
gsap_grid_merged$poor215_rwi_dup<-gsap_grid_merged$rwi_weight_dup*gsap_grid_merged$poor215_gsap_dup
gsap_grid_merged$poor365_rwi_dup<-gsap_grid_merged$rwi_weight_dup*gsap_grid_merged$poor365_gsap_dup
gsap_grid_merged$poor685_rwi_dup<-gsap_grid_merged$rwi_weight_dup*gsap_grid_merged$poor685_gsap_dup
gsap_grid_merged$poor215_rwi_dup[is.na(gsap_grid_merged$rwi_weight_dup)]<-gsap_grid_merged$poor215_gsap_dup[is.na(gsap_grid_merged$rwi_weight_dup)]
gsap_grid_merged$poor365_rwi_dup[is.na(gsap_grid_merged$rwi_weight_dup)]<-gsap_grid_merged$poor365_gsap_dup[is.na(gsap_grid_merged$rwi_weight_dup)]
gsap_grid_merged$poor685_rwi_dup[is.na(gsap_grid_merged$rwi_weight_dup)]<-gsap_grid_merged$poor685_gsap_dup[is.na(gsap_grid_merged$rwi_weight_dup)]

#If poverty rates above 100, replace them with 100
gsap_grid_merged$poor215_rwi_dup <- ifelse(gsap_grid_merged$poor215_rwi_dup > 1, 1, gsap_grid_merged$poor215_rwi_dup)
gsap_grid_merged$poor365_rwi_dup <- ifelse(gsap_grid_merged$poor365_rwi_dup > 1, 1, gsap_grid_merged$poor365_rwi_dup)
gsap_grid_merged$poor685_rwi_dup <- ifelse(gsap_grid_merged$poor685_rwi_dup > 1, 1, gsap_grid_merged$poor685_rwi_dup)



# 3.4 Estimated number of poor
gsap_grid_merged$poor215_count_gsap_dup<-gsap_grid_merged$pop_count_dup_grid_id*gsap_grid_merged$poor215_gsap_dup
gsap_grid_merged$poor365_count_gsap_dup<-gsap_grid_merged$pop_count_dup_grid_id*gsap_grid_merged$poor365_gsap_dup
gsap_grid_merged$poor685_count_gsap_dup<-gsap_grid_merged$pop_count_dup_grid_id*gsap_grid_merged$poor685_gsap_dup
gsap_grid_merged$poor215_count_rwi_dup<-gsap_grid_merged$pop_count_dup_grid_id*gsap_grid_merged$poor215_rwi_dup
gsap_grid_merged$poor365_count_rwi_dup<-gsap_grid_merged$pop_count_dup_grid_id*gsap_grid_merged$poor365_rwi_dup
gsap_grid_merged$poor685_count_rwi_dup<-gsap_grid_merged$pop_count_dup_grid_id*gsap_grid_merged$poor685_rwi_dup





###########################################################
# 5. Export Data
###########################################################

saveRDS(gsap_grid_merged, file.path(final_replication, "grid_rwi_gsap_final.Rds"))

###########################################################
# 6. Summarize at ADM0 Level
###########################################################

grid_adm0 <- gsap_grid_merged %>%
  group_by(g_cd2_n, WB_ADM0_NA) %>%
  summarise(
    poor215_gsap = sum(poor215_count_gsap_dup),
    poor215_rwi = sum(poor215_count_rwi_dup)
  ) %>%
  ungroup()




