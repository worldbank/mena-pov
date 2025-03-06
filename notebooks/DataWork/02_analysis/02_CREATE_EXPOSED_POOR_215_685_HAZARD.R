# ================================================================
# Script Name: 02a_CREATE_EXPOSED_POOR_215_HAZARD
# Purpose: Estimate the number of poor exposed to each hazard
# Input Dataset: grid_rwi_gsap_final.Rds, merged_dataset_10km.Rds
# Output Dataset:
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# Load data ------------------------------------------------------------------
gsap_grid_merged <- readRDS(file.path(final_replication,"grid_rwi_gsap_final.Rds"))
pop_grid_merged <- readRDS(file.path(final_replication,"merged_dataset_10km.Rds"))




# Calculate exposure to Heat ----------------------------------------------
gsap_grid_merged <- gsap_grid_merged %>%
  dplyr::mutate(
    # Define exposure based on upper_bound threshold
    exposed_heat32 = ifelse(upper_bound >= 32, 1, 0),

    # Calculate exposed poor count based on RWI
    exposed_heat32_poor215_rwi = exposed_heat32 * poor215_count_rwi_dup,

    # Calculate exposed poor count based on GSAP
    exposed_heat32_poor215_gsap = exposed_heat32 * poor215_count_gsap_dup
  )

pop_grid_merged <- pop_grid_merged %>%
  dplyr::mutate(
    # Define exposure based on upper_bound threshold
    exposed_heat32 = ifelse(upper_bound >= 32, 1, 0),

    # Calculate exposed population count
    exposed_heat32_pop = exposed_heat32 * pop_count,
  )

# Calculate exposure to PM 2_5 --------------------------------------------
gsap_grid_merged <- gsap_grid_merged %>%
  dplyr::mutate(

    # Define exposure based on annual_mean_pm25 threshold
    exposed_airpol15 = ifelse(annual_mean2_pm25 >= 15, 1, 0),

    # Calculate exposed poor count based on RWI
    exposed_airpol15_poor215_rwi = exposed_airpol15 * poor215_count_rwi_dup ,

    # Calculate exposed poor count based on GSAP
    exposed_airpol15_poor215_gsap = exposed_airpol15 * poor215_count_gsap_dup
  )


pop_grid_merged <- pop_grid_merged %>%
  dplyr::mutate(

    # Define exposure based on annual_mean_pm25 threshold
    exposed_airpol15 = ifelse(annual_mean2_pm25 >= 15, 1, 0),

    # Calculate exposed population count
    exposed_airpol15_pop = exposed_airpol15 * pop_count,

  )


# Calculate exposure to Drought -------------------------------------------
gsap_grid_merged <- gsap_grid_merged %>%
  dplyr::mutate(

    # Define exposure based on drought frequency distribution
    exposed_drought20 = ifelse(drought_freq >20,1,0),


    # Calculate exposed population count
    exposed_drought20_pop = exposed_drought20*pop_count,

    # Calculate exposed poor count based on RWI
    exposed_drought20_poor215_rwi = exposed_drought20 * poor215_count_rwi_dup ,

    # Calculate exposed poor count based on GSAP
    exposed_drought20_poor215_gsap = exposed_drought20 * poor215_count_gsap_dup
  )

pop_grid_merged <- pop_grid_merged %>%
  dplyr::mutate(

    # Define exposure based on drought frequency distribution
    exposed_drought20 = ifelse(drought_freq >20,1,0),


    # Calculate exposed population count
    exposed_drought20_pop = exposed_drought20*pop_count,
  )


# Calculate exposure to Floods --------------------------------------------

gsap_grid_merged <- gsap_grid_merged %>%
  dplyr::mutate(

    # Calculate exposed poor count based on RWI
    exposed_flood5_poor215_rwi = grid_pop_flood_expected * poor215_rwi_dup ,

    # Calculate exposed poor count based on GSAP
    exposed_flood5_poor215_gsap = grid_pop_flood_expected * poor215_gsap_dup
  )




# Export ------------------------------------------------------------------
saveRDS(gsap_grid_merged,file.path(final_replication,"grid_10km_poor_215.Rds"))
saveRDS(pop_grid_merged,file.path(final_replication,"grid_10km_people.Rds"))




