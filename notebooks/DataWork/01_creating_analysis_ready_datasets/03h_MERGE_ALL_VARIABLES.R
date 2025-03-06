# ================================================================
# Script Name: 03e_MERGE_VARIABLES.R
# Purpose: Merge all the different variables
# Input Dataset: grid_pop_10km, grid_rwi_10km, grid_wbgt_10km.Rds
# grid_flood_10km,grid_PM25_10km.Rds , grid_drought_10km.Rds
# grid_forest_loss_10km.Rds, grid_ntl_nogf_10km.Rds, grid_ntl_onlygf_10km.Rds,
# grid_acled_10km.Rds, grid_tt_health_educ_markets, grid_social_dim2
# Output Dataset: grid_final.Rds
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# 1. Load Data ---------------------------------------------------------------

#GRID
grid <- readRDS(file.path(final_replication, "grid_10km.Rds"))




#demographic variables
pop <- readRDS(file.path(final_replication,"grid_pop_10km.Rds"))
rwi <- readRDS(file.path(final_replication, "grid_rwi_10km.Rds"))


#hazards
heat_stress <- readRDS(file.path(final_replication,"grid_wbgt_10km.Rds"))
flood <- readRDS(file.path(final_replication, "grid_flood_10km.Rds"))
air_pol <- readRDS(file.path(final_replication, "grid_PM25_10km.Rds"))
drought_asi <- readRDS(file.path(final_replication, "grid_drought_10km.Rds"))

#vulnerability
#Aggregate Yield Achievement Ratio
agg_ratio <- readRDS(file.path(final_replication,"grid_yield_achievement_ratio_10km.Rds")) # Agg yield ratio
nogf_ntl <- readRDS(file.path(final_replication, "grid_ntl_nogf_10km.Rds")) # Nighttime Lights without gas flares
gf_ntl <- readRDS(file.path(final_replication ,"grid_ntl_onlygf_10km.Rds")) #Nighttime lights with gas flare
conflict <- readRDS(file.path(final_replication, "grid_acled_10km.Rds")) # ACLED
tt_all <- readRDS(file.path(final_replication, "grid_tt_health_educ_markets.Rds")) #Time travel
social_dim_hh_survey <- readRDS(file.path(final_replication, "grid_social_dim2.Rds")) #Social Dimensions
forest_loss <- readRDS(file.path(final_replication,"grid_forest_loss_10km.Rds")) #forest loss




# 2. Create the duplicated variables in the data -----------------------------



duplicated_vars <- c("OBJECTID", "ADM0_CODE", "ADM1_CODE", "ADM2_CODE", "ISO_A2",
                     "WB_ADM1_CO", "WB_ADM0_CO", "WB_ADM0_NA", "WB_ADM1_NA", "WB_ADM2_CO",
                     "WB_ADM2_NA", "ID_ADM", "grid_id")






# Renaming before merging -------------------------------------------------

gf_ntl <- gf_ntl %>%
  rename(
    gf_ntl_mean_2017 = ntl_mean_2017,
    gf_ntl_mean_2018 = ntl_mean_2018,
    gf_ntl_mean_2019 = ntl_mean_2019,
    gf_ntl_mean_2020 = ntl_mean_2020,
    gf_ntl_mean_2021 = ntl_mean_2021,
    gf_change_in_ntl_2021_2017 = change_in_ntl_2021_2017
  )

# For the second dataset (nogf_ntl), add prefix 'nogf_'
nogf_ntl <- nogf_ntl %>%
  rename(
    nogf_ntl_mean_2017 = ntl_mean_2017,
    nogf_ntl_mean_2018 = ntl_mean_2018,
    nogf_ntl_mean_2019 = ntl_mean_2019,
    nogf_ntl_mean_2020 = ntl_mean_2020,
    nogf_ntl_mean_2021 = ntl_mean_2021,
    nogf_change_in_ntl_2021_2017 = change_in_ntl_2021_2017
  )



# 3. Merging the datasets ----------------------------------------------------

# Function to clean dataset by removing duplicated columns except from first dataset
clean_merge <- function(base_df, additional_df, by = "grid_id", duplicated_vars) {
  # Remove geometry and duplicated variables from additional dataset
  if (inherits(additional_df, "sf")) {
    additional_df <- sf::st_drop_geometry(additional_df)
  }
  cols_to_keep <- setdiff(names(additional_df), duplicated_vars)
  additional_df_clean <- additional_df[, c(by, cols_to_keep), drop = FALSE]
  
  # Merge
  merged_df <- base_df %>%
    left_join(additional_df_clean, by = by)
  
  return(merged_df)
}






# Main merging pipeline
merge_all_datasets <- function(grid, pop, rwi, heat_stress, flood, air_pol, 
                               drought_asi, agg_ratio, nogf_ntl, gf_ntl, 
                               conflict, tt_all, social_dim_hh_survey, forest_loss,
                               duplicated_vars) {
  
  # Start with the base grid
  merged_data <- grid
  
  # Drop geometry from all additional datasets before creating the list
  datasets <- list(
    pop = if(inherits(pop, "sf")) sf::st_drop_geometry(pop) else pop,
    rwi = if(inherits(rwi, "sf")) sf::st_drop_geometry(rwi) else rwi,
    heat_stress = if(inherits(heat_stress, "sf")) sf::st_drop_geometry(heat_stress) else heat_stress,
    flood = if(inherits(flood, "sf")) sf::st_drop_geometry(flood) else flood,
    air_pol = if(inherits(air_pol, "sf")) sf::st_drop_geometry(air_pol) else air_pol,
    drought_asi = if(inherits(drought_asi, "sf")) sf::st_drop_geometry(drought_asi) else drought_asi,
    agg_ratio = if(inherits(agg_ratio, "sf")) sf::st_drop_geometry(agg_ratio) else agg_ratio,
    nogf_ntl = if(inherits(nogf_ntl, "sf")) sf::st_drop_geometry(nogf_ntl) else nogf_ntl,
    gf_ntl = if(inherits(gf_ntl, "sf")) sf::st_drop_geometry(gf_ntl) else gf_ntl,
    conflict = if(inherits(conflict, "sf")) sf::st_drop_geometry(conflict) else conflict,
    tt_all = if(inherits(tt_all, "sf")) sf::st_drop_geometry(tt_all) else tt_all,
    social_dim_hh_survey = if(inherits(social_dim_hh_survey, "sf")) sf::st_drop_geometry(social_dim_hh_survey) else social_dim_hh_survey,
    forest_loss = if(inherits(forest_loss, "sf")) sf::st_drop_geometry(forest_loss) else forest_loss
  )
  
  # Merge each dataset
  for (dataset_name in names(datasets)) {
    merged_data <- clean_merge(
      merged_data, 
      datasets[[dataset_name]], 
      by = "grid_id", 
      duplicated_vars = duplicated_vars
    )
    
    # Print progress
    message(sprintf("Merged %s dataset", dataset_name))
  }
  
  return(merged_data)
}







# Execute the merge
final_dataset <- merge_all_datasets(
  grid = grid,
  pop = pop,
  rwi = rwi,
  heat_stress = heat_stress,
  flood = flood,
  air_pol = air_pol,
  drought_asi = drought_asi,
  agg_ratio = agg_ratio,
  nogf_ntl = nogf_ntl,
  gf_ntl = gf_ntl,
  conflict = conflict,
  tt_all = tt_all,
  social_dim_hh_survey = social_dim_hh_survey,
  forest_loss = forest_loss,
  duplicated_vars = duplicated_vars
)

names(final_dataset)
# Check for any remaining duplicate column names
duplicate_cols <- names(final_dataset)[duplicated(names(final_dataset))]
if (length(duplicate_cols) > 0) {
  warning("Found duplicate columns: ", paste(duplicate_cols, collapse = ", "))
}

names(final_dataset)


# Export ------------------------------------------------------------------

# Save the merged dataset
saveRDS(final_dataset, file.path(final_replication, "merged_dataset_10km.Rds"))

