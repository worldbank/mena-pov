# ================================================================
# Script Name: 02_EXTRACT_CONFLICT.R
# Purpose: Creates 10*10km grid with the total number of conflict events (last 2, 5, 10 yrs)
# Input Dataset: World_Cities.geojson,MENA_ADM1.shp
# Output Dataset: "cities_", country, ".shp"
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# Time Taken: 6 mins
# ================================================================

system.time({
  


# Load Data ---------------------------------------------------------------
grid_sf <- st_read(file.path(final_replication, "grid_10km.shp")) # Read the shapefile grid
acled_conflict <- read_excel(file.path(raw_replication,"CONFLICT","ACLED_MENA_1997_2023.xlsx")) # conflict data





# Cleaning conflict data --------------------------------------------------
# Convert numeric codes to ISO three-letter codes
numeric_codes <- unique(acled_conflict$iso)
letter_codes <- countrycode(numeric_codes, "iso3n", "iso3c")

mapping_df <- data.frame(
  iso = numeric_codes,
  country_code = letter_codes
)

#merge back to df
acled_conflict <- acled_conflict %>%
  left_join(mapping_df, by = c("iso"))

#Filter out turkey
acled_conflict_sub <- acled_conflict %>% filter(country_code != "TUR") %>%
  dplyr::select(-c(29:53))






# Remove duplicates -------------------------------------------------------

# Remove duplicates based on the 'event_id_cnty' column
acled_conflict_unique_dt <- unique(acled_conflict_sub, by = "event_id_cnty")

#convert to sf before joining to grid
acled_sf <- st_as_sf(acled_conflict_unique_dt, coords = c("longitude", "latitude"), crs = 4326)






# Join ACLED and grid data ------------------------------------------------
#transform projection
grid_sf <- st_transform(grid_sf, 4326)
joined_data <- st_join(acled_sf,grid_sf)



#check if event_id is unique
joined_data_sub <- unique(joined_data, by = c("event_id_cnty","event_date"))





# Filter data based on years ----------------------------------------------

#convert into date format
joined_data_sub$event_date <-
  as.Date(joined_data_sub$event_date, format = "%d %B %Y")

#Create binary vars for the cutoff dates - 2 years, 5 years, 10 years
joined_data_sub<- joined_data_sub %>%
  mutate(event_date = as.Date(event_date, format = "%d %B %Y"),
         last_2yrs = as.integer(event_date >= as.Date("2022-01-01")),
         last_5yrs = as.integer(event_date >= as.Date("2019-01-01")),
         last_10yrs = as.integer(event_date >= as.Date("2014-01-01"))) 


setDT(joined_data_sub)

# Adding new columns for 2, 5, and 10 years based on conditions
joined_data_sub[, `:=`(
  demonstrations_2yrs = fifelse(disorder_type %in% c("Demonstrations", "Political violence; Demonstrations") & last_2yrs == 1, 1, 0),
  demonstrations_5yrs = fifelse(disorder_type %in% c("Demonstrations", "Political violence; Demonstrations") & last_5yrs == 1, 1, 0),
  demonstrations_10yrs = fifelse(disorder_type %in% c("Demonstrations", "Political violence; Demonstrations") & last_10yrs == 1, 1, 0),
  political_violence_2yrs = fifelse(disorder_type %in% c("Political violence", "Political violence; Demonstrations") & last_2yrs == 1, 1, 0),
  political_violence_5yrs = fifelse(disorder_type %in% c("Political violence", "Political violence; Demonstrations") & last_5yrs == 1, 1, 0),
  political_violence_10yrs = fifelse(disorder_type %in% c("Political violence", "Political violence; Demonstrations") & last_10yrs == 1, 1, 0),
  strategic_dev_2yrs = fifelse(disorder_type == "Strategic developments" & last_2yrs == 1, 1, 0),
  strategic_dev_5yrs = fifelse(disorder_type == "Strategic developments" & last_5yrs == 1, 1, 0),
  strategic_dev_10yrs = fifelse(disorder_type == "Strategic developments" & last_10yrs == 1, 1, 0),
  protests_2yrs = fifelse(event_type == "Protests" & last_2yrs == 1, 1, 0),
  protests_5yrs = fifelse(event_type == "Protests" & last_5yrs == 1, 1, 0),
  protests_10yrs = fifelse(event_type == "Protests" & last_10yrs == 1, 1, 0),
  riots_2yrs = fifelse(event_type == "Riots" & last_2yrs == 1, 1, 0),
  riots_5yrs = fifelse(event_type == "Riots" & last_5yrs == 1, 1, 0),
  riots_10yrs = fifelse(event_type == "Riots" & last_10yrs == 1, 1, 0),
  explosion_2yrs = fifelse(event_type == "Explosions/Remote violence" & last_2yrs == 1, 1, 0),
  explosion_5yrs = fifelse(event_type == "Explosions/Remote violence" & last_5yrs == 1, 1, 0),
  explosion_10yrs = fifelse(event_type == "Explosions/Remote violence" & last_10yrs == 1, 1, 0),
  violence_civilians_2yrs = fifelse(event_type == "Violence against civilians" & last_2yrs == 1, 1, 0),
  violence_civilians_5yrs = fifelse(event_type == "Violence against civilians" & last_5yrs == 1, 1, 0),
  violence_civilians_10yrs = fifelse(event_type == "Violence against civilians" & last_10yrs == 1, 1, 0),
  battles_2yrs = fifelse(event_type == "Battles" & last_2yrs == 1, 1, 0),
  battles_5yrs = fifelse(event_type == "Battles" & last_5yrs == 1, 1, 0),
  battles_10yrs = fifelse(event_type == "Battles" & last_10yrs == 1, 1, 0),
  strategic_dev_events_2yrs = fifelse(event_type == "Strategic developments" & last_2yrs == 1, 1, 0),
  strategic_dev_events_5yrs = fifelse(event_type == "Strategic developments" & last_5yrs == 1, 1, 0),
  strategic_dev_events_10yrs = fifelse(event_type == "Strategic developments" & last_10yrs == 1, 1, 0)
)]



# Create different datasets by disorder, event_type and totals ------------

#Create separate dfs for total events, by event type, and disorder type
tot_conflicts <- joined_data_sub%>%
  group_by(grid_id) %>%
  summarise(
    tot_2yrs = sum(last_2yrs, na.rm = T),
    tot_5yrs = sum(last_5yrs, na.rm = T),
    tot_10yrs = sum(last_10yrs, na.rm = T)
  )


tot_conflicts_acled_indicator <- joined_data_sub%>%
  group_by(grid_id) %>%
  summarise(
    tot_2yrs_acled_indicator = sum(riots_2yrs + explosion_2yrs + violence_civilians_2yrs + battles_2yrs, na.rm = T),
    tot_5yrs_acled_indicator = sum(riots_5yrs + explosion_5yrs + violence_civilians_5yrs + battles_5yrs, na.rm = T),
    tot_10yrs_acled_indicator = sum(riots_10yrs + explosion_10yrs + violence_civilians_10yrs + battles_10yrs, na.rm = T)
  )

tot_conflicts_disorder <- joined_data_sub%>%
  group_by(grid_id) %>%
  summarise(
    tot_demonstrations_2yrs = sum(demonstrations_2yrs, na.rm = T),
    tot_demonstrations_5yrs = sum(demonstrations_5yrs, na.rm = T),
    tot_demonstrations_10yrs = sum(demonstrations_10yrs, na.rm = T),
    tot_political_violence_2yrs = sum(political_violence_2yrs, na.rm = T),
    tot_political_violence_5yrs = sum(political_violence_5yrs, na.rm = T),
    tot_political_violence_10yrs = sum(political_violence_10yrs, na.rm = T),
    tot_strategic_dev_2yrs = sum(strategic_dev_2yrs, na.rm = T),
    tot_strategic_dev_5yrs = sum(strategic_dev_5yrs, na.rm = T),
    tot_strategic_dev_10yrs = sum(strategic_dev_10yrs, na.rm = T)
  )

tot_conflicts_by_event <- joined_data_sub%>%
  group_by(grid_id) %>%
  summarise(
    tot_protests_2yrs = sum(protests_2yrs, na.rm = T),
    tot_protests_5yrs = sum(protests_5yrs, na.rm = T),
    tot_protests_10yrs = sum(protests_10yrs, na.rm = T),
    tot_riots_2yrs = sum(riots_2yrs, na.rm = T),
    tot_riots_5yrs = sum(riots_5yrs, na.rm = T),
    tot_riots_10yrs = sum(riots_10yrs, na.rm = T),
    tot_explosion_2yrs = sum(explosion_2yrs, na.rm = T),
    tot_explosion_5yrs = sum(explosion_5yrs, na.rm = T),
    tot_explosion_10yrs = sum(explosion_10yrs, na.rm = T),
    tot_violence_civilians_2yrs = sum(violence_civilians_2yrs , na.rm = T),
    tot_violence_civilians_5yrs = sum(violence_civilians_5yrs , na.rm = T),
    tot_violence_civilians_10yrs = sum(violence_civilians_5yrs , na.rm = T),
    tot_battles_2yrs = sum(battles_2yrs, na.rm = T),
    tot_battles_5yrs = sum(battles_5yrs, na.rm = T),
    tot_battles_10yrs = sum(battles_10yrs, na.rm = T),
    tot_strategic_events_2yrs = sum(strategic_dev_events_2yrs, na.rm = T),
    tot_strategic_events_5yrs = sum(strategic_dev_events_5yrs, na.rm = T),
    tot_strategic_events_10yrs = sum(strategic_dev_events_2yrs, na.rm = T)
  )


# Drop the geometry to merge with original grid
tot_conflicts <- tot_conflicts %>% st_drop_geometry()
tot_conflicts_acled_indicator <- tot_conflicts_acled_indicator %>% st_drop_geometry()
tot_conflicts_disorder <- tot_conflicts_disorder %>% st_drop_geometry()
tot_conflicts_by_event <- tot_conflicts_by_event %>% st_drop_geometry()

# Merge back to the original grid
grid_sf <- grid_sf %>%
  left_join(tot_conflicts, by = c("grid_id"))

grid_sf <- grid_sf %>%
  left_join(tot_conflicts_acled_indicator, by = c("grid_id"))

grid_sf <- grid_sf %>%
  left_join(tot_conflicts_disorder, by = c("grid_id"))

grid_sf <- grid_sf %>%
  left_join(tot_conflicts_by_event, by = c("grid_id"))





# Convert NA to 0 ---------------------------------------------------------

# List of columns to transform
columns_to_transform <- c(
  "tot_2yrs",
  "tot_5yrs",
  "tot_10yrs",
  "tot_2yrs_acled_indicator",
  "tot_5yrs_acled_indicator",
  "tot_10yrs_acled_indicator",
  "tot_demonstrations_2yrs",
  "tot_demonstrations_5yrs",
  "tot_demonstrations_10yrs",
  "tot_political_violence_2yrs",
  "tot_political_violence_5yrs",
  "tot_political_violence_10yrs",
  "tot_strategic_dev_2yrs",
  "tot_strategic_dev_5yrs",
  "tot_strategic_dev_10yrs",
  "tot_protests_2yrs",
  "tot_protests_5yrs",
  "tot_protests_10yrs",
  "tot_riots_2yrs",
  "tot_riots_5yrs",
  "tot_riots_10yrs",
  "tot_explosion_2yrs",
  "tot_explosion_5yrs",
  "tot_explosion_10yrs",
  "tot_violence_civilians_2yrs",
  "tot_violence_civilians_5yrs",
  "tot_violence_civilians_10yrs",
  "tot_battles_2yrs",
  "tot_battles_5yrs",
  "tot_battles_10yrs",
  "tot_strategic_events_2yrs",
  "tot_strategic_events_5yrs",
  "tot_strategic_events_10yrs"
)

# Replacing NA with 0 in specified columns
grid_sf <- grid_sf %>% mutate(across(all_of(columns_to_transform), ~ ifelse(is.na(.), 0, .)))
summary(grid_sf)



# Export ------------------------------------------------------------------
saveRDS(grid_sf,
        file.path(final_replication,
                  "grid_acled_10km.Rds"))


})