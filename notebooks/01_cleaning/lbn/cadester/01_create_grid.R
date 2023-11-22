
# Create empty grid -------------------------------------------------------
unique_ids <- read_csv("M:/LBN/GEO/Nighttime_Lights/final/cas_id.csv")
months <- 1:12
years <- 2000:2020


# Creating a data frame using crossing from tidyr to create all combinations
grid_panel <- tidyr::crossing(ACS_CODE_1 = unique_ids$ACS_CODE_1,
                              year = years, 
                              month = months)

# Remove ACS_CODE_1
grid_panel <- grid_panel %>%
  filter(ACS_CODE_1 != 0)





# Export ------------------------------------------------------------------
saveRDS(grid_panel, "M:/LBN/GEO/Nighttime_Lights/final/grid_blank.Rds" )





