# Load data ---------------------------------------------------------------
r <- stack(file.path(lbn_file_path,
                     "Nighttime_Lights",
                     "raw", 
                     "monthly", 
                     "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))


shp <- st_read(file.path(lbn_file_path,
               "Boundaries",
               "cadaster.shp"))

# Check Projection --------------------------------------------------------

# Check CRS of both
raster_crs <- crs(r)
sf_crs <- st_crs(shp)


# Extract -----------------------------------------------------------------

ntl_mean    <- exact_extract(r, shp, fun = "mean")
ntl_median  <- exact_extract(r, shp, fun = "median")

ntl_mean$ACS_CODE_1 <- shp$ACS_CODE_1
ntl_median$ACS_CODE_1 <- shp$ACS_CODE_1




# Process data ------------------------------------------------------------

process_data <- function(my_data, data_name, start_year = 2012, start_month = 4) {
  
  # Melt the data
  data_melted <- my_data %>%
    mutate(uid = 1:nrow(.)) %>%
    melt(id.vars = "ACS_CODE_1") %>%
    mutate(variable = gsub("mean.avg_rad", "", variable))
  
  # Initialize the year and month values
  year <- start_year
  month <- start_month
  
  # Assign year and month based on unique variable
  for(i in unique(data_melted$variable)) {
    data_melted$year[data_melted$variable %in% i] <- year 
    data_melted$month[data_melted$variable %in% i] <- month 
    
    month <- month + 1
    
    if(month == 13) {
      month <- 1
      year <- year + 1
    }
  }
  
  # Rename the 'value' column using the data_name and remove the 'variable' column
  data_melted <- data_melted %>%
    select(-variable) %>%
    dplyr::rename_with(~ data_name, .cols = "value")
  
  return(data_melted)
}


# Call the function on each dataset
ntl_mean_melted <- process_data(ntl_mean, "ntl_mean")
ntl_median_melted <- process_data(ntl_median, "ntl_median")



# Merge with admin boundaries ---------------------------------------------
# List all the data frames you want to merge
list_of_dfs <- list(ntl_mean_melted,ntl_median_melted)

# Use reduce() to sequentially merge all data frames in the list
merged_df<- reduce(list_of_dfs, left_join, by = c("ACS_CODE_1", "month", "year"))




# Extract population ------------------------------------------------------
setwd("M:/LBN/GEO/Population/raw")
rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
rastlist



# extract to shapefile
years <- 2012:2020
cas_pop <- list()

for (i in 1:length(years)) {
  cas_pop[[paste0("cas_pop_", years[i])]] <- exact_extract(allrasters[[i]], shp, 'sum')
}


# Convert the list to a data frame
cas_pop_2012_2020 <- as.data.frame(do.call(cbind, cas_pop))

# Add the uid column
cas_pop_2012_2020$ACS_CODE_1 <- shp$ACS_CODE_1


# Melt the dataframe
cas_pop_2012_2020_melted <- cas_pop_2012_2020 %>%
  pivot_longer(cols = starts_with("cas_pop_"), 
               names_to = "year", 
               values_to = "population") %>%
  mutate(year = as.numeric(str_remove(year, "cas_pop_")))


# Merge NTL and population ------------------------------------------------
merged_df <- merged_df %>%
  left_join(cas_pop_2012_2020_melted, by = c("ACS_CODE_1","year"))



# Import refugee population -----------------------------------------------

## Importing Refugee Populations
# Define the file path
file_path <- "M:/LBN/GEO/Team/TeamData/Breakdown of Registered Syrians by Cadaster 2012-Jun 2023.xlsx"

# Get the names of all sheets
all_sheets <- excel_sheets(file_path)

# Assuming that each sheet has the same structure, you can read and bind them together
ref_pop <- lapply(all_sheets, function(sheet) {
  data <- read_excel(file_path, sheet = sheet)
  
  # Adding a column for the date from the sheet name (assuming your sheet names are exactly in the format "Dec 2012", "Jan 2013" etc.)
  data$Date <- as.Date(paste("01", sheet), format="%d %b %Y")
  
  return(data)
}) %>% bind_rows()
  


# Merge shp attributes ----------------------------------------------------
shp_nogeom <- shp %>% st_drop_geometry() %>% select(ACS_CODE_1,Cadaster,District,Governorat)




#Export
saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "cas_pop_ntl.Rds"))

write_csv(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "cas_pop_ntl.csv"))

write_csv(shp_nogeom, file.path(lbn_file_path,
                               "Nighttime_Lights",
                               "final",
                               "cas_id.csv"))



