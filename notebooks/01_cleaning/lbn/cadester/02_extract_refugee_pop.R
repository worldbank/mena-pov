
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

ref_pop$year<- lubridate::year(ref_pop$Date)



# Remove cadaster info for Dec 2012
ref_pop_sub <- ref_pop %>% filter(Date!= "2012-12-01")
ref_pop_sub$CAS_CODE <- as.numeric(ref_pop_sub$CAS_CODE)

# Merge shp attributes ----------------------------------------------------
shp_nogeom <- shp %>% st_drop_geometry() %>% select(ACS_CODE_1,Cadaster,District,Governorat)


# Merge NTL, WorldPop and Ref Pop -----------------------------------------
merged_df_final <- comb_df %>%
  left_join(ref_pop_sub, by = c("ACS_CODE_1" = "CAS_CODE", "year"))



# Import Data Lab values --------------------------------------------------

# Set the working directory
setwd("C:/Users/wb569257/OneDrive - WBG/lbn_geospatial_analysis/DataLab/cadaster/individual-monthly")

# List all the .Rds files
rds_files <- list.files(pattern = "\\.Rds$")

# Read each .Rds file and store in a list
list_data <- lapply(rds_files, function(file) readRDS(file))

# Assuming each .Rds file contains a data.frame and you want to rbind them together
combined_data <- do.call(rbind, list_data)

# Create new vars
combined_data$year <- year(combined_data$date)
combined_data$month <- month(combined_data$date)


#merge with my data
merged_df_final <- merged_df_final %>%
  left_join(combined_data, by = c("ACS_CODE_1", "year", "month")) %>%
  mutate(diff_mean = ntl_mean - ntl_bm_mean)

summary(merged_df_final$diff_mean)

test <- merged_df_final %>% filter(diff_mean < 0)
