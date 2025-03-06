# ================================================================
# Script Name: 02a_CLEAN_GAS_FLARES.R
# Purpose: Clean the gas flares data from 2017 to 2021
# Input Dataset: [2017-2021] Global Gas Flaring Volumes.xlsx,
# Output Dataset: gas_flare_locations.Rds,gas_flare_locations.csv
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-06
# ================================================================

# Cleaning gas flares data
#Using script written by Rob Marty : https://github.com/datapartnership/myanmar-economic-monitor/blob/main/notebooks/nighttime-lights/analysis-2023/01_clean_gas_flaring_data.R

# Create dataset of gas flaring locations in MENA
# Raw data from: https://datacatalog.worldbank.org/search/dataset/0037743


# Function to clean gas flares data ---------------------------------------
# Define a function to clean data
clean_data <- function(x) x %>%
  clean_names() %>%  # Standardize column names
  dplyr::filter(iso_code %in% c("ARE","BHR", "DJI","DZA", "EGY","ISR",
                                "IRQ", "IRN", "JOR", "KWT", "LBN", "LBY",
                                "MAR", "MLT","OMN", "PSE", "QAT", "SAU",
                                "SYR", "TUN", "YEM"))  # Filter rows based on specific ISO country codes

# Read and clean 2021 gas flaring data
df_2021 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "2021 Global Gas Flaring Volumes.xlsx"), 2) %>%
  clean_data() %>% 
  dplyr::mutate(year = 2021)

# Read and clean three sheets from 2020 gas flaring data
df_2020_1 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "2020 Global Gas Flaring Volumes.xlsx"), 1) %>%
  clean_data() %>% dplyr::mutate(year = 2020)

df_2020_2 <- read_xlsx(file.path(raw_replication,"NTL","raw", "2020 Global Gas Flaring Volumes.xlsx"), 2) %>%
  clean_data() %>% dplyr::mutate(year = 2020)

df_2020_3 <- read_xlsx(file.path(raw_replication,"NTL","raw", "2020 Global Gas Flaring Volumes.xlsx"), 3) %>%
  clean_data() %>% dplyr::mutate(year = 2020)

df_2020_4 <- read_xlsx(file.path(raw_replication,"NTL","raw", "2020 Global Gas Flaring Volumes.xlsx"), 4) %>%
  clean_data() %>% dplyr::mutate(year = 2020)

# Read and clean 2019 gas flaring data
df_2019 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2019_web_v20201114-3.xlsx"), 1) %>%
  clean_data() %>% dplyr::mutate(year = 2019)

# Read and clean three sheets from 2018 gas flaring data
df_2018_4 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2018_web.xlsx"), 4) %>%
  clean_data() %>% dplyr::mutate(year = 2018)
df_2018_5 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2018_web.xlsx"), 5) %>%
  clean_data()  %>% dplyr::mutate(year = 2018)
df_2018_6 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2018_web.xlsx"), 6) %>%
  clean_data()  %>% dplyr::mutate(year = 2018)

# Read and clean three sheets from 2017 gas flaring data
df_2017_1 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2017_web_v1.xlsx"), 1) %>%
  clean_data()  %>% dplyr::mutate(year = 2017)
df_2017_2 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2017_web_v1.xlsx"), 2) %>%
  clean_data()  %>% dplyr::mutate(year = 2017)
df_2017_3 <- read_xlsx(file.path(raw_replication,"NTL", "raw", "viirs_global_flaring_d.7_slope_0.029353_2017_web_v1.xlsx"), 3) %>%
  clean_data()  %>% dplyr::mutate(year = 2017)









# Combining data ----------------------------------------------------------

gs_df <- bind_rows(
  df_2021,
  df_2020_1,
  df_2020_2,
  df_2020_3,
  df_2020_4,
  df_2019,
  df_2018_4,
  df_2018_5,
  df_2018_6,
  df_2017_1,
  df_2017_2,
  df_2017_3
)

# Select only latitude and longitude columns, remove duplicates, and add a unique identifier
gs_df <- gs_df %>%
  dplyr::select(latitude, longitude, year) %>%
  distinct() %>%
  dplyr::mutate(uid = 1:n())







# Export -----------------------------------------------------------------

# Save the final data frame in RDS format
saveRDS(gs_df, file.path(final_replication, "gas_flare_locations.Rds"))

# Also save the data frame as a CSV file
write_csv(gs_df, file.path(final_replication, "gas_flare_locations.csv"))
