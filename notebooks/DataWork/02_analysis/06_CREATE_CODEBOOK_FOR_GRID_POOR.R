# ================================================================
# Script Name: 06_CREATE_CODEBOOK
# Purpose: Create codebook with names and descriptions
# Input Dataset: grid_10km_poor_vul_exposed.Rds
# Output Dataset: codebook_grid10km_poor_vul_exposed.csv
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================


# Load Data ---------------------------------------------------------------
poor_vul_exposed <- readRDS(file.path(final_replication,"grid_10km_poor_vul_exposed.Rds"))


#drop geometry
poor_vul_exposed_without_geom <- poor_vul_exposed %>% st_drop_geometry() %>% dplyr::select(-geometry.y)


# Define the vector of column names (all 249 columns)
col_names <- names(poor_vul_exposed_without_geom)
print(col_names)

# Define a corresponding vector of descriptions (modify these as needed)
descriptions <- c(
  "Unique grid identifier",
  "GSAP ID",
  "GSAP Code",
  "GSAP Base Year",
  "GSAP Survey Name",
  "Poverty Rate $2.15/day",
  "Poverty Rate $3.65/day",
  "Poverty Rate $6.85/day",
  "GAUL identifier",
  "Country-level administrative code",
  "First-level administrative code",
  "Second-level administrative code",
  "ISO Alpha-2 country code",
  "World Bank admin level 1 code",
  "World Bank country code",
  "World Bank country name",
  "World Bank admin level 1 name",
  "World Bank admin level 2 code",
  "World Bank admin level 2 name",
  "Local administrative identifier",
  "Population count",
  "Population density",
  "Relative Wealth Index for Algeria",
  "Relative Wealth Index for Djibouti",
  "Relative Wealth Index for Egypt",
  "Relative Wealth Index for Jordan",
  "Relative Wealth Index for Lebanon",
  "Relative Wealth Index for Libya",
  "Relative Wealth Index for Morocco",
  "Relative Wealth Index for Tunisia",
  "Overall Relative Wealth Index (Average)",
  "Temperature return period (100-year)",
  "Temperature return period (20-year)",
  "Temperature return period (5-year)",
  "Upper bound of extreme heat [>32 celsius]",
  "Flood risk for grid (5-year return period)",
  "Population exposed to flood risk (5-year)",
  "Resampled population count (10km, 5-year)",
  "Flood risk for grid (10-year return period)",
  "Population exposed to flood risk (10-year)",
  "Resampled population count (10km, 10-year)",
  "Flood risk for grid (20-year return period)",
  "Population exposed to flood risk (20-year)",
  "Resampled population count (10km, 20-year)",
  "Flood risk for grid (50-year return period)",
  "Population exposed to flood risk (50-year)",
  "Resampled population count (10km, 50-year)",
  "Flood risk for grid (75-year return period)",
  "Population exposed to flood risk (75-year)",
  "Resampled population count (10km, 75-year)",
  "Flood risk for grid (100-year return period)",
  "Population exposed to flood risk (100-year)",
  "Resampled population count (10km, 100-year)",
  "Flood risk for grid (200-year return period)",
  "Population exposed to flood risk (200-year)",
  "Resampled population count (10km, 200-year)",
  "Flood risk for grid (250-year return period)",
  "Population exposed to flood risk (250-year)",
  "Resampled population count (10km, 250-year)",
  "Flood risk for grid (500-year return period)",
  "Population exposed to flood risk (500-year)",
  "Resampled population count (10km, 500-year)",
  "Flood risk for grid (1000-year return period)",
  "Population exposed to flood risk (1000-year)",
  "Resampled population count (10km, 1000-year)",
  "Lower bound for flood risk",
  "Population count at lower flood risk bound",
  "Upper bound for flood risk",
  "Population count at upper flood risk bound",
  "Expected flood risk value",
  "Expected population flood risk",
  "PM2.5 concentration in 1998",
  "PM2.5 concentration in 1999",
  "PM2.5 concentration in 2000",
  "PM2.5 concentration in 2001",
  "PM2.5 concentration in 2002",
  "PM2.5 concentration in 2003",
  "PM2.5 concentration in 2004",
  "PM2.5 concentration in 2005",
  "PM2.5 concentration in 2006",
  "PM2.5 concentration in 2007",
  "PM2.5 concentration in 2008",
  "PM2.5 concentration in 2009",
  "PM2.5 concentration in 2010",
  "PM2.5 concentration in 2011",
  "PM2.5 concentration in 2012",
  "PM2.5 concentration in 2013",
  "PM2.5 concentration in 2014",
  "PM2.5 concentration in 2015",
  "PM2.5 concentration in 2016",
  "PM2.5 concentration in 2017",
  "PM2.5 concentration in 2018",
  "PM2.5 concentration in 2019",
  "PM2.5 concentration in 2020",
  "PM2.5 concentration in 2021",
  "Annual mean PM2.5 concentration",
  "Alternate annual mean PM2.5 concentration",
  "Frequency of drought events",
  "Categorical drought frequency",
  "Yield achievement ratio",
  "Nighttime lights mean 2017 (No Gas Flare)",
  "Nighttime lights mean 2018 (No Gas Flare)",
  "Nighttime lights mean 2019 (No Gas Flare)",
  "Nighttime lights mean 2020 (No Gas Flare)",
  "Nighttime lights mean 2021 (No Gas Flare)",
  "Change in nighttime lights (2021 vs 2017, No Gas Flare)",
  "Nighttime lights mean 2017 (Only Gas Flare)",
  "Nighttime lights mean 2018 (Only Gas Flare)",
  "Nighttime lights mean 2019 (Only Gas Flare)",
  "Nighttime lights mean 2020 (Only Gas Flare)",
  "Nighttime lights mean 2021 (Only Gas Flare)",
  "Change in nighttime lights (2021 vs 2017, Only Gas Flare)",
  "Total events in past 2 years",
  "Total events in past 5 years",
  "Total events in past 10 years",
  "ACLED indicator (2-year)",
  "ACLED indicator (5-year)",
  "ACLED indicator (10-year)",
  "Total demonstrations in past 2 years",
  "Total demonstrations in past 5 years",
  "Total demonstrations in past 10 years",
  "Total political violence (2-year)",
  "Total political violence (5-year)",
  "Total political violence (10-year)",
  "Total strategic development (2-year)",
  "Total strategic development (5-year)",
  "Total strategic development (10-year)",
  "Total protests (2-year)",
  "Total protests (5-year)",
  "Total protests (10-year)",
  "Total riots (2-year)",
  "Total riots (5-year)",
  "Total riots (10-year)",
  "Total explosions (2-year)",
  "Total explosions (5-year)",
  "Total explosions (10-year)",
  "Total violence against civilians (2-year)",
  "Total violence against civilians (5-year)",
  "Total violence against civilians (10-year)",
  "Total battles (2-year)",
  "Total battles (5-year)",
  "Total battles (10-year)",
  "Total strategic events (2-year)",
  "Total strategic events (5-year)",
  "Total strategic events (10-year)",
  "Minimum travel time to health facility",
  "Health facility ID",
  "Minimum travel time to educational facility",
  "Educational facility ID",
  "Minimum travel time to markets",
  "Market facility ID",
  "Country code",
  "Subnational identifier",
  "Secondary subnational identifier",
  "Tertiary subnational identifier",
  "Administrative identifier",
  "Share without water access",
  "Share without electricity",
  "Share without education",
  "Share below $2.15/day in 2019",
  "Share below $3.65/day in 2019",
  "Financial access deficit",
  "Share without service provision",
  "Maximum dimension (poverty $2.15)",
  "Maximum dimension (poverty $3.65)",
  "Maximum dimension overall",
  "Dependency on water infrastructure",
  "Dependency on electricity infrastructure",
  "Dependency on education",
  "Poverty status ($2.15/day, 2019)",
  "Poverty status ($3.65/day, 2019)",
  "Dependency on service provision",
  "Dependency on financial resources",
  "Exposure dimension (poverty $2.15)",
  "Exposure dimension (poverty $3.65)",
  "Overall exposure dimension",
  "Exposure dimension (poverty $2.15) category 0",
  "Exposure dimension (poverty $2.15) category 1",
  "Exposure dimension (poverty $2.15) category 2",
  "Exposure dimension (poverty $2.15) category 3",
  "Exposure dimension (poverty $2.15) category 4",
  "Exposure dimension (poverty $2.15) category 5",
  "Exposure dimension (poverty $2.15) category 6",
  "Exposure dimension (poverty $3.65) category 0",
  "Exposure dimension (poverty $3.65) category 1",
  "Exposure dimension (poverty $3.65) category 2",
  "Exposure dimension (poverty $3.65) category 3",
  "Exposure dimension (poverty $3.65) category 4",
  "Exposure dimension (poverty $3.65) category 5",
  "Exposure dimension (poverty $3.65) category 6",
  "Overall exposure dimension category 0",
  "Overall exposure dimension category 1",
  "Overall exposure dimension category 2",
  "Overall exposure dimension category 3",
  "Overall exposure dimension category 4",
  "Overall exposure dimension category 5",
  "Duplicate original area (km2)",
  "Duplicate flag",
  "Original area in km2",
  "Weight for duplicate grid",
  "Population count for duplicate grid",
  "Relative wealth index (duplicate, method 1)",
  "Relative wealth index (duplicate, method 2)",
  "Alternate population count for duplicates",
  "Relative wealth index (population-based)",
  "GSAP poverty ($2.15) ",
  "GSAP poverty ($3.65)",
  "GSAP poverty ($6.85)",
  "Population relative wealth index (GSAP)",
  "Population count (GSAP & RWI",
  "Population count (GSAP)",
  "RWI weight by population (GSAP)",
  "RWI weight ",
  "RWI poverty ($2.15)",
  "RWI poverty ($3.65)",
  "RWI poverty ($6.85)",
  "Count GSAP poverty ($2.15, )",
  "Count GSAP poverty ($3.65, )",
  "Count GSAP poverty ($6.85, )",
  "Count RWI poverty ($2.15, )",
  "Count RWI poverty ($3.65, )",
  "Count RWI poverty ($6.85, )",
  "Population exposed to extreme heat (>=32°C)",
  "Extreme heat exposure (poor, RWI, $2.15)",
  "Extreme heat exposure (poor, GSAP, $2.15)",
  "Extreme heat exposure (poor, RWI, $6.85)",
  "Extreme heat exposure (poor, GSAP, $6.85)",
  "Air pollution exposure (>15 µg/m³)",
  "Air pollution exposure (poor, RWI, $2.15)",
  "Air pollution exposure (poor, GSAP, $2.15)",
  "Air pollution exposure (poor, RWI, $6.85)",
  "Air pollution exposure (poor, GSAP, $6.85)",
  "Exposed to drought (20-year return period)",
  "Population exposed to drought",
  "Drought exposure (poor, RWI, $2.15)",
  "Drought exposure (poor, GSAP, $2.15)",
  "Drought exposure (poor, RWI, $6.85)",
  "Drought exposure (poor, GSAP, $6.85)",
  "Flood risk exposure (poor, RWI, 5-year)",
  "Flood risk exposure (poor, GSAP, 5-year)",
  "Flood risk exposure (poor, RWI, 5-year)",
  "Flood risk exposure (poor, GSAP, 5-year)",
  "Vulnerability share",
  "Total exposed population",
  "Vulnerability (poor, RWI, 042)",
  "Vulnerability (poor, GSAP, 042)",
  "Vulnerability (poor, RWI, 042) for $6.85"
)


# Create the codebook data frame
codebook <- data.frame(Column_Names = col_names,
                       Description = descriptions,
                       stringsAsFactors = FALSE)



# Export the codebook to an Excel file
write_xlsx(codebook, file.path(final_replication,"codebook_grid10km_poor_vul_exposed.xlsx"))


# Export data as csv
write.csv(poor_vul_exposed_without_geom,
          file = file.path(final_replication, "grid_10km_poor_vul_exposed_without_geom.csv"),
          row.names = FALSE)

unique(poor_vul_exposed_without_geom$ISO_A2)

