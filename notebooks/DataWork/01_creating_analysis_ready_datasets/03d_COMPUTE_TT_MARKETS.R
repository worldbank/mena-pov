# ================================================================
# Script Name: 03b_COMPUTE_TT_MARKETS.R
# Purpose: Compute the travel time by car to our definition of markets
# Input Dataset:mena_markets.Rds, grid_10km.Rds,MENA_ADM0.shp
# Output Dataset: "final_output_", COUNTRY_NAME, ".csv"
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-09
# ================================================================




#Note: Update list once the code is completed for a specific country
# Countries Completed: Bahrain, Djibouti, Arab Republic of Egypt, Republic of Yemen,Saudi Arabia, Oman, United Arab Emirates,
# Qatar,Kuwait,Israel,West Bank and Gaza,Lebanon, Morocco, Libya, Iraq,Tunisia, Jordan, Syrian Arab Republic, Algeria,
#Islamic Republic of Iran


# Load Data ---------------------------------------------------------------
markets_fac <- readRDS(file.path(final_replication,"mena_markets.Rds"))
grid_sf <- readRDS(file.path(final_replication,"grid_10km.Rds"))
grid_sf <- st_transform(grid_sf, 4326)
mena_adm1 <- st_read(file.path(final_replication, "MENA_ADM0.shp"))



# Create a temporary folder -----------------------------------------------

### ENTER COUNTRY NAME HERE #####
COUNTRY_NAME <- "Islamic Republic of Iran"
# Define the directory path -- creates a folder to store results from parallel processing in network drive
output_dir <- paste0("M:/MENA/GEO/OpenStreetMap/temp/markets/", COUNTRY_NAME) # Replace Path
# Attempt to create the directory with recursive = TRUE
dir.create(output_dir, recursive = TRUE)

# Check if the directory was created successfully
if (dir.exists(output_dir)) {
  print(paste("Directory created at:", output_dir))
} else {
  print("Failed to create directory. Please check the path and network drive availability.")
}




# Functions ---------------------------------------------------------------

# Function to split dataframe into chunks
split_into_chunks <- function(df, chunk_size) {
  split(df, ceiling(seq_len(nrow(df)) / chunk_size))
}



# Function to compute travel time
process_source <-
  function(source_index,
           grid_sf_country,
           destination_chunks,
           output_dir) {
    results <-
      data.frame()  # Initialize an empty data frame to store results
    print(paste("Processing source index:", source_index, "\n"))
    
    source <- grid_sf_country[source_index,]
    overall_min_time <- Inf
    overall_closest_dest_id <- NA  # Initialize with NA
    
    for (chunk in destination_chunks) {
      travel_times <- osrmTable(
        src = source[, c('longitude', 'latitude')],
        dst = chunk[, c('longitude', 'latitude')],
        measure = "duration",
        osrm.profile = "car"
      )$durations
      
      if (length(travel_times) > 0 && all(!is.na(travel_times))) {
        min_time <- min(travel_times, na.rm = TRUE)
        closest_dest_index <- which.min(travel_times)
        
        if (is.finite(min_time) && min_time < overall_min_time) {
          overall_min_time <- min_time
          overall_closest_dest_id <-
            chunk$osm_id[closest_dest_index]
        }
      }
    }
    
    results <-
      rbind(results,
            c(source$grid_id, overall_min_time, overall_closest_dest_id))
    
    # Create a unique file name for the output
    output_file <-
      paste0(output_dir, "/result_",source$grid_id , ".csv")
    
    # Write the results to the CSV file
    write.csv(results, file = output_file, row.names = FALSE)
    
    return(output_file)
  }





# Loop by country ---------------------------------------------------------
#Wherever an "i" replace with country name


# Source - Grid Cells
# Ensure IDs are retained in the dataframes
market_fac_country <- markets_fac %>%
  dplyr::filter(country == COUNTRY_NAME) %>%
  dplyr::select(osm_id, geometry) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  cbind(markets_fac$osm_id[markets_fac$country == COUNTRY_NAME]) %>%
  setNames(c("longitude", "latitude", "osm_id"))



# Destination - Health Facilities
grid_sf_country <- grid_sf %>%
  filter(WB_ADM0_NA == COUNTRY_NAME) %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  cbind(grid_sf$grid_id[grid_sf$WB_ADM0_NA == COUNTRY_NAME]) %>%
  setNames(c("longitude", "latitude", "grid_id"))


#Note: For Algeria and Iran to break up the grids since the code is computationally intensive
#For Algeria 
# grid_sf_country_1 <- grid_sf_country[1:16000,]
# grid_sf_country_2 <- grid_sf_country[16001:33863,]

#Iran -- break the grid into two before running it
# grid_sf_country_1 <- grid_sf_country[1:10000,]
# grid_sf_country_2 <- grid_sf_country[10001:22463,]

# Split data into chunks due to API limitations
chunk_size <- 100  # Adjust this based on your needs and API limitations
# Split destinations into chunks
destination_chunks <-
  split_into_chunks(market_fac_country, chunk_size)

# Run the scripts using parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Load necessary libraries in each worker
clusterEvalQ(cl, {
  library(osrm)
})


#Exporting the relevant datasets
clusterExport(
  cl,
  varlist = c(
    "split_into_chunks",
    "process_source",
    "grid_sf_country",
    "destination_chunks",
    "output_dir"
  )
) #Change grid name for Algeria and Iran


source_indices <- 1:nrow(grid_sf_country) #Change grid name for Algeria and Iran

# Run the parallel process
output_files <- parLapply(cl, source_indices, function(index) {
  process_source(index, grid_sf_country, destination_chunks, output_dir) #Change grid name for Algeria and Iran
})



stopCluster(cl)






# Combine CSVs ------------------------------------------------------------
folder_path <- "M:/MENA/GEO/OpenStreetMap/temp/markets/Algeria"
#List all CSV files in the specified directory
output_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)
all_results <- lapply(output_files,fread)


# Function to rename columns
rename_columns <- function(df) {
  colnames(df) <- c("grid_id", "min_tt", "facility_osm_id")
  return(df)
}

# Applying the function to all data frames in the list
all_results <- lapply(all_results, rename_columns)
tt_results <- do.call(rbind, all_results)










# Export ------------------------------------------------------------------
output_file_name <- file.path(raw_replication,"OSM", "intermediate", "markets", paste0("final_output_", COUNTRY_NAME, ".csv")) #INSERT COUNTRY NAME & Replace Path
write.csv(tt_results, output_file_name, row.names = FALSE)



