# ================================================================
# Script Name: 00_MASTER.R
# Purpose: Master script to run all other scripts in a specific order
# Input Dataset: NA
# Output Dataset: NA
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# Define the path to the directory containing the scripts
script_path <- "C:/Users/wb569257/OneDrive - WBG/Documents/GitHub/mena-vulnerability/replication_package"


#After running the "00_SET_PATHS.R" script to set the locals
# Source each script in the specified order
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "01_CREATE_GRID.R"))


## HAZARDS
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_AIR_POL.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_DROUGHT.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_FLOOD.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_HEAT_STRESS.R"))


#SOCIO-ECONOMIC CHARACTERISTICS
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_POP.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_RWI.R"))


## VULNERABILITY DIMENSIONS
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_AIRPORTS.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_PORTS.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_CITY_CENTERS.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_CONFLICT.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_CROP_YIELD.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_EDUC_FACILITIES.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_HEALTH_FACILITIES.R"))
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02_EXTRACT_FOREST_LOSS.R"))

#Create indicator for non-oil and oil economic activity
source(file.path(script_path,"DataWork","01_creating_analysis_ready_datasets", "02a_CLEAN_GAS_FLARES.R"))

message("All scripts executed successfully.")
