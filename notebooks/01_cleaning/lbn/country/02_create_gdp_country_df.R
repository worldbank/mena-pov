# ## Create a dataset that includes all 
# 1. GDP indicators (country level)



# Load Data ---------------------------------------------------------------
ntl <- readRDS(file.path(lbn_file_path,
                         "Nighttime_Lights",
                         "final",
                         "lbn_grid_viirs.Rds"))


# Load and process GDP data -----------------------------------------------

# Helper function to load and process the data
load_and_process <- function(dir_path, file_name, column_name) {
  df <- read.csv(file.path(dir_path, "data", "country", "raw", file_name), skip = 3)
  
  df <- df %>%
    clean_names() %>%
    filter(country_name == "Lebanon") %>%
    melt() %>%
    rename("year" = "variable") %>%
    mutate(year = as.numeric(sub("x", "", year)),
           date = as.Date(paste(year, "06", "01", sep = "-"), format = "%Y-%m-%d")) %>%
    filter(year >= 2012) %>%
    rename(!!column_name := value) %>%
    select(column_name,year) %>%
  
  return(df)
}

# Directory path
dir_path <- lbn_onedrive_dir

# Use the helper function for each dataset
gdp_constant <- load_and_process(dir_path, "GDP_constant.csv", "gdp_constant")
gdp_constant_lcu <- load_and_process(dir_path, "GDP_constant_lcu.csv", "gdp_constant_lcu")
gdp_pcap_constant <- load_and_process(dir_path, "GDP_pcap_constant.csv", "gdp_pcap_constant")
gdp_pcap_constant_lcu <- load_and_process(dir_path, "GDP_pcap_constant_lcu.csv", "gdp_pcap_constant_lcu")


# Combine all the datasets ------------------------------------------------
ntl_annual <- ntl %>%
  group_by(year) %>%
  summarise(ntl_mean = mean(avg_rad_df), 
            ntl_median = median(avg_rad_df),
            .groups = "drop")

merged_df <- ntl_annual %>%
  left_join(gdp_constant, by = "year") %>%
  left_join(gdp_constant_lcu, by = "year") %>%
  left_join(gdp_pcap_constant, by = "year") %>%
  left_join(gdp_pcap_constant_lcu, by = "year")

# Export ------------------------------------------------------------------
saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "lbn_gdp_ntl.Rds"))

saveRDS(merged_df, file.path(lbn_onedrive_dir,
                             "data",
                             "country",
                             "final",
                             "lbn_gdp_ntl.Rds"))

write_csv(merged_df, file.path(lbn_onedrive_dir,
                             "data",
                             "country",
                             "final",
                             "lbn_gdp_ntl.csv"))


