# Create datasets that include:
# NTL at District Level


# Load data ---------------------------------------------------------------
r <- stack(file.path(lbn_file_path,
                                 "Nighttime_Lights",
                                 "raw", 
                                 "monthly", 
                                 "lebanon_viirs_raw_monthly_start_201204_end_202303_avg_rad.tif"))

district_sf <- st_read(file.path(lbn_file_path,
                                 "Boundaries",
                                 "lbn_admbnda_adm2_cdr_20200810.shp"))


# Check Projection --------------------------------------------------------

# Check CRS of both
raster_crs <- crs(r)
sf_crs <- st_crs(district_sf)


# Extract -----------------------------------------------------------------


ntl_mean    <- exact_extract(r, district_sf, fun = "mean")
ntl_median  <- exact_extract(r, district_sf, fun = "median")

ntl_mean$admin2Pcod <- district_sf$admin2Pcod
ntl_median$admin2Pcod <- district_sf$admin2Pcod



process_data <- function(my_data, data_name, start_year = 2012, start_month = 4) {
  
  # Melt the data
  data_melted <- my_data %>%
    mutate(uid = 1:nrow(.)) %>%
    melt(id.vars = "admin2Pcod") %>%
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
list_of_dfs <- list(ntl_mean_melted, ntl_median_melted)

# Use reduce() to sequentially merge all data frames in the list
merged_df<- reduce(list_of_dfs, left_join, by = c("admin2Pcod", "month", "year"))


# Extract population ------------------------------------------------------
setwd("M:/LBN/GEO/Population/raw")
rastlist <- list.files(path = "M:/LBN/GEO/Population/raw", pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

#import all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
rastlist

# Extract values ----------------------------------------------------------

# Create an empty list to store the results
district_pop_list <- list()

# Loop through the years and perform extraction
for (i in 1:9) {  # Adjust the range as needed
  year <- 2011 + i
  district_pop_list[[year]] <- exact_extract(allrasters[[i]], district_sf, 'sum')
}


# Convert the list to a data frame
district_pop_2012_2020 <- as.data.frame(do.call(cbind, district_pop_list))
# Name the columns
names(district_pop_2012_2020) <- paste0("district_pop_", 2012:2020)

# Add the uid column
district_pop_2012_2020$admin2Pcod <- district_sf$admin2Pcod

# Melt the dataframe
district_pop_2012_2020_melted <- district_pop_2012_2020 %>%
  pivot_longer(cols = starts_with("district_pop_"), 
               names_to = "year", 
               values_to = "population") %>%
  mutate(year = as.numeric(str_remove(year, "district_pop_")))



## Check against WDI numbers [DO NOT MATCH]
district_pop_2012_2020_melted %>%
  group_by(year) %>%
  summarise(total_pop = sum(population))


# Merge NTL and population ------------------------------------------------
merged_df <- merged_df %>%
  left_join(district_pop_2012_2020_melted, by = c("admin2Pcod","year"))




## Importing Refugee Populations
# Define the file path
file_path <- "M:/LBN/GEO/Team/TeamData/Breakdown of Registered Syrians by Cadaster 2012-Jun 2023.xlsx"

# Get the names of all sheets
all_sheets <- excel_sheets(file_path)

# Assuming that each sheet has the same structure, you can read and bind them together
all_data <- lapply(all_sheets, function(sheet) {
  data <- read_excel(file_path, sheet = sheet)
  
  # Adding a column for the date from the sheet name (assuming your sheet names are exactly in the format "Dec 2012", "Jan 2013" etc.)
  data$Date <- as.Date(paste("01", sheet), format="%d %b %Y")
  
  return(data)
}) %>% bind_rows() %>% 
  group_by(AliasGovernorate,District,Date) %>% 
  reframe(ref_pop = sum(`Registered Syrian Individuals`))


##cleaning the dataset to match the UN OCHA dataset

syrian_pop <- all_data %>%
  filter(AliasGovernorate != "-" & District != "-") %>%
  mutate(AliasGovernorate = case_when(
    AliasGovernorate == "Baalbek Hermel" ~ "Baalbek-El Hermel",
    AliasGovernorate == "Nabatiyeh" ~ "El Nabatieh",
    TRUE ~ AliasGovernorate
  ),
  year = year(Date)
) %>%
  select(-Date) %>%
  arrange(AliasGovernorate,District,year) %>%
  filter(AliasGovernorate != "NULL")




# Prepare data ------------------------------------------------------------
df_annual <- merged_df %>%
  group_by(admin2Pcod,year) %>%
  reframe(ntl_mean = mean(ntl_mean),
          ntl_median = median(ntl_median),
          pop_count = population) %>%
  distinct()%>%
  filter(year <= 2020) %>%
  left_join(district_sf, by = c("admin2Pcod")) %>%
  select(admin2Pcod,admin2Name,admin1Pcod, admin1Name,ntl_mean,ntl_median,pop_count,year) %>%
  left_join(syrian_pop, by = c("admin2Name" = "District","year")) %>%
  select(-AliasGovernorate)






# Plot --------------------------------------------------------------------

# Correlation b/w Percentage Change with NTL and Pop Count (ADM2, 2012 & 2015)

df_annual %>%
  group_by(admin2Pcod) %>%
  filter(year == 2015 | year ==  2012) %>%
  mutate(pop_pct_change_2012_2015 = ((pop_count[year == 2015] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
         ntl_pct_change_2012_2015 = ((ntl_mean[year == 2015] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100) %>%
  ggplot(aes(x = pop_pct_change_2012_2015, y = ntl_pct_change_2012_2015)) +
  geom_point(size = 1)+
  geom_line(size = 1)+
  geom_text(aes(label = admin2Name), nudge_x = 0.5, nudge_y = 0.5, check_overlap = TRUE, size = 2.5) +  # adding district labels
  geom_text(
    aes(label = paste("Correlation =", round(cor(pop_pct_change_2012_2015,ntl_pct_change_2012_2015), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  labs( x = "Percentage Change in Syrian Population Count (2012 & 2015)",
        y = "Percentage Change in Nighttime Lights (2012 & 2015)",
        title = "Comparing the Percentage Change in NTL and Syrian Population Count (2012 & 2015),\n(District/ADM2)") +
  theme_classic2()





# # Export ------------------------------------------------------------------
# saveRDS(merged_df, file.path(lbn_file_path,
#                              "Nighttime_Lights",
#                              "final",
#                              "lbn_distict_ntl_pop.Rds"))
write.csv(df_annual,file.path(lbn_onedrive_dir,
                              "data",
                              "ntl_pop_district.csv"))
