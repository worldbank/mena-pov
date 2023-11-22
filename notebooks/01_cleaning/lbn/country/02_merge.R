# Merge VIIRS and DMSP
# Add the adjustment factor for comparison with VIIRS


# Load data ---------------------------------------------------------------

viirs <- readRDS(file.path(lbn_file_path,
                           "Nighttime_Lights",
                           "final",
                           "viirs_2012_2023.Rds"))

dmsp <- readRDS(file.path(lbn_file_path,
                          "Nighttime_Lights",
                          "final",
                          "ntl_dmsp_2011.Rds"))

pop <- readRDS(file.path(lbn_file_path,
                         "Population",
                         "final",
                         "pop_2000_2023.Rds"))


# Merge -------------------------------------------------------------------

merged_df <- rbind(dmsp,viirs)

#merge population
merged_df <- merge(merged_df,pop, by = c("GID_0","year"))

#create uid
merged_df$uid <- 1:nrow(merged_df)




# Export ------------------------------------------------------------------
saveRDS(merged_df, file.path(lbn_file_path,
                             "Nighttime_Lights",
                             "final",
                             "ntl_2011_2023.Rds"))

write.csv(merged_df,file.path(lbn_file_path,
                              "Nighttime_Lights",
                              "final",
                              "ntl_2011_2023.csv"))

