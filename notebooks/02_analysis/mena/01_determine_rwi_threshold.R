

# Load Data ---------------------------------------------------------------
grid_10km <- readRDS(file.path(mena_file_path,"Allsources","grid_10km_all.Rds"))





# Step 1: Create thresholds and est. poor for each threshold --------------
grid_rwi <- grid_10km %>%
  select(grid_id,WB_ADM1_NA,WB_ADM1_CO,WB_ADM0_NA,rwi_all,pop_count,pop_density)



# Create threshold sequence
thresholds <- seq(-1.5, 1.5, by = 0.001)
thresholds <- round(thresholds,4)


# Create rwi threshold columns
for (thresh in thresholds) {
  new_col_name <- paste0("rwi_poor", thresh)
  
  # Create binary threshold column
  grid_rwi[[new_col_name]] <- ifelse(grid_rwi$rwi_all > thresh, 0, 1)
  
  
  # Multiply by pop_count
  grid_rwi[[new_col_name]] <- grid_rwi[[new_col_name]] * grid_rwi$pop_count
  
}



# Step 2: Merge data with GSAP Poverty rates("red polygon!")--------------

# Import crosswalk prepared by Sandra in ArcGIS
rwi_crosswalk <- st_read(file.path(mena_file_path,"RWICrosswalk", "grid_10km_all_gsap_union.shp")) %>% 
  filter(region == "MNA" & grid_id !=0) %>%
  select(grid_id,id,surveyd,region,code,g_cd2_n)
  


# Add the poverty rate data from the excel sheet, since the shapefiles did not include it
gsap_excel <- read_excel(file.path(local_dir,"MENA","Poverty","global-subnational-poverty-gsap-2019-data.xlsx")) %>%
  filter(region == "MNA")


rwi_crosswalk_merged <- left_join(rwi_crosswalk,gsap_excel, by = c("g_cd2_n"="geo_code2_new"))
rwi_crosswalk_merged <- rwi_crosswalk_merged %>% 
  select(grid_id,id,g_cd2_n,poor215_ln,poor365_ln,poor685_ln)

# drop geometry from grid to avoid using st_join
grid_rwi_df <- grid_rwi %>% st_drop_geometry()



# merge GSAP/Our data crosswalk with the grid
gsap_grid_merged <- rwi_crosswalk_merged %>% 
  left_join(grid_rwi_df, by = c("grid_id"))



# compute the area of each grid cell


# Step 3: Compute share of poor at ADM1 level -----------------------------

# Create threshold column names
threshold_cols <- paste0("rwi_poor", thresholds)



# Summarise at ADM1 level
grid_adm1 <- gsap_grid_merged %>%
  group_by(g_cd2_n,WB_ADM1_NA) %>%
  summarise(
    across(all_of(threshold_cols), ~sum(., na.rm = TRUE)/sum(pop_count, na.rm = TRUE), .names = "share_{.col}"),
    poor215_ln = mean(poor215_ln),
    poor365_ln = mean(poor365_ln),
    poor685_ln = mean(poor685_ln)) %>%
  ungroup()







# Step 4: Compute difference between RWI and poverty rate -----------------

threshold_share_cols <-  paste0("share_", threshold_cols)

grid_adm1 <- grid_adm1 %>%
  mutate(across(all_of(threshold_share_cols), 
                ~ . - poor215_ln, 
                .names = "diff_{.col}"))


grid_adm1_error <- grid_adm1 %>%
  rowwise() %>%
  mutate(
    abs_min_diff = min(abs(c_across(starts_with("diff_"))), na.rm = TRUE),
    min_col = {
      vals = c_across(starts_with("diff_share_rwi_poor"))
      if (all(is.na(vals))) {
        NA_character_   # or some default value like "NoData"
      } else {
        names_ = names(select(., starts_with("diff_share_rwi_poor")))
        names_[which.min(abs(vals))]
      }
    }
  ) %>%
  ungroup()




# Create histograms of errors and thresholds ------------------------------

# First plot
p1 <- ggplot(grid_adm1_error, aes(x=abs_min_diff)) + 
  geom_histogram() +
  xlim(-0.5,1)+
  theme_minimal() +
  ggtitle("Error Distribution") +
  theme_classic2()


# Extract the numeric portion for the x-axis labels
grid_adm1_error$label_num <- str_extract(grid_adm1_error$min_col, "(?<=diff_share_rwi_poor)-?\\d+\\.?\\d*")

# Calculate the frequencies
grid_adm1_error <- grid_adm1_error %>%
  mutate(freq = as.numeric(table(label_num)[label_num]))



# Second plot
p2 <- ggplot(grid_adm1_error, aes(x = reorder(label_num, freq))) + 
  geom_bar() +
  labs(x = "Thresholds (Relative Wealth Index)") +
  theme_minimal() +
  ggtitle("Distribution of Relative Wealth Index (RWI) \nThresholds Based on Minimum Errors") +
  theme_classic2()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8))  # Rotate x-axis labels



grid.arrange(p1, p2, ncol=2)










