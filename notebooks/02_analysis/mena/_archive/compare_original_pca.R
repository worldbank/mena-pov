## Compare the original method and the PCA approach


# Load Data ---------------------------------------------------------------

vul_indicators <- read.csv(file.path(mena_file_path,
                                         "csv_files_all_layers",
                                         "Vul_Indicators_ADM2_original.csv")) %>% as.data.frame()

vul_pca <- read.csv("C:/Users/wb569257/OneDrive - WBG/WoE/Vulnerability/data/Vul_PCA.csv")

MENA_adm <- st_read(file.path(mena_file_path,
                              "Boundaries",
                              "MENA_ADM2.shp"))

MENA_adm0 <- st_read(file.path(mena_file_path,
                               "Boundaries",
                               "MENA_ADM0.shp"))



# Compute original Vulnerability Index ------------------------------------
vul_sub <- vul_indicators %>%
  select(CO2_mean_2020,NO2_mean_2020,pm25_mean_2019,Loss_sqkm_2016_2020,PRECIP_DIFF_2016_2020,TEMP_DIFF_2016_2020,
         CDI_2020_mean,QUAKE_2016_2020,FLOOD_2016_2020,ID_ADM,NTL_mean_2020,RWI_mean,road_raw,
         POP_mean_2020) %>%
  drop_na() %>%
  mutate(across(-c(ID_ADM,POP_mean_2020), normalize)) %>%
  mutate(POP_mean_2020_log_norm = normalize(log(POP_mean_2020)),
         exposure_ind = normalize(CO2_mean_2020 +NO2_mean_2020 + pm25_mean_2019 +
                                    Loss_sqkm_2016_2020 + PRECIP_DIFF_2016_2020 +
                                    TEMP_DIFF_2016_2020 + CDI_2020_mean +
                                    QUAKE_2016_2020 + FLOOD_2016_2020),
         VulPopSh_2016_2020_original = (exposure_ind - NTL_mean_2020 - road_raw - RWI_mean)*POP_mean_2020_log_norm,
         VulPopSh_2016_2020_original = normalize(VulPopSh_2016_2020_original))
  


#combine shape and CombinedX_PC1
vul_sub_original <- left_join(MENA_adm,vul_sub, by = "ID_ADM")



# Create natural breaks for the original data -----------------------------
# Specify the number of desired breaks
num_breaks_original <- 5

# Apply Jenks natural breaks classification
jenks_classes_original <- classIntervals(vul_sub_original$VulPopSh_2016_2020_original, n = num_breaks_original, style = "jenks")

# Extract the breakpoints
breakpoints_original <- jenks_classes_original$brks

# View the resulting breakpoints
print(breakpoints_original)

# Create the categorical variable
vul_sub_original$VI_quintiles_original <- cut(vul_sub_original$VulPopSh_2016_2020, breaks = breakpoints_original, 
                                     labels = c("Lowest", "Quintile 2", "Quintile 3", "Quintile 4", "Highest"))




# Map VI original ---------------------------------------------------------
library(ggplot2)
library(RColorBrewer)


# Generate colors using the PuOr palette
colors <- rev(brewer.pal(5, "PuOr"))

map_original <- ggplot() +
  geom_sf(data = vul_sub_original, aes(fill = VI_quintiles_original),
          linewidth = 0.05, alpha = 0.8, color = "black") +
  theme_classic() +
  theme(text = element_text(color = "#22211d"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = "right") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Vulnerability Index adjusted for Population Share (2020)",
       subtitle = "without PCA") +
  geom_sf(data = MENA_adm0, fill = NA,
          linewidth = 0.2, alpha = 0.8, color = "blue") +
  scale_fill_manual(name = "Vulnerability",
                    values = colors,
                    na.value = "grey",
                    drop = TRUE,
                    guide = guide_legend(
                      keyheight = unit(10, units = "mm"),
                      keywidth = unit(15, units = "mm"),
                      title.position = "top",
                      reverse = FALSE,
                      title.hjust = 0.5,
                      label.hjust = 0.5))

map_original



ggsave(map_original, filename = "C:/Users/wb569257/OneDrive - WBG/WoE/Vulnerability/output/figures/vul_original.png", height= 10, width = 16, units = c("in"))




# Map Vulnerability with PCA  ---------------------------------------------
#combine shape and vul_sub_original
vul_sub_pca <- left_join(MENA_adm,vul_pca, by = "ID_ADM")


# Generate colors using the PuOr palette
colors <- rev(brewer.pal(5, "PuOr"))

vul_sub_pca$VI_quintiles <- factor(vul_sub_pca$VI_quintiles, levels = c("Lowest","Quintile 2", "Quintile 3",
                                                                        "Quintile 4", "Highest"))


map_pca <- ggplot() +
  geom_sf(data = vul_sub_pca, aes(fill = VI_quintiles),
          linewidth = 0.05, alpha = 0.8, color = "black") +
  theme_classic() +
  theme(text = element_text(color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = "right") + 
  labs(x = NULL, 
       y = NULL, 
       title = "Vulnerability Index adjusted for Population Share (2020)",
       subtitle = "With PCA") +
  geom_sf(data = MENA_adm0, fill = NA,
          linewidth = 0.2, alpha = 0.8, color = "blue") +
  scale_fill_manual(name = "Vulnerability",
                    values = colors,
                    na.value = "grey",
                    drop = TRUE,
                    guide = guide_legend(
                      keyheight = unit(10, units = "mm"),
                      keywidth = unit(15, units = "mm"),
                      title.position = "top",
                      reverse = FALSE,
                      title.hjust = 0.5,
                      label.hjust = 0.5))
map_pca
ggsave(map_pca, filename = "C:/Users/wb569257/OneDrive - WBG/WoE/Vulnerability/output/figures/vul_pca.png", height= 10, width = 16, units = c("in"))


# Create ranking by country for original and pca VI -----------------------


# Plot

vul_sub_original_bycountry <- vul_sub_original %>%
  st_make_valid() %>%
  group_by(WB_ADM0_NA) %>%
  summarise(VulPopSh_2016_2020_original_mean = mean(VulPopSh_2016_2020_original, na.rm = T)) %>%
  drop_na() %>%
  mutate(rank_original = dense_rank(desc(VulPopSh_2016_2020_original_mean)))

# First plot
plot1 <- ggplot(vul_sub_original_bycountry, aes(x = reorder(WB_ADM0_NA, VulPopSh_2016_2020_original_mean), y = VulPopSh_2016_2020_original_mean)) +
  geom_bar(stat = "identity", fill = "maroon") +
  coord_flip() +
  ylim(0, 2) +
  labs(x = "Ranking", y = "Vulnerability Index (VI)", title = "Country Vulnerability Rankings (Without PCA)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),axis.text = element_text(size=12,face= "bold"))

plot1
# Second plot
vul_sub_pca_bycountry <- vul_sub_pca %>%
  st_make_valid() %>%
  group_by(WB_ADM0_NA) %>%
  summarise(VulPopSh_2016_2020_PC1_mean = mean(VulPopSh_2016_2020_PC1, na.rm = TRUE)) %>%
  drop_na() %>%
  mutate(rank_pca = dense_rank(desc(VulPopSh_2016_2020_PC1_mean))) 

plot2 <- ggplot(vul_sub_pca_bycountry, aes(x = reorder(WB_ADM0_NA,VulPopSh_2016_2020_PC1_mean), y = VulPopSh_2016_2020_PC1_mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ylim(0, 2) +
  labs(x = "Ranking", y = "Vulnerability Index (VI)", title = "Country Vulnerability Rankings (With PCA)") +
  theme_minimal() +
  theme(panel.grid = element_blank(),axis.text = element_text(size=12, face= "bold"))

plot2
# Compute the change in PCA rank
vul_sub_original_bycountry$rank_pca <- vul_sub_pca_bycountry$rank_pca

vul_sub_original_bycountry <- vul_sub_original_bycountry %>%
  mutate(rank_diff = rank_original - rank_pca,
         change_va = ifelse(rank_diff < 0 , "Decreased in Vulnerability", "Increased in Vulnerability"),
         change_va_final = ifelse(rank_diff == 0, "No Change", change_va))

# Compute the maximum absolute rank difference
max_abs_rank_diff <- max(abs(vul_sub_original_bycountry$rank_diff))

plot3 <- ggplot(vul_sub_original_bycountry, aes(x = reorder(WB_ADM0_NA, rank_diff), y = rank_diff, fill = change_va)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "", y = "Rank Difference", title = "Change in Vulnerability Rankings", fill = "Change in VI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), axis.text = element_text(size=12, face= "bold"))

# Combine plots into one panel
combined_plot <- grid.arrange(plot1, plot2,plot3, ncol = 3)



# Scatter Plot ------------------------------------------------------------

scatter_df <- vul_sub_original_bycountry %>%
  select(WB_ADM0_NA, rank_pca, rank_original, VulPopSh_2016_2020_original_mean)

scatter_df$VulPopSh_2016_2020_PC1_mean <- vul_sub_pca_bycountry$VulPopSh_2016_2020_PC1_mean


# Plot

scatter_plot <- ggplot() +
  geom_point(data = scatter_df, aes(x = VulPopSh_2016_2020_PC1_mean, y = reorder(WB_ADM0_NA, rank_pca)), color = "steelblue") +
  geom_text_repel(data = scatter_df, aes(x = VulPopSh_2016_2020_PC1_mean, y = reorder(WB_ADM0_NA, rank_pca), label = WB_ADM0_NA, color = "steelblue"), size = 3.5) +
  geom_point(data = scatter_df, aes(x = VulPopSh_2016_2020_original_mean, y = reorder(WB_ADM0_NA, rank_original)), color = "maroon") +
  geom_text_repel(data = scatter_df, aes(x = VulPopSh_2016_2020_original_mean, y = reorder(WB_ADM0_NA, rank_original), label = WB_ADM0_NA, color = "maroon"), size = 3.5) +
  labs(x = "Vulnerability PCA", y = "", title = "", color = "Approach") +
  scale_color_manual(values = c("maroon" = "maroon","steelblue" = "steelblue",), labels = c("Original", "PCA")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line())

scatter_plot































