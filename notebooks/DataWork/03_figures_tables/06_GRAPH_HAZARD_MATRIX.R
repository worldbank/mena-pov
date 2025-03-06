# ================================================================
# Script Name: 06_GRAPH_HAZARD_MATRIX
# Purpose: Create an overlapping hazard matrix
# Input Dataset:grid_10km_poor_215_685.Rds,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================



# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication, "grid_10km_poor_215_685.Rds"))


grid_people <-readRDS(file.path(final_replication, "grid_10km_people.Rds"))



# Reshape -----------------------------------------------------------------


people_exposed <- grid_people %>%
  dplyr::select(ISO_A2,starts_with("exposed"), grid_pop_flood_expected,pop_count)


people_exposed_summ <- people_exposed %>%
  group_by(ISO_A2) %>%
  dplyr::summarize(tot_exposed_heat = sum(exposed_heat32_pop, na.rm = T),
                   tot_exposed_drought = sum(exposed_drought20_pop, na.rm = T),
                   tot_exposed_airpol = sum(exposed_airpol15_pop, na.rm = T),
                   tot_exposed_flood = sum(grid_pop_flood_expected,na.rm = T),
                   tot_pop = sum(pop_count, na.rm = T)) %>% 
  st_drop_geometry()

#create share of population exposed var
people_exposed_summ <- people_exposed_summ %>%
  dplyr::mutate(share_exposed_heat = tot_exposed_heat/tot_pop,
                share_exposed_drought = tot_exposed_drought/tot_pop,
                share_exposed_airpol = tot_exposed_airpol/tot_pop,
                share_exposed_flood = tot_exposed_flood/tot_pop)





# Categorize the exposure levels
data <- people_exposed_summ %>%
  mutate(
    cat_exposed_heat = cut(share_exposed_heat, breaks = c(-1, 0.3, 0.5, 1), labels = c("0-0.3", "0.3-0.5", "0.5-1")),
    cat_exposed_drought = cut(share_exposed_drought, breaks = c(-1, 0.3, 0.5, 1), labels = c("0-0.3", "0.3-0.5", "0.5-1")),
    cat_exposed_airpol = cut(share_exposed_airpol, breaks = c(-1, 0.3, 0.5, 1), labels = c("0-0.3", "0.3-0.5", "0.5-1")),
    cat_exposed_flood = cut(share_exposed_flood, breaks = c(-1, 0.3, 0.5, 1), labels = c("0-0.3", "0.3-0.5", "0.5-1"))
  )

# Transform data into long format
data_melt <- melt(data, id.vars = "ISO_A2", measure.vars = c("cat_exposed_heat", "cat_exposed_drought", "cat_exposed_airpol", "cat_exposed_flood"),
                  variable.name = "Hazard", value.name = "Exposure")

# Adjust the Hazard names
data_melt$Hazard <- gsub("cat_exposed_", "", data_melt$Hazard)
data_melt$Hazard <- factor(data_melt$Hazard, levels = c("airpol", "drought","heat","flood"))



# Define a scoring function based on Exposure levels
score_exposure <- function(exposure) {
  if (exposure == "0-0.3") return(1)
  if (exposure == "0.3-0.5") return(2)
  if (exposure == "0.5-1") return(3)
  return(0)
}

# Calculate the score for each country
data_melt <- data_melt %>%
  mutate(Score = sapply(Exposure, score_exposure)) %>%
  group_by(ISO_A2) %>%
  summarise(Total_Score = sum(Score)) %>%
  arrange(desc(Total_Score)) %>%
  left_join(data_melt, by = "ISO_A2")

# Reorder ISO_A2 based on Total_Score
data_melt$ISO_A2 <- factor(data_melt$ISO_A2, levels = unique(data_melt$ISO_A2[order(data_melt$Total_Score, decreasing = F)]))







# Plot --------------------------------------------------------------------
# Create the matrix chart with Polychrome "dreaming" palette colors
ggplot(data_melt, aes(x = Hazard, y = ISO_A2)) +
  geom_tile(aes(fill = Exposure), color = "white") +
  scale_fill_manual(values = c("0-0.3" = "#F1D302", "0.3-0.5" = "#D97E18", "0.5-1" = "#C1292E"), 
                    name = "Share of \nPopulation Exposed") +
  labs(title = "", x = "Hazard", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank() )  # Remove minor grid lines)

ggsave(filename = file.path(graphs,"hazard_matrix.png"), height = 10, width = 8 , units = c("in") )
