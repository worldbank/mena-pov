# ================================================================
# Script Name: 06_GRAPH_POOR_EXPOSED_ALLHAZARDS_COUNTRY
# Purpose: Create a table of exposed poor
# Input Dataset:grid_10km_poor_215_685.Rds,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication, "grid_10km_poor_215_685.Rds"))


grid_people <-readRDS(file.path(final_replication, "grid_10km_people.Rds"))





#############################################################################
############################## COUNTRY - LEVEL ##############################
#############################################################################

# Subset ----------------------------------------------------------

people_exposed <- grid_poor %>%
  dplyr::select(ISO_A2,grid_id,starts_with("exposed"), grid_pop_flood_expected,pop_count,starts_with("poor")) %>%
  filter(!ISO_A2 %in% c("YE","IQ","IR","PS","SY", "SA","LY"))

names(people_exposed)
unique(people_exposed$ISO_A2)

# Compute exposure (3.65 & 6.85) ------------------------------------------

people_exposed <- people_exposed %>%
  dplyr::mutate(exposed_heat32_poor365_rwi = exposed_heat32*poor365_count_rwi_dup,
                exposed_heat32_poor685_rwi = exposed_heat32*poor685_count_rwi_dup,
                
                exposed_drought20_poor365_rwi = exposed_drought20*poor365_count_rwi_dup,
                exposed_drought20_poor685_rwi = exposed_drought20*poor685_count_rwi_dup,
                
                exposed_airpol15_poor365_rwi = exposed_airpol15*poor365_count_rwi_dup,
                exposed_airpol15_poor685_rwi = exposed_airpol15*poor685_count_rwi_dup,
                
                exposed_flood5_poor365_rwi = grid_pop_flood_expected*poor365_rwi_dup,
                exposed_flood5_poor685_rwi = grid_pop_flood_expected*poor685_rwi_dup)


names(people_exposed)

people_exposed_summ <- people_exposed %>%
  group_by(ISO_A2) %>%
  #Heat
  dplyr::summarize(tot_exposed_heat_215 = sum(exposed_heat32_poor215_rwi, na.rm = T),
                   tot_exposed_heat_365 = sum(exposed_heat32_poor365_rwi, na.rm = T),
                   tot_exposed_heat_685 = sum(exposed_heat32_poor685_rwi, na.rm = T),
                   
                   #Drought 
                   tot_exposed_drought_215 = sum(exposed_drought20_poor215_rwi, na.rm = T),
                   tot_exposed_drought_365 = sum(exposed_drought20_poor365_rwi, na.rm = T),
                   tot_exposed_drought_685 = sum(exposed_drought20_poor685_rwi, na.rm = T),
                   
                   #Air Pollution
                   tot_exposed_airpol_215 = sum(exposed_airpol15_poor215_rwi, na.rm = T),
                   tot_exposed_airpol_365 = sum(exposed_airpol15_poor365_rwi, na.rm = T),
                   tot_exposed_airpol_685 = sum(exposed_airpol15_poor685_rwi, na.rm = T),
                   
                   #Flood
                   tot_exposed_flood_215 = sum(exposed_flood5_poor215_rwi,na.rm = T),
                   tot_exposed_flood_365 = sum(exposed_flood5_poor365_rwi,na.rm = T),
                   tot_exposed_flood_685 = sum(exposed_flood5_poor685_rwi,na.rm = T),
                   tot_pop = sum(pop_count, na.rm = T)) %>% 
  st_drop_geometry()

unique(people_exposed_summ$ISO_A2)
#create share of population exposed var
people_exposed_summ <- people_exposed_summ %>%
  #Heat
  dplyr::mutate(share_exposed_heat_215 = tot_exposed_heat_215/tot_pop,
                share_exposed_heat_365 = tot_exposed_heat_365/tot_pop,
                share_exposed_heat_685 = tot_exposed_heat_685/tot_pop,
                
                #Drought
                share_exposed_drought_215 = tot_exposed_drought_215/tot_pop,
                share_exposed_drought_365 = tot_exposed_drought_365/tot_pop,
                share_exposed_drought_685 = tot_exposed_drought_685/tot_pop,
                
                #Air Pol
                share_exposed_airpol_215 = tot_exposed_airpol_215/tot_pop,
                share_exposed_airpol_365 = tot_exposed_airpol_365/tot_pop,
                share_exposed_airpol_685 = tot_exposed_airpol_685/tot_pop,
                
                #Flood
                share_exposed_flood_215 = tot_exposed_flood_215/tot_pop,
                share_exposed_flood_365 = tot_exposed_flood_365/tot_pop,
                share_exposed_flood_685 = tot_exposed_flood_685/tot_pop)



# Reshape the data to long format
data_long <- people_exposed_summ %>%
  pivot_longer(
    cols = starts_with("tot_exposed_") | starts_with("share_exposed_"),
    names_to = c(".value", "hazard", "pov_ln"),
    names_pattern = "(tot_exposed|share_exposed)_(.*)_(.*)"
  ) %>%
  mutate(hazard = case_when(
    hazard == "heat" ~ "Heat Exposure[Extreme Heat Stress >32°C]",
    hazard == "drought" ~ "Drought Exposure[Severe Drought Occurence >20%]",
    hazard == "airpol" ~ "Air Pollution Exposure[PM2.5 >15µg/m³]",
    hazard == "flood" ~ "Flood Risk[Water Depth >0.5m]"
  ),
  pov_ln = case_when(
    pov_ln == 215 ~ "$2.15/day",
    pov_ln == 365 ~ "$3.65/day",
    pov_ln == 685 ~ "$6.85/day"
  ))



#check countries by total exposure as number of exposed instead of share
test <- data_long %>%
  filter(pov_ln == "$6.85/day") %>%
  group_by(ISO_A2, hazard) %>%
  summarise(total_exposure = sum(tot_exposed, na.rm = T)) %>%
  filter(total_exposure > 0) %>%
  ungroup() %>%
  dplyr::select(ISO_A2, hazard, total_exposure) %>%
  distinct()





# Create Graph ------------------------------------------------------------
# Step 1: Filter out countries with zero exposure for all poverty lines for each hazard
filtered_data_long <- data_long %>%
  group_by(ISO_A2, hazard) %>%
  summarise(total_exposure = sum(share_exposed)) %>%
  filter(total_exposure > 0) %>%
  ungroup() %>%
  dplyr::select(ISO_A2, hazard) %>%
  distinct()

data_long_filtered <- data_long %>%
  semi_join(filtered_data_long, by = c("ISO_A2", "hazard"))

# Step 2: List of dropped countries for each hazard
dropped_countries <- data_long %>%
  group_by(ISO_A2, hazard) %>%
  summarise(total_exposure = sum(share_exposed)) %>%
  filter(total_exposure <0.001) %>%
  dplyr::select(ISO_A2, hazard) %>%
  distinct()

dropped_note <- dropped_countries %>%
  group_by(hazard) %>%
  summarise(countries = paste(ISO_A2, collapse = ", ")) %>%
  mutate(note = paste("No Exposure for Poor:", countries))

print(dropped_note$note)


# Step 2: Drop countries explicitly based on the note for each hazard
drop_countries_for_hazard <- function(data, dropped_note) {
  for (i in 1:nrow(dropped_note)) {
    hazard_name <- dropped_note$hazard[i]
    countries_to_drop <- unlist(strsplit(dropped_note$countries[i], ", "))
    data <- data %>% filter(!(hazard == hazard_name & ISO_A2 %in% countries_to_drop))
  }
  return(data)
}

data_long_filtered <- drop_countries_for_hazard(data_long_filtered, dropped_note)

# Step 3: Update the plotting function to use the filtered data and add the note
plot_exposure <- function(data_long_filtered, hazard_name, title, fill_colors, note) {
  data_6.85 <- data_long_filtered %>% filter(pov_ln == "$6.85/day" & hazard == hazard_name)
  data_6.85 <- data_6.85 %>% arrange(hazard, desc(share_exposed))
  country_order <- data_6.85 %>% pull(ISO_A2)
  
  new_order <- data_long_filtered %>%
    filter(hazard == hazard_name) %>%
    mutate(ISO_A2 = factor(ISO_A2, levels = rev(country_order)))
  
  plot <- ggplot(new_order, aes(x = ISO_A2, y = share_exposed, fill = pov_ln)) +
    geom_col(position = "dodge", width = 0.9) +
    labs(title = title, x = "", y = "Share of Total Population, Exposed and Poor", caption = note) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(angle = 0, hjust = 1, size = 12),
      axis.text.x = element_text(),
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      legend.position = "top",  # Move legend to the top
      legend.direction = "horizontal",  # Set legend direction to horizontal
      legend.title = element_blank() , # Remove the legend title
      plot.caption = element_text(size = 11)
    ) +
    geom_text(aes(label = scales::percent(share_exposed, accuracy = 0.01)),
              vjust = 0.5, hjust = -0.3,
              position = position_dodge(.9), size = 4) +
    coord_flip() +
    scale_fill_manual(values = fill_colors) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.6), expand = c(0, 0, 0, 0))  # Adjust expand to reduce spacing and fix limits)  # Format y-axis as percentages
  return(plot) 
}

# Define colors
fill_colors <- c("$2.15/day" = "#EA9E8D", "$3.65/day" = "#881600", "$6.85/day" = "#5D2E8C")

# Heat Exposure Plot
heat_note <- dropped_note %>% filter(hazard == "Heat Exposure[Extreme Heat Stress >32°C]") %>% pull(note)
heat_plot <- plot_exposure(data_long_filtered, "Heat Exposure[Extreme Heat Stress >32°C]", "Poor and Exposed to Heat [Extreme Heat Stress >32°C]", fill_colors, heat_note)
heat_plot <- heat_plot + labs(y = "")
print(heat_plot)


# Drought Exposure Plot
drought_note <- dropped_note %>% filter(hazard == "Drought Exposure[Severe Drought Occurence >20%]") %>% pull(note)
drought_plot <- plot_exposure(data_long_filtered, "Drought Exposure[Severe Drought Occurence >20%]", "Poor and Exposed to Drought [Severe Drought>20%]", fill_colors,drought_note)
drought_plot <- drought_plot + labs(y = "")

# Air Pollution Exposure Plot
airpol_note <- dropped_note %>% filter(hazard == "Air Pollution Exposure[PM2.5 >15µg/m³]") %>% pull(note)
airpol_plot <- plot_exposure(data_long_filtered, "Air Pollution Exposure[PM2.5 >15µg/m³]", "Poor and Exposed to Air Pollution [PM2.5 >15µg/m³]", fill_colors,airpol_note)
airpol_plot

# Flood Exposure Plot
flood_note <- dropped_note %>% filter(hazard == "Flood Risk[Water Depth >0.5m]") %>% pull(note)
flood_plot <- plot_exposure(data_long_filtered, "Flood Risk[Water Depth >0.5m]", "Poor and Exposed to Flood [Water Depth >0.5m]", fill_colors,flood_note)
flood_plot <- flood_plot



# Create grid
ggpubr::ggarrange(
  heat_plot, drought_plot, airpol_plot, flood_plot, # list of plots
  labels = "AUTO", # labels
  common.legend = TRUE, # COMMON LEGEND
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 2, # number of rows
  ncol = 2 # number of columns
)



ggsave(file.path(graphs,"share_exposed_poor_allhazards.png"), width = 12, height = 9)



