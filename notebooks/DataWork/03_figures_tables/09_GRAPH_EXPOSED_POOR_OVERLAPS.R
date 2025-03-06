# ================================================================
# Script Name: 07_GRAPH_POOR_EXPOSED_ADM1
# Purpose: Create maps of the exposed poor for Egypt
# Input Dataset:grid_10km_poor_215_685.Rds,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================


# Load Data ---------------------------------------------------------------

grid_poor <- readRDS(file.path(final_replication, "grid_10km_poor_215_685.Rds"))




# Check countries with no data on poor ------------------------------------
# test <- grid_poor %>% 
#   group_by(ISO_A2) %>%
#   dplyr::summarise(poor215 = sum(poor215_count_rwi_dup, na.rm = T),
#                    poor365 =  sum(poor365_count_rwi_dup, na.rm = T),
#                    poor685 =  sum(poor685_count_rwi_dup, na.rm = T))
# 
# names(grid_poor)

# Create Vars -------------------------------------------------------------

grid_people_sub <- grid_poor %>%
  filter(!ISO_A2 %in% c("YE","IQ","IR","PS","SY", "SA","LY")) %>%
  mutate(
    exposed_flood5 = ifelse(grid_pop_flood_expected >0,1,0),
    exposed_heat_flood = ifelse(exposed_heat32 == 1 & exposed_flood5 == 1,1,0),
    exposed_heat_drought = ifelse(exposed_heat32 == 1 & exposed_drought20 == 1,1,0),
    exposed_heat_airpol = ifelse(exposed_heat32 == 1 & exposed_airpol15 == 1,1,0),
    exposed_flood_drought = ifelse(exposed_flood5 == 1 & exposed_drought20 == 1,1,0),
    exposed_flood_airpol = ifelse(exposed_flood5 == 1 & exposed_airpol15 == 1, 1, 0),
    exposed_drought_airpol = ifelse(exposed_drought20 ==1 & exposed_airpol15 == 1, 1,0),
    exposed_heat_flood_drought = ifelse(exposed_heat32 == 1 & exposed_flood5 == 1 & exposed_drought20 == 1,1,0),
    exposed_airpol_flood_drought = ifelse(exposed_airpol15 == 1 & exposed_flood5 == 1 & exposed_drought20 == 1,1,0),
    exposed_airpol_heat_drought = ifelse(exposed_heat32 == 1 & exposed_airpol15 == 1 & exposed_drought20 == 1,1,0),
    exposed_airpol_heat_flood = ifelse(exposed_airpol15 == 1 & exposed_heat32 == 1 & exposed_flood5 == 1,1,0),
    exposed_all_hazards = ifelse(exposed_heat32 == 1 & exposed_airpol15 == 1 & exposed_drought20 == 1 & exposed_flood5 ==1,1,0)
  )


# Define the scenarios and the corresponding exposure types
scenarios <- c("heat_flood", "heat_drought", "heat_airpol", "flood_drought", "flood_airpol", "drought_airpol", "heat_flood_drought", 
               "airpol_flood_drought","airpol_heat_drought","airpol_heat_flood","all_hazards")
pov_lns <- c(215, 365, 685)

# Perform the mutations
for (scenario in scenarios) {
  for (pov_ln in pov_lns) {
    # Skip creating variables for non-existent combinations
    #if (!(scenario == "airpol_flood_drought" && pov_ln != 215)) {
    grid_people_sub <- grid_people_sub %>%
      mutate(!!paste0("exposed_", scenario, "_", pov_ln) := !!sym(paste0("exposed_", scenario)) * !!sym(paste0("poor", pov_ln, "_count_rwi_dup")))
  }
  #}
}


# First, compute the total population for each country
poor_adm1 <- grid_people_sub %>%
  group_by(ISO_A2) %>%
  mutate(tot_pop_country = sum(pop_count, na.rm = TRUE)) %>%
  ungroup %>%
  group_by(ISO_A2,WB_ADM1_NA) %>% dplyr::summarise(
    
    #Heat and Flood
    exposed_heat_flood_215_adm1 = sum(exposed_heat_flood_215, na.rm = T),
    exposed_heat_flood_365_adm1 = sum(exposed_heat_flood_365, na.rm = T),
    exposed_heat_flood_685_adm1 = sum(exposed_heat_flood_215, na.rm = T),
    
    
    #Heat and Drought
    exposed_heat_drought_215_adm1 = sum(exposed_heat_drought_215, na.rm = T),
    exposed_heat_drought_365_adm1 = sum(exposed_heat_drought_365, na.rm = T),
    exposed_heat_drought_685_adm1 = sum(exposed_heat_drought_685, na.rm = T),
    
    
    
    #Heat and Airpol
    exposed_heat_airpol_215_adm1 = sum(exposed_heat_airpol_215, na.rm = T),
    exposed_heat_airpol_365_adm1 = sum(exposed_heat_airpol_365, na.rm = T),
    exposed_heat_airpol_685_adm1 = sum(exposed_heat_airpol_685, na.rm = T),
    
    
    
    #Flood and Drought
    exposed_flood_drought_215_adm1 = sum(exposed_flood_drought_215, na.rm = T),
    exposed_flood_drought_365_adm1 = sum(exposed_flood_drought_365, na.rm = T),
    exposed_flood_drought_685_adm1 = sum(exposed_flood_drought_685, na.rm = T),
    
    
    
    #Flood and Airpol
    exposed_flood_airpol_215_adm1 = sum(exposed_flood_airpol_215, na.rm = T),
    exposed_flood_airpol_365_adm1 = sum(exposed_flood_airpol_365, na.rm = T),
    exposed_flood_airpol_685_adm1 = sum(exposed_flood_airpol_685, na.rm = T),
    
    
    #Drought and Airpol
    exposed_drought_airpol_215_adm1 = sum(exposed_drought_airpol_215, na.rm = T),
    exposed_drought_airpol_365_adm1 = sum(exposed_drought_airpol_365, na.rm = T),
    exposed_drought_airpol_685_adm1 = sum(exposed_drought_airpol_685, na.rm = T),
    
    
    
    #Heat, Flood and Drought
    exposed_heat_flood_drought_215_adm1 = sum(exposed_heat_flood_drought_215, na.rm = T),
    exposed_heat_flood_drought_365_adm1 = sum(exposed_heat_flood_drought_365, na.rm = T),
    exposed_heat_flood_drought_685_adm1 = sum(exposed_heat_flood_drought_685, na.rm = T),
    
    
    #Airpol, Flood and Drought
    exposed_airpol_flood_drought_215_adm1 = sum(exposed_airpol_flood_drought_215, na.rm = T),
    exposed_airpol_flood_drought_365_adm1 = sum(exposed_airpol_flood_drought_365, na.rm = T),
    exposed_airpol_flood_drought_685_adm1 = sum(exposed_airpol_flood_drought_685, na.rm = T),
    
    
    #Airpol, Heat and Drought
    exposed_airpol_heat_drought_215_adm1 = sum(exposed_airpol_heat_drought_215, na.rm = T),
    exposed_airpol_heat_drought_365_adm1 = sum(exposed_airpol_heat_drought_365, na.rm = T),
    exposed_airpol_heat_drought_685_adm1 = sum(exposed_airpol_heat_drought_685, na.rm = T),
    
    
    #Airpol,Heat and Flood
    exposed_airpol_heat_flood_215_adm1 = sum(exposed_airpol_heat_flood_215,na.rm = T),
    exposed_airpol_heat_flood_365_adm1 = sum(exposed_airpol_heat_flood_365,na.rm = T),
    exposed_airpol_heat_flood_685_adm1 = sum(exposed_airpol_heat_flood_685,na.rm = T),
    
    
    #All Hazards
    exposed_all_hazards_215_adm1 = sum(exposed_all_hazards_215,na.rm = T),
    exposed_all_hazards_365_adm1 = sum(exposed_all_hazards_365,na.rm = T),
    exposed_all_hazards_685_adm1 = sum(exposed_all_hazards_685,na.rm = T),
    
    tot_pop_country = first(tot_pop_country), # Retain the total population of the country
    .groups = 'drop'
  )%>%
  
  mutate(
    #Heat and Flood
    share_heat_flood_215_adm1 = exposed_heat_flood_215_adm1  / tot_pop_country,
    share_heat_flood_365_adm1 = exposed_heat_flood_365_adm1 / tot_pop_country,
    share_heat_flood_685_adm1 = exposed_heat_flood_685_adm1/ tot_pop_country,
    
    #Heat and Drought
    share_heat_drought_215_adm1 = exposed_heat_drought_215_adm1  / tot_pop_country,
    share_heat_drought_365_adm1 = exposed_heat_drought_365_adm1 / tot_pop_country,
    share_heat_drought_685_adm1 = exposed_heat_drought_685_adm1/ tot_pop_country,
    
    #Heat and Airpol
    share_heat_airpol_215_adm1 = exposed_heat_airpol_215_adm1  / tot_pop_country,
    share_heat_airpol_365_adm1 = exposed_heat_airpol_365_adm1 / tot_pop_country,
    share_heat_airpol_685_adm1 = exposed_heat_airpol_685_adm1/ tot_pop_country,
    
    #Flood and Drought
    share_flood_drought_215_adm1 = exposed_flood_drought_215_adm1  / tot_pop_country,
    share_flood_drought_365_adm1 = exposed_flood_drought_365_adm1 / tot_pop_country,
    share_flood_drought_685_adm1 = exposed_flood_drought_685_adm1/ tot_pop_country,
    
    #Flood and Airpol
    share_flood_airpol_215_adm1 = exposed_flood_airpol_215_adm1  / tot_pop_country,
    share_flood_airpol_365_adm1 = exposed_flood_airpol_365_adm1 / tot_pop_country,
    share_flood_airpol_685_adm1 = exposed_flood_airpol_685_adm1/ tot_pop_country,
    
    
    #Drought and Airpol
    share_drought_airpol_215_adm1 = exposed_drought_airpol_215_adm1  / tot_pop_country,
    share_drought_airpol_365_adm1 = exposed_drought_airpol_365_adm1 / tot_pop_country,
    share_drought_airpol_685_adm1 = exposed_drought_airpol_685_adm1/ tot_pop_country,
    
    #Heat, Flood and Drought
    share_heat_flood_drought_215_adm1 = exposed_heat_flood_drought_215_adm1  / tot_pop_country,
    share_heat_flood_drought_365_adm1 = exposed_heat_flood_drought_365_adm1 / tot_pop_country,
    share_heat_flood_drought_685_adm1 = exposed_heat_flood_drought_685_adm1/ tot_pop_country,
    
    #Airpol, Flood and Drought
    share_airpol_flood_drought_215_adm1 = exposed_airpol_flood_drought_215_adm1  / tot_pop_country,
    share_airpol_flood_drought_365_adm1 = exposed_airpol_flood_drought_365_adm1 / tot_pop_country,
    share_airpol_flood_drought_685_adm1 = exposed_airpol_flood_drought_685_adm1/ tot_pop_country,
    
    #Airpol, Heat and Drought
    share_airpol_heat_drought_215_adm1 = exposed_airpol_heat_drought_215_adm1  / tot_pop_country,
    share_airpol_heat_drought_365_adm1 = exposed_airpol_heat_drought_365_adm1 / tot_pop_country,
    share_airpol_heat_drought_685_adm1 = exposed_airpol_heat_drought_685_adm1/ tot_pop_country,
    
    
    #Airpol, Heat and Flood
    share_airpol_heat_flood_215_adm1 = exposed_airpol_heat_flood_215_adm1  / tot_pop_country,
    share_airpol_heat_flood_365_adm1 = exposed_airpol_heat_flood_365_adm1 / tot_pop_country,
    share_airpol_heat_flood_685_adm1 = exposed_airpol_heat_flood_685_adm1/ tot_pop_country,
    
    #All Hazards
    share_all_hazards_215_adm1 = exposed_all_hazards_215_adm1  / tot_pop_country,
    share_all_hazards_365_adm1 = exposed_all_hazards_365_adm1 / tot_pop_country,
    share_all_hazards_685_adm1 = exposed_all_hazards_685_adm1/ tot_pop_country
    
  )


# Pivoting the data from wide to long
long_data <- poor_adm1 %>%
  pivot_longer(
    cols = -c(ISO_A2,WB_ADM1_NA, tot_pop_country, geometry),
    names_to = c(".value", "scenario", "pov_ln", "adm"),
    names_pattern = "^(exposed|share)_(.+)_(\\d+)_adm(\\d+)$"
  ) %>%
  dplyr::select(-adm) %>%
  st_drop_geometry() %>%
  dplyr::mutate(WB_ADM1_NA_combined = paste0(WB_ADM1_NA, ", ",ISO_A2))






# Plot --------------------------------------------------------------------


# Function to create the plot
create_scenario_plot <- function(data, scenario, title, note) {
  # Identify the order of countries based on the subgroup 685 for the given scenario
  order_685 <- data %>%
    dplyr::filter(scenario == !!scenario & pov_ln == 685) %>%
    group_by(ISO_A2) %>%
    arrange(desc(exposed)) %>%
    slice(1) %>%
    arrange(desc(exposed)) %>%
    pull(WB_ADM1_NA_combined)
  
  
  # Filter the data for the given scenario
  scenario_data <- data %>%
    dplyr::filter(scenario == !!scenario & WB_ADM1_NA_combined %in% order_685)
  
  # Reorder the factor levels of ISO_A2 based on the order_685
  scenario_data <- scenario_data %>% dplyr::mutate(new_order = factor(WB_ADM1_NA_combined,levels = rev(order_685)))
  
  # Drop missing variables
  scenario_data <- scenario_data %>% dplyr::filter(exposed>0)
  
  # Convert share into percentages
  scenario_data <- scenario_data %>% dplyr::mutate(share = share*100)
  
  
  # Create a custom palette with light to dark red shades
  custom_palette <- colorRampPalette(brewer.pal(5, "Reds"))(length(unique(scenario_data$pov_ln)))
  
  # Create the ggplot
  p <- ggplot(scenario_data, aes(x = new_order, y = exposed, fill = factor(pov_ln))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
    labs(title = title,
         subtitle = "",
         caption = note,
         x = "",
         y = "Number of Poor Exposed",
         fill = "Poverty Line") +
    geom_text(aes(label = paste0("(",round(share, 2), "%)")),
              position = position_dodge(width = 0.9),
              vjust = 0.5, hjust = -0.3,
              size = 4) +
    theme_minimal() +
    theme(
      #axis.text.y = element_text(angle = 0, hjust = 1,size = 12),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      panel.grid.major.y = element_blank(),  # Remove major grid lines
      # panel.grid.minor = element_line(color = "darkgrey"),
      legend.position = "top",  # Move legend to the top
      legend.direction = "horizontal",  # Set legend direction to horizontal
      legend.title = element_blank(),  # Remove the legend title
      plot.caption = element_text(size = 11),
      plot.title = element_text(size = 12)
    ) +
    coord_flip() +
    scale_fill_manual(values = custom_palette,
                      labels = c("685" = "$6.85/day", "365" = "$3.65/day", "215" = "$2.15/day"),
                      na.value = "grey") +
    scale_y_continuous(labels = scales::label_comma(), limits = c(0,7000000))
  #scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  # scale_y_log10(breaks = scales::log_breaks(n = 7),
  #               labels = scales::label_comma())
  # 
  print(p)
}



# Create individual plots
plot1 <- create_scenario_plot(long_data, "airpol_heat_flood", "Exposed to Air Pollution[<15µg/m³], Heat Exposure[<32°C] and \nFlood Exposure[<0.5m]", "No Exposure for MT. Exposure for LB and AE less than 5000")
plot2 <- create_scenario_plot(long_data, "flood_airpol", "Exposed to Air Pollution[<15µg/m³] and Flood Exposure[<0.5m]", "No Exposure for MT. Exposure for AE less than 5000")
plot3 <- create_scenario_plot(long_data, "heat_airpol", "Exposed to Air Pollution[<15µg/m³] and Heat Exposure[<32°C]", "No Exposure for MT. Exposure for LB less than 5000")

# Create grid
figure <- ggpubr::ggarrange(
  plot2, plot3,
  labels = "AUTO", # labels
  common.legend = TRUE, # COMMON LEGEN
  legend = "bottom", # legend position
  align = "hv", # Align them both, horizontal and vertical
  nrow = 1, # number of rows
  ncol = 2 # number of columns
)

# Annotate the figure
annotated_figure <- annotate_figure(figure,
                                    bottom = text_grob(
                                      "1. ADM1 with the highest number of exposed within each country is shown\n2. In brackets, share of total population exposed and poor is mentioned.\n3. Shares labeled 0 are less than 0.001",
                                      size = 10 , hjust = 1, x = 1, # Adjust horizontal justification if needed
                                    )
)

# Print the annotated figure
print(annotated_figure)



ggsave(file.path(graphs,"share_exposed_poor_overlaps.png"), width = 12, height = 8)
