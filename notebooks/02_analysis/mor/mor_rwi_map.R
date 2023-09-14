## Load Packages
# Load required packages
library(stringdist)
library(exactextractr)
library(rjson)
library(jsonlite)
library(dplyr)
library(htmlwidgets)
library(shiny)


getOption("max.print")
options(max.print = 10000)

# Load data ---------------------------------------------------------------
morocco_shp <- st_read(file.path(mor_file_path,
                                   "Boundaries",
                                   "gadm41_MAR_4.shp"))


rwi <- read.csv(file.path(mor_file_path,
                          "RWI",
                          "raw",
                          "morocco_relative_wealth_index.csv"))

affected_pop <- read_excel(file.path(mor_onedrive_dir,
                                     "data",
                                     "earthquake_impact",
                                     "listes-localités.xlsx"))

intensity <- st_read(file.path(mor_onedrive_dir,
                               "data",
                               "shakemap_shp",
                               "pga.shp"))

mor_pop <- raster(file.path(mor_file_path,
                            "Population",
                            "raw",
                            "mar_ppp_2020_1km_Aggregated_UNadj.tif"))



# Prepare the intensity categories ----------------------------------------
intensity <- intensity %>%
  mutate(pga = PARAMVALUE*100)


# Extract Population ------------------------------------------------------
summarize_population <- function(values, coverage_fractions) {
  sum(values * coverage_fractions)
}


pop_worldpop <- exact_extract(mor_pop, morocco_shp, summarize_population)
morocco_shp$pop_worldpop <- pop_worldpop





# Create UID --------------------------------------------------------------
# clean


affected_pop_summ <- affected_pop %>%
  rename("NAME_4" = "commune_fr") %>%
  mutate(
    population = as.numeric(as.character(population)),
    menage = as.numeric(as.character(menage))
  ) %>%
  group_by(NAME_4) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    total_menages = sum(menage, na.rm = TRUE)
  ) %>%
  ungroup() ## 160 communes where we have information of affected people


## clean names in shapefile
morocco_shp_clean <- morocco_shp %>%
mutate(NAME_4 = case_when(
  NAME_4 == "Zaouiat Annahlia" ~ "Zaouia Annahlia",
  NAME_4 == "Sidi Abdallah Ou Said" ~ "Sidi Abdellah Ou Said",
  NAME_4 == "Agoudim" ~"Aghouatim",
  NAME_4 == "Ouad Lbour" ~ "Ouad L'bour" ,
  NAME_4 == "Oulad Dlim" ~ "Ouled Dlim",
  NAME_4 == "Jnan Bouih" ~ "Jnane Bouih" ,
  NAME_4 == "Issaguen" ~"Iznaguen",
  NAME_4 == "Tizgazaouine" ~ "Tizgzaouine" ,
  NAME_4 == "Sidi Ahmed Ou Abdellah" ~"Sidi Ahmed Ou Abdallah",
  NAME_4 == "Zaouiat Sidi Tahar" ~ "Zaouia Sidi Tahar",
  TRUE ~ NAME_4
),
uid = 1:nrow(morocco_shp)) %>%
  left_join(affected_pop_summ, by = c("NAME_4"))


those_affected <- morocco_shp_clean %>%
  filter(!is.na(total_population))

#Calculate the difference
those_affected$difference <- those_affected$total_population - those_affected$pop_worldpop


ggplot(data = those_affected) +
  geom_line(aes(x = NAME_4, y = total_population, group = 1, color = "Total Population")) +
  geom_line(aes(x = NAME_4, y = pop_worldpop, group = 2, color = "World Population")) +
  geom_line(aes(x = NAME_4, y = difference, group = 3, color = "Difference")) +
  geom_point(aes(x = NAME_4, y = total_population, color = "Total Population")) + 
  geom_point(aes(x = NAME_4, y = pop_worldpop, color = "World Population")) +
  scale_color_manual(values = c("Total Population" = "red", "World Population" = "blue")) +
  labs(
    title = "Comparison between Total Population and World Population by NAME_4",
    x = "Commune",
    y = "Population Count",
    color = "Legend"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, size = 4, hjust = 1)) 

  


# Merge RWI and Shapefile -------------------------------------------------

#convert rwi into a sf object
rwi_sf <- st_as_sf(rwi, coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(st_crs(morocco_shp))

#merge the points data and shapefile
merge_rwi_shp <- st_join(rwi_sf,morocco_shp_clean, left = TRUE)



# Aggregate the data by Commune -------------------------------------------
agg_data_commune <- merge_rwi_shp %>%
  group_by(NAME_4) %>%
  summarise(mean_rwi = mean(rwi), na.rm = T) %>%
  ungroup() %>%
  mutate(quartile = case_when(
    mean_rwi <= quantile(mean_rwi, 0.25) ~ "Bottom 25%",
    mean_rwi > quantile(mean_rwi, 0.25) & mean_rwi <= quantile(mean_rwi, 0.5) ~ "25th to 50th Percentiles",
    mean_rwi > quantile(mean_rwi, 0.5) & mean_rwi <= quantile(mean_rwi, 0.75) ~ "50th to 75th Percentiles",
    mean_rwi > quantile(mean_rwi, 0.75) ~ "Top 25%",
    TRUE ~ NA_character_
  ))



# Merge it to the morocco shape clean file --------------------------------
merged_data <- st_join( morocco_shp_clean, agg_data_commune)
  

  
# Cropping it to the earthquake area --------------------------------------------------
cropped_sf <- st_crop(merged_data,intensity)



# Plot --------------------------------------------------------------------
qpal <- colorFactor(palette = "OrRd", levels = c("Bottom 25%", 
                                                 "25th to 50th Percentiles", 
                                                 "50th to 75th Percentiles",
                                                 "Top 25%"))
legendLabels <- c("Bottom 25%", "25th to 50th Percentiles", "50th to 75th Percentiles", "Top 25%")
legendColors <- sapply(legendLabels, qpal)

pga_pal <- colorNumeric(palette = "viridis", domain = intensity$pga)


label_data <- paste0(
  "Commune:", cropped_sf$NAME_4.x, 
  "<br>",
  "RWI: ", 
  ifelse(is.na(cropped_sf$mean_rwi), "NA", round(cropped_sf$mean_rwi, 2)),
  "<br>",
  "Quartile:", 
  ifelse(is.na(cropped_sf$quartile), "NA", cropped_sf$quartile), 
  "<br>",
  "Affected Population: ", 
  ifelse(is.na(cropped_sf$total_population), "NA", cropped_sf$total_population),
  "<br>",
  "Population (WorldPop): ", round(cropped_sf$pop_worldpop,0)
)





rwi <- leaflet(cropped_sf) %>%
  # Base layer
  addTiles() %>%
  
  # Add the cropped_sf polygons
  addPolygons(
    fillColor = ~qpal(quartile),
    weight = 0.7,
    opacity = 0.5,
    color = "white",
    dashArray = "3",
    fillOpacity = 2,
    popup = ~label_data,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.5,
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", 
                   padding = "3px 8px"), 
      textsize = "15px", 
      direction = "auto")
  ) %>%
  
  # Add the intensity layer
  # Add the intensity layer with PGA coloring
  addPolylines(
    data = intensity,
    color = ~pga_pal(pga),
    fillColor = NA,
    fillOpacity = 0.6,
    weight = 2
  ) %>%
  
  # Add custom legend without NA values
  addLegend(colors = legendColors, 
            labels = legendLabels,
            opacity = 1, 
            title = "Relative Wealth Index", 
            position = "bottomright") %>%
  # Add a horizontal legend for PGA
  addLegend(pal = pga_pal, 
          values = intensity$pga,
          title = "PGA Values",
          position = "bottomright")

rwi
# Save the leaflet widget as an html file
saveWidget(rwi, "C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/Projects/mor_earthquake_analysis/maps/rwi_map.html", selfcontained = F)

#Export csv
affected_pop <- affected_pop %>%
  left_join(agg_data_commune)

write.csv(affected_pop, file = file.path(mor_onedrive_dir, "data", "earthquake_impact", "affected_ppl_rwi.csv"), row.names = FALSE)


