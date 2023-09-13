## Load Packages
# Load required packages
library(stringdist)
library(exactextractr)
library(rjson)
library(jsonlite)
library(dplyr)
library(htmlwidgets)

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




# Create UID --------------------------------------------------------------
# clean


affected_pop <- affected_pop %>%
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
  ungroup()


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
  left_join(affected_pop, by = c("NAME_4"))



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


label_data <- paste0(
  cropped_sf$NAME_4.x, 
  "\nRWI: ", 
  ifelse(is.na(cropped_sf$mean_rwi), "NA", round(cropped_sf$mean_rwi, 2)),
  " ,", 
  ifelse(is.na(cropped_sf$quartile), "NA", cropped_sf$quartile), 
  " Affected Population: ", 
  ifelse(is.na(cropped_sf$total_population), "NA", cropped_sf$total_population)
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
    label = ~label_data,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.5,
    ),
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
  ) %>%
  
  # Add the intensity layer
  addPolygons(data = intensity, fill = NA, color = "black", weight = 0.5) %>%
  
  # Add custom legend without NA values
  addLegend(colors = legendColors, 
            labels = legendLabels,
            opacity = 1, 
            title = "Relative Wealth Index", 
            position = "bottomright")


rwi
# Save the leaflet widget as an html file
saveWidget(rwi, "C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/Projects/mor_earthquake_analysis/maps/rwi_map.html", selfcontained = F)

#Export csv
affected_pop <- affected_pop %>%
  left_join(agg_data_commune)

write.csv(affected_pop, file = file.path(mor_onedrive_dir, "data", "earthquake_impact", "affected_ppl_rwi.csv"), row.names = FALSE)


