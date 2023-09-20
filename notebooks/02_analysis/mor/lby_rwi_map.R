## Libya Relative Wealth Index

lby_shp <- st_read(file.path(lby_file_path,
                                 "Boundaries",
                                 "gadm41_LBY_1.shp"))


rwi <- read.csv(file.path(lby_file_path,
                          "RWI",
                          "raw",
                          "lby_relative_wealth_index.csv"))


pop_worldpop <- raster(file.path(lby_file_path,
                                 "Population",
                                 "raw",
                                 "lby_ppp_2020_1km_Aggregated_UNadj.tif"))


# Filter out affected regions ---------------------------------------------
## Filter out affected areas
ls_affected <- c("Benghazi","Al Marj","Darnah","Al Jabal al Akhdar")

lby_flood_affected <- lby_shp %>%
  filter(NAME_1 %in% ls_affected)





# Extract population for affected regions ---------------------------------
crs(pop_worldpop) <- CRS('+init=EPSG:4326')

lby_flood_affected_sp <- as(lby_flood_affected, "Spatial")
lby_flood_affected_sp <- spTransform(lby_flood_affected_sp, CRSobj = crs(pop_worldpop))

pop_worldpop <- extract(pop_worldpop, lby_flood_affected_sp, fun = sum, na.rm = TRUE)





# Convert RWI to sf -------------------------------------------------------
## Convert RWI to sf
rwi_sf <- st_as_sf(rwi, coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(st_crs(lby_flood_affected))

#merge the points data and shapefile
merge_rwi_shp <- st_join(rwi_sf,lby_flood_affected, left = TRUE)

agg_rwi <- merge_rwi_shp %>%
  filter(!is.na(NAME_1)) %>%
  group_by(NAME_1) %>%
  summarise(mean_rwi = mean(rwi), na.rm = T) %>%
  ungroup() %>%
  mutate(quartile = case_when(
    mean_rwi <= quantile(mean_rwi, 0.25) ~ "Bottom 25%",
    mean_rwi > quantile(mean_rwi, 0.25) & mean_rwi <= quantile(mean_rwi, 0.5) ~ "25th to 50th Percentiles",
    mean_rwi > quantile(mean_rwi, 0.5) & mean_rwi <= quantile(mean_rwi, 0.75) ~ "50th to 75th Percentiles",
    mean_rwi > quantile(mean_rwi, 0.75) ~ "Top 25%",
    TRUE ~ NA_character_
  )) %>%
  left_join()



# Plot --------------------------------------------------------------------

qpal <- colorFactor(palette = "OrRd", levels = c("Bottom 25%", 
                                                 "25th to 50th Percentiles", 
                                                 "50th to 75th Percentiles",
                                                 "Top 25%"))
legendLabels <- c("Bottom 25%", "25th to 50th Percentiles", "50th to 75th Percentiles", "Top 25%")
legendColors <- sapply(legendLabels, qpal)

#pga_pal <- colorNumeric(palette = "viridis", domain = intensity$pga)



label_data <- paste0(
  "District:", agg_rwi$NAME_4, 
  "<br>",
  "RWI: ", 
  ifelse(is.na(agg_rwi$mean_rwi), "NA", round(agg_rwi$mean_rwi, 2)),
  "<br>",
  "Quartile:", 
  ifelse(is.na(cropped_sf$quartile), "NA", cropped_sf$quartile), 
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
