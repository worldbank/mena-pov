library(htmltools)

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


# Transform projection to calculate distance ------------------------------
# Transform to UTM Zone 33N (EPSG code 32633)
lby_shp_utm <- st_transform(lby_shp, 32633)

# Buffer by 1 meter
lby_shp_utm_buffered <- st_buffer(lby_shp_utm, dist = 1000)

# (Optional) Transform back to WGS 84
lby_shp_buffered <- st_transform(lby_shp_utm_buffered, 4326)



# Extract population  ---------------------------------
crs(pop_worldpop) <- CRS('+init=EPSG:4326')

lby_shp_buffered_sp <- as(lby_shp_buffered, "Spatial")
lby_shp_buffered_sp <- spTransform(lby_shp_buffered_sp, CRSobj = crs(pop_worldpop))

pop_worldpop <- extract(pop_worldpop, lby_shp_buffered_sp, fun = sum, na.rm = TRUE)
lby_shp_buffered_sp@data$pop_worldpop <- pop_worldpop


lby_shp_buffered_sf <- st_as_sf(lby_shp_buffered_sp)

# Convert RWI to sf -------------------------------------------------------
## Convert RWI to sf
rwi_sf <- st_as_sf(rwi, coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(st_crs(lby_shp_buffered_sf))


#merge the points data and shapefile
merge_rwi_shp <- st_join(rwi_sf,lby_shp_buffered_sf, left = TRUE)




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
  st_drop_geometry()


lby_shp_buffered_sf <- lby_shp_buffered_sf %>%
  left_join(agg_rwi, by = c("NAME_1"))


#Filter out affected regions ---------------------------------------------
## Filter out affected areas
ls_affected <- c("Benghazi","Al Marj","Darnah","Al Jabal al Akhdar")


lby_shp_affected <- lby_shp_buffered_sf %>% filter(NAME_1 %in% ls_affected)
bboxes <- st_bbox(lby_shp_affected)




# Plot --------------------------------------------------------------------

qpal <- colorFactor(palette = "OrRd", levels = c("Bottom 25%", 
                                                 "25th to 50th Percentiles", 
                                                 "50th to 75th Percentiles",
                                                 "Top 25%"))
legendLabels <- c("Bottom 25%", "25th to 50th Percentiles", "50th to 75th Percentiles", "Top 25%")
legendColors <- sapply(legendLabels, qpal)

legend_html <- HTML(
  paste0(
    "<div style='padding: 5px 0;'><strong>Relative Wealth Index</strong></div>",
    paste(
      sprintf("<i style='background: %s; opacity: 1; width: 15px; height: 15px; display: inline-block; vertical-align: middle; margin-right: 5px;'></i> %s<br>", legendColors, legendLabels),
      collapse = ""
    ),
    "<div style='padding-top: 10px;'><div style='display: inline-block; border: 2px solid red; width: 15px; height: 15px; margin-right: 5px; vertical-align: middle;'></div>",
    "<span>Flood Affected Region</span></div>"
  )
)




label_data <- paste0(
  "District:", agg_rwi$NAME_1, 
  "<br>",
  "RWI: ", 
  ifelse(is.na(agg_rwi$mean_rwi), "NA", round(agg_rwi$mean_rwi, 2)),
  "<br>",
  "Quartile:", 
  ifelse(is.na(lby_shp_buffered_sf$quartile), "NA", lby_shp_buffered_sf$quartile), 
  "<br>",
  "Population (WorldPop): ", round(lby_shp_buffered_sf$pop_worldpop,0)
)





rwi <- leaflet(lby_shp_buffered_sf) %>%
  # Base layer
  addTiles() %>%
  
  # Add the lby_shp_buffered polygons
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
  addRectangles(
    lng1 = bboxes["xmin"],
    lat1 = bboxes["ymin"],
    lng2 = bboxes["xmax"],
    lat2 = bboxes["ymax"],
    color = "red",  # choose a color for the bounding box
    fill = FALSE,
    weight = 2
  ) %>%
  
  # Add custom legend
  addControl(legend_html, position = "bottomright")

rwi


# Export ------------------------------------------------------------------

setwd("C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/Projects/lby_flood_analysis/maps")

# Save the leaflet widget as an html file
saveWidget(rwi, file = "lby_rwi_map.html", selfcontained = T)
