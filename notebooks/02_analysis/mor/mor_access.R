## Extract Road Density and Measure Access


# Load Data ---------------------------------------------------------------
morocco_shp <- st_read(file.path(mor_file_path,
                                 "Boundaries",
                                 "gadm41_MAR_4.shp"))


intensity <- st_read(file.path(mor_onedrive_dir,
                               "data",
                               "shakemap_shp",
                               "pga.shp"))

roads <- st_read(file.path(mor_file_path,
                           "Roads",
                           "raw",
                           "gis_osm_roads_free_1.shp"))



# Road Intersections ------------------------------------------------------

roads_intersect_intensity <- st_crop(roads,intensity)


ggplot()+
  geom_sf(data = roads_intersect_intensity, aes(color = fclass))+
  geom_sf(data = intensity, fill = NA)+
  geom_sf(data = morocco_shp) +
  theme_void()
