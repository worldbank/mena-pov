## Extract Road Density and Measure Access
install.packages("osrm")

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



# Take centroids of each commune ------------------------------------------
## transform the projection to UTM

mor_utm <- morocco_shp %>% st_transform(32629)
mor_cent <- st_centroid(mor_utm)



# Road Intersections ------------------------------------------------------


earthquake_affected_areas <- st_intersection(roads, intensity)






ggplot()+
  geom_sf(data = roads_intersect_intensity, aes(color = fclass))+
  geom_sf(data = intensity, fill = NA)+
  theme_void()
