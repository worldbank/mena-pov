## Create Clusters using Landuse data


# Load Data ---------------------------------------------------------------
yemen <- readOGR(file.path(yem_file_path, 
                           "Boundaries", 
                           "raw"), 
                 layer = "gadm41_YEM_0")

urban_constant <- raster(file.path(yem_file_path, 
                                   "Landcover",
                                   "raw",
                                   "globcover",
                                   "GLOBCOVER_L4_200901_200912_V2.3.tif")) %>% crop(extent(yemen))


yemen <- spTransform(yemen, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

##Globcover
# Create a raster that is 1 if urban in any time period
urban_constant[] <- as.numeric(urban_constant[] %in% c(20))


# Masking -----------------------------------------------------------------
## Crop/Mask to Iraq
gc_binary <- urban_constant %>% crop(yemen) %>% mask(yemen)

# Define raster layer of clusters ----------------------------------------------
gc_clumps <- clump(gc_binary, directions=8)

clumps_unique_values <- unique(gc_clumps[])[!is.na(unique(gc_clumps[]))]

plot(gc_clumps)


# Polygonize clusters ----------------------------------------------------------
## Polgyzonize raster grids
clump_sp_all <- rasterToPolygons(gc_clumps,
                                 n=4,
                                 na.rm=TRUE,
                                 digits=12,
                                 dissolve=F)
clump_sp_all$cluster_n_cells <- 1

## Collapse grids of same cluster
clumps_sp <- raster::aggregate(clump_sp_all,
                               by="clumps",
                               list(list(sum, 'cluster_n_cells')))


# Group together clusters  ------------------------------------------------

## Centroid
points_sp <- coordinates(clumps_sp) %>%
  as.data.frame() %>%
  dplyr::rename(lon = V1,
                lat = V2) %>%
  bind_cols(clumps_sp@data)

## Spatially Define and project
coordinates(points_sp) <- ~lon+lat
crs(points_sp) <- CRS("+init=epsg:4326")
points_sp <- spTransform(points_sp, CRS(UTM_YEM))

## Back to dataframe
points <- as.data.frame(points_sp)

## Clusters
points_dist <- points[,c("lat", "lon")] %>% dist()
clumps_sp$wardheirch_clust_id <- hclust(points_dist, method = "ward.D2") %>%
  cutree(h = 10000)

clumps_sp <- raster::aggregate(clumps_sp, by = "wardheirch_clust_id",
                               sums=list(list(sum, 'cluster_n_cells')))

clumps_sp@data <- clumps_sp@data %>%
  dplyr::select(-c(wardheirch_clust_id)) %>%
  dplyr::mutate(cell_id = 1:n())

# # Export -----------------------------------------------------------------------
# We save "polygon" and "points" file, where "points" is actually just the polygon.
# We do this to make compatible with some scripts that also process grid data


## Dataframe with number of cells
saveRDS(clumps_sp, file.path(yem_file_path,
                             "Landcover",
                             "final",
                             "cluster_n_cells.Rds"))
clumps_sp$cluster_n_cells <- NULL

## Main Files - 1km road cut out
saveRDS(clumps_sp, file.path(yem_file_path,
                             "Landcover",
                             "final",
                             "polygons.Rds"))


clumps_sf <- st_as_sf(clumps_sp)
st_write(clumps_sf, file.path(yem_file_path,
                             "Landcover",
                             "final",
                             "polygons.geojson"))

leaflet() %>%
  addTiles() %>%
  addPolygons(data = clumps_sf)
