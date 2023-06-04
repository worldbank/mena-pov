#Prep Base Shapefile

# Load ADM Data ----------------------------------------------------------------

yem_adm2 <- readRDS(file.path(yem_file_path,
                     "Landcover",
                     "final",
                     "polygons.Rds"))

yem_adm2$uid <- 1:nrow(yem_adm2)

#### Blank
yem_adm2_blank <- yem_adm2
yem_adm2_blank@data <- yem_adm2_blank@data %>%
  dplyr::select(uid)

# Export -----------------------------------------------------------------------
saveRDS(yem_adm2_blank, file.path(yem_file_path,
                                  "Boundaries",
                                  "final",
                                  "yem_landuse_clusters.Rds"))





