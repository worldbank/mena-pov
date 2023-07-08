# Add GADM


# Load Data ---------------------------------------------------------------
cluster <- readRDS(file.path(yem_file_path,
                             "Landcover",
                             "final",
                             "polygons.Rds"))

gadm <- readOGR(file.path(yem_file_path,
                          "Boundaries",
                          "raw"),
                layer = "gadm41_YEM_2")



# Merge -------------------------------------------------------------------
cluster_over_gadm <- over(cluster,gadm)
cluster_over_gadm$uid <- cluster$cell_id


# Export ------------------------------------------------------------------
saveRDS(cluster_over_gadm,file.path(yem_file_path,
                                    "Boundaries",
                                    "final",
                                    "yem_landuse_clusters_gadm.Rds"))

