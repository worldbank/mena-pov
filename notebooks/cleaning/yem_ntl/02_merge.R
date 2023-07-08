## Merge


# Load Data ---------------------------------------------------------------
cluster <- readRDS(file.path(yem_file_path,
                             "Boundaries",
                             "final",
                             "yem_landuse_clusters_gadm.Rds"))

viirs <- readRDS(file.path(yem_file_path,
                           "Nighttime_Lights",
                           "final",
                           "yem_landuse_clusters_viirs_monthly.Rds"))


# Merged ------------------------------------------------------------------
viirs_merged <- merge(viirs,cluster, by = "uid")



# Export ------------------------------------------------------------------
saveRDS(viirs_merged, file.path(yem_file_path,
                                "Nighttime_Lights",
                                "final",
                                "yem_landuse_cluster_viirs_monthly_gadm.Rds"))

write.csv(viirs_merged,file.path(yem_file_path,
                                 "Nighttime_Lights",
                                 "final",
                                 "yem_landuse_clusters_viirs_monthly_gadm.csv"))



