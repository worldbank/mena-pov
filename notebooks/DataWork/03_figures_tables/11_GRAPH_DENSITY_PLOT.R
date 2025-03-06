# ================================================================
# Script Name: 11_DENSITY_PLOT
# Purpose: Create map of exposed, poor and vulnerable
# Input Dataset: ,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================

# Load Data ---------------------------------------------------------------

poor_vul_exposed <- readRDS(file.path(final_replication,"grid_10km_poor_vul_exposed.Rds"))
grid_sf <- readRDS(file.path(final_replication, "grid_vul_10km_final.Rds"))

vul_pop_sub_prj <- st_transform(grid_sf,4326)






# Distribution of the vulnerability ---------------------------------------

ggplot(data.frame(x = vul_pop_sub_prj$vul_share), aes(x = x)) +
  geom_density() +
  geom_vline(xintercept = vul_pop_sub_prj$vul_thresh_region, color = "red") +
  labs(
    title = "Density Plot of Share of Vulnerability across the region",
    x = "Share of Vulnerability",
    y = "Density"
  ) + theme_minimal()

# Save using ggsave
ggsave(
  filename = file.path(graphs, "dens_plot.png"),
  width = 8,     # Width in inches
  height = 6,    # Height in inches
  dpi = 300      # Resolution
)


