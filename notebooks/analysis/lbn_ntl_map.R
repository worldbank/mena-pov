## Nightlights

# Load Data ---------------------------------------------------------------
lbn_gadm <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                    layer = "gadm41_LBN_0")

lbn_gadm1 <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                    layer = "gadm41_LBN_1")

lbn_gadm3 <- readOGR(file.path(lbn_file_path,
                               "Boundaries"),
                     layer = "gadm41_LBN_3") %>% st_as_sf()

ntl <- raster(file.path(lbn_file_path,
                        "Nighttime_Lights",
                        "raw",
                        "annual",
                        "lebanon_viirs_corrected_annual_median_2014_2021_avg_rad.tif"))



# Prep data ---------------------------------------------------------------
ntl_2021 <- ntl %>% mask(lbn_gadm) 

ntl_2021_df <- rasterToPoints(ntl_2021, spatial = TRUE) %>% as.data.frame()

## Remove very low values of NTL; can be considered noise 
ntl_2021_df$avg_rad[ntl_2021_df$avg_rad <= 2] <- 0

## Distribution is skewed, so log
ntl_2021_df$avg_rad_adj <- log(ntl_2021_df$avg_rad+1)

##### Map 
ggplot() +
  geom_raster(data = ntl_2021_df, 
              aes(x = x, y = y, 
                  fill = avg_rad_adj)) +
  scale_fill_gradient2(low = "black",
                       mid = "yellow",
                       high = "red",
                       midpoint = 4.5) +
  labs(fill = "Nightlights \n(2022)") +
  coord_quickmap() + 
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
