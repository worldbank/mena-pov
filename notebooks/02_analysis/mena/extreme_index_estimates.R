library(ggplot2)
library(raster)
library(sf)
library(dplyr)

options(scipen=999)

# Load Data
r <- raster(file.path(mena_file_path, "Hazards", "TEMP", "raw", "wbgt_returnperiod20y.tif"))
shp <- st_read(file.path(mena_file_path, "Boundaries", "MENA_ADM2.shp"))
pop_raster <- raster(file.path(mena_file_path, "Population", "raw","gpw_v4_population_count_rev11_2000_30_sec.tif"))

# Project CRS -------------------------------------------------------------
crs(r) <- st_crs(shp)$proj4string
masked_r <- mask(crop(r, shp), shp)
crs(pop_raster)

# Extract population and temp ---------------------------------------------

shp$population <- exact_extract(pop_raster, shp, 'sum')
shp$mean_temp <- exact_extract(masked_r, shp, 'mean')



# Convert Raster ----------------------------------------------------------
df_raster <- as.data.frame(trim(masked_r), xy = TRUE) %>%
  filter(!is.na(wbgt_returnperiod20y))

# Step 1: Modify the DataFrame
df_raster$temp_category <- cut(df_raster$wbgt_returnperiod20y, 
                               breaks = c(-Inf, 28, 32, Inf), 
                               labels = c("Below 28", "28-32", "Above 32"))





# Plot without affected pop -----------------------------------------------
# Step 1: Modify the DataFrame


ggplot() +
  # Plot raster data
  geom_tile(data = df_raster, aes(x = x, y = y, fill = temp_category)) +
  
  # Plot shapefile boundaries
  geom_sf(data = shp, color = "white", fill = NA) +
  
  # Custom color scale
  scale_fill_manual(name = "Temperature (C)",
                    values = c("Below 28" = seq_gradient_pal("green", "yellow")(0.5),
                               "28-32" = "orange",
                               "Above 32" = "red"),
                    labels = c("Below 28", "28-32", "Above 32")) +
  
  theme_void() +
  labs(title = "Wet Bulb Global Temperature", x = "Longitude", y = "Latitude", 
       caption = "20 Year Return Period")


# Plot Population ---------------------------------------------------------
# Define colors

ggplot() +
  geom_sf(data = shp, aes(fill = population), color = "white") +
  scale_fill_gradient(name = "Population Count", 
                      low = "thistle", 
                      high = "darkorchid", 
                      na.value = "white") +
  theme_void() +
  labs(title = "Population Count by ADM2(Sub-Division 2)", x = "Longitude", y = "Latitude")




# Affected Population Plot ------------------------------------------------
shp <- shp %>%
  mutate(
    affected_pop = case_when(
      mean_temp < 28             ~ "Population in Below 28",
      mean_temp >= 28 & mean_temp <= 32 ~ "Population in 28-32",
      mean_temp > 32             ~ "Population in Above 32"
    )
  )

colors <- brewer.pal(3, "Set2")

ggplot() +
  geom_sf(data = shp, aes(fill = affected_pop), color = "white") +
  scale_fill_manual(values = c("Population in Below 28" = colors[1],
                               "Population in 28-32" = colors[2],
                               "Population in Above 32" = colors[3]),
                    name = "Affected Population",
                    na.value = "white") +
  theme_void() +
  labs(title = "Population Impacted by Categories of Heat Stress", x = "Longitude", y = "Latitude")









