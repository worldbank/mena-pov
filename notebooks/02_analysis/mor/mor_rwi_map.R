## Load Packages
# Load required packages
library(stringdist)
library(exactextractr)
library(rjson)
library(jsonlite)
library(dplyr)
library(htmlwidgets)
library(shiny)


getOption("max.print")
options(max.print = 10000)

# Load data ---------------------------------------------------------------
morocco_shp <- st_read(file.path(mor_file_path,
                                   "Boundaries",
                                   "gadm41_MAR_4.shp"))


rwi <- read.csv(file.path(mor_file_path,
                          "RWI",
                          "raw",
                          "morocco_relative_wealth_index.csv"))

affected_pop <- read_excel(file.path(mor_onedrive_dir,
                                     "data",
                                     "earthquake_impact",
                                     "listes-localités.xlsx"))

intensity <- st_read(file.path(mor_onedrive_dir,
                               "data",
                               "shakemap_shp",
                               "pga.shp"))

mor_pop <- raster(file.path(mor_file_path,
                            "Population",
                            "raw",
                            "mar_ppp_2020_1km_Aggregated_UNadj.tif"))





# Prepare the intensity categories ----------------------------------------
intensity <- intensity %>%
  mutate(pga = PARAMVALUE*100)


# Extract Population ------------------------------------------------------
summarize_population <- function(values, coverage_fractions) {
  sum(values * coverage_fractions)
}


pop_worldpop <- exact_extract(mor_pop, morocco_shp, summarize_population)
morocco_shp$pop_worldpop <- pop_worldpop





# Create UID --------------------------------------------------------------
# clean

affected_pop_summ <- affected_pop %>%
  mutate(
    population = as.numeric(as.character(population)),
    menage = as.numeric(as.character(menage))
  ) %>%
  group_by(commune_fr,cercle_fr,province_fr) %>%
  summarise(
    total_population = sum(population, na.rm = TRUE),
    total_menages = sum(menage, na.rm = TRUE)
  ) %>%
  ungroup() ## 190 communes where we have information of affected people


## clean names in shapefile
morocco_shp_clean <- morocco_shp %>%
  mutate(NAME_4 = case_when(
    NAME_4 == "Zaouiat Annahlia" ~ "Zaouia Annahlia",
    NAME_4 == "Sidi Abdallah Ou Said" ~ "Sidi Abdellah Ou Said",
    NAME_4 == "Agoudim" ~"Aghouatim",
    NAME_4 == "Ouad Lbour" ~ "Ouad L'bour" ,
    NAME_4 == "Oulad Dlim" ~ "Ouled Dlim",
    NAME_4 == "Jnan Bouih" ~ "Jnane Bouih" ,
    NAME_4 == "Issaguen" ~"Iznaguen",
    NAME_4 == "Tizgazaouine" ~ "Tizgzaouine" ,
    NAME_4 == "Sidi Ahmed Ou Abdellah" ~"Sidi Ahmed Ou Abdallah",
    NAME_4 == "Zaouiat Sidi Tahar" ~ "Zaouia Sidi Tahar",
    TRUE ~ NAME_4),
    NAME_3 = case_when(
      NAME_3 ==  "NA (Ben Slimane)" ~ "Ben Slimane",
      NAME_3 ==  "NA (Hattane)" ~ "Hattane",
      NAME_3 ==  "NA (Mehdia)" ~ "Mehdia",
      NAME_3 ==  "NA (Ouezzane)" ~ "Ouezzane",
      NAME_3 ==  "NA (Al Idrissia)"  ~ "Al Idrissia",
      NAME_3 ==   "NA (Bou Chentouf)" ~  "Bou Chentouf",
      NAME_3 ==   "NA (Machouar Casablanca)" ~  "Machouar Casablanca",
      NAME_3 ==   "NA (Assa)"  ~  "Assa" ,
      NAME_3 ==   "NA (Tan-Tan)" ~ "Tan-Tan",
      NAME_3 == "NA (Foum Zguid)" ~ "Foum Zguid",
      NAME_3 == "NA (Ain Bni Mathar)"  ~ "Ain Bni Mathar",
      NAME_3 == "NA (Neima)" ~ "Neima" ,
      NAME_3 ==  "NA (Touarga)"  ~  "Touarga",
      NAME_3 == "NA (Demnate)"  ~ "Demnate", 
      NAME_3 == "Igherm" ~ "Irherm",
      NAME_3 == "Agadir Banl" ~ "Agadir Banlieue",
      NAME_3 == "Oulad Berhi" ~ "Oulad Berhil",
      NAME_3 =="Sidi Bou Ot" ~ "Sidi Bou Othmane",
      
      
      TRUE ~ NAME_3
    ))




# Merge RWI and Shapefile -------------------------------------------------

#convert rwi into a sf object
rwi_sf <- st_as_sf(rwi, coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(st_crs(morocco_shp))

#merge the points data and shapefile
merge_rwi_shp <- st_join(rwi_sf,morocco_shp_clean, left = TRUE)

agg_rwi <- merge_rwi_shp %>%
  group_by(NAME_4, NAME_3) %>%
  summarise(mean_rwi = mean(rwi), na.rm = T) %>%
  ungroup() %>%
  mutate(quartile = case_when(
    mean_rwi <= quantile(mean_rwi, 0.25) ~ "Bottom 25%",
    mean_rwi > quantile(mean_rwi, 0.25) & mean_rwi <= quantile(mean_rwi, 0.5) ~ "25th to 50th Percentiles",
    mean_rwi > quantile(mean_rwi, 0.5) & mean_rwi <= quantile(mean_rwi, 0.75) ~ "50th to 75th Percentiles",
    mean_rwi > quantile(mean_rwi, 0.75) ~ "Top 25%",
    TRUE ~ NA_character_
  )) %>% #1487 communes out of 1515 for which we have RWI
  st_drop_geometry() %>%
  left_join(affected_pop_summ, by = c("NAME_3" = "cercle_fr","NAME_4" = "commune_fr"))


merged_data <- morocco_shp_clean %>% left_join(agg_rwi)


# Cropping it to the earthquake area --------------------------------------------------
cropped_sf <- st_crop(merged_data,intensity)



# Plot --------------------------------------------------------------------
qpal <- colorFactor(palette = "OrRd", levels = c("Bottom 25%", 
                                                 "25th to 50th Percentiles", 
                                                 "50th to 75th Percentiles",
                                                 "Top 25%"))
legendLabels <- c("Bottom 25%", "25th to 50th Percentiles", "50th to 75th Percentiles", "Top 25%")
legendColors <- sapply(legendLabels, qpal)

pga_pal <- colorNumeric(palette = "viridis", domain = intensity$pga)



label_data <- paste0(
  "Commune:", cropped_sf$NAME_4, 
  "<br>",
  "RWI: ", 
  ifelse(is.na(cropped_sf$mean_rwi), "NA", round(cropped_sf$mean_rwi, 2)),
  "<br>",
  "Quartile:", 
  ifelse(is.na(cropped_sf$quartile), "NA", cropped_sf$quartile), 
  "<br>",
  "Population (WorldPop): ", round(cropped_sf$pop_worldpop,0)
)





rwi <- leaflet(cropped_sf) %>%
  # Base layer
  addTiles() %>%
  
  # Add the cropped_sf polygons
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
  
  # Add the intensity layer
  # Add the intensity layer with PGA coloring
  addPolylines(
    data = intensity,
    color = ~pga_pal(pga),
    fillColor = NA,
    fillOpacity = 0.6,
    weight = 2
  ) %>%
  
  # Add custom legend without NA values
  addLegend(colors = legendColors, 
            labels = legendLabels,
            opacity = 1, 
            title = "Relative Wealth Index", 
            position = "bottomright") %>%
  # Add a horizontal legend for PGA
  addLegend(pal = pga_pal, 
          values = intensity$pga,
          title = "PGA Values",
          position = "bottomright")

rwi



# ## Add Mean RWI
# agg_rwi_export <- agg_rwi %>%
#   rename("commune_fr" = "NAME_4" ,
#          "cercle_fr" = "NAME_3")






# Share of affected population --------------------------------------------
# First, calculate the total pop_worldpop across all quartiles
total_pop_worldpop <- sum(cropped_sf$pop_worldpop, na.rm = TRUE)

# Now, calculate mean_affected using the pre-calculated total_pop_worldpop
summ_data <- cropped_sf %>%
  group_by(quartile) %>%
  summarise(share_affected = (sum(total_population, na.rm = TRUE) / total_pop_worldpop)*100,
            share_affected_byquant = (sum(total_population, na.rm = TRUE) / sum(pop_worldpop, na.rm = T))*100) %>%
  drop_na()

summ_data$quartile <- factor(summ_data$quartile, levels = c("Bottom 25%", "25th to 50th Percentiles", "50th to 75th Percentiles",
                                                            "Top 25%"))



ggplot() +
  geom_point(data = summ_data, aes(x = quartile, y = share_affected)) +
  geom_line(data = summ_data, aes(x = quartile, y = share_affected, group = 1)) +
  geom_text(data = summ_data, aes(x = quartile, y = share_affected, label = round(share_affected, 2))) + # Label for blue dots
  
  geom_point(data = summ_data, aes(x = quartile, y = share_affected_byquant), color = "red") +
  geom_line(data = summ_data, aes(x = quartile, y = share_affected_byquant, group = 1), color = "red") +
  geom_text(data = summ_data, aes(x = quartile, y = share_affected_byquant, label = round(share_affected_byquant, 2)), color = "red") + # Label for red dots
  
  labs(x = "Distribution of Relative Wealth Index",
       y = "Share of Affected Population (%)",
       title = "Share of Affected Population by Relative Wealth Index, Morocco",
       caption = "Share of Affected Population = Total Population (provided by country team)/Total Population (WorldPop)") +
  theme_classic2()






# Export ------------------------------------------------------------------
setwd("C:/Users/wb569257/OneDrive - WBG/MENAPOV Geospatial Documents - MENAPOV Geospatial Files/Projects/mor_earthquake_analysis/maps")

# Save the leaflet widget as an html file
saveWidget(rwi, file = "rwi_map.html", selfcontained = T)


#Export as csv
install.packages("writexl")
library(writexl)

write.csv(agg_rwi_export, file = file.path(mor_onedrive_dir, 
                                         "data", 
                                         "earthquake_impact", 
                                         "commune_rwi.csv"))


