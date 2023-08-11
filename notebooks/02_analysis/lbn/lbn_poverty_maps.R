## Lebanon Maps for Poverty Analysis



# Load Data ---------------------------------------------------------------
lbn_gadm <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                              layer = "gadm41_LBN_1") %>% st_as_sf()

lbn_gadm2 <- readOGR(file.path(lbn_file_path,
                              "Boundaries"),
                    layer = "gadm41_LBN_2") %>% st_as_sf() %>% distinct()


seg1 <- read_excel(file.path(lbn_file_path,
                          "Team",
                          "Projects",
                          "Sampling",
                          "01- LVAP Listing map data",
                          "Phase1",
                          "Segment",
                          "Segmentation_Phase1.xls"))

seg2 <- read_excel(file.path(lbn_file_path,
                            "Team",
                            "Projects",
                            "Sampling",
                            "01- LVAP Listing map data",
                            "Phase2",
                            "Segment",
                            "Segmentation_Phase2.xls"))

#predicted poverty estimates
est_pov_gov <- read_excel(file.path(lbn_file_path,
                              "Team",
                              "Projects",
                              "pov_est",
                              "LBN_POV_subnational.xlsx"), sheet = 1)

est_pov_dist <- read_excel(file.path(lbn_file_path,
                                    "Team",
                                    "Projects",
                                    "pov_est",
                                    "LBN_POV_subnational.xlsx"), sheet = 2)


# Subset Data -------------------------------------------------------------
seg_sub1 <- seg1 %>%
  select(admin2Name,admin2Pcod) %>%
  group_by(admin2Name) %>%
  summarise(admin2Pcod = admin2Pcod) %>%
  distinct()

seg_sub2 <- seg2 %>%
  select(admin2Name,admin2Pcod) %>%
  group_by(admin2Name) %>%
  summarise(admin2Pcod = admin2Pcod) %>%
  distinct()

seg_sub <- bind_rows(seg_sub1, seg_sub2) %>%
  distinct() %>%
  rename("District" = "admin2Pcod")

# Merging Poverty Estimates -------------------------------------------------------

#Governorates
est_pov_gov_clean <- est_pov_gov %>%
  rename("NAME_1" = "Governorate") %>%
  filter(NAME_1 != "Total") %>%
  mutate(NAME = case_when(
    NAME_1 == "Baalbek-El Hermel" ~ "Baalbak - Hermel",
    NAME_1 == "El Nabatieh" ~ "Nabatiyeh",
    TRUE ~ NAME_1
  ))

lbn_gadm <- lbn_gadm %>%
  rename("NAME" = "NAME_1")

lbn_gadm_merged <- merge(lbn_gadm,est_pov_gov_clean, by = c("NAME"))



#Districts
est_pov_dist_clean <- merge(est_pov_dist, seg_sub, by = c("District"))

est_pov_dist_clean <- est_pov_dist_clean %>%
  mutate(NAME_2 = case_when(
    admin2Name == "El Hermel" ~ "Hermel",
    admin2Name == "El Minieh-Dennie" ~ "Minieh-Danieh",
    admin2Name == "El Nabatieh" ~ "Nabatiyeh",
    admin2Name == "Baalbek" ~ "Baalbeck",
    admin2Name == "Rachaya" ~ "Rachiaya",
    admin2Name == "Bent Jbeil" ~ "Bint Jbayl",
    admin2Name == "El Koura" ~ "Koura",
    admin2Name == "Zahle" ~ "Zahleh",
    admin2Name == "Jbeil" ~ "Jubail",
    admin2Name == "Kesrwane" ~ "Kasrouane",
    TRUE ~ admin2Name
  ))

#merge
lbn_gadm2_merged <- merge(lbn_gadm2,est_pov_dist_clean, by = c("NAME_2"))


main_cities <- data.frame(name = c("Tyre", "Sidon","Baalbek"),
                          lat = c(33.2705, 33.5571, 34.0047),
                          lon = c(35.2038, 35.3729, 36.2110))


# Plot --------------------------------------------------------------------

### Governorates
observed <- ggplot() +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black", size = 2) +
  geom_sf(data = lbn_gadm_merged, aes(fill = Observed),
          color = "black", alpha = 0.6) +
  labs(fill = "Observed", color = "") +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME, 1)), size = 2)+
  geom_text(data = main_cities, aes(x = lon, y = lat, label = name), size = 1.5) +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "brown"),
                       labels = scales::percent,
                       limits = c(0, 0.25))

observed

imputed <- ggplot() +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black", size = 3) +
  geom_sf(data = lbn_gadm_merged, aes(fill = Imputed),
          color = 'black', alpha = 0.5, size = 5) +
  labs(fill = "Imputed", color = "") +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME, 1)), size = 2)+
  geom_text(data = main_cities, aes(x = lon, y = lat, label = name), size = 1.5) +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("white","yellow", "orange", "brown"),
                       labels = scales::percent,
                       limits = c(0, 0.25))

imputed

total <- ggplot() +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black", size = 3) +
  geom_sf(data = lbn_gadm_merged, aes(fill = Total),
          color = "black", alpha = 0.5, size = 5) +
  labs(fill = "Total", color = "") +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME, 1)), size = 2)+
  geom_text(data = main_cities, aes(x = lon, y = lat, label = name), size = 1.5) +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("white","yellow", "orange", "brown"),
                       labels = scales::percent,
                       limits = c(0, 0.25))
total


## Districts
observed_dist <- ggplot() +
  geom_sf(data = lbn_gadm2_merged, aes(fill = Observed),
          color = "grey", alpha = 0.6, size = 3) +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black", size = 5) +
  labs(fill = "Observed", color = "") +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME, 1)), size = 2)+
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("white", "yellow", "orange", "brown"),
                       labels = scales::percent,
                       limits = c(0, 0.25),
                       na.value="grey")

observed_dist

imputed_dist <- ggplot() +
  geom_sf(data = lbn_gadm2_merged, aes(fill = Imputed),
          color = "grey", alpha = 0.5, size = 3) +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black", size = 5) +
  labs(fill = "Imputed", color = "") +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME, 1)), size = 2)+
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("white","yellow", "orange", "brown"),
                       labels = scales::percent,
                       limits = c(0, 0.25))

imputed_dist

total_dist <- ggplot() +
  geom_sf(data = lbn_gadm2_merged, aes(fill = Total),
          color = "grey", alpha = 0.5, size = 3) +
  geom_sf(data = st_as_sf(lbn_gadm), 
          fill = NA, 
          color = "black", size = 5) +
  labs(fill = "Total", color = "") +
  geom_sf_text(data =st_as_sf(lbn_gadm), aes(label = str_wrap(NAME, 1)), size = 2)+
  geom_text(data = main_cities, aes(x = lon, y = lat, label = name), size = 1.5) +
  theme_void() +
  coord_sf()+
  scale_fill_gradientn(colors = c("white","yellow", "orange", "brown"),
                       labels = scales::percent,
                       limits = c(0, 0.25))

total_dist
