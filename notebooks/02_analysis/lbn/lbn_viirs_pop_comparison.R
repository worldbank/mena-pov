## Population and NTL


# Load Data ---------------------------------------------------------------
df <- readRDS(file.path(lbn_onedrive_dir,
                        "data",
                        "municipalities",
                        "lbn_municipality_ntl_pop.Rds"))

municipality_sp <- st_read(file.path(lbn_file_path,
                                     "Boundaries",
                                     "gadm41_LBN_3.shp")) %>% as_Spatial()

wdi_pop_count <- read_csv(file.path(lbn_onedrive_dir,
                                    "data",
                                    "country",
                                    "raw",
                                    "POP_count.csv"), skip = 3) %>%
  clean_names() %>%
  filter(country_name == "Lebanon") %>%
  select(country_name,starts_with("x")) %>%
  melt(., id = "country_name") %>%
  mutate(year = as.numeric(gsub("x","",variable))) %>%
  select(-variable) %>%
  rename("pop_count_wdi"="value") %>%
  filter(year>= "2012" & year<= "2020")

# Prepare data ------------------------------------------------------------
df_annual <- df %>%
  group_by(uid,year) %>%
  reframe(ntl_mean = mean(ntl_mean),
            ntl_median = median(ntl_median),
            ntl_prop_g2 = mean(ntl_prop_g2),
            pop_count = pop_count) %>%
  distinct()%>%
  filter(year <= 2020)


# Plot --------------------------------------------------------------------
df_annual %>%
  ggplot(aes(x = pop_count, y = ntl_mean)) +
  geom_point(size = 1) +
  geom_line(size = 1) +
  geom_text(
    aes(label = paste("Correlation =", round(cor(ntl_mean,pop_count), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  labs(x = "Population Count(Municipality Level/ADM 3)",  y = "Nighttime Lights, Average", 
       title = "Correlation b/w NTL and Population Count (Municipality Level/ADM 3)") +
  facet_wrap(~year) +
  scale_x_continuous(labels = scales::comma) +
  theme_classic2()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pop_ntl_crossection.png"), width = 8, height = 5)


df_annual %>%
  group_by(year) %>%
  reframe(pop_count_worldpop = sum(pop_count)) %>%
  left_join(wdi_pop_count, by = "year") %>%
  select(-country_name) %>%
  melt(., id = "year") %>%
  ggplot(aes(x = year, y = value, color = variable)) +
  geom_line()+
  geom_point() +
  labs(x = "Year", y = "Population Count (Country Level)",
       title = "Comparison b/w Population from WorldPop and WDI (2012 - 2020)", color = "") +
  scale_y_continuous(labels = scales::comma)+
  geom_vline(xintercept = 2015, color = "grey", linetype = "dashed")+
  geom_text(
    aes(label = paste("Divergence in data starting 2015")),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 4,
    color = "black"
  ) +
  theme_classic2()
  
ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pop_comp.png"), width = 8, height = 6)

df_annual %>%
  group_by(uid) %>%
  mutate(pop_pct_change_2012_2018 = ((pop_count[year == 2018] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
         ntl_pct_change_2012_2018 = ((ntl_mean[year == 2018] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100) %>%
  ggplot(aes(x = pop_pct_change_2012_2018, y = ntl_pct_change_2012_2018)) +
  geom_point(size = 1)+
  geom_line(size = 1)+
  geom_text(
    aes(label = paste("Correlation =", round(cor(pop_pct_change_2012_2018,ntl_pct_change_2012_2018), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  labs( x = "Percentage Change in Population Count (2012 & 2018)",
        y = "Percentage Change in Nighttime Lights (2012 & 2018)",
        title = "Comparing the Percentage Change in NTL and Population Count (2012 & 2018),\n(Municipality Level/ADM3)") +
  theme_classic2()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pct_change_ntl_pop_2012_2018_adm3.png"), width = 8, height = 6)


df_annual %>%
  group_by(uid) %>%
  mutate(pop_pct_change_2012_2020 = ((pop_count[year == 2020] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
         ntl_pct_change_2012_2020 = ((ntl_mean[year == 2020] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100) %>%
  ggplot(aes(x = pop_pct_change_2012_2020, y = ntl_pct_change_2012_2020)) +
  geom_point(size = 1)+
  geom_line(size = 1)+
  geom_text(
    aes(label = paste("Correlation =", round(cor(pop_pct_change_2012_2020,ntl_pct_change_2012_2020), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  labs( x = "Percentage Change in Population Count (2012 & 2020)",
        y = "Percentage Change in Nighttime Lights (2012 & 2020)",
        title = "Comparing the Percentage Change in NTL and Population Count (2012 & 2020),\n(Municipality Level/ADM3)") +
  theme_classic2()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pct_change_ntl_pop_2012_2020_adm3.png"), width = 8, height = 6)



municipality_sf <- st_as_sf(municipality_sp)
municipality_sf$uid <- 1:nrow(municipality_sf)

df_annual %>%
  left_join(municipality_sf, by = "uid") %>%
  group_by(NAME_2,year) %>%
  reframe(ntl_mean = mean(ntl_mean),
          ntl_median = median(ntl_median),
          pop_count = sum(pop_count)) %>%
  ggplot(aes(x = pop_count, y = ntl_mean)) +
  geom_point(size = 1) +
  geom_line(size = 1) +
  geom_text(
    aes(label = paste("Correlation =", round(cor(ntl_mean,pop_count), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  facet_wrap(~year)+
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Population Count (District/ADM2)",
       y =  "Nighttime Lights, Average",
       title = "Comparison b/w NTL and Population Count (District/ADM2)") +
  theme_classic2()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pop_ntl_crossection_adm2.png"), width = 10, height = 8)
  

df_annual %>%
  left_join(municipality_sf, by = "uid") %>%
  group_by(NAME_2,year) %>%
  reframe(ntl_mean = mean(ntl_mean),
          pop_count = sum(pop_count)) %>%
  ungroup() %>%
  group_by(NAME_2) %>%
  mutate(pop_pct_change_2012_2018 = ((pop_count[year == 2018] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
         ntl_pct_change_2012_2018 = ((ntl_mean[year == 2018] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100) %>%
  ggplot(aes(x = pop_pct_change_2012_2018, y = ntl_pct_change_2012_2018)) +
  geom_point(size = 1)+
  geom_line(size = 1)+
  geom_text(
    aes(label = paste("Correlation =", round(cor(pop_pct_change_2012_2018,ntl_pct_change_2012_2018), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  labs( x = "Percentage Change in Population Count (2012 & 2018)",
        y = "Percentage Change in Nighttime Lights (2012 & 2018)",
        title = "Comparing the Percentage Change in NTL and Population Count (2012 & 2018),\n(District/ADM2)") +
  theme_classic2()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pct_change_ntl_pop_2012_2018_adm2.png"), width = 8, height = 6)


df_annual %>%
  left_join(municipality_sf, by = "uid") %>%
  group_by(NAME_2,year) %>%
  reframe(ntl_mean = mean(ntl_mean),
          pop_count = sum(pop_count)) %>%
  ungroup() %>%
  group_by(NAME_2) %>%
  mutate(pop_pct_change_2012_2020 = ((pop_count[year == 2020] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
         ntl_pct_change_2012_2020 = ((ntl_mean[year == 2020] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100) %>%
  ggplot(aes(x = pop_pct_change_2012_2020, y = ntl_pct_change_2012_2020)) +
  geom_point(size = 1)+
  geom_line(size = 1)+
  geom_text(
    aes(label = paste("Correlation =", round(cor(pop_pct_change_2012_2020,ntl_pct_change_2012_2020), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 3,
    color = "black"
  ) +
  labs( x = "Percentage Change in Population Count (2012 & 2020)",
        y = "Percentage Change in Nighttime Lights (2012 & 2020)",
        title = "Comparing the Percentage Change in NTL and Population Count (2012 & 2020),\n(District/ADM2)") +
  theme_classic2()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "graphs",
                            "pct_change_ntl_pop_2012_2020_adm2.png"), width = 8, height = 6)



map_df <- df_annual %>%
  left_join(municipality_sf, by = "uid") %>%
  group_by(NAME_2,year) %>%
  reframe(ntl_mean = mean(ntl_mean),
          pop_count = sum(pop_count)) %>%
  ungroup() %>%
  group_by(NAME_2) %>%
  reframe(pop_pct_change_2012_2018 = ((pop_count[year == 2018] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
          ntl_pct_change_2012_2018 = ((ntl_mean[year == 2018] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100,
          pop_pct_change_2012_2020 = ((pop_count[year == 2020] - pop_count[year == 2012])/(pop_count[year == 2012]))*100,
         ntl_pct_change_2012_2020 = ((ntl_mean[year == 2020] - ntl_mean[year == 2012])/(ntl_mean[year==2012]))*100) %>%
  distinct() %>%
  left_join(municipality_sf, by = "NAME_2")


ggplot(data = st_as_sf(map_df)) +
  geom_sf(aes(fill = pop_pct_change_2012_2020), color = NA, alpha = 0.7) +
  labs(title = "Percentage Change in Population Count b/w 2012 and 2020 (District/ADM2)", 
       fill = "Pct Change \n(2012 & 2020)") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_void()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "maps",
                            "pct_change_pop_2012_2020_adm2_map.png"), width = 8, height = 6)


ggplot(data = st_as_sf(map_df)) +
  geom_sf(aes(fill = ntl_pct_change_2012_2020), color = NA, alpha = 0.8) +
  labs(title = "Percentage Change in Nighttime Lights b/w 2012 and 2020 (District/ADM2)", 
       fill = "Pct Change \n(2012 & 2020)") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_void()

ggsave(filename = file.path(lbn_onedrive_dir,
                            "maps",
                            "pct_change_ntl_2012_2020_adm2_map.png"), width = 8, height = 6)
