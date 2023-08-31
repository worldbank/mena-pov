## Population and NTL


# Load Data ---------------------------------------------------------------
df <- readRDS(file.path(lbn_file_path,
          "Nighttime_Lights",
          "final",
          "lbn_municipality_pop.Rds"))

municipality_sp <- st_read(file.path(lbn_file_path,
                                     "Boundaries",
                                     "gadm41_LBN_3.shp")) %>% as_Spatial()

# Prepare data ------------------------------------------------------------
df_summ <- df %>%
  group_by(uid, year,) %>%
  summarise(ntl_mean = mean(ntl_mean_prop2),
            population = mean(population)) %>%
  filter(year <= 2020) 


# Plot --------------------------------------------------------------------
df_annual <- df_summ %>%
  group_by(uid,year) %>%
  summarize(pop_mean = mean(population))





df_summ %>%
  ggplot(aes(x = ntl_mean, y = population)) +
  geom_smooth(size = 1, color = "black") +
  geom_text(
    aes(label = paste("Correlation =", round(cor(ntl_mean,population), 2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(x = "Mean NTL", y = "Population", title = "Correlation") +
  theme_classic2()
