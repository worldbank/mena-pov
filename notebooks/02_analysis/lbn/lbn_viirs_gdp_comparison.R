## Create NTL Trends


# Load Data ---------------------------------------------------------------
df <- readRDS(file.path(lbn_onedrive_dir,
                        "data",
                        "country",
                        "final",
                        "lbn_gdp_ntl.Rds"))


grid <- readRDS(file.path(lbn_file_path,
                        "Nighttime_Lights",
                        "final",
                        "lbn_grid_viirs.Rds"))


grid_sub <- grid %>%
  select(id,avg_rad_df,year) %>%
  group_by(id,year) %>%
  summarise(ntl_mean_pixel = mean(avg_rad_df), na.rm = T,
            ntl_median_pixel = median(avg_rad_df), na.rm = T)

# Trend Plot -----------------------------------------------------------------
df %>%
  melt(.,id = "year") %>%
  ggplot(aes(x = year, y = value)) +
  geom_point() +
  geom_line(size = 1, color = "black") +
  geom_vline(xintercept = 2018, color = "darkgrey", linetype = "dashed") +
  facet_wrap(~variable, scales = "free") +
  theme_classic2()


ggsave(filename = file.path(lbn_onedrive_dir,
                       "graphs",
                       "gdp_comp.png"), width = 8, height = 4)
  



# Correlation Plot --------------------------------------------------------
options(scipen=999)

p1 <- df %>%
  filter(!is.na(gdp_constant)) %>%
  ggplot(aes(y = ntl_mean, x = gdp_constant))+
  geom_point(size = 2, color = "black") +
  geom_text(aes(label = year), nudge_y = 0.05, size = 3, vjust = 0, check_overlap = TRUE) +  # Adding year labels
  geom_smooth( color = "black", se = F)+
  geom_text(
    aes(label = paste("Correlation =", round(cor(ntl_mean,gdp_constant),2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(x = "GDP (Constant 2015 US$)", y = "Nighttime Lights, Average", 
       title = "Correlation b/w NTL and GDP (Constant 2015 US$) (2012-2020)") +
  scale_x_continuous(labels = scales::comma) +
  theme_classic2()
p1
ggsave(p1, file = file.path(lbn_onedrive_dir,
                            "graphs",
                            "cor_ntl_gdp_constant.png"), width = 7, height = 4)

p2 <- df %>%
  filter(!is.na(gdp_constant)) %>%
  ggplot(aes(y = ntl_mean, x = gdp_constant_lcu))+
  geom_point(size = 2, color = "black") +
  geom_text(aes(label = year), nudge_y = 0.05, size = 3, vjust = 0, check_overlap = TRUE) +  # Adding year labels
  geom_smooth( color = "black", se = F)+
  geom_text(
    aes(label = paste("Correlation =", round(cor(ntl_mean,gdp_constant),2))),
    x = Inf, y = Inf,
    hjust = 1, vjust = 1,
    size = 5,
    color = "black"
  ) +
  labs(x = "GDP (Constant LCU)", y = "Nighttime Lights, Average", 
       title = "Correlation b/w NTL and GDP (Constant LCU) (2012-2020)") +
  scale_x_continuous(labels = scales::comma) +
  theme_classic2()

ggsave(p2, file = file.path(lbn_onedrive_dir,
                            "graphs",
                            "cor_ntl_gdp_constant_lcu.png"), width = 7.5, height = 4)

