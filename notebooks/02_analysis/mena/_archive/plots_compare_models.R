# Compare models : Linear, Polynomial, GAM

# Step 1: We do a visual comparison of each model as a grid panel (with and without RWI)
# Step 2: Compare models by creating a table with each of the coefficients
# Step 3: Create a data frame with RMSE for each model, create a table




# Load data ---------------------------------------------------------------
vul_indicators <- read.csv(file.path(mena_file_path,
                                     "csv_files_all_layers",
                                     "Vul_Indicators_ADM2_original.csv")) %>% as.data.frame()

MENA_adm <- st_read(file.path(mena_file_path,
                              "Boundaries",
                              "MENA_ADM2.shp"))



# Subset indicators -------------------------------------------------------
vul_sub <- vul_indicators %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,POP_mean_2020,RWI_mean, NTL_mean_2020) %>% 
  mutate(log_ntl = log(NTL_mean_2020),
         log_co2 = log(CO2_mean_2020),
         log_no2 = log(NO2_mean_2020),
         log_pm = log(pm25_mean_2019))

# Create the simple index
vul_sub_original_index <- vul_sub %>%
  mutate(across(-c(ID_ADM,POP_mean_2020), normalize, na.rm = T )) %>%
  mutate(POP_mean_2020_log_norm = normalize(log(POP_mean_2020))) %>%
  mutate(exposure_2020 = CO2_mean_2020 + NO2_mean_2020 + pm25_mean_2019 +
           Loss_sqkm_2016_2020 + PRECIP_DIFF_2016_2020 +TEMP_DIFF_2016_2020 +
           CDI_2020_mean + QUAKE_2016_2020 + FLOOD_2016_2020,
         exposure = normalize(exposure_2020, na.rm = T)) %>%
  mutate(VulPopSh_2016_2020 = (exposure - RWI_mean - NTL_mean_2020 - road_raw)*POP_mean_2020_log_norm,
         VulPopSh_2016_2020 = normalize(VulPopSh_2016_2020, na.rm = T),
         VulPopSh_2016_2020_without_rwi = (exposure - NTL_mean_2020 - road_raw)*POP_mean_2020_log_norm,
         VulPopSh_2016_2020_without_rwi = normalize(VulPopSh_2016_2020_without_rwi,na.rm = T))


vul_sub$VI_rwi <- vul_sub_original_index$VulPopSh_2016_2020
vul_sub$VI_no_rwi <- vul_sub_original_index$VulPopSh_2016_2020_without_rwi

# Partition data ----------------------------------------------------------
set.seed(123)

training.samples <- vul_sub$NTL_mean_2020 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- vul_sub[training.samples, ]
test.data <- vul_sub[-training.samples, ]



# Plots -------------------------------------------------------------------
plot_co2 <- ggplot(train.data, aes(CO2_mean_2020, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"), show.legend = T) +
  labs (color = "",
        x = "Mean CO2 (kiloton)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "green", "GAM(Spline)" = "black")) +
  theme_classic()


plot_no2 <- ggplot(train.data, aes(log_no2, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "% Change in Mean NO2",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "orange", "GAM(Spline)" = "black")) +
  theme_classic()


plot_pm <- ggplot(train.data, aes(log_pm, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "% Change in Mean PM 2.5",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "purple", "GAM(Spline)" = "black")) +
  theme_classic() 

plot_urban <- ggplot(train.data, aes(URBAN_2020, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Urban Density (sq. km)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "pink", "GAM(Spline)" = "black")) +
  theme_classic() 

plot_temp <- ggplot(train.data, aes(TEMP_DIFF_2016_2020, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Difference in Temperature(C)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "red", "GAM(Spline)" = "black")) +
  theme_classic() 

plot_precip <- ggplot(train.data, aes(PRECIP_DIFF_2016_2020, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Difference in Rainfall",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "brown", "GAM(Spline)" = "black")) +
  theme_classic() 

plot_drought <- ggplot(train.data, aes(CDI_2020_mean, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Composite Drought Index",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "yellow", "GAM(Spline)" = "black")) +
  theme_classic() 


plot_rwi <- ggplot(train.data, aes(RWI_mean, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Mean Relative Wealth Index",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "blue", "GAM(Spline)" = "black")) +
  theme_classic() 
plot_road <- ggplot(train.data, aes(road_raw, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Road Density",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "#33F0FF", "GAM(Spline)" = "black")) +
  theme_classic() 


# Combine the plots into a grid panel
library(gridExtra)
grid.arrange(plot_co2, plot_no2, plot_pm,
             plot_temp,plot_precip,plot_urban,
             plot_drought,plot_rwi, plot_road, 
             nrow = 3, ncol = 3)



# Plot Simple Index vs. NTL -----------------------------------------------
plot_compare_rwi <- ggplot(train.data, aes(VI_rwi, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Simple Vulnerability Index (with RWI)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "#008080", "GAM(Spline)" = "black")) +
  theme_classic() 



plot_compare_no_rwi <- ggplot(train.data, aes(VI_no_rwi, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Simple Vulnerability Index (without RWI)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "#008080", "GAM(Spline)" = "black")) +
  theme_classic() 
 

# Compare VI by country ---------------------------------------------------

country_list <- c("Algeria", "Iraq", "Islamic Republic of Iran",
                  "Israel","Jordan","Lebanon","Libya", "Morocco","Qatar", "Yemen",
                  "Syrian Arab Republic", "Tunisia", "Arab Republic of Egypt",
                  "West Bank and Emirates")

#add adm level data from MENA shp
train.data <- train.data %>%
  #left_join(MENA_adm, by = "ID_ADM") %>%
  filter(WB_ADM0_NA %in% country_list)

# Plot
ggplot(train.data, aes(VI_no_rwi, log_ntl)) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "Simple Vulnerability Index (without RWI)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "#008080", "GAM(Spline)" = "black")) +
  theme_classic() +
  facet_wrap(~ WB_ADM0_NA)

