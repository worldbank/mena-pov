## Polynomial Regression without RWI

# Loading Data ------------------------------------------------------------
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
         TEMP_DIFF_2016_2020,POP_mean_2020, NTL_mean_2020)



# Create the simple index
vul_sub_original_index <- vul_sub %>%
  mutate(across(-c(ID_ADM,POP_mean_2020), normalize, na.rm = T )) %>%
  mutate(POP_mean_2020_log_norm = normalize(log(POP_mean_2020))) %>%
  mutate(exposure_2020 = CO2_mean_2020 + NO2_mean_2020 + pm25_mean_2019 +
           Loss_sqkm_2016_2020 + PRECIP_DIFF_2016_2020 +TEMP_DIFF_2016_2020 +
           CDI_2020_mean + QUAKE_2016_2020 + FLOOD_2016_2020,
         exposure = normalize(exposure_2020, na.rm = T)) %>%
  mutate(VulPopSh_2016_2020_without_rwi = (exposure - NTL_mean_2020 - road_raw)*POP_mean_2020_log_norm,
         VulPopSh_2016_2020_without_rwi = normalize(VulPopSh_2016_2020_without_rwi,na.rm = T)) %>%
  left_join(MENA_adm, by = "ID_ADM")

# Create a summary table for all numeric variables by group
summary_table <- vul_sub_original_index %>%
  group_by(WB_ADM0_NA) %>%
  summarise(count = n())


# Without RWI -------------------------------------------------------------

poly_df_without_rwi <- vul_sub %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,NTL_mean_2020) %>%
  mutate(CO2_mean_2020_squared = CO2_mean_2020^2,
         NO2_mean_2020_squared =(log(NO2_mean_2020))^2,
         road_raw_squared = road_raw^2,
         URBAN_2020_squared = URBAN_2020^2,
         pm25_mean_2019_squared = (log(pm25_mean_2019))^2,
         FLOOD_2016_2020_squared = FLOOD_2016_2020^2,
         QUAKE_2016_2020_squared = QUAKE_2016_2020^2,
         Loss_sqkm_2016_2020_squared = Loss_sqkm_2016_2020^2,
         CDI_2020_mean_squared = CDI_2020_mean^2,
         PRECIP_DIFF_2016_2020_squared = PRECIP_DIFF_2016_2020^2,
         TEMP_DIFF_2016_2020_squared = TEMP_DIFF_2016_2020^2,
         CO2_road_interaction = log(CO2_mean_2020) * road_raw,
         CO2_URBAN_interaction = CO2_mean_2020 * URBAN_2020,
         NO2_road_interaction = log(NO2_mean_2020) * road_raw,
         NO2_URBAN_interaction = log(NO2_mean_2020) * URBAN_2020,
         road_pm25_interaction = road_raw * log(pm25_mean_2019),
         road_FLOOD_interaction = road_raw * FLOOD_2016_2020,
         road_QUAKE_interaction = road_raw * QUAKE_2016_2020,
         road_Loss_sqkm_interaction = road_raw * Loss_sqkm_2016_2020,
         road_CDI_interaction = road_raw * CDI_2020_mean,
         road_PRECIP_interaction = road_raw * PRECIP_DIFF_2016_2020,
         road_TEMP_interaction = road_raw * TEMP_DIFF_2016_2020,
         URBAN_pm25_interaction = URBAN_2020 * log(pm25_mean_2019),
         URBAN_FLOOD_interaction = URBAN_2020 * FLOOD_2016_2020,
         URBAN_QUAKE_interaction = URBAN_2020 * QUAKE_2016_2020,
         URBAN_Loss_sqkm_interaction = URBAN_2020 * Loss_sqkm_2016_2020,
         URBAN_CDI_interaction = URBAN_2020 * CDI_2020_mean,
         URBAN_PRECIP_interaction = URBAN_2020 * PRECIP_DIFF_2016_2020,
         URBAN_TEMP_interaction = URBAN_2020 * TEMP_DIFF_2016_2020)

poly_df_without_rwi <- poly_df_without_rwi[is.finite(log(poly_df_without_rwi$NTL_mean_2020)), ]


# Create a summary table for all numeric variables by group
poly_model_without_rwi <- lm(log(NTL_mean_2020) ~ CO2_mean_2020 + log(NO2_mean_2020) + road_raw +
                               URBAN_2020 + log(pm25_mean_2019) + FLOOD_2016_2020 + QUAKE_2016_2020 +
                               Loss_sqkm_2016_2020 + CDI_2020_mean + PRECIP_DIFF_2016_2020 + TEMP_DIFF_2016_2020 +
                               CO2_mean_2020_squared + NO2_mean_2020_squared + pm25_mean_2019_squared +
                               FLOOD_2016_2020_squared + QUAKE_2016_2020_squared + Loss_sqkm_2016_2020_squared +
                               CDI_2020_mean_squared + PRECIP_DIFF_2016_2020_squared + TEMP_DIFF_2016_2020_squared +
                               CO2_road_interaction + CO2_URBAN_interaction + NO2_road_interaction + NO2_URBAN_interaction  + 
                               road_pm25_interaction + road_FLOOD_interaction + road_QUAKE_interaction + road_Loss_sqkm_interaction + 
                               road_CDI_interaction + road_PRECIP_interaction + road_TEMP_interaction +  URBAN_pm25_interaction +
                               URBAN_FLOOD_interaction + URBAN_QUAKE_interaction + URBAN_Loss_sqkm_interaction + URBAN_CDI_interaction + 
                               URBAN_PRECIP_interaction + URBAN_TEMP_interaction , data = poly_df_without_rwi)

# summary with cluster-robust SEs
summary(poly_model_without_rwi, cluster="ID_ADM") 

# create table in stargazer
stargazer(poly_model_without_rwi, se=list(coef(summary(poly_model_without_rwi,cluster = c("ID_ADM")))[, 2]), type = "text") 

# Extract fitted values and residuals
fitted_values <- fitted(poly_model_without_rwi)
residuals <- residuals(poly_model_without_rwi)

# Calculate confidence intervals for the fitted values
confidence_intervals <- predict(poly_model_without_rwi, interval = "confidence", level = 0.95)

poly_df_without_rwi <- poly_df_without_rwi %>%
  drop_na()

VI <- vul_sub_original_index %>%
  drop_na() %>%
  semi_join(poly_df_without_rwi, by = "ID_ADM")

plot_data <- data.frame(VI= VI$VulPopSh_2016_2020_without_rwi,
  mean_CO2 = log(poly_df_without_rwi$CO2_mean_2020),
  mean_NO2 = log(poly_df_without_rwi$NO2_mean_2020),
  mean_pm = log(poly_df_without_rwi$pm25_mean_2019),
  urban = poly_df_without_rwi$URBAN_2020,
  temp_diff = poly_df_without_rwi$TEMP_DIFF_2016_2020 ,
  precip_diff = poly_df_without_rwi$PRECIP_DIFF_2016_2020,
  drought = poly_df_without_rwi$CDI_2020_mean,
  Fitted_Values = fitted_values,
  ID_ADM = poly_df_without_rwi$ID_ADM)

plot_data <- plot_data %>%
  left_join(MENA_adm, by = "ID_ADM")

count_by_country <- plot_data %>%
  group_by(WB_ADM0_NA) %>%
  summarise(count = n())


# Plots -------------------------------------------------------------------
plot_co2 <- ggplot(plot_data, aes(y = Fitted_Values, x = mean_CO2)) +
  geom_point(color = "grey", size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "green") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "Mean CO2 (kiloton)",
    y = "% Change in Mean NTL") +
  theme_classic()

plot_no2 <- ggplot(plot_data, aes(y = Fitted_Values, x = mean_NO2)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "orange") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "% Change in Mean NO2",
    y = "% Change in Mean NTL") +
  theme_classic()

plot_pm <- ggplot(plot_data, aes(y = Fitted_Values, x = mean_pm)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "black") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "% Change in Mean PM 2.5",
    y = "% Change in Mean NTL") +
  theme_classic()


plot_urban <- ggplot(plot_data, aes(y = Fitted_Values, x = urban)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "pink") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "Urban Density (sq.km)",
    y = "% Change in Mean NTL") +
  theme_classic()

plot_temp <- ggplot(plot_data, aes(y = Fitted_Values, x = temp_diff)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "red") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "Difference in Temperature (C)",
    y = "% Change in Mean NTL") +
  theme_classic()

plot_precip <- ggplot(plot_data, aes(y = Fitted_Values, x = precip_diff)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "brown") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "Difference in Rainfall",
    y = "% Change in Mean NTL") +
  theme_classic()

plot_drought <- ggplot(plot_data, aes(y = Fitted_Values, x = drought)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "purple") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "Combined Drought Index",
    y = "% Change in Mean NTL") +
  theme_classic()


# Combine the plots into a grid panel
library(gridExtra)
grid.arrange(plot_co2, plot_no2, plot_pm,plot_temp,
             plot_precip,plot_urban,plot_drought,
             nrow = 3, ncol = 3)


plot_fitted_values<- ggplot() +
  geom_point(data = plot_data, aes(y = Fitted_Values, x = VI), color = "#008080")+
  geom_smooth(data = plot_data, aes(y = Fitted_Values, x = VI), color = "black") +
  labs(
    x = " Simple Vulnerability Index (w/o RWI) ",
    y = "% Change in Mean NTL (Estimated)") +
  theme_classic()

plot_fitted_values

plot_fitted_values_by_country <- ggplot() +
  geom_point(data = plot_data, aes(y = Fitted_Values, x = VI), color = "#008080", size = 1)+
  geom_smooth(data = plot_data, aes(y = Fitted_Values, x = VI), color = "black") +
  labs(
    x = " Simple Vulnerability Index (w/o RWI) ",
    y = "% Change in Mean NTL (Estimated)") +
  theme_classic() +
  facet_wrap(~WB_ADM0_NA)

plot_fitted_values_by_country





