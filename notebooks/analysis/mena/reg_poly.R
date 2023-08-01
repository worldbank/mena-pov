## Validate the simple Vulnerability Index by comparing it to a regression where
## Nighttime lights is the dependent variable. Use three specifications : Polynomial,
## Cubic Spline, Bins, and Linear Spline to see which fits the data.


# Loading Data ------------------------------------------------------------
vul_indicators <- read.csv(file.path(mena_file_path,
                                     "csv_files_all_layers",
                                     "Vul_Indicators_ADM2_original.csv")) %>% as.data.frame()


# Subset indicators -------------------------------------------------------

vul_sub <- vul_indicators %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,POP_mean_2020,RWI_mean, NTL_mean_2020)



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



# Regressions -------------------------------------------------------------


### Polynomial
poly_df <- vul_sub %>%
  drop_na() %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,RWI_mean, NTL_mean_2020) %>%
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
         RWI_mean_squared = RWI_mean^2,
         CO2_road_interaction = log(CO2_mean_2020) * road_raw,
         CO2_RWI_interaction = CO2_mean_2020 * RWI_mean,
         CO2_URBAN_interaction = CO2_mean_2020 * URBAN_2020,
         NO2_road_interaction = log(NO2_mean_2020) * road_raw,
         NO2_URBAN_interaction = log(NO2_mean_2020) * URBAN_2020,
         NO2_RWI_interaction = log(NO2_mean_2020) * RWI_mean,
         road_pm25_interaction = road_raw * log(pm25_mean_2019),
         road_FLOOD_interaction = road_raw * FLOOD_2016_2020,
         road_QUAKE_interaction = road_raw * QUAKE_2016_2020,
         road_Loss_sqkm_interaction = road_raw * Loss_sqkm_2016_2020,
         road_CDI_interaction = road_raw * CDI_2020_mean,
         road_PRECIP_interaction = road_raw * PRECIP_DIFF_2016_2020,
         road_TEMP_interaction = road_raw * TEMP_DIFF_2016_2020,
         road_RWI_interaction = road_raw * RWI_mean,
         URBAN_pm25_interaction = URBAN_2020 * log(pm25_mean_2019),
         URBAN_FLOOD_interaction = URBAN_2020 * FLOOD_2016_2020,
         URBAN_QUAKE_interaction = URBAN_2020 * QUAKE_2016_2020,
         URBAN_Loss_sqkm_interaction = URBAN_2020 * Loss_sqkm_2016_2020,
         URBAN_CDI_interaction = URBAN_2020 * CDI_2020_mean,
         URBAN_PRECIP_interaction = URBAN_2020 * PRECIP_DIFF_2016_2020,
         URBAN_TEMP_interaction = URBAN_2020 * TEMP_DIFF_2016_2020,
         URBAN_RWI_interaction = URBAN_2020 * RWI_mean)

poly_model_rwi <- lm(log(NTL_mean_2020) ~ CO2_mean_2020 + log(NO2_mean_2020) + road_raw +
                   URBAN_2020 + log(pm25_mean_2019) + FLOOD_2016_2020 + QUAKE_2016_2020 +
                   Loss_sqkm_2016_2020 + CDI_2020_mean + PRECIP_DIFF_2016_2020 + RWI_mean + TEMP_DIFF_2016_2020 +
                  CO2_mean_2020_squared + NO2_mean_2020_squared + pm25_mean_2019_squared +
                  FLOOD_2016_2020_squared + QUAKE_2016_2020_squared + Loss_sqkm_2016_2020_squared +
                  CDI_2020_mean_squared + PRECIP_DIFF_2016_2020_squared + TEMP_DIFF_2016_2020_squared +
                  RWI_mean_squared + CO2_road_interaction + CO2_RWI_interaction + CO2_URBAN_interaction + 
                  NO2_road_interaction + NO2_URBAN_interaction + NO2_RWI_interaction + road_pm25_interaction +
                  road_FLOOD_interaction + road_QUAKE_interaction + road_Loss_sqkm_interaction + road_CDI_interaction + 
                  + road_PRECIP_interaction + road_TEMP_interaction + road_RWI_interaction + URBAN_pm25_interaction +
                  URBAN_FLOOD_interaction + URBAN_QUAKE_interaction + URBAN_Loss_sqkm_interaction + URBAN_CDI_interaction + 
                  URBAN_PRECIP_interaction + URBAN_TEMP_interaction + URBAN_RWI_interaction, 
                  data = poly_df)

# summary with cluster-robust SEs
summary(poly_model_rwi, cluster="ID_ADM") 

# create table in stargazer
stargazer(poly_model_rwi, se=list(coef(summary(poly_model_rwi,cluster = c("ID_ADM")))[, 2]), type = "text") 

# Extract fitted values and residuals
fitted_values <- fitted(poly_model_rwi)
residuals <- residuals(poly_model_rwi)

# Calculate confidence intervals for the fitted values
confidence_intervals <- predict(poly_model_rwi, interval = "confidence", level = 0.95)

# drop NAs
VI <- vul_sub_original_index %>%
  drop_na()


plot_data <- data.frame(VI = VI$VulPopSh_2016_2020,
                        mean_CO2 = log(poly_df$CO2_mean_2020),
                        mean_NO2 = log(poly_df$NO2_mean_2020),
                        mean_pm = log(poly_df$pm25_mean_2019),
                        urban = poly_df$URBAN_2020,
                        temp_diff = poly_df$TEMP_DIFF_2016_2020 ,
                        interaction_no2_rwi = poly_df$NO2_RWI_interaction,
                        RWI_mean_squared = poly_df$RWI_mean_squared,
                        precip_diff = poly_df$PRECIP_DIFF_2016_2020,
                        drought = poly_df$CDI_2020_mean,
                        Fitted_Values = fitted_values)





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

plot_rwi_squared <- ggplot(plot_data, aes(y = Fitted_Values, x = RWI_mean_squared)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "yellow") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = "Mean Relative Wealth Index, squared",
    y = "% Change in Mean NTL") +
  theme_classic()


plot_no2_rwi <- ggplot(plot_data, aes(y = Fitted_Values, x = interaction_no2_rwi)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "blue") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
       x = "Interaction b/w log(NO2) and RWI",
       y = "% Change in Mean NTL") +
  theme_classic()



# Combine the plots into a grid panel
library(gridExtra)
grid.arrange(plot_co2, plot_no2, plot_pm,plot_temp,
             plot_precip,plot_urban,plot_drought,
             plot_rwi_squared, plot_no2_rwi, 
             nrow = 3, ncol = 3)


plot_fitted_values <- ggplot(plot_data, aes(y = Fitted_Values, x = VI)) +
  geom_point(color = "grey",size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x,2, raw = TRUE), color = "black") +
  geom_smooth(color = "black", linetype = "dotted") +
  labs(
    x = " Simple Vulnerability Index (w/ RWI) ",
    y = "% Change in Mean NTL (Estimated)") +
  theme_classic()

plot_fitted_values

plot_fitted_values <- ggplot(plot_data, aes(y = Fitted_Values, x = VI)) +
  geom_point(color = "grey", size = 1) +
  stat_smooth(method = "gam", formula = y ~ poly(x, 2, raw = TRUE), color = "black") +
  geom_smooth(aes(linetype = "Perfect Fit", color = "Polynomial Fit (Order 2)"), show.legend = TRUE) +
  labs(
    x = "Simple Vulnerability Index (w/ RWI)",
    y = "% Change in Mean NTL (Estimated)",
    linetype = "",
    color = ""
  ) +
  scale_linetype_manual(values = "dotted") +
  scale_color_manual(values = "black") +
  theme_classic()

print(plot_fitted_values)




