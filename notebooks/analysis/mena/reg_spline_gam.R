## Generalized Additive Models (Essentially Splines with knots chosen automatically)


# Loading Data ------------------------------------------------------------
vul_indicators <- read.csv(file.path(mena_file_path,
                                     "csv_files_all_layers",
                                     "Vul_Indicators_ADM2_original.csv")) %>% as.data.frame()


# Subset indicators -------------------------------------------------------
vul_sub <- vul_indicators %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,POP_mean_2020,RWI_mean, NTL_mean_2020) %>% drop_na()

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


# GAM -----------------------------------
library(mgcv)

# Creating test and training data

set.seed(123)

training.samples <- vul_sub$NTL_mean_2020 %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- vul_sub[training.samples, ]
test.data <- vul_sub[-training.samples, ]

# Using length() to get total count for each variable
total_counts <- sapply(train.data, function(x) length(x[!is.na(x)]))
total_counts


CO2_mean_2020 + log(NO2_mean_2020) + road_raw +
  URBAN_2020 + log(pm25_mean_2019) + FLOOD_2016_2020 + QUAKE_2016_2020 +
  Loss_sqkm_2016_2020 + CDI_2020_mean + PRECIP_DIFF_2016_2020 + RWI_mean + TEMP_DIFF_2016_2020


# Build the model
model <- gam(log(NTL_mean_2020) ~ CO2_mean_2020 + s(log(NO2_mean_2020)) + s(road_raw) + s(URBAN_2020) + 
               s(log(pm25_mean_2019)) + FLOOD_2016_2020 + QUAKE_2016_2020 + Loss_sqkm_2016_2020 +
               CDI_2020_mean + PRECIP_DIFF_2016_2020 + s(RWI_mean) + s(TEMP_DIFF_2016_2020),  
             data = train.data)

summary(model)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$NTL_mean_2020),
  R2 = R2(predictions, test.data$NTL_mean_2020)
)



