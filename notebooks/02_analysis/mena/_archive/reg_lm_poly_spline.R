# Run regressions using the two models with train and test data. Compare their RMSE


# Load data ---------------------------------------------------------------
vul_indicators <- read.csv(file.path(mena_file_path,
                                     "csv_files_all_layers",
                                     "Vul_Indicators_ADM2_original.csv")) %>% as.data.frame()

# MENA_adm <- st_read(file.path(mena_file_path,
#                               "Boundaries",
#                               "MENA_ADM2.shp"))

################################### WITH RWI #########################################

# Subset indicators -------------------------------------------------------
vul_sub <- vul_indicators %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,POP_mean_2020,RWI_mean, NTL_mean_2020) %>% 
  mutate(log_ntl = log(NTL_mean_2020),
         log_pm  = log(pm25_mean_2019),
         log_no2 = log(NO2_mean_2020)) %>%
  drop_na()


# Partition data ----------------------------------------------------------
set.seed(123)

training.samples <- vul_sub$log_ntl %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- vul_sub[training.samples, ]
test.data <- vul_sub[-training.samples, ]


# Run lm ------------------------------------------------------------------

# Build the model
model_lm_rwi <- lm(log_ntl ~ CO2_mean_2020 +log_no2+log_pm+
                 FLOOD_2016_2020+QUAKE_2016_2020+
                 Loss_sqkm_2016_2020+CDI_2020_mean+
                 PRECIP_DIFF_2016_2020+TEMP_DIFF_2016_2020+
                 road_raw+URBAN_2020+RWI_mean
                 , data = train.data)
summary(model_lm_rwi)

# Make predictions
predictions_lm_rwi <- model_lm_rwi %>% predict(test.data)

# Model performance
lm_rwi <- data.frame(
  RMSE = RMSE(predictions_lm_rwi, test.data$log_ntl, na.rm = T),
  R2 = R2(predictions_lm_rwi, test.data$log_ntl, na.rm = T)
)

lm_rwi

# Run polynomial reg ------------------------------------------------------
model_poly_rwi <- lm(log_ntl ~ poly(CO2_mean_2020, 2, raw = TRUE) +
                   poly(log_no2, 2, raw = TRUE) +
                   poly(log_pm, 2, raw = TRUE) +
                   poly(FLOOD_2016_2020, 2, raw = TRUE) +
                   poly(QUAKE_2016_2020, 2, raw = TRUE) +
                   poly(Loss_sqkm_2016_2020, 2, raw = TRUE) +
                   poly(CDI_2020_mean, 2, raw = TRUE) +
                   poly(PRECIP_DIFF_2016_2020, 2, raw = TRUE) +
                   poly(TEMP_DIFF_2016_2020, 2, raw = TRUE) +
                   poly(road_raw, 2, raw = TRUE) +
                   poly(URBAN_2020, 2, raw = TRUE) +
                   poly(RWI_mean, 2, raw = TRUE),
                 data = train.data)


summary(model_poly_rwi)

# Make predictions
predictions_poly_rwi<- model_poly_rwi %>% predict(test.data)

# Model performance
poly_rwi <- data.frame(
  RMSE = RMSE(predictions_poly_rwi, test.data$log_ntl, na.rm = T),
  R2 = R2(predictions_poly_rwi, test.data$log_ntl, na.rm = T)
)

poly_rwi
# # Run a GAM Spline --------------------------------------------------------
# library(mgcv)
# model_gam_rwi <- gam(log_ntl ~ CO2_mean_2020 +
#   s(log_no2) +
#   s(log_pm) +
#   FLOOD_2016_2020 +
#   QUAKE_2016_2020 +
#   Loss_sqkm_2016_2020 +
#   CDI_2020_mean +
#   PRECIP_DIFF_2016_2020 +
#   s(TEMP_DIFF_2016_2020) +
#   s(road_raw) +
#   s(URBAN_2020) +
#   s(RWI_mean), data = train.data)
# 
# 
# # Print a summary of the model
# summary(model_gam_rwi)
# 
# # Make predictions
# predictions_gam_rwi<- model_gam_rwi %>% predict(test.data)
# 
# # Model performance
# gam_rwi <- data.frame(
#   RMSE = RMSE(predictions_gam_rwi, test.data$log_ntl, na.rm = T),
#   R2 = R2(predictions_gam_rwi, test.data$log_ntl, na.rm = T)
# )
# gam_rwi

############################################### WITHOUT RWI #################


# Subset data -------------------------------------------------------------
vul_sub_no_rwi <- vul_indicators %>%
  select(ID_ADM,CO2_mean_2020,NO2_mean_2020,road_raw,URBAN_2020,pm25_mean_2019,FLOOD_2016_2020,
         QUAKE_2016_2020,Loss_sqkm_2016_2020,CDI_2020_mean,PRECIP_DIFF_2016_2020,
         TEMP_DIFF_2016_2020,POP_mean_2020,NTL_mean_2020) %>% 
  mutate(log_ntl = log(NTL_mean_2020),
         log_pm  = log(pm25_mean_2019),
         log_no2 = log(NO2_mean_2020))


# Create a logical vector indicating which rows have all finite values
finite_rows_mask <- apply(vul_sub_no_rwi, 1, function(row) all(is.finite(row)))

# Use logical indexing to filter the rows with all finite values
vul_sub_finite_no_rwi <- vul_sub_no_rwi[finite_rows_mask, ]

# Partition data ----------------------------------------------------------
set.seed(123)

training.samples <- vul_sub_finite_no_rwi$log_ntl %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- vul_sub_finite_no_rwi[training.samples, ]
test.data <- vul_sub_finite_no_rwi[-training.samples, ]


# Run lm ------------------------------------------------------------------

# Build the model
model_lm_no_rwi <- lm(log_ntl ~ CO2_mean_2020 +log_no2+log_pm+
                     FLOOD_2016_2020+QUAKE_2016_2020+
                     Loss_sqkm_2016_2020+CDI_2020_mean+
                     PRECIP_DIFF_2016_2020+TEMP_DIFF_2016_2020+
                     road_raw+URBAN_2020
                   , data = train.data)
summary(model_lm_no_rwi)

# Make predictions
predictions_lm_no_rwi <- model_lm_no_rwi %>% predict(test.data)

# Model performance
lm_no_rwi <- data.frame(
  RMSE = RMSE(predictions_lm_no_rwi, test.data$log_ntl, na.rm = T),
  R2 = R2(predictions_lm_no_rwi, test.data$log_ntl, na.rm = T)
)

lm_no_rwi

# Run polynomial reg ------------------------------------------------------
model_poly_no_rwi <- lm(log_ntl ~ poly(CO2_mean_2020, 2, raw = TRUE) +
                       poly(log_no2, 2, raw = TRUE) +
                       poly(log_pm, 2, raw = TRUE) +
                       poly(FLOOD_2016_2020, 2, raw = TRUE) +
                       poly(QUAKE_2016_2020, 2, raw = TRUE) +
                       poly(Loss_sqkm_2016_2020, 2, raw = TRUE) +
                       poly(CDI_2020_mean, 2, raw = TRUE) +
                       poly(PRECIP_DIFF_2016_2020, 2, raw = TRUE) +
                       poly(TEMP_DIFF_2016_2020, 2, raw = TRUE) +
                       poly(road_raw, 2, raw = TRUE) +
                       poly(URBAN_2020, 2, raw = TRUE),
                     data = train.data)


summary(model_poly_no_rwi)

# Make predictions
predictions_poly_no_rwi<- model_poly_no_rwi %>% predict(test.data)

# Model performance
poly_no_rwi <- data.frame(
  RMSE = RMSE(predictions_poly_no_rwi, test.data$log_ntl, na.rm = T),
  R2 = R2(predictions_poly_no_rwi, test.data$log_ntl, na.rm = T)
)
poly_no_rwi

# Run a GAM Spline --------------------------------------------------------
# model_gam_no_rwi <- gam(log_ntl ~ CO2_mean_2020 +
#                        s(log_no2) +
#                        s(log_pm) +
#                        FLOOD_2016_2020 +
#                        QUAKE_2016_2020 +
#                        Loss_sqkm_2016_2020 +
#                        CDI_2020_mean +
#                        PRECIP_DIFF_2016_2020 +
#                        s(TEMP_DIFF_2016_2020) +
#                        s(road_raw) +
#                        s(URBAN_2020), data = train.data)
# 
# 
# # Print a summary of the model
# summary(model_gam_no_rwi)
# 
# # Make predictions
# predictions_gam_no_rwi<- model_gam_no_rwi %>% predict(test.data)
# 
# # Model performance
# gam_no_rwi <- data.frame(
#   RMSE = RMSE(predictions_gam_no_rwi, test.data$log_ntl, na.rm = T),
#   R2 = R2(predictions_gam_no_rwi, test.data$log_ntl, na.rm = T)
# )
# gam_no_rwi


# Combine into one table --------------------------------------------------

# Add a new column with the dataset names
lm_rwi$Dataset <- "lm_rwi"
#gam_rwi$Dataset <- "gam_rwi"
poly_rwi$Dataset <- "poly_rwi"
lm_no_rwi$Dataset <- "lm_no_rwi"
#gam_no_rwi$Dataset <- "gam_no_rwi"
poly_no_rwi$Dataset <- "poly_no_rwi"

# Assuming you have the 'combined_data' dataset as described

# Install and load the 'xtable' package (if not already installed)
install.packages("xtable")
library(xtable)

combined_data <- rbind(lm_rwi,poly_rwi,lm_no_rwi,poly_no_rwi)

# Add a new column indicating with or without RWI
combined_data$RWI_Status <- ifelse(grepl("no_rwi", combined_data$Dataset), "Without RWI", "With RWI")

# Create the LaTeX table using xtable
RMSE_table <- xtable(combined_data, caption = "Summary of Results",
                      label = "tab:results")


# Print the LaTeX table
print(RMSE_table)



# Regression output -------------------------------------------------------
# Assuming you have the regression models for lm_rwi, poly_rwi, gam_rwi, lm_no_rwi, poly_no_rwi, gam_no_rwi
# Install and load the required packages
install.packages("texreg")
library(texreg)


# Convert each model to a texreg object
texreg_lm_rwi <- extract(model_lm_rwi)
texreg_poly_rwi <- extract(model_poly_rwi)
#texreg_gam_rwi <- extract(model_gam_rwi)

texreg_lm_no_rwi <- extract(model_lm_no_rwi)
texreg_poly_no_rwi <- extract(model_poly_no_rwi)
#texreg_gam_no_rwi <- extract(model_gam_no_rwi)

# Combine all the texreg objects into a list
model_list <- list(texreg_lm_rwi, texreg_poly_rwi,
                   texreg_lm_no_rwi, texreg_poly_no_rwi)

# Create a vector of model names for the column labels
model_names <- c("Linear(with RWI)", "Poly(With RWI)", "Linear(w/o RWI)", "Poly(w/o RWI)")

# Format the list of models as LaTeX code
latex_code <- texreg(model_list, custom.model.names = model_names)

# Print the LaTeX code
cat(latex_code)



