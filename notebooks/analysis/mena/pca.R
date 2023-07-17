#Select the PCA components, merge to regression dataset and run polynomial regression



# Load Data ---------------------------------------------------------------
vul_indicators <- read.csv(file.path(mena_file_path,
                                     "csv_files_all_layers",
                                     "Vul_Indicators_ADM2_original.csv")) %>% as.data.frame()


# Subset data -------------------------------------------------------------
vul_sub <- vul_indicators %>%
  select(CO2_mean_2020,NO2_mean_2020,pm25_mean_2019,Loss_sqkm_2016_2020,PRECIP_DIFF_2016_2020,TEMP_DIFF_2016_2020,
         CDI_2020_mean,URBAN_DEN_2020,QUAKE_2016_2020,FLOOD_2016_2020,ID_ADM,NTL_mean_2020,RWI_mean,road_raw,
         POP_mean_2020) %>%
  drop_na() %>%
  mutate(across(-c(ID_ADM,POP_mean_2020), normalize)) %>%
  mutate(POP_mean_2020_log_norm = normalize(log(POP_mean_2020))) %>%
  mutate(VulPopSh_2016_2020 = normalize((CO2_mean_2020 + 
                                           NO2_mean_2020 +
                                           pm25_mean_2019 +
                                           Loss_sqkm_2016_2020 +
                                           PRECIP_DIFF_2016_2020 +
                                           TEMP_DIFF_2016_2020 +
                                           CDI_2020_mean +
                                           URBAN_DEN_2020 +
                                           QUAKE_2016_2020 + 
                                           FLOOD_2016_2020 - 
                                           NTL_mean_2020 -
                                           road_raw -
                                           RWI_mean)*POP_mean_2020_log_norm))



# Step 1: Define the column names for predictors
predictors_columns <- c(
  "CO2_mean_2020",
  "NO2_mean_2020",
  "pm25_mean_2019",
  "Loss_sqkm_2016_2020",
  "PRECIP_DIFF_2016_2020",
  "TEMP_DIFF_2016_2020",
  "CDI_2020_mean",
  "URBAN_DEN_2020",
  "QUAKE_2016_2020",
  "FLOOD_2016_2020",
  "NTL_mean_2020",
  "road_raw",
  "RWI_mean"
)

X <- vul_sub[,predictors_columns]  # Select the columns representing predictors
Y <- vul_sub[, "VulPopSh_2016_2020"]  # Select the column representing the response variable


# Step 2: Create a Correlation Plot for the Subset of Variables

hazards <- c(
  "CO2_mean_2020",
  "NO2_mean_2020",
  "pm25_mean_2019",
  "Loss_sqkm_2016_2020",
  "PRECIP_DIFF_2016_2020",
  "TEMP_DIFF_2016_2020",
  "CDI_2020_mean",
  "URBAN_DEN_2020",
  "QUAKE_2016_2020",
  "FLOOD_2016_2020")

subsetX <- X[,hazards]
correlationMatrix <- cor(subsetX) #correlation
ggcorrplot(correlationMatrix,hc.order = TRUE, type = "lower") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) #Correlation Plot


latex_table <- xtable(correlationMatrix, caption = "Correlation Table") # Convert the correlation matrix to a LaTeX table
print(latex_table, include.rownames = TRUE) # Print the LaTeX code for the table


# Step 3: Perform PCA on the Subset of Variables
pca <- prcomp(subsetX, scale = TRUE)


# Step 4: Create a Scree Plot with Variance Labels
var_exp <- (pca$sdev^2) / sum(pca$sdev^2) # Compute the variance explained by each principal component



barplot(var_exp, main = "Variance Explained by Principal Components",
        xlab = "Principal Components", ylab = "Percentage of Variance Explained",
        col = "lightblue", border = "black",names.arg = paste("PC", 1:10)) # Scree plot with bars representing variation


screeplot(pca, type = "l", main = "Screeplot for Exposure Indicators") # screeplot
abline(1,0, col = "red", lty = 2) # cutoff line of 1 to observe the "elbow"

summary(pca) # see the cumulative variation


fviz_pca_var(pca,
             col.var = "contrib", # Control variable color using their contributions to the PC
             gradient.cols = c("#70f6ff", "#00AFBB", "#ffd224",
                               "#d8ac00", "#FC4E07", "#a73203"),
             repel = TRUE,     # Avoid text overlapping
             ggtheme = theme_minimal()
)


selected_components <- c(3, 4) # Specify the PCA components to choose


fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols = c("#70f6ff", "#00AFBB", "#ffd224", "#d8ac00", "#FC4E07", "#a73203"),
             repel = TRUE,
             ggtheme = theme_minimal(),
             axes = selected_components
) # Call fviz_pca_var() with the specified components and gradient colors



# Step 5: Combine PCA components with remaining variables
combinedX <- cbind(pca$x[,1:5], X[, !colnames(X) %in% hazards])



# Step 6: Run regressions

# Linear model
lm <- lm(Y ~ ., data = combinedX)
predictions <- predict(lm, combinedX)
residuals <- Y - predictions
rmse_lm <- sqrt(mean(residuals^2)) # RMSE


# Polynomial Regression model
polymodel <- lm(
  formula = Y ~ PC1 + PC2 + PC3 + PC4 + PC5 + 
    NTL_mean_2020 + road_raw + RWI_mean + 
    I(PC1^2) + I(PC2^2) + I(PC3^2) + I(PC4^2) + I(PC5^2) + 
    I(NTL_mean_2020^2) + I(road_raw^2) + I(RWI_mean^2) +
    I((PC1^2)*NTL_mean_2020) + I((PC2^2)*NTL_mean_2020) + I((PC3^2)*NTL_mean_2020) +
    I((PC4^2)*NTL_mean_2020) + I((PC5^2)*NTL_mean_2020) +
    I((PC1^2)*road_raw) + I((PC2^2)*road_raw) + I((PC3^2)*road_raw) + I((PC4^2)*road_raw) +
    I((PC5^2)*road_raw), data = combinedX
)
predictions_poly <- predict(polymodel, combinedX)
residuals_poly <- Y - predictions_poly 
rmse_poly <- sqrt(mean(residuals_poly^2)) # RMSE


results <- data.frame(
  Model = c("Linear Model", "Polynomial Regression"),
  RMSE = c(rmse_lm, rmse_poly)
)  # Create a data frame with model names and RMSE values



stargazer(lm,
          polymodel,
          dep.var.labels = c("Vulnerability Index"),
          align = TRUE, 
          omit.stat = c("LL", "ser"),
          column.labels = c("Linear", "Polynomial Regression"),
          add.lines = list(c("RMSE", round(results$RMSE, 2))),
          model.numbers = FALSE,
          type = "latex") # Display the results using stargazer

