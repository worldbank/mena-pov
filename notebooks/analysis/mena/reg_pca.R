####################################################################################################

# combinedX_PC2 <- combinedX %>%
#   select(exposure_PC2,NTL_mean_2020,road_raw,RWI_mean)
# 
# combinedX_PC3 <- combinedX %>%
#   select(exposure_PC3,NTL_mean_2020,road_raw,RWI_mean)
# 
# combinedX_PC4 <- combinedX %>%
#   select(exposure_PC4,NTL_mean_2020,road_raw,RWI_mean)
# 
# combinedX_PC5 <- combinedX %>%
#   select(exposure_PC5,NTL_mean_2020,road_raw,RWI_mean)


# Step 6: Run regressions

# Linear model
lm_PC1 <- lm(Y_PC1 ~ ., data = combinedX_PC1)
predictions_PC1 <- predict(lm_PC1, combinedX_PC1)
residuals_PC1 <- Y_PC1 - predictions_PC1
rmse_lm_PC1 <- sqrt(mean(residuals_PC1^2)) # RMSE
summary(lm_PC1)


lm_PC2 <- lm(Y_PC2 ~ ., data = combinedX_PC2)
predictions_PC2 <- predict(lm_PC2, combinedX_PC2)
residuals_PC2 <- Y_PC2 - predictions_PC2
rmse_lm_PC2 <- sqrt(mean(residuals_PC2^2)) # RMSE
summary(lm_PC2)


lm_PC3 <- lm(Y_PC3 ~ ., data = combinedX_PC3)
predictions_PC3 <- predict(lm_PC3, combinedX_PC3)
residuals_PC3 <- Y_PC3 - predictions_PC3
rmse_lm_PC3 <- sqrt(mean(residuals_PC3^2)) # RMSE
summary(lm_PC3)


lm_PC4 <- lm(Y_PC4 ~ ., data = combinedX_PC4)
predictions_PC4 <- predict(lm_PC4, combinedX_PC4)
residuals_PC4 <- Y_PC4 - predictions_PC4
rmse_lm_PC4 <- sqrt(mean(residuals_PC4^2)) # RMSE
summary(lm_PC4)

lm_PC5 <- lm(Y_PC5 ~ ., data = combinedX_PC5)
predictions_PC5 <- predict(lm_PC5, combinedX_PC5)
residuals_PC5 <- Y_PC5 - predictions_PC5
rmse_lm_PC5 <- sqrt(mean(residuals_PC5^2)) # RMSE
summary(lm_PC5)


# Polynomial Regression model
polymodel_PC1 <- lm(
  formula = Y_PC1 ~ exposure_PC1 + 
    NTL_mean_2020 + road_raw + RWI_mean + 
    I(exposure_PC1^2) +  
    I(NTL_mean_2020^2) + I(road_raw^2) + I(RWI_mean^2) +
    I((exposure_PC1^2)*NTL_mean_2020) +
    I((exposure_PC1^2)*road_raw) +
    I((exposure_PC1^2)*RWI_mean), data = combinedX_PC1
)
predictions_poly_PC1 <- predict(polymodel_PC1, combinedX_PC1)
residuals_poly_PC1 <- Y_PC1 - predictions_poly_PC1
rmse_poly_PC1 <- sqrt(mean(residuals_poly_PC1^2)) # RMSE
summary(polymodel_PC1)

polymodel_PC2 <- lm(
  formula = Y_PC2 ~ exposure_PC2 + 
    NTL_mean_2020 + road_raw + RWI_mean + 
    I(exposure_PC2^2) +  
    I(NTL_mean_2020^2) + I(road_raw^2) + I(RWI_mean^2) +
    I((exposure_PC2^2)*NTL_mean_2020) +
    I((exposure_PC2^2)*road_raw) +
    I((exposure_PC2^2)*RWI_mean), data = combinedX_PC2
)
predictions_poly_PC2 <- predict(polymodel_PC2, combinedX_PC2)
residuals_poly_PC2 <- Y_PC2 - predictions_poly_PC2
rmse_poly_PC2 <- sqrt(mean(residuals_poly_PC2^2)) # RMSE
summary(polymodel_PC2)

polymodel_PC3 <- lm(
  formula = Y_PC3 ~ exposure_PC3 + 
    NTL_mean_2020 + road_raw + RWI_mean + 
    I(exposure_PC3^2) +  
    I(NTL_mean_2020^2) + I(road_raw^2) + I(RWI_mean^2) +
    I((exposure_PC3^2)*NTL_mean_2020) +
    I((exposure_PC3^2)*road_raw) +
    I((exposure_PC3^2)*RWI_mean), data = combinedX_PC3
)
predictions_poly_PC3 <- predict(polymodel_PC3, combinedX_PC3)
residuals_poly_PC3 <- Y_PC3 - predictions_poly_PC3
rmse_poly_PC3 <- sqrt(mean(residuals_poly_PC3^2)) # RMSE
summary(polymodel_PC3)


polymodel_PC4 <- lm(
  formula = Y_PC4 ~ exposure_PC4 + 
    NTL_mean_2020 + road_raw + RWI_mean + 
    I(exposure_PC4^2) +  
    I(NTL_mean_2020^2) + I(road_raw^2) + I(RWI_mean^2) +
    I((exposure_PC4^2)*NTL_mean_2020) +
    I((exposure_PC4^2)*road_raw) +
    I((exposure_PC4^2)*RWI_mean), data = combinedX_PC4
)
predictions_poly_PC4 <- predict(polymodel_PC4, combinedX_PC4)
residuals_poly_PC4 <- Y_PC4 - predictions_poly_PC4
rmse_poly_PC4 <- sqrt(mean(residuals_poly_PC4^2)) # RMSE
summary(polymodel_PC4)

polymodel_PC5 <- lm(
  formula = Y_PC5 ~ exposure_PC5 + 
    NTL_mean_2020 + road_raw + RWI_mean + 
    I(exposure_PC5^2) +  
    I(NTL_mean_2020^2) + I(road_raw^2) + I(RWI_mean^2) +
    I((exposure_PC5^2)*NTL_mean_2020) +
    I((exposure_PC5^2)*road_raw) +
    I((exposure_PC5^2)*RWI_mean), data = combinedX_PC5
)
predictions_poly_PC5 <- predict(polymodel_PC5, combinedX_PC5)
residuals_poly_PC5 <- Y_PC5 - predictions_poly_PC5
rmse_poly_PC5 <- sqrt(mean(residuals_poly_PC5^2)) # RMSE
summary(polymodel_PC5)





results <- data.frame(
  Model = c("lm_PC1", "lm_PC2", "Polymodel_PC1", "Polymodel_PC2"),
  RMSE = c(rmse_lm_PC1, rmse_lm_PC2, rmse_poly_PC1,
           rmse_poly_PC2)
)  # Create a data frame with model names and RMSE values



stargazer(lm_PC1,
          lm_PC2,
          polymodel_PC1,
          polymodel_PC2,
          dep.var.labels = c("Vulnerability Index"),
          align = TRUE, 
          omit.stat = c("LL", "ser"),
          column.labels = c("lm_PC1","lm_PC2",
                            "polymodel_PC1","polymodel_PC2"),
          add.lines = list(c("RMSE", round(results$RMSE, 2))),
          model.numbers = FALSE,
          type = "latex") # Display the results using stargazer

