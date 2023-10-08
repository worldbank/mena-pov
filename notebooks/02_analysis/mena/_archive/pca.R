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
  mutate(POP_mean_2020_log_norm = normalize(log(POP_mean_2020)))




# Step 1: Define the column names for predictors
predictors_columns <- c(
  "CO2_mean_2020",
  "NO2_mean_2020",
  "pm25_mean_2019",
  "Loss_sqkm_2016_2020",
  "PRECIP_DIFF_2016_2020",
  "TEMP_DIFF_2016_2020",
  "CDI_2020_mean",
  "QUAKE_2016_2020",
  "FLOOD_2016_2020",
  "NTL_mean_2020",
  "road_raw",
  "RWI_mean"
)

X <- vul_sub[,predictors_columns]  # Select the columns representing predictors


# Step 2: Create a Correlation Plot for the Subset of Variables

hazards <- c(
  "CO2_mean_2020",
  "NO2_mean_2020",
  "pm25_mean_2019",
  "Loss_sqkm_2016_2020",
  "PRECIP_DIFF_2016_2020",
  "TEMP_DIFF_2016_2020",
  "CDI_2020_mean",
  "QUAKE_2016_2020",
  "FLOOD_2016_2020")

subsetX <- X[,hazards]
correlationMatrix <- cor(subsetX) #correlation
cor_plot <- ggcorrplot(correlationMatrix,hc.order = TRUE, type = "lower") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) #Correlation Plot

ggsave(cor_plot, filename = "C:/Users/wb569257/OneDrive - WBG/WoE/Vulnerability/output/figures/corr_plot_exp.png", height = 8, width = 6)


latex_table <- xtable(correlationMatrix, caption = "Correlation Table") # Convert the correlation matrix to a LaTeX table
print(latex_table, include.rownames = TRUE) # Print the LaTeX code for the table


# Step 3: Perform PCA on the Subset of Variables
pca <- prcomp(subsetX, scale. = TRUE)


# Step 4: PCA Plots 
var_exp <- (pca$sdev^2) / sum(pca$sdev^2) # Compute the variance explained by each principal component


# barplot
barplot(var_exp, main = "Variance Explained by Principal Components",
        xlab = "Principal Components", ylab = "Percentage of Variance Explained",
        col = "lightblue", border = "black",names.arg = paste("PC", 1:9)) # Scree plot with bars representing variation



#scree plot
screeplot(pca, type = "l", main = "Screeplot for Exposure Indicators") # screeplot
abline(1,0, col = "red", lty = 2) # cutoff line of 1 to observe the "elbow"

# Contribution plot without labels
contrib_plot <- fviz_pca_var(pca,
                             col.var = "contrib",
                             gradient.cols = c("#70f6ff", "#00AFBB", "#ffd224",
                                               "#d8ac00", "#FC4E07", "#a73203"),
                             repel = TRUE,
                             ggtheme = theme_minimal()
)

contrib_plot


# weights table

# Extract the weights from PC1 (the first principal component)
weights <- pca$rotation[, 1]

# Create a table of weights
weight_table <- data.frame(Variable = colnames(subsetX), Weight = weights)

# Print the weight table
print(weight_table)


# Step 5: Combine PCA components with remaining variables
PC1 <- pca$x[,1]
pop_log_norm_mean_2020 <- vul_sub$POP_mean_2020_log_norm
ID_ADM <- vul_sub$ID_ADM
combinedX <- cbind(ID_ADM, PC1, X, pop_log_norm_mean_2020)

# create exposure variables using PCs as weights

# Define the PC variables and hazard variables
PC_vars <- c("PC1")

# Create new variables using PC vars as weights for hazards
for (pc_var in PC_vars) {
  for (hazard_var in hazards) {
    new_var_name <- paste(hazard_var, pc_var, sep = "_")
    new_var <- combinedX[[hazard_var]] * combinedX[[pc_var]]
    normalized_var <- normalize(new_var)
    combinedX[[new_var_name]] <- normalized_var
  }
}


combinedX <- combinedX %>%
  mutate(exposure_PC1 = CO2_mean_2020_PC1 + NO2_mean_2020_PC1 + pm25_mean_2019_PC1 +
         Loss_sqkm_2016_2020_PC1 + PRECIP_DIFF_2016_2020_PC1 +TEMP_DIFF_2016_2020_PC1 +
           CDI_2020_mean_PC1 + QUAKE_2016_2020_PC1 + FLOOD_2016_2020_PC1,
         exposure_PC1 = normalize(exposure_PC1))

# Create the Vulnerability Index
combinedX <- combinedX %>%
  mutate(VulPopSh_2016_2020_PC1 = (exposure_PC1 - RWI_mean - NTL_mean_2020 - road_raw)*vul_sub$POP_mean_2020_log_norm,
         VulPopSh_2016_2020_PC1 = normalize(VulPopSh_2016_2020_PC1))

combinedX_PC1 <- combinedX %>%
  select(exposure_PC1,VulPopSh_2016_2020_PC1,NTL_mean_2020,road_raw,RWI_mean,
  pop_log_norm_mean_2020, ID_ADM)


# Plot
plot_compare_pca_w_rwi <- ggplot(combinedX_PC1, aes(VulPopSh_2016_2020_PC1, log(NTL_mean_2020))) +
  geom_point(color = "darkgrey", size = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), aes(color = "Poly (Order 2)"), show.legend = T) +
  geom_smooth(method = "gam", formula = y ~ s(x), aes(color = "GAM(Spline)"),show.legend = T) +
  labs (color = "",
        x = "PCA Vulnerability Index (with RWI)",
        y = "% Change in Mean NTL") +
  scale_color_manual(values = c("Poly (Order 2)" = "#008080", "GAM(Spline)" = "black")) +
  theme_classic() 

plot_compare_pca_w_rwi

# Create natural breaks(Jenks)

# Specify the number of desired breaks
num_breaks <- 5

# Apply Jenks natural breaks classification
jenks_classes <- classIntervals(combinedX_PC1$VulPopSh_2016_2020_PC1, n = num_breaks, style = "jenks")

# Extract the breakpoints
breakpoints <- jenks_classes$brks

# View the resulting breakpoints
print(breakpoints)

# Create the categorical variable
combinedX_PC1$VI_quintiles <- cut(combinedX_PC1$VulPopSh_2016_2020_PC1, breaks = breakpoints, 
                                  labels = c("Lowest", "Quintile 2", "Quintile 3", "Quintile 4", "Highest"))

# View the updated data frame with the new Quintile variable
combinedX_PC1[,c("VI_quintiles", "VulPopSh_2016_2020_PC1")]



# Export ------------------------------------------------------------------
write.csv(combinedX_PC1, file = "C:/Users/wb569257/OneDrive - WBG/WoE/Vulnerability/data/Vul_PCA.csv",row.names = F)


