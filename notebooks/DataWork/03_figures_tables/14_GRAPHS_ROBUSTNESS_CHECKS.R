# ================================================================
# Script Name: 12_RWI_ROBUSTNESS_CHECK
# Purpose: Create graph in the appendix which estimates the number of 
# poor people using RWI at the ADM+1 level
# Input Dataset: ,  MENA_ADM2.shp, MENA_Country.shp
# Author: Chitra Balasubramanian
# Last Updated: 2024-08-05
# ================================================================


# Load Data ---------------------------------------------------------------
poor_vul_exposed <- readRDS(file.path(final_replication,"grid_10km_poor_vul_exposed.Rds"))
gsap_rwi_grid <- readRDS(file.path(final_replication,"grid_rwi_gsap_final.Rds")) # RWI grid at 1km


# Estimate RWI at ADM+1 (one level higher than GSAP) ----------------------------------------------------
gsap_rwi_grid$GSAP_admin <- str_split_i(gsap_rwi_grid$g_cd2_n, "_",3)
gsap_rwi_grid$GSAP_admin <- ifelse(gsap_rwi_grid$GSAP_admin == "ADM0" | gsap_rwi_grid$GSAP_admin == "ADM1", gsap_rwi_grid$GSAP_admin, "ADM2")


# Select variables ----------------------------------------------------
gsap_rwi_grid_sub <- gsap_rwi_grid %>%
  dplyr::select(grid_id,g_cd2_n,ISO_A2,ADM0_CODE, ADM1_CODE, ADM2_CODE, WB_ADM1_NA, WB_ADM2_NA, WB_ADM0_NA, GSAP_admin, pop_count,poor215_ln,
                poor365_ln,poor685_ln,rwi_pos2_dup, rwi_pop_dup, pop_count, rwi_pop_gsap_dup, population_gsap_rwi_dup,population_gsap_dup,rwi_weight_pop_gsap_dup,rwi_weight_dup,
                poor215_rwi_dup,poor365_rwi_dup,poor685_rwi_dup,poor215_count_gsap_dup,poor365_count_gsap_dup,poor685_count_gsap_dup,
                poor215_count_rwi_dup,poor365_count_rwi_dup,poor685_count_rwi_dup, pop_count2_dup, rwi_all)





############ POVERTY RATE ($2.15) ###################

# Create a variable with level for GSAP
gsap_rwi_grid_sub$GSAP_ADM_CO <- ifelse(gsap_rwi_grid_sub$GSAP_admin == "ADM0", gsap_rwi_grid_sub$ADM0_CODE, ifelse(gsap_rwi_grid_sub$GSAP_admin == "ADM1", gsap_rwi_grid_sub$ADM1_CODE,gsap_rwi_grid_sub$ADM2_CODE))
# Create a variable with level N+1 for GSAP
gsap_rwi_grid_sub$GSAP_ADM_CO2 <- ifelse(gsap_rwi_grid_sub$GSAP_admin == "ADM0", gsap_rwi_grid_sub$ADM0_CODE, ifelse(gsap_rwi_grid_sub$GSAP_admin == "ADM1", gsap_rwi_grid_sub$ADM0_CODE,gsap_rwi_grid_sub$ADM1_CODE))


# 3.2: Population, poverty rate, and RWI*pop in each GSAP polygon
# Average / sum inside polygons of ADM+1
gsap_rwi_grid_sub<-gsap_rwi_grid_sub %>%
  group_by(GSAP_ADM_CO2) %>%
  mutate(
    poor215_gsap_sim = mean(poor215_ln),
    poor365_gsap_sim = mean(poor365_ln),
    poor685_gsap_sim = mean(poor685_ln),
    rwi_pop_gsap_sim = sum(rwi_pop_dup, na.rm=TRUE),
    population_gsap_rwi_sim = sum(pop_count2_dup, na.rm=TRUE),
    population_gsap_sim = sum(pop_count)) %>%
  ungroup()


# 3.3 Estimated pov rate

# Weighted average of rwi_pos by population
gsap_rwi_grid_sub$rwi_weight_pop_gsap_sim<-gsap_rwi_grid_sub$rwi_pop_gsap_sim/gsap_rwi_grid_sub$population_gsap_rwi_sim

# RWI weights
gsap_rwi_grid_sub$rwi_weight_sim<-gsap_rwi_grid_sub$rwi_pos2_dup/gsap_rwi_grid_sub$rwi_weight_pop_gsap_sim


# New poverty rates
gsap_rwi_grid_sub$poor215_rwi_sim<-gsap_rwi_grid_sub$rwi_weight_sim*gsap_rwi_grid_sub$poor215_gsap_sim
gsap_rwi_grid_sub$poor215_rwi_sim[is.na(gsap_rwi_grid_sub$rwi_weight_sim)]<-gsap_rwi_grid_sub$poor215_gsap_sim[is.na(gsap_rwi_grid_sub$rwi_weight_sim)]

gsap_rwi_grid_sub$poor365_rwi_sim<-gsap_rwi_grid_sub$rwi_weight_sim*gsap_rwi_grid_sub$poor365_gsap_sim
gsap_rwi_grid_sub$poor365_rwi_sim[is.na(gsap_rwi_grid_sub$rwi_weight_sim)]<-gsap_rwi_grid_sub$poor365_gsap_sim[is.na(gsap_rwi_grid_sub$rwi_weight_sim)]

gsap_rwi_grid_sub$poor685_rwi_sim<-gsap_rwi_grid_sub$rwi_weight_sim*gsap_rwi_grid_sub$poor685_gsap_sim
gsap_rwi_grid_sub$poor685_rwi_sim[is.na(gsap_rwi_grid_sub$rwi_weight_sim)]<-gsap_rwi_grid_sub$poor685_gsap_sim[is.na(gsap_rwi_grid_sub$rwi_weight_sim)]


# 3.4 Estimated number of poor
gsap_rwi_grid_sub$poor215_count_gsap_sim<-gsap_rwi_grid_sub$pop_count*gsap_rwi_grid_sub$poor215_gsap_sim
gsap_rwi_grid_sub$poor215_count_rwi_sim<-gsap_rwi_grid_sub$pop_count*gsap_rwi_grid_sub$poor215_rwi_sim

gsap_rwi_grid_sub$poor365_count_gsap_sim<-gsap_rwi_grid_sub$pop_count*gsap_rwi_grid_sub$poor365_gsap_sim
gsap_rwi_grid_sub$poor365_count_rwi_sim<-gsap_rwi_grid_sub$pop_count*gsap_rwi_grid_sub$poor365_rwi_sim

gsap_rwi_grid_sub$poor685_count_gsap_sim<-gsap_rwi_grid_sub$pop_count*gsap_rwi_grid_sub$poor685_gsap_sim
gsap_rwi_grid_sub$poor685_count_rwi_sim<-gsap_rwi_grid_sub$pop_count*gsap_rwi_grid_sub$poor685_rwi_sim


############################################################
# Aggregate the simulated RWI to GSAP ADMIN level to compare.

# Variable with level for GSAP was created earlier: gsap_rwi_grid_sub$GSAP_ADM_CO 

# Average / sum inside polygons of ADM
# 3.2: Simulated number of poor and population in each GSAP polygon
gsap_rwi_grid_sub_sim<-gsap_rwi_grid_sub %>%
  group_by(GSAP_ADM_CO) %>%
  mutate(
    poor215_count_rwi_sim_gsap_adm = sum(poor215_count_rwi_sim, na.rm=TRUE),
    poor365_count_rwi_sim_gsap_adm = sum(poor365_count_rwi_sim, na.rm=TRUE),
    poor685_count_rwi_sim_gsap_adm = sum(poor685_count_rwi_sim, na.rm=TRUE),
    population_gsap_rwi_sim_gsap_adm = sum(pop_count2_dup, na.rm=TRUE),
    population_gsap_sim_gsap_adm = sum(pop_count)) %>%
  ungroup()

gsap_rwi_grid_sub_sim$poor215_rate_rwi_sim_gsap_adm<-gsap_rwi_grid_sub_sim$poor215_count_rwi_sim_gsap_adm/gsap_rwi_grid_sub_sim$population_gsap_rwi_sim_gsap_adm
gsap_rwi_grid_sub_sim$poor365_rate_rwi_sim_gsap_adm<-gsap_rwi_grid_sub_sim$poor365_count_rwi_sim_gsap_adm/gsap_rwi_grid_sub_sim$population_gsap_rwi_sim_gsap_adm
gsap_rwi_grid_sub_sim$poor685_rate_rwi_sim_gsap_adm<-gsap_rwi_grid_sub_sim$poor685_count_rwi_sim_gsap_adm/gsap_rwi_grid_sub_sim$population_gsap_rwi_sim_gsap_adm


############################################################

# Step 1: Calculate poverty metrics using GSAP
gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  group_by(grid_id) %>%
  mutate(pov_count_gsap = pop_count * poor215_rwi_dup, .groups = 'drop') # Poverty count using GSAP

gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  group_by(g_cd2_n) %>%
  mutate(total_pov_gsap = sum(pov_count_gsap, na.rm = TRUE), # Sum of poverty count per country
         total_pop = sum(pop_count, na.rm = TRUE), # Total population per country
         rwi_pop_mean_gsap = total_pov_gsap / total_pop) %>% # Poverty rate using GSAP
  ungroup()

# Step 2: Calculate poverty metrics using RWI
gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  group_by(grid_id) %>%
  mutate(pov_count_rwi = pop_count * poor215_rwi_dup, .groups = 'drop') # Poverty count using RWI

gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  group_by(ISO_A2) %>%
  mutate(total_pov_rwi = sum(pov_count_rwi, na.rm = TRUE), # Sum of poverty count per country using RWI
         avg_pov_rwi = total_pov_rwi / total_pop) %>% # Average poverty using RWI
  ungroup()

# Step 3: Compute the RWI weight
gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  mutate(rwi_weight = ifelse(!is.na(avg_pov_rwi), poor215_rwi_dup / avg_pov_rwi, avg_pov_rwi))

# Step 4: Compute the Poverty rate adjusted by RWI
gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  mutate(adj_pov_rate_rwi = ifelse(!is.na(rwi_weight_dup),  rwi_pop_mean_gsap* rwi_weight_dup, rwi_pop_mean_gsap))

#Step 5: Calculate the simulated GSAP

gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  dplyr::mutate(pov_count_gsapsim = adj_pov_rate_rwi*pop_count)

gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>%
  group_by(g_cd2_n) %>%
  mutate(total_pov_gsapsim = sum(pov_count_gsapsim, na.rm = TRUE), # Sum of poverty count per country
         total_pop = sum(pop_count, na.rm = TRUE), # Total population per country
         pov_rate_gsapsim = total_pov_gsapsim / total_pop) %>% # Poverty rate using GSAP
  ungroup()

sum(is.na(gsap_rwi_grid_sub$poor215_gsap_dup))


names(gsap_rwi_grid_sub)

# Scatterplot (with correlation) ------------------------------------------
#Calculate correlation
#There are NAs in poor215_gsap

gsap_rwi_grid_sub <- gsap_rwi_grid_sub %>% dplyr::filter(!is.na(poor215_ln))
correlation <- cor(gsap_rwi_grid_sub$poor215_ln, gsap_rwi_grid_sub$pov_rate_gsapsim)

# # Create scatterplot
# ggplot(gsap_rwi_grid_sub, aes(x = poor215_ln, y = pov_rate_gsapsim, color = ISO_A2)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines for each country
#   facet_wrap(~ ISO_A2) +  # Create separate plots for each country
#   ggtitle("Scatterplot of pov_rate_gsap vs. pov_rate_gsapsim by Country") +
#   xlab("Poverty Rate GSAP") +
#   ylab("Poverty Rate GSAP Simulation") +
#   theme_minimal()  

# # Create scatterplot (by country in one graph)
# ggplot(gsap_rwi_grid_sub, aes(x = poor215_ln, y = pov_rate_gsapsim, color = ISO_A2)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines for each country
#   ggtitle("Scatterplot of pov_rate_gsap vs. pov_rate_gsapsim by Country") +
#   xlab("Poverty Rate GSAP") +
#   ylab("Poverty Rate GSAP Simulation") +
#   theme_minimal() 
# 
# 
# ggplot(gsap_rwi_grid_sub, aes(x = poor215_ln, y = pov_rate_gsapsim)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines for each country
#   ggtitle("Scatterplot of pov_rate_gsap vs. pov_rate_gsapsim") +
#   xlab("Poverty Rate GSAP") +
#   ylab("Poverty Rate GSAP Simulation") +
#   theme_minimal() 
# 
