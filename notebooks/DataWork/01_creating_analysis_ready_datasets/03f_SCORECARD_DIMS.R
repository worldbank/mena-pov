# =============================================================================
# Script Name: 03f_SCORECARD_DIMS.R
# Purpose: Calculate the indicators for social protection, and financial access
# Input Dataset: grid_10km.shp, ASPIRE.dta, Findex_quintiles.dta
# Output Dataset: tt_health_educ_markets.Rds
# Author: Sandra Baquie
# Last Updated: 2024-08-05
# =============================================================================

############################################################
# 1. Combine all household surveys ---------------------------------------------------------------
############################################################


# List all files ending with "_v2.dta"
file_list <- list.files(path = data_hh_survey, pattern = "_v2.dta$", full.names = TRUE)


file_list

# Check if there are any matching files
if (length(file_list) == 0) {
  stop("No files found matching the pattern '_v2.dta'")
}

# Initialize an empty data frame to store appended data
hh_surveys <- data.frame()

# Loop through each file and append it to the appended_data data frame
for (file in file_list) {
  # Read the data from the current file
  data <- read_dta(file)
  # Loop over variables to check if they exist, if not create empty ones
  for (col_name in c("countrycode", "year" ,"hhid", "welfare", "pid", "subnatid", "subnatid2", "subnatid3", "welfarenom", "welfaredef", "educy", "electricity", "weight_h", "weight_p", "age", "hsize", "cpi2017", "icp2017", "urban", "imp_wat_rec", "educat4")) {
    # Check if the column exists in the dataset
    if (!(col_name %in% names(data))) {
      # If the column does not exist, create it and fill it with NA values
      data[[col_name]] <- NA
    }
  }
  data<-data[,c("countrycode", "year" ,"hhid", "welfare", "pid", "subnatid", "subnatid2", "subnatid3", "welfarenom", "welfaredef", "educy", "electricity", "weight_h", "weight_p", "age", "hsize", "cpi2017", "icp2017", "urban", "imp_wat_rec", "educat4")]
  # Append the data to the appended_data data frame
  hh_surveys <- rbind(hh_surveys, data)
  print(file)
}

#Add unique id
hh_surveys <- hh_surveys %>%
  mutate(unique_id = row_number())

# Optionally, you can save the appended data to a new file
write_dta(hh_surveys, file.path(data_hh_survey, "hh_surveys_all.dta"))


############################################################
# 2.  ASPIRE and FINDEX ---------------------------------------------------------------
############################################################

# List of countries
country_codes <- str_extract(file_list, "[A-Za-z]{3}(?=_[0-9])")  # Start and end positions of the country code


# ASPIRE: Filter aspire data for countries we have
aspire <- read_dta( file.path(data_hh_survey, "ASPIRE.dta"))
aspire_mena <- subset(aspire, Country_Code %in% country_codes)
unique(aspire_mena$Country_Code)

# 10 countries in MENA (missing MLT)
aspire_mena <- aspire_mena %>%
  filter(grepl("per_allsp\\.cov_q[1-5]_(rur|tot|urb)", Indicator_Code))


# FINDEX: Filter aspire data for countries we have
findex <- read_dta(file.path(data_hh_survey,"Findex_quintiles.dta"))
findex_mena <- subset(findex, code %in% country_codes)
unique(findex_mena$code)
# 11 MENA countries

# Keep the latest year for each country 
# It seems that Minh may have used 2021; we don't have the pre-cleaning
# and aggregation to bottom 40 and top 60
findex_mena <- findex_mena %>%
  group_by(code) %>%
  filter(year == max(year))
# All 2021 but DJI and Yemen


############################################################
# 3. Deprivation dimensions ---------------------------------------------------------------
############################################################

# Energy
hh_surveys <- hh_surveys %>%
  mutate(dep_infra_elec = ifelse(!is.na(electricity) & electricity == 0, 1, ifelse(is.na(electricity), NA, 0)))

# Water 
hh_surveys <- hh_surveys %>%
  mutate(dep_infra_water = ifelse(!is.na(imp_wat_rec) & imp_wat_rec == 0, 1, ifelse(is.na(imp_wat_rec), NA, 0)))

# Education (above 15 and less than primary school)
hh_surveys <- hh_surveys %>%
  mutate(dep_educ = ifelse(!is.na(educat4) &!is.na(age) & age>=15 & educat4<2, 1, ifelse(is.na(age) | is.na(educat4) | age<15, NA, 0)))

# Poverty
sum(hh_surveys$welfare < 0, na.rm = TRUE)
hh_surveys <- hh_surveys %>%
  mutate(welfare = ifelse(welfare < 0, 0, welfare))
hh_surveys$welfare_ppp <- hh_surveys$welfare / hh_surveys$cpi2017 / hh_surveys$icp2017 / 365
# Replace negative values of welfare by 0

# Poverty rates
hh_surveys <-hh_surveys %>%
  mutate(poor_215 = ifelse(!is.na(welfare_ppp) & welfare_ppp<2.15, 1, ifelse(is.na(welfare_ppp), NA, 0))) %>%
  mutate(poor_365 = ifelse(!is.na(welfare_ppp) & welfare_ppp<3.65, 1, ifelse(is.na(welfare_ppp), NA, 0)))

# Understand the weights
# Drop the observation with missing weight
hh_surveys <- hh_surveys[complete.cases(hh_surveys$weight_p), ]

# Calculate total weight_p for each Country_code
total_weight_by_country <- aggregate(weight_p ~ countrycode, data = hh_surveys, FUN = sum)
total_weight_by_country <- total_weight_by_country %>%
  rename (total_weight_by_country=weight_p)

# Total poverty weights is really off compared to population totals in some countries. 
hh_surveys <- hh_surveys %>%
  left_join(total_weight_by_country, by = c("countrycode" = "countrycode"))
# We have to rescale them such that the total will match in what we have with wordpop.


# Get totals from Wordpop
pop <- readRDS(file.path(mena_file_path,"Population","final","grid_pop_10km_v2.Rds"))
total_population <- pop %>%
  group_by(ISO_A2) %>%
  summarise(total_population = sum(pop_count, na.rm = TRUE))

# Merge
total_population$ISO_A3<- countrycode(total_population$ISO_A2, "iso2c", "iso3c")
hh_surveys <- hh_surveys %>%
  left_join(total_population, by = c("countrycode" = "ISO_A3"))

# Rescale the weights
hh_surveys$weight_p2<-hh_surveys$weight_p*(hh_surveys$total_population/hh_surveys$total_weight_by_country)
total_weight_by_country2 <- aggregate(weight_p2 ~ countrycode, data = hh_surveys, FUN = sum)


############################################################
# 4. Check if it fits poverty numbers from the original surveys ---------------------------------------------------------------
############################################################

# Collapse to the country level
hh_survey_country <- hh_surveys %>%
  group_by(countrycode) %>%
  summarise(
    poor_215 = sum(poor_215 * weight_p2) / sum(weight_p2),
    poor_365 = sum(poor_365 * weight_p2) / sum(weight_p2),
    .groups = 'drop'  # Drop grouping structure after summarising
  )

# Fetch poverty data using wbstats (pov numbers, most recent value)
wb_pov_2019 <- wb_data(indicator = c("SI.POV.DDAY", "SI.POV.LMIC"), 
                       country = country_codes, 
                       date=2019,
                       gapfill=TRUE, 
                       mrv=10)

## The numbers are the same; so the discrepancy with GSAP really comes from the projection. 
## The underlying dataset doing that was not shared by Mingh. 

# ############################################################
# # 5. Collapse at subadmin level and check fit GSAP ---------------------------------------------------------------
# ############################################################
# 
# vuln_hh <- hh_surveys %>%
#   group_by(countrycode, subnatid, subnatid2, subnatid3) %>%
#   summarise(
#     dep_infra_elec = sum(dep_infra_elec * weight_p2) / sum(weight_p2),
#     dep_infra_water = sum(dep_infra_water * weight_p2) / sum(weight_p2),
#     dep_educ = sum(dep_educ * weight_p2) / sum(weight_p2),
#     poor_215 = sum(poor_215 * weight_p2) / sum(weight_p2),
#     poor_365 = sum(poor_365 * weight_p2) / sum(weight_p2),
#     .groups = 'drop'  # Drop grouping structure after summarising
#   )
# 
# #write.xlsx(vuln_hh, file = file.path(data_hh_survey, "vuln_estimates.xlsx"))
# 
# grid_poor<- readRDS(file.path(final_replication,"grid_10km_poor_final_corrected2.Rds"))
# grid_comp <- grid_poor %>%
#   group_by(ISO_A2, WB_ADM1_NA, WB_ADM2_NA, baseyear, survname, id) %>%
#   summarise(
#     poor_215_gsap = mean(poor215_ln),
#     poor_365_gsap = mean(poor365_ln),
#     .groups = 'drop'  # Drop grouping structure after summarizing
#   )
# 
# ## See the excel for the comparison. There is a difference because of the lineup to 2019. 
# ## The numbers in GSAP are projections for 2019, not the original ones. 
# ## I write the code to do the projection below.

############################################################
# 6. Add the projection to 2019 ---------------------------------------------------------------
############################################################

## We download the GDP growth in all years between survey year and 2019. 
## Then, we assume neutral distribution and 0.7 pass through. 

# The variable with growth rate has more missing value so I calculate it from the level
# Fetch GDP per capita (constant PPP)
gdp_growth_data <- wb_data(indicator = 'NY.GDP.PCAP.PP.KD',  # GDP per capita in constant 2017 PPP
                           country = country_codes[country_codes != "YEM"], 
                           start = 2010, 
                           end = 2019,
                           freq = 'Y')  # Yearly frequency

# Yemen only has data in constant 2015 prices
yemen_growth_data <- wb_data(indicator = 'NY.GDP.PCAP.KD',  # GDP per capita
                             country = c("YEM"), 
                             start = 2010, 
                             end = 2019,
                             freq = 'Y') %>%
  mutate(NY.GDP.PCAP.PP.KD=NY.GDP.PCAP.KD) %>%
  dplyr::select(-NY.GDP.PCAP.KD)

gdp_growth_data <- rbind(gdp_growth_data, yemen_growth_data)

# GDP growth rate
gdp_growth_data <- gdp_growth_data %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    gdp_growth_rate = (NY.GDP.PCAP.PP.KD / lag(NY.GDP.PCAP.PP.KD) - 1) * 100  # Calculating growth rate as a percentage
  ) %>%
  ungroup()

# Make it a wide dataset
gdp_growth_wide <- gdp_growth_data %>%
  dplyr::select(country = iso3c, year = date, gdp_growth = gdp_growth_rate) %>%
  spread(key = year, value = gdp_growth)
gdp_growth_wide <- gdp_growth_wide %>%
  rename_at(vars(-country), ~paste0("gdp_growth_", .))

# Project poverty rates in hh_surveys_proj.
hh_surveys_proj <- hh_surveys[,c("countrycode", "subnatid", "subnatid2", "subnatid3", "year", "unique_id", "weight_p2", "welfare_ppp", "poor_215", "poor_365")]
hh_surveys_proj <- left_join(hh_surveys_proj, gdp_growth_wide, by = c("countrycode" = "country"))

start_year <- min(hh_surveys_proj$year)
# Generate the projection with neutral distribution and 0.7 passthrough
# Loop through each year from the year after the start_year to the end_year
for (i in (start_year + 1):2019) {
  # Create the new welfare variable name for year i and the previous year (i-1)
  welfare_var <- paste0("welfare_ppp", i)
  previous_welfare_var <- paste0("welfare_ppp", i - 1)
  gdp_growth_var <- paste0("gdp_growth_", i)
  # Check if previous year's welfare exists; if not, initialize it
  hh_surveys_proj[[previous_welfare_var]] <- ifelse(hh_surveys_proj$year==i-1, hh_surveys_proj$welfare_ppp, ifelse(hh_surveys_proj$year<i-1, hh_surveys_proj[[previous_welfare_var]], NA))
  hh_surveys_proj[[welfare_var]] <- ifelse(hh_surveys_proj$year<i,(1 + 0.7*(hh_surveys_proj[[gdp_growth_var]] / 100)) * hh_surveys_proj[[previous_welfare_var]], NA)
}          


# Generate the projection with neutral distribution and 1 passthrough (for Yemen)
# Loop through each year from the year after the start_year to the end_year
for (i in (start_year + 1):2019) {
  # Create the new welfare variable name for year i and the previous year (i-1)
  welfare_var <- paste0("YEM_welfare_ppp", i)
  previous_welfare_var <- paste0("YEM_welfare_ppp", i - 1)
  gdp_growth_var <- paste0("gdp_growth_", i)
  # Check if previous year's welfare exists; if not, initialize it
  hh_surveys_proj[[previous_welfare_var]] <- ifelse(hh_surveys_proj$year==i-1, hh_surveys_proj$welfare_ppp, ifelse(hh_surveys_proj$year<i-1, hh_surveys_proj[[previous_welfare_var]], NA))
  hh_surveys_proj[[welfare_var]] <- ifelse(hh_surveys_proj$year<i,(1 + 1*(hh_surveys_proj[[gdp_growth_var]] / 100)) * hh_surveys_proj[[previous_welfare_var]], NA)
}          

hh_surveys_proj <- hh_surveys_proj %>%
  mutate(welfare_ppp2019 = ifelse(year>=2019, welfare_ppp, ifelse(countrycode=="YEM", YEM_welfare_ppp2019, welfare_ppp2019)))

# Poverty projections
hh_surveys_proj <-hh_surveys_proj %>%
  mutate(poor_215_2019 = ifelse(!is.na(welfare_ppp2019) & welfare_ppp2019<2.15, 1, ifelse(is.na(welfare_ppp2019), NA, 0))) %>%
  mutate(poor_365_2019 = ifelse(!is.na(welfare_ppp2019) & welfare_ppp2019<3.65, 1, ifelse(is.na(welfare_ppp2019), NA, 0)))

# Aggregate to compare to GSAP
vuln_hh_proj <- hh_surveys_proj %>%
  group_by(countrycode, subnatid, subnatid2, subnatid3) %>%
  summarise(
    poor_215_2019 = sum(poor_215_2019 * weight_p2) / sum(weight_p2),
    poor_365_2019 = sum(poor_365_2019 * weight_p2) / sum(weight_p2),
    .groups = 'drop'  # Drop grouping structure after summarising
  )
## It is very close or exactly the same for all. 
## In Iran, it is similar but not as close as elsewhere, likely because they do not use neutral distribution.

## Merge projected variables to hh_survey
hh_surveys_proj <- hh_surveys_proj %>%
  dplyr::select(unique_id, welfare_ppp2019, poor_215_2019, poor_365_2019)

hh_surveys <- hh_surveys %>%
  left_join(hh_surveys_proj, by = "unique_id")


############################################################
# 7. Merge to SP and Fin based on 2019 welfare  ---------------------------------------------------------------
############################################################


# Calculate the quantile variable by country
# Get unique country codes
unique_countries <- unique(hh_surveys$countrycode)
# Loop over each unique country code
for (country in unique_countries) {
  # Subset the data for the current country
  subset_data <- hh_surveys[hh_surveys$countrycode == country, ]
  subset_data$q5_temp <- Hmisc::cut2(subset_data$welfare_ppp2019, g = 5, weights = subset_data$weight_p2)
  new_labels <- setNames(c(1,2,3,4,5), levels(subset_data$q5_temp))
  subset_data$q5 <- factor(subset_data$q5_temp, levels = names(new_labels), labels = new_labels)
  hh_surveys <- hh_surveys %>%
    left_join(subset_data[,c("q5","unique_id")], by = c("unique_id" = "unique_id"))
}

hh_surveys <- hh_surveys %>%
  mutate(
    q5_all = coalesce(q5, q5.x, q5.y, q5.x.x, q5.y.y, q5.x.x.x, q5.y.y.y, q5.x.x.x.x, q5.y.y.y.y, q5.x.x.x.x.x, q5.y.y.y.y.y)
  )


# Merge with SP data 
# Isolate the quantile in the SP data
aspire_mena$q5_sp <- gsub(".*_q([0-9]+)_.*", "\\1", aspire_mena$Indicator_Code)
aspire_mena <- aspire_mena %>%
  mutate(urban = if_else(Sub_Topic4 == "Rural", 0, ifelse(Sub_Topic4=="Urban",1,NA)))

aspire_mena <- aspire_mena %>%
  filter(!is.na(urban)) %>%                      # First, exclude rows where urban is NA
  group_by(Country_Code) %>%                     # Group by Country_Code
  filter(Year == max(Year)) %>%                  # Filter to keep only rows with the max year in each group
  ungroup()  

hh_surveys <- hh_surveys %>%
  left_join(aspire_mena[,c("q5_sp", "Country_Code", "urban", "val")], by = c("countrycode" = "Country_Code", "urban"="urban", "q5_all"="q5_sp"))
hh_surveys <- hh_surveys %>%
  rename(sp_cov = val)

# Merge with Financial inclusion
hh_surveys <- hh_surveys %>%
  mutate(q5_all = as.numeric(q5_all))
# Perform the join
hh_surveys <- hh_surveys %>%
  left_join(findex_mena[, c("code", "account", "inc_q")], by = c("countrycode" = "code", "q5_all" = "inc_q"))
hh_surveys <- hh_surveys %>%
  mutate(account = account * 100)

# Make them probabilities
hh_surveys$account<-hh_surveys$account/100
hh_surveys$sp_cov<-hh_surveys$sp_cov/100

## Save the temp file
saveRDS(data_hh_survey, file.path(data_hh_survey,
                              "HH_surveys_SP_Fin.Rds"))


############################################################
# 8. Joint distribution  ---------------------------------------------------------------
############################################################

### GU uses simulations to calculate the probabilities for each have a given number of dimensions.
### They first get number for a simulation (aggregation in subadmins with pov weights). 
### For each simulation, they get an estimated share of people with number of dim in each subadmin
### Then, they take the average accross simulations to have the expected share of people having a number of dimensions by subadmin

### This would make the adjustment at the cell level very time consuming. 
### Instead, we calculate the expected value of the number of dimensions for each household. 
### Then, we aggregate the expected value the admin2 level with pov weights. 
### We can do that because the sum signs can be exchanged. 
### Also we can calculate the expected value of the sum of vulnerability dimensions easily without simulations
### We only assume two independent Bernouilli distributions (given income) as they do in the GU. 

#Number of dimensions apart from SP and Fin: 
hh_surveys <- hh_surveys %>%
  mutate(
    vuln_dim_sum_215 = rowSums(dplyr::select(., dep_infra_elec, dep_infra_water, dep_educ, poor_215_2019), na.rm = TRUE), # Sum columns, ignoring NAs
    vuln_dim_sum_365 = rowSums(dplyr::select(., dep_infra_elec, dep_infra_water, dep_educ, poor_365_2019), na.rm = TRUE), # Sum columns, ignoring NAs
    vuln_dim_sum = rowSums(dplyr::select(., dep_infra_elec, dep_infra_water, dep_educ), na.rm = TRUE), # Sum columns, ignoring NAs
    max_dim_215 = rowSums(!is.na(dplyr::select(., dep_infra_elec, dep_infra_water, dep_educ, poor_215_2019, sp_cov, account)), na.rm = TRUE),
    max_dim_365 = rowSums(!is.na(dplyr::select(., dep_infra_elec, dep_infra_water, dep_educ, poor_365_2019, sp_cov, account)), na.rm = TRUE),
    max_dim = rowSums(!is.na(dplyr::select(., dep_infra_elec, dep_infra_water, dep_educ, sp_cov, account)), na.rm = TRUE),
  )
summary(hh_surveys$vul_dum_sum215)
### Probability to not have one of SP / FIN and Both
# TO not have one of them, we need one and not the other
# Probability to have no deprivation
hh_surveys$sp_acc_0dim<- ifelse(is.na(hh_surveys$sp_cov)==0,(hh_surveys$sp_cov*hh_surveys$account), hh_surveys$account)
# Probability to not have it given income is 1-coverage
# sp_cov has missing values for some countries
hh_surveys$sp_acc_1dim<- ifelse(is.na(hh_surveys$sp_cov)==0, (hh_surveys$sp_cov*(1- hh_surveys$account)) + (hh_surveys$account*(1- hh_surveys$sp_cov)), 1-hh_surveys$account)
# Probability to not have both, we can multiply probabilities because we assume independence given income
hh_surveys$sp_acc_2dim<- ifelse(is.na(hh_surveys$sp_cov)==0, (1-hh_surveys$sp_cov)*(1-hh_surveys$account),0)

## Expected number of dimensions
hh_surveys$exp_dim_215<-hh_surveys$vuln_dim_sum_215 + 1*hh_surveys$sp_acc_1dim + 2*hh_surveys$sp_acc_2dim
hh_surveys$exp_dim_365<-hh_surveys$vuln_dim_sum_365 + 1*hh_surveys$sp_acc_1dim + 2*hh_surveys$sp_acc_2dim
hh_surveys$exp_dim<-hh_surveys$vuln_dim_sum + 1*hh_surveys$sp_acc_1dim + 2*hh_surveys$sp_acc_2dim


# Write the different combinations
hh_surveys <- hh_surveys %>%
  mutate(
    exp_dim_215_0 = ifelse(vuln_dim_sum_215 == 0 & sp_acc_0dim>0, sp_acc_0dim, 0),
    exp_dim_215_1 = ifelse(vuln_dim_sum_215 == 1 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_215 == 0 & sp_acc_1dim>0, sp_acc_1dim, 0)),
    exp_dim_215_2 = ifelse(vuln_dim_sum_215 == 2 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_215 == 1 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_215 == 0 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_215_3 = ifelse(vuln_dim_sum_215 == 3 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_215 == 2 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_215 == 1 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_215_4 = ifelse(vuln_dim_sum_215 == 4 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_215 == 3 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_215 == 2 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_215_5 = ifelse(vuln_dim_sum_215 == 5 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_215 == 4 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_215 == 3 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_215_6 = ifelse(vuln_dim_sum_215 == 6 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_215 == 5 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_215 == 4 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
  )

hh_surveys <- hh_surveys %>%
  mutate(
    exp_dim_365_0 = ifelse(vuln_dim_sum_365 == 0 & sp_acc_0dim>0, sp_acc_0dim, 0),
    exp_dim_365_1 = ifelse(vuln_dim_sum_365 == 1 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_365 == 0 & sp_acc_1dim>0, sp_acc_1dim, 0)),
    exp_dim_365_2 = ifelse(vuln_dim_sum_365 == 2 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_365 == 1 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_365 == 0 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_365_3 = ifelse(vuln_dim_sum_365 == 3 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_365 == 2 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_365 == 1 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_365_4 = ifelse(vuln_dim_sum_365 == 4 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_365 == 3 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_365 == 2 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_365_5 = ifelse(vuln_dim_sum_365 == 5 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_365 == 4 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_365 == 3 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_365_6 = ifelse(vuln_dim_sum_365 == 6 & sp_acc_0dim>0, sp_acc_0dim,
                           ifelse(vuln_dim_sum_365 == 5 & sp_acc_1dim >= 0, sp_acc_1dim,
                                  ifelse(vuln_dim_sum_365 == 4 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
  )

hh_surveys <- hh_surveys %>%
  mutate(
    exp_dim_0 = ifelse(vuln_dim_sum == 0 & sp_acc_0dim>0, sp_acc_0dim, 0),
    exp_dim_1 = ifelse(vuln_dim_sum == 1 & sp_acc_0dim>0, sp_acc_0dim,
                       ifelse(vuln_dim_sum == 0 & sp_acc_1dim>0, sp_acc_1dim, 0)),
    exp_dim_2 = ifelse(vuln_dim_sum == 2 & sp_acc_0dim>0, sp_acc_0dim,
                       ifelse(vuln_dim_sum == 1 & sp_acc_1dim >= 0, sp_acc_1dim,
                              ifelse(vuln_dim_sum == 0 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_3 = ifelse(vuln_dim_sum == 3 & sp_acc_0dim>0, sp_acc_0dim,
                       ifelse(vuln_dim_sum == 2 & sp_acc_1dim >= 0, sp_acc_1dim,
                              ifelse(vuln_dim_sum == 1 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_4 = ifelse(vuln_dim_sum == 4 & sp_acc_0dim>0, sp_acc_0dim,
                       ifelse(vuln_dim_sum == 3 & sp_acc_1dim >= 0, sp_acc_1dim,
                              ifelse(vuln_dim_sum == 2 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    exp_dim_5 = ifelse(vuln_dim_sum == 5 & sp_acc_0dim>0, sp_acc_0dim,
                       ifelse(vuln_dim_sum == 4 & sp_acc_1dim >= 0, sp_acc_1dim,
                              ifelse(vuln_dim_sum == 3 & sp_acc_2dim >= 0, sp_acc_2dim, 0))),
    
  )

### Save the file
saveRDS(hh_surveys, file.path(data_hh_survey,
                              "HH_surveys_all_dim.Rds"))

# Aggregate to compare with GSAP and GU
hh_surveys_ADM <- hh_surveys %>%
  group_by(countrycode, subnatid, subnatid2, subnatid3) %>%
  summarise(
    na_share_water=sum((is.na(dep_infra_water)* weight_p2) / sum(weight_p2)),
    na_share_elec=sum((is.na(dep_infra_elec)* weight_p2) / sum(weight_p2)),
    na_share_educ=sum((is.na(dep_educ)* weight_p2) / sum(weight_p2)),
    na_share_poor_215_2019=sum((is.na(poor_215_2019)* weight_p2) / sum(weight_p2)),
    na_share_poor_365_2019=sum((is.na(poor_365_2019)* weight_p2) / sum(weight_p2)),
    na_share_fin=sum((is.na(account)* weight_p2) / sum(weight_p2)),
    na_share_sp=sum((is.na(sp_cov)* weight_p2) / sum(weight_p2)),
    max_dim_215=max(max_dim_215),
    max_dim_365=max(max_dim_365),
    max_dim=max(max_dim),
    dep_infra_water= sum(dep_infra_water * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(dep_infra_water))),
    dep_infra_elec= sum(dep_infra_elec * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(dep_infra_elec))),
    dep_educ= sum(dep_educ * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(dep_educ))),
    poor_215_2019 = sum(poor_215_2019 * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(poor_215_2019))),
    poor_365_2019 = sum(poor_365_2019 * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(poor_365_2019))),
    dep_sp = sum((1- sp_cov) * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(sp_cov))),
    dep_fin = sum((1- account) * weight_p2, na.rm=T) / sum(weight_p2*(1-is.na(account))),
    exp_dim_215 = sum(exp_dim_215 * weight_p2) / sum(weight_p2),
    exp_dim_365 = sum(exp_dim_365 * weight_p2) / sum(weight_p2),
    exp_dim = sum(exp_dim * weight_p2) / sum(weight_p2),
    exp_dim_215_0 = sum(exp_dim_215_0 * weight_p2) / sum(weight_p2),
    exp_dim_215_1 = sum(exp_dim_215_1 * weight_p2) / sum(weight_p2),
    exp_dim_215_2 = sum(exp_dim_215_2 * weight_p2) / sum(weight_p2),
    exp_dim_215_3 = sum(exp_dim_215_3 * weight_p2) / sum(weight_p2),
    exp_dim_215_4 = sum(exp_dim_215_4 * weight_p2) / sum(weight_p2),
    exp_dim_215_5 = sum(exp_dim_215_5 * weight_p2) / sum(weight_p2),
    exp_dim_215_6 = sum(exp_dim_215_6 * weight_p2) / sum(weight_p2),
    exp_dim_365_0 = sum(exp_dim_365_0 * weight_p2) / sum(weight_p2),
    exp_dim_365_1 = sum(exp_dim_365_1 * weight_p2) / sum(weight_p2),
    exp_dim_365_2 = sum(exp_dim_365_2 * weight_p2) / sum(weight_p2),
    exp_dim_365_3 = sum(exp_dim_365_3 * weight_p2) / sum(weight_p2),
    exp_dim_365_4 = sum(exp_dim_365_4 * weight_p2) / sum(weight_p2),
    exp_dim_365_5 = sum(exp_dim_365_5 * weight_p2) / sum(weight_p2),
    exp_dim_365_6 = sum(exp_dim_365_6 * weight_p2) / sum(weight_p2),
    exp_dim_0 = sum(exp_dim_0 * weight_p2) / sum(weight_p2),
    exp_dim_1 = sum(exp_dim_1 * weight_p2) / sum(weight_p2),
    exp_dim_2 = sum(exp_dim_2 * weight_p2) / sum(weight_p2),
    exp_dim_3 = sum(exp_dim_3 * weight_p2) / sum(weight_p2),
    exp_dim_4 = sum(exp_dim_4 * weight_p2) / sum(weight_p2),
    exp_dim_5 = sum(exp_dim_5 * weight_p2) / sum(weight_p2),
    .groups = 'drop'  # Drop grouping structure after summarising
  )

saveRDS(hh_surveys_ADM, file.path(data_hh_survey,
                                  "HH_surveys_all_dim_ADM2.Rds"))

### Here, we have the estimated share of people vulnerable to each dimension (or 1,2,3,4,5,6)
### This is by admin1/2
### It can be merged to the grid with the codebook I mention in the code below.
### Then, we can assume that all dimensions (but the poverty one) are spatially distributed homogenously inside admin 2.



##### Below this is experimental code to try to reweight the household surveys 
# to match the RWI in each cell
# Unfortunately, I think it would be too computationally intensive (~1M rows for DJI only)
# to be processed on WB VMs. Will have to wait for a better, faster, more digital Bank :)


############################################################
# 9. Merge hhs to the grid and reweight them to match pov numbers and RWI distribution  ---------------------------------------------------------------
############################################################

# I can match households to the grid based on Admin 2
# The aggregated poverty number will be the one in GSAP (above check)
# For each cell in the admin2; I want to reweight the households for the pov rate to take into account RWI
# I recompute the share of people with a give number of deprivation dimensions using this weight

#Shortcut to not process the above code each time (but faster to process it than to open it on my VM)
#hh_surveys<- readRDS(file.path(data_hh_survey,"HH_surveys_all_dim.Rds"))

# If  grid_10km_all_vul_indicators.Rds had the id variable we could us it directly, but it does not
# grid_sf_vul <- readRDS(file.path(
#   mena_file_path,
#   "Allsources",
#   "grid_10km_all_vul_indicators.Rds"
# ))


# I manually created a codebook between id in the grid and the surveys
codebook_survey_grid <- read_excel(file.path(
  data_hh_survey,
  "codebook_survey_grid.xlsx"))

# The codebook refers to the subadmin in the surveys and a id string in the initial grid
gsap_grid_merged<-readRDS(file.path(
  mena_file_path,
  "RWI",
  "final",
  "grid_rwi_gsap_final_corrected.Rds"))

#### Pilot on Djibouti
dji_hh_surveys <- hh_surveys %>%
  dplyr::filter(countrycode == "DJI")

dji_gsap_grid_merged <- gsap_grid_merged %>%
  dplyr::filter(ISO_A2 == "DJ")
#  dplyr::filter(ISO_A2 == "DJ" & id=="10 - Djibouti")

# Merge grid and households in the survey
dji_hh_surveys <- left_join(dji_hh_surveys, codebook_survey_grid, by = c("countrycode", "subnatid"))
dji_hh_surveys %>%
  group_by(subnatid) %>%
  summarise(count = n(), .groups = 'drop')  # Summarize and drop grouping
# 10,877 will be "10 - Djibouti" and matched to one in the 8 corresponding grid cells when selecting "10-Djibouti" above

# Count number of grid cells 
unique_grid_count <- dji_gsap_grid_merged%>%
  dplyr::select(-geometry) %>%  
  as.data.frame()  %>%  
  group_by(ISO_A2, id) %>%
  summarise(unique_grid_count = n_distinct(grid_id))
dji_gsap_grid_merged <- left_join(dji_gsap_grid_merged, unique_grid_count, by = c("ISO_A2", "id"))

dji_hh_surveys <- left_join(dji_gsap_grid_merged, dji_hh_surveys, by = c("ISO_A2"="ISO_A2.x", "id"="id_adm"))
# All grid cells are matched to all the households in the survey (10877*8 observations)

# We divide by the number of grid cells "hosting" the same households to avoid double counting them.
dji_hh_surveys$weight_p3<-dji_hh_surveys$weight_p2/dji_hh_surveys$unique_grid_count
# Now sum of weight_p3 should be the population in the admin on which we are matching "id"

# Try to find population weights not deviating too much from weight_p2 but giving the new poverty rate for that cell
# For each gridcell we would have to use "rake" to find new weights given poverty rate. 

# View(dji_hh_surveys[,c("unique_id", "id", "rwi_all", "poor215_gsap_dup", "poor365_gsap_dup", "poor215_rwi_dup", "poor365_rwi_dup", "rwi_weight_dup", "poor_215", "poor_365", "population_gsap_dup", "pop_count2_dup")])





