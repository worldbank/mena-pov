## Create NTL Trends


# Load Data ---------------------------------------------------------------
lbn_ntl <- readRDS(file.path(lbn_file_path,
                              "Nighttime_Lights",
                              "final",
                              "lbn_grid_viirs.Rds"))

gdp_per_cap <- read.csv(file.path(lbn_file_path,
                                  "Urban",
                                  "raw",
                                  "gdp_per_cap.csv"), skip = 3) %>%
  clean_names() %>% filter(country_name == "Lebanon")

gdp_current_usd <- read.csv(file.path(lbn_file_path,
                                  "Urban",
                                  "raw",
                                  "gdp_current_usd.csv"), skip = 3) %>%
  clean_names() %>% filter(country_name == "Lebanon")


gdp_current_lcu <- read.csv(file.path(lbn_file_path,
                                      "Urban",
                                      "raw",
                                      "gdp_current_lcu.csv"), skip = 3) %>%
  clean_names() %>% filter(country_name == "Lebanon")


# Clean GDP data ----------------------------------------------------------
gdp_per_cap <- gdp_per_cap %>%
  melt() %>%
  rename("year" = "variable") %>%
  # Remove the letter 'x' from the 'year' column
  mutate(year = sub("x", "", year),
         # Convert year to Date format with fixed month and day (June 1st)
         date = as.Date(paste(year, "06", "01", sep = "-"), format = "%Y-%m-%d", na.rm = TRUE)) %>%
  # Filter for years greater than or equal to 2012 and log-transform 'value'
  filter(year >= 2012) %>%
  mutate(gdp_per_cap = log(value))

gdp_current_usd <- gdp_current_usd %>%
  melt() %>%
  rename("year" = "variable") %>%
  # Remove the letter 'x' from the 'year' column
  mutate(year = sub("x", "", year),
         # Convert year to Date format with fixed month and day (June 1st)
         date = as.Date(paste(year, "06", "01", sep = "-"), format = "%Y-%m-%d", na.rm = TRUE)) %>%
  # Filter for years greater than or equal to 2012 and log-transform 'value'
  filter(year >= 2012) %>%
  mutate(gdp_current_usd = log(value))

gdp_current_lcu <- gdp_current_lcu %>%
  melt() %>%
  rename("year" = "variable") %>%
  # Remove the letter 'x' from the 'year' column
  mutate(year = sub("x", "", year),
         # Convert year to Date format with fixed month and day (June 1st)
         date = as.Date(paste(year, "06", "01", sep = "-"), format = "%Y-%m-%d", na.rm = TRUE)) %>%
  # Filter for years greater than or equal to 2012 and log-transform 'value'
  filter(year >= 2012) %>%
  mutate(gdp_current_lcu = log(value))


# Summarize data ----------------------------------------------------------
lbn_ntl_monthly <- lbn_ntl %>%
  group_by(month,year) %>%
  summarise(viirs_mean = log(mean(avg_rad_df)))

# Convert year and month to Date format
lbn_ntl_monthly$date <- as.Date(paste(lbn_ntl_monthly$year, lbn_ntl_monthly$month, "01", sep="-"), format="%Y-%m-%d")

# Plot --------------------------------------------------------------------
#... [previous code remains the same]

# Trend Plot -----------------------------------------------------------------

# For better readability, it is advisable to break down the plot creation. However, for brevity, it's combined here.
ggplot() +
  geom_line(data = lbn_ntl_monthly, aes(x = date, y = viirs_mean, color = "Nighttime Lights (Log)"), size = 0.8) +
  geom_point(data = lbn_ntl_monthly, aes(x = date, y = viirs_mean, color = "Nighttime Lights (Log)"), size = 1) +
  
  geom_line(data = gdp_per_cap, aes(x = date, y = gdp_per_cap, color = "GDP Per Capita (Log)"), size = 0.8) +
  geom_point(data = gdp_per_cap, aes(x = date, y = gdp_per_cap, color = "GDP Per Capita (Log)"), size = 1) +
  
  # Add these two new GDP measures to the plot
  geom_line(data = gdp_current_usd, aes(x = date, y = gdp_current_usd, color = "Current USD GDP (Log)"), size = 0.8) +
  geom_point(data = gdp_current_usd, aes(x = date, y = gdp_current_usd, color = "Current USD GDP (Log)"), size = 1) +
  
  geom_line(data = gdp_current_lcu, aes(x = date, y = gdp_current_lcu, color = "Current LCU GDP (Log)"), size = 0.8) +
  geom_point(data = gdp_current_lcu, aes(x = date, y = gdp_current_lcu, color = "Current LCU GDP (Log)"), size = 1) +
  
  labs(title = "Trends for Nighttime Lights & Various GDP Measures", 
       x = "Date", 
       y = "Log Value",
       color = "") + 
  scale_color_manual(values = c("Nighttime Lights (Log)" = "darkgreen", 
                                "GDP Per Capita (Log)" = "darkblue",
                                "Current USD GDP (Log)" = "purple",
                                "Current LCU GDP (Log)" = "red")) +
  theme_minimal()

# Correlation Plot Grid ------------------------------------------------------
# Assuming lbn_ntl is also structured similarly to GDP data, we can create individual datasets for correlation

ntl_annual <- lbn_ntl %>%
  group_by(year) %>%
  summarise(viirs_mean = mean(avg_rad_df, na.rm = T)) %>%
  filter(year < 2022)

gdp_per_cap_annual <- gdp_per_cap %>%
  filter(year != 2022) %>%
  select(gdp_per_cap) 

gdp_current_lcu_annual <- gdp_current_lcu %>%
  filter(year != 2022) %>%
  select(gdp_current_lcu) 

gdp_current_usd_annual <- gdp_current_usd %>%
  filter(year != 2022) %>%
  select(gdp_current_usd)


combined <- cbind(ntl_annual,gdp_current_lcu_annual,gdp_current_usd_annual,gdp_per_cap_annual)

cor(ntl_annual,gdp_per_cap_annual)

# Generate individual plots
generate_plot <- function(data, y_column, title){
  cor_value <- cor(data[[y_column]], data$viirs_mean, use = "complete.obs")
  
  p <- ggplot(data, aes_string(x = y_column, y = "viirs_mean")) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = paste("Correlation:", title, "r =", round(cor_value, 2)),
         x = title,
         y = "Nighttime Lights Radiance (Log)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(p)
}

plot1 <- generate_plot(combined, "gdp_per_cap", "GDP Per Capita (Log)")
plot2 <- generate_plot(combined, "gdp_current_usd", "Current USD GDP (Log)")
plot3 <- generate_plot(combined, "gdp_current_lcu", "Current LCU GDP (Log)")

# Use grid.arrange to display the three plots in a grid
library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol = 3)
