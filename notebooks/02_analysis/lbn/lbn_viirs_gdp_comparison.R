## Create NTL Trends


# Load Data ---------------------------------------------------------------
df <- readRDS(file.path(lbn_onedrive_dir,
                        "data",
                        "country",
                        "final",
                        "lbn_gdp_ntl.Rds"))




# Prepare data ------------------------------------------------------------
df <- df %>%
  mutate(ntl_prop2 = ifelse(ntl_mean >2, ntl_mean,NA))



# Trend Plot -----------------------------------------------------------------
df %>%
  select(-ntl_mean) %>%
  melt(.,id = "year") %>%
  ggplot(aes(x = year, y = value)) +
  geom_smooth(size = 1, color = "black") +
  geom_vline(xintercept = 2018, color = "darkgrey", linetype = "dashed") +
  facet_wrap(~variable, scales = "free") +
  theme_classic2()


ggsave(filename = file.path(lbn_onedrive_dir,
                       "graphs",
                       "gdp_comp.png"), width = 8, height = 4)
  


head(df)

# Correlation Plot --------------------------------------------------------


# Function to create a correlation plot
create_correlation_plot <- function(data, x_col, y_col, x_label, y_label, title, filename) {
  plot <- data %>%
    select(-ntl_mean) %>%
    filter(!is.na({{ x_col }})) %>%
    ggplot(aes(x = {{ x_col }}, y = {{ y_col }})) +
    geom_smooth(size = 1, color = "black") +
    geom_text(
      aes(label = paste("Correlation =", round(cor({{ x_col }}, {{ y_col }}), 2))),
      x = Inf, y = Inf,
      hjust = 1, vjust = 1,
      size = 5,
      color = "black"
    ) +
    labs(x = x_label, y = y_label, title = title) +
    theme_classic2()
  
  ggsave(filename = filename, plot = plot, width = 6, height = 4)
}

# Call the function for each case
create_correlation_plot(df, gdp_constant, ntl_prop2,
                        "Nighttime Lights [Proportion > 2]", "GDP (Constant 2015 US$)",
                        "NTL [Prop <2] and GDP [Constant 2015 US$]",
                        file.path(lbn_onedrive_dir, "graphs", "gdp_constant.png"))

create_correlation_plot(df, gdp_constant_lcu, ntl_prop2,
                        "Nighttime Lights [Proportion > 2]", "GDP (Constant LCU)",
                        "NTL [Prop <2] and GDP [Constant LCU]",
                        file.path(lbn_onedrive_dir, "graphs", "gdp_constant_lcu.png"))

create_correlation_plot(df, gdp_pcap_constant, ntl_prop2,
                        "Nighttime Lights [Proportion > 2]", "GDP Per Capita[Constant US$]",
                        "NTL [Prop <2] and GDP Per Capita [Constant US$]",
                        file.path(lbn_onedrive_dir, "graphs", "gdp_pcap_constant.png"))

create_correlation_plot(df, gdp_pcap_constant_lcu, ntl_prop2,
                        "Nighttime Lights [Proportion > 2]", "GDP Per Capita[Constant LCU]",
                        "NTL [Prop <2] and GDP Per Capita [Constant LCU]",
                        file.path(lbn_onedrive_dir, "graphs", "gdp_pcap_constant_lcu.png"))

