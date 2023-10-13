# Plots


# Plot --------------------------------------------------------------------
grid_10km_prj <- st_transform(grid_10km,4326)

ggplot() +
  geom_sf(data = grid_10km_prj, aes(fill = rwi_all),
          linewidth = 0.05, alpha = 0.8, color = "black") +
  theme_classic() +
  theme(text = element_text(color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = "right") +  
  labs(x = NULL, 
       y = NULL, 
       title = "Relative Wealth Index", 
       subtitle = "Source: Chi et. al, 2021", 
       caption = "Missing values in Black") +
  geom_sf(data = mena_adm2, linewidth = 0.2, fill = NA, color = "grey", alpha = 0.8) +
  geom_sf(data = mena_adm0, fill = NA,
          linewidth = 0.2, alpha = 0.8, color = "blue") +
  scale_fill_gradient2(name = "Relative Wealth Index", 
                       low = "yellow", mid = "orange", high = "red",
                       midpoint = 0,
                       na.value = 'black',
                       guide = guide_colourbar(
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       )
  )





ggsave(filename = file.path(figures_dir,"rwi_missing_val.png"), height= 12, width = 16, units = c("in"))


## Exposed Poor to Heat Stress
gsap_grid_merged_filtered_prj <- st_transform(gsap_grid_merged_filtered,4326)

ggplot() +
  geom_sf(data = gsap_grid_merged_filtered_prj, aes(fill = heat_affected_poor),
          linewidth = 0.05, alpha = 0.8, color = "black") +
  theme_classic() +
  theme(text = element_text(color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.position = "right") +  
  labs(x = NULL, 
       y = NULL, 
       title = "Total Number of Poor Affected by Heat Stress", 
       subtitle = "Source: Wet Bulb Globe Temperature >= 32 Celsius", 
       caption = "Missing values in Black") +
  geom_sf(data = mena_adm2, linewidth = 0.2, fill = NA, color = "grey", alpha = 0.8) +
  geom_sf(data = mena_adm0, fill = NA,
          linewidth = 0.2, alpha = 0.8, color = "blue") +
  scale_fill_gradient2(name = "Total Number of Poor \nAffected By Heat Stress", 
                       low = "yellow", mid = "orange", high = "red",
                       na.value = 'black',
                       guide = guide_colourbar(
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5
                       )
  )

ggsave(filename = file.path(figures_dir,"poor_affected_heat.png"), height= 12, width = 16, units = c("in"))

