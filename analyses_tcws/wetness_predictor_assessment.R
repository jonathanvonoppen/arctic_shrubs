# Temperature variability and biotic interactions explain shrub species abundance along a coastal-inland gradient in arctic Greenland

# von Oppen, J. et al. 2021

# Comparison of wetness predictor power for predicting Cyperaceae & bryophyte abundance


# dependencies
library(tidyverse)
library(cowplot)

# species data (raw pinpoint data) [from Jacob Nabe-Nielsen]
spec_nuuk <- read.csv(file.path("data", "input_data", "Nuuk plant data 150201 - Pin-point data - stacked.csv")) %>% 
  as_tibble

# extract abundance of focal taxa (Cyperaceae & bryophytes)
abund_cyp_plus_bry <- spec_nuuk %>% 
  
  # filter for focal taxa
  filter(taxon %in% c("Cyperaceae", "Mosses")) %>% 
  
  # calculate abundance per plot
  group_by(plot) %>% 
  summarise(cover = sum(presence) / 25) %>% 
  ungroup() %>% 
  
  mutate(taxon = "combined") %>% 
  
  relocate(taxon, .after = plot)

abund_cyp_bry <- spec_nuuk %>% 
  
  # filter for focal taxa
  filter(taxon %in% c("Cyperaceae", "Mosses")) %>% 
  
  # calculate abundance per plot
  group_by(plot, taxon) %>% 
  summarise(cover = sum(presence) / 25) %>% 
  ungroup() %>% 
  
  # bind with combined cover data
  bind_rows(abund_cyp_plus_bry) %>% 
  
  arrange(plot, taxon) %>% 
  
  # join with topographic predictor data
  left_join(read.csv(file.path("data", "processed", "nuuk_env_cover_plots_topo_variables.csv"), 
                     header = T) %>% 
              select(plot, 
                     twi_fd8 = kopecky_twi,       # Topographic Wetness Index based on 30m GIMP MEaSUREs DEM
                                                    # and Freeman FD8 flow algorithm (Kopecky et al. 2020 SciTotEnv)
                     twi_saga = saga_twi,         # TWI based on 30m GIMP MEaSUREs DEM and SAGA GIS flow algorithm
                     tcws = TCwet_new             # Tasseled-Cap Wetness Index based on original Landsat imagery
              ) %>% 
              distinct(plot, .keep_all = T),
            by = c("plot"))

# correlations
correlate(filter(abund_cyp_bry, taxon == "combined") %>% select(-plot, -taxon))

# fit linear model
summary(aov(data = filter(abund_cyp_bry, taxon == "combined"), 
           formula = cover ~ tcws + twi_fd8 + twi_saga))

# extract R2 for individual relationships
# TCWS
summary(lm(data = filter(abund_cyp_bry, taxon == "combined"), 
           formula = cover ~ tcws))$r.squared
# FD8 TWI
summary(lm(data = filter(abund_cyp_bry, taxon == "combined"), 
           formula = cover ~ twi_fd8))$r.squared
# SAGA TWI
summary(lm(data = filter(abund_cyp_bry, taxon == "combined"), 
           formula = cover ~ twi_saga))$r.squared

# combine R2s into dataframe
r2labels <- tibble(wetness_var = c("twi_fd8", "twi_saga", "tcws"),
                   pos_x = c(-750, 13, 7.8),
                   pos_y = c(rep(0.85, 3)),
                   r2lab = c(paste0(rep("R^2[combined] == ", 3),
                                    round(c(summary(lm(data = filter(abund_cyp_bry, taxon == "combined"), 
                                                       formula = cover ~ tcws))$r.squared,
                                            summary(lm(data = filter(abund_cyp_bry, taxon == "combined"), 
                                                       formula = cover ~ twi_fd8))$r.squared,
                                            summary(lm(data = filter(abund_cyp_bry, taxon == "combined"), 
                                                       formula = cover ~ twi_saga))$r.squared),
                                          2))))

# prepare plotting data
abund_cyp_bry_plot <- abund_cyp_bry %>% 
  
  pivot_longer(cols = c("twi_fd8", "twi_saga", "tcws"),
               names_to = "wetness_var",
               values_to = "wetness_value")

# plot relationships
(comparison_plot <- ggplot(data = abund_cyp_bry_plot,
        aes(x = wetness_value,
            y = cover,
            colour = taxon)) +
    
    geom_point(alpha = .6) +
    
    geom_smooth(aes(colour = taxon),
                method = "lm",
                size = 2) +
    
    ylim(c(0,1.5)) +
    
    labs(x = "wetness value",
         y = "abundance") +
    
    # annotate("text",
    #          data = r2labels,
    #          aes(x = pos_x, y = pos_y,
    #              label = r2lab),
    #          size = 6,
    #          parse = TRUE) +
    
    facet_wrap(vars(wetness_var),
               scales = "free_x") +
    
    scale_colour_manual(values = c("gold", "navy", "darkgreen")) +
    
    theme_bw()) 

save_plot(file.path("figures", "nuuk_shrub_drivers_wetness_predictors_comparison.pdf"),
          comparison_plot, base_height = 8, base_aspect_ratio = 1.2)


