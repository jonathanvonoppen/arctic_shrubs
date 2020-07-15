# Quick script to extract Riley's TRI for Jonathan's Nuuk fjord plots.
# Jakob Assmann j.assmann@bios.au.dk 14 July 2020

library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(cowplot)

# Load files
jonathan_plots <- read.csv("D:/Jakob/ArcticDEM/jonathan/nuuk_env_cover_plots.csv",
                           stringsAsFactors = F) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)
  
plot(jonathan_plots)
tri_mosaic <- raster("D:/Jakob/ArcticDEM/jonathan/nuuk_fjord_tri_mosaic.vrt")

# buffer plot area
jonathan_plots_buffered <- jonathan_plots %>%
  st_transform(crs = crs(tri_mosaic)) %>% 
  st_buffer(6)

# Extract weighted mean
jonathan_plots$tri <- extract(tri_mosaic,
                        as_Spatial(jonathan_plots_buffered),
                        weights = T,
                        fun = mean)

# Quick quality control
ggplot(distinct(jonathan_plots, plot,plotgroup, tri), 
       aes(x = tri, fill = as.factor(plotgroup))) +
  geom_histogram() +
  labs(x = "TRI", y = "Count") +
  theme_cowplot(15) +
  theme(legend.position = "none")

# export data
jonathan_plots_df <- read.csv("D:/Jakob/ArcticDEM/jonathan/nuuk_env_cover_plots.csv",
                           stringsAsFactors = F) 
jonathan_plots_df$tri <- jonathan_plots$tri
write.csv(jonathan_plots_df, 
          "D:/Jakob/ArcticDEM/jonathan/nuuk_env_cover_plots_with_tri.csv",
          row.names = F)
