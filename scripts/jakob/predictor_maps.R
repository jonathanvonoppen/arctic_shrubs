## Visualise predictor rasters for whole of study area
# Jakob Assmann j.assmann@bio.au.dk


# Dependencies
library(raster)
library(rasterVis)
library(colorspace)
library(sf)
library(tidyverse)
library(maptools)

## Load rasters 
raster_path <- "O:/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/pred_rasters/"
predictor_paths <- c(
  "insol.asc",
  "jjaprecip.asc",
  "mamprecip.asc",
  "tempjja.asc",
  "tempmax.asc",
  "tempmin.asc",
  "tempcont.asc")
# template with projection
projection_temp <- raster("O:/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/processed/main_ras.tif")

# load rasters and assign projection
raster_list <- lapply(
  predictor_paths,
  function(predictor_raster){
    predictor_raster <- raster(
      paste0(raster_path,
             predictor_raster))
    crs(predictor_raster) <- crs(projection_temp)
    return(predictor_raster)
  }
)

# add TRI raster to list
raster_list <- append(
  raster_list, 
  setNames(
    raster("D:/Jakob/ArcticDEM/jonathan/nuuk_fjord_tri_mosaic.vrt"),
    "tri"))

## Load plot locations
nuuk_plots <- read.csv("data/nuuk_env_cover_plots.csv",
                       stringsAsFactors = F) %>%
  distinct(site, lat, long) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs(projection_temp)) %>% 
  mutate(site = paste0("Station ", site)) %>%
  group_by(site) %>%
  summarise() %>%
  st_centroid() %>%
  as_Spatial()

# Lookup table for pretty names and colour ramps
lookup_table <- data.frame(
  raster_names = unlist(lapply(raster_list, names)),
  pretty_names = c("Insolation",
                   "Mean Precipitation June-July-August",
                   "Mean Precipitation March-April-May ",
                   "Mean Temperature June-July-August",
                   "Annual Maximum Temperature",
                   "Annual Minimum Temperature",
                   "Temperature Continentality",
                   "Topographic Roughness Index"),
  scale_name = c("Oranges",
                 "Blues",
                 "Blues",
                 "Oranges",
                 "Oranges",
                 "Blues",
                 "Greens",
                 "YlOrBr"),
  label_colour = c(
    "white",
    "black",
    "black",
    "white",
    "white",
    "white",
    "white",
    "black"
  ),
  stringsAsFactors = F
)

# Write function to plot rasters
plot_raster <- function(predictor_raster){
  # Create plot
  raster_plot <- levelplot(predictor_raster, 
                           margin = F,
                           main = lookup_table$pretty_names[lookup_table$raster_names == names(predictor_raster)],
                           scales = list(draw = F),
                           par.settings = rasterTheme(
                             region = sequential_hcl(
                               100, 
                               lookup_table$scale_name[lookup_table$raster_names == names(predictor_raster)],
                               rev = T)))  + 
    latticeExtra::layer(sp.points(nuuk_plots,
                                  pch = 21,
                                  cex = 1,
                                  lwd = 3,
                                  col = lookup_table$label_colour[lookup_table$raster_names == names(predictor_raster)]),
                        data = list(predictor_raster = predictor_raster)) +
    latticeExtra::layer(sp.pointLabel(nuuk_plots,
                                      label = nuuk_plots$site,
                                      cex = 1,
                                      col = lookup_table$label_colour[lookup_table$raster_names == names(predictor_raster)]),
                        data = list(predictor_raster = predictor_raster)) 
  # Export File
  png(paste0("figures/predictor_maps/",
             names(predictor_raster),
             ".png"), 
      width = 8,
      height = 8 * 0.82,
      units = "in",
      res = 300)
  print(raster_plot)
  dev.off()
  
}

# Execute function
lapply(raster_list, plot_raster)
