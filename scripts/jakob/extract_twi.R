# Wee script to extract and compare TWI values for the NUUk Fjord plots
# Jakob Assmann j.assmann@bio.au.dk  1 December 2020 

# Dependencies
library(raster)
library(sf)
library(tidyverse)

# Set raster progress bar
rasterOptions(progress = "text")

# Load Plot Geometries
nuuk_plots <- read_sf("scripts/jakob/nuuk_plots.shp")

# Load nuuk plot data frame
nuuk_plots_df <- read.csv("data/processed/nuuk_env_cover_plots.csv")

# Load rasters
kopecky_twi <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/twi/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_twi.tif")
saga_twi <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/twi/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.tif")
tri <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/tri/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_tri.tif")
TCwet <- raster("D:/Jakob/ArcticDEM/nathalie_nuuk/landsat_NUUK_UTM22.tif")
# Extract data
nuuk_plots$kopecky_twi <- raster::extract(kopecky_twi, 
                                          as_Spatial(st_buffer(nuuk_plots, 5)),
                                           weights = T, fun = mean)
nuuk_plots$saga_twi <- raster::extract(saga_twi, 
                                          as_Spatial(st_buffer(nuuk_plots, 5)),
                                          weights = T, fun = mean)
nuuk_plots$tri_new <- raster::extract(tri, 
                                       as_Spatial(st_buffer(nuuk_plots, 5)),
                                       weights = T, fun = mean)
nuuk_plots$TCwet_new <- raster::extract(TCwet, 
                                    as_Spatial(st_buffer(nuuk_plots, 5)),
                                    weights = T, fun = mean)
# merge with nuuk_lot data frame
nuuk_plots_df <- nuuk_plots %>%
  st_drop_geometry() %>%
  full_join(nuuk_plots_df)

# Test for correlation with TCwetness
cor(select(nuuk_plots_df, tcws, TCwet_new, twi_90m, kopecky_twi, saga_twi, ndvi))

# Save data frame
write_csv(nuuk_plots_df, "data/processed/nuuk_env_cover_plots_with_new_twi.csv")

# Code to applu a maks to the GIMP MEASURES rasters
mask <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_MASK.vrt")
mask <- reclassify(mask, c(-0.1,0.1,1,0.9,1.1,NA))
writeRaster(mask, "D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_MASK_inverted.tif", overwrite = T)
plot(mask)
