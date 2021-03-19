# Wee script to extract and compare TWI values for the NUUk Fjord plots
# Jakob Assmann j.assmann@bio.au.dk  1 December 2020 

## 1) Housekeeping ----

# Dependencies
library(raster)
library(sf)
library(tidyverse)
library(rasterVis)
library(maptools)
library(ggplot2)
library(cowplot)

# Set raster progress bar
rasterOptions(progress = "text")

## 2) DEM preparations ----

# Start running steps in batch script first.
# R shell somewhow does not pass on arguments to batch scripts
# So this hybrid solution is needed.

# Crop DEM

# Load Nathalie's raster to obatin extent for cropping
projection_temp <- raster("O:/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/processed/main_ras.tif")
extent_poly <- as(extent(projection_temp), "SpatialPolygons")
crs(extent_poly) <- crs(projection_temp)

# load GIMP DEM 30 m
GIMP_dem <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/dem/nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt")

# crop
GIMP_dem_cropped <- crop(GIMP_dem, extent_poly)

# save
writeRaster(GIMP_dem_cropped,
            "D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/dem/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_cropped.tif",
            overwrite = T)

# Load Plot Geometries
nuuk_plots <- read_sf("scripts/JJA/nuuk_plots.shp")

# Load nuuk plot data frame
nuuk_plots_df <- read.csv("data/processed/nuuk_env_cover_plots_topo_variables.csv")
nuuk_plots_df <- select(nuuk_plots_df, -kopecky_twi, -TCwet_new, -saga_twi)

# Load rasters
kopecky_twi <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/twi/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_twi.tif")
saga_twi <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/twi/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.tif")
tri_arctic_dem <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/tri/nuuk_fjord_ArcticDEM_mosaic_2m_tri.tif")
tri_gimp <- raster("D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/tri/nuuk_fjord_GIMP_MEaSUREs_30m_DEM_tri.tif")
TCwet <- raster("D:/Jakob/ArcticDEM/nathalie_nuuk/landsatTCwet_NUUK_UTM22.tif")

# Extract values for 5 m circle around each plot (weighted mean)
nuuk_plots$kopecky_twi <- raster::extract(kopecky_twi, 
                                          as_Spatial(st_buffer(nuuk_plots, 5)),
                                           weights = T, fun = mean)
nuuk_plots$saga_twi <- raster::extract(saga_twi, 
                                          as_Spatial(st_buffer(nuuk_plots, 5)),
                                          weights = T, fun = mean)
nuuk_plots$tri_arctic_dem <- raster::extract(tri_arctic_dem, 
                                       as_Spatial(st_buffer(nuuk_plots, 5)),
                                       weights = T, fun = mean)
nuuk_plots$tri_gimp <- raster::extract(tri_gimp, 
                                       as_Spatial(st_buffer(nuuk_plots, 5)),
                                       weights = T, fun = mean)
nuuk_plots$TCwet_new <- raster::extract(TCwet, 
                                    as_Spatial(st_buffer(nuuk_plots, 5)),
                                    weights = T, fun = mean)
# merge with nuuk_lot data frame
nuuk_plots_df <- nuuk_plots %>%
  st_drop_geometry() %>%
  full_join(nuuk_plots_df)

# Test for correlation with TCwetness and amongst each other
cor(select(nuuk_plots_df, 
           tcws, 
           TCwet_new, 
           twi_90m, 
           kopecky_twi, 
           saga_twi, 
           ndvi,
           tri_gimp,
           tri_arctic_dem))

# Save data frame
write_csv(nuuk_plots_df, "data/processed/nuuk_env_cover_plots_topo_variables.csv")

## Compare ArcticDEM and GIMP Measures 30m ----
# Load ArcticDEM at 2 m res 
ArcticDEM <-  raster("D:/Jakob/ArcticDEM/nathalie_nuuk/2m/nuuk_fjord_ArcticDEM_mosaic_2m.vrt")

# Extract elevations
nuuk_plots$ArcticDEM <- raster::extract(ArcticDEM, 
                                       as_Spatial(st_buffer(nuuk_plots, 5)),
                                       weights = T, fun = mean)
nuuk_plots$GIMP_dem <- raster::extract(GIMP_dem_cropped, 
                                        as_Spatial(st_buffer(nuuk_plots, 5)),
                                        weights = T, fun = mean)

# Graph Differences (scatter)
GIMPdemvsArcticDEMScatter <- ggplot(nuuk_plots, aes(x = GIMP_dem, y = ArcticDEM)) + 
  ggtitle("Plot Elevation GIMPdem vs ArcticDEM (m)") +
  geom_point() + 
  theme_cowplot()
save_plot("figures/dem_comparison/GIMPdemvsArcticDEM_pre_plot_scatter.png", 
          GIMPdemvsArcticDEMScatter)

# Graph Differences (histogram)
DEM_diff_histogram <- ggplot(nuuk_plots, aes(x = ArcticDEM - GIMP_dem)) + 
  ggtitle("Diff. Plot Elevation ArcticDEM - GIMPdem (m)") +
  geom_histogram() +
  theme_cowplot()
save_plot("figures/dem_comparison/ArcticDEMvsGIMPdem_diff_per_plot_hist.png", 
          DEM_diff_histogram)

# Resample the ArcticDEM to GIMP DEM 30 m resolution
ArcticDEM_resampled <- resample(ArcticDEM, GIMP_dem_cropped)
writeRaster(ArcticDEM_resampled, "D:/Jakob/ArcticDEM/jonathan_wet/GIMP_MEaSUREs_30m/dem/ArcticDEM_30m_resampled.tif")

# Compute difference
diff_DEM <- ArcticDEM_resampled - GIMP_dem_cropped 

# Load site coordinates
nuuk_sites <- read.csv("data/processed/nuuk_env_cover_plots.csv",
                       stringsAsFactors = F) %>%
  distinct(site, lat, long) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs(projection_temp)) %>% 
  mutate(site = paste0("Site ", site)) %>%
  group_by(site) %>%
  summarise() %>%
  st_centroid() %>%
  as_Spatial()

# Plot difference map
set.seed(10)
difference_plot <- levelplot(diff_DEM, margin = F, scales = list(draw = F),
          main = "Difference ArcticDEM - GimpDEM at 30 m (in m)") + 
  latticeExtra::layer(sp.points(nuuk_sites,
                                pch = 21,
                                cex = 1,
                                lwd = 3,
                                col = "#F7DB20")) +
  latticeExtra::layer(sp.pointLabel(nuuk_sites,
                                    label = nuuk_sites$site,
                                    cex = 1,
                                    col = "#F7DB20")) 
png("figures/dem_comparison/ArcticDEMvsGIMPdem30m.png", 
    width = 8,
    height = 8 * 0.82,
    units = "in",
    res = 300)
print(difference_plot)
dev.off()

