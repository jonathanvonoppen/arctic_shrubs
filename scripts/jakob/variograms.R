# Variograms for predictor rasters 
# Jakob Assmann j.assmann@bio.au.dk 16 September 2020

# Dependencies
library(raster)
library(gstat)
library(parallel)
library(cowplot)
library(ggplot2)
library(sf)
library(dplyr)

# 
# JJA (June, July, August) 
# insolation ('insol.asc')
# JJA precip ('jjaprecip')
# MAM (March, April, May) precip ('mamprecip.asc')
# JJA maximum temps ('tempjja.asc')
# yearly max temp ('tempmax.asc')
# yearly min temp ('tempmin.asc')
# temperature continentality (diff. between yearly max and min temps) ('tempcont.asc')
# 
# These are located in the following folder:
#   'Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/pred_rasters/'

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

# add TRI and TCW to raster to list
raster_list <- append(
  raster_list,
  setNames(
    raster("D:/Jakob/ArcticDEM/jonathan/nuuk_fjord_tri_mosaic.vrt"),
    "tri"))
raster_list <- append(
  raster_list,
  setNames(raster("O:/Nat_Ecoinformatics/C_Write/_Proj/Greenland_NormandVegDyn_au150176/NuukFjord/spatial_data_for_Nathalie_by_Jakob/nathalie_90m_grid_polar_stereo/landsatTCwet_nuuk.tif"),
           "tcws"))

### Define functions to calculate and fit variograms ----

# Define function to calculuate a variogram 
sample_variogram <- function(predictor_raster, thin = 10, bin_width = 90) { 

  # Convert raster to spdf
  predictor_spdf <- as(predictor_raster, "SpatialPixelsDataFrame" ) 
  
  # Square out spdf (needed due to a bug in gstat)
  predictor_spdf@grid@cellsize[1] <- as.numeric(formatC(predictor_spdf@grid@cellsize[1], 
                                             format = "d"))
  predictor_spdf@grid@cellsize[2] <- as.numeric(formatC(predictor_spdf@grid@cellsize[2], 
                                                        format = "d"))
  # Set variogram forumla 
  vario_forumla <- as.formula(paste0(names(predictor_raster),
                                           " ~ 1"))
  # Sample the variogram (this can take ages)
  vario <- variogram(vario_forumla, 
                     predictor_spdf[sample(nrow(predictor_spdf) / thin),],
                     width = bin_width,
                     verbose = T) 
  
  # Change id colum
  vario$id <- names(predictor_raster)
  
  # clean memory
  gc()
  
  # Return variogram
  return(vario)
}

# Prep parallel envrionment
cl <- makeCluster(8)
clusterEvalQ(cl, {
  library(gstat)
  library(raster)
  })

# Sample variograms
vario_list <- parLapply(cl, raster_list, sample_variogram)
save(vario_list, file = "scripts/jakob/vario_list.Rda")
stopCluster(cl)

# Look up table for pretty names
lookup_table <- data.frame(
  raster_names = unlist(lapply(raster_list, names)),
  pretty_names = c("Insolation",
                   "Mean Precipitation June-July-August (mm)",
                   "Mean Precipitation March-April-May (mm)",
                   "Mean Temperature June-July-August (°C)",
                   "Annual Maximum Temperature (°C)",
                   "Annual Minimum Temperature (°C)",
                   "Temperature Continentality",
                   "Landsat Tasseled Cap Wetness (90 m)"),
  stringsAsFactors = F)

# Plot Variograms
plot_variogram <- function(vario){
  vario_plot <- ggplot(
    vario, 
    aes(x = dist / 1000, y = gamma)) + 
    geom_point() +
    labs(x = "Distance (km)", 
         y = "Semivariance",
         title = lookup_table$pretty_names[lookup_table$raster_names == unique(vario$id)]) +
    scale_x_continuous(limits = c(0,40),
                       breaks = seq(0,40,5)) +
    theme_cowplot(15)
  save_plot(paste0("figures/variograms/", unique(unique(vario$id)), ".png"),
            vario_plot,
            base_aspect_ratio = 1.3,
            base_height = 5)
}
lapply(vario_list, plot_variogram)

## Variograms for SRI (a non-raster variable)
nuuk_plots <- read.csv("data/nuuk_env_cover_plots.csv",
                       stringsAsFactors = F) %>%
  distinct(plot, lat, long, sri) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs(projection_temp)) %>%
  as_Spatial()

sri_vario <- variogram(sri ~ 1, nuuk_plots,
          width = 90)

sri_vario_plot <- ggplot(
  sri_vario, 
  aes(x = dist / 1000, y = gamma)) + 
  geom_point() +
  labs(x = "Distance (km)", 
       y = "Semivariance",
       title = "Solar Radiation") +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  theme_cowplot(15)

save_plot("figures/variograms/sri.png",
          sri_vario_plot,
          base_aspect_ratio = 1.3,
          base_height = 5)
