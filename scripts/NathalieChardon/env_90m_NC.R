## N. Chardon
## Start: 15 April 2020
## Aims:
## 1. Add additional environmental data to ts_plot.R
## 2. Correct erroneous inclin_down values for P338-P342


rm(list=ls()) #clear environment
showTmpFiles() #show temporary files
removeTmpFiles(h = 0) #clear all temporary files


## LIBRARIES ##
library(raster)
library(rgdal)


## WORKING DIRECTORIES ##

# Environmental data input
topo <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/topographic/'

# Output rasters
out.ras <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/processed/'

# Plot-level Rdata  
my.dat <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/scripts/RData/'

# Figures
figs <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/figs/stats_Apr2020/'


## DATA INPUT ##

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
# individual environmental rasters loaded separately below




####################################################################################################

# # SAGA WETNESS INDEX FROM 2 M DEM # # 

####################################################################################################

# Load data
setwd(topo)
twi <- raster('SAGA_TWI_nuuk_ArcticDEM.tif')
twi
summary(twi) #TWI range should be = (1, 10)
plot(twi)


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat 
ts_plot$twi <- extract(twi, gps)
summary(ts_plot$twi) 
ts_plot[which(ts_plot$twi < 1),]$id #Inacccurate values (6 plots at site 4) reflect holes in Arctic DEM 


# Check data distribution
par(mfrow = c(3,1))
hist(ts_plot$twi, breaks = 100)
boxplot(ts_plot$twi, horizontal = T) 
dotchart(ts_plot$twi, labels = '') 


# Correlations with other variables
setwd(figs)
foo <- ts_plot[which(ts_plot$twi >= 1),] #remove incorrect values

pdf('twi_env-params.pdf')
par(mfrow = c(3,2), cex.lab = 1.3, cex.axis = 1.3)

plot(twi ~ inclin_dir, data = foo, xlab = 'Aspect (º)', ylab = 'TWI',
     main = paste('cor =', round(cor(foo$twi, foo$inclin_dir), 2)))
plot(twi ~ tempjja_ts_30, data = foo, xlab = 'Mean Summer Temperature (Cº)', ylab = 'TWI',
     main = paste('cor =', round(cor(foo$twi, foo$tempjja_ts_30), 2)))

plot(twi ~ inclin_down, data = foo[which(foo$inclin_down < 90),], xlab = 'Slope (º)', ylab = 'TWI',
     main = paste('cor =', round(cor(foo[which(foo$inclin_down < 90),]$twi, foo[which(foo$inclin_down < 90),]$inclin_dir), 2)))
plot(twi ~ precipjja_ts_30, data = foo, xlab = 'Mean Summer Cumulative Precipitation (mm)', ylab = 'TWI',
     main = paste('cor =', round(cor(foo$twi, foo$precipjja_ts_30), 2)))

plot(twi ~ alt, data = foo, xlab = 'Elevation (m)', ylab = 'TWI',
     main = paste('cor =', round(cor(foo$twi, foo$alt), 2)))
plot(twi ~ insoljja_ts_30, data = foo, xlab = 'Mean Summer Insolation', ylab = 'TWI',
     main = paste('cor =', round(cor(foo$twi, foo$insoljja_ts_30), 2)))

dev.off()




####################################################################################################

# # SAGA WETNESS INDEX FROM 90 M DEM # # 

####################################################################################################

# Load data
setwd(topo)
twi <- raster('SAGA_TWI_nuuk_gimpdem.tif')
twi
summary(twi) #TWI range should be = (1, 10)
plot(twi)


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat 
ts_plot$twi_90m <- extract(twi, gps)
summary(ts_plot$twi_90m) 


# Check data distribution
par(mfrow = c(2,2))
hist(ts_plot$twi_90m, breaks = 100)
boxplot(ts_plot$twi_90m, horizontal = T) 
dotchart(ts_plot$twi_90m, labels = '') 
plot(twi_90m ~ twi, data = ts_plot, main = paste('cor =', round(cor(ts_plot$twi_90m, ts_plot$twi), 2)))




####################################################################################################

# # TASSELED CAP WETNESS FROM SENTINEL AND LANDSAT IMAGES # # 

####################################################################################################

# Load data
setwd(topo)
tcws <- raster('sentinelTCwet_nuuk.tif')
tcwl <- raster('landsatTCwet_nuuk.tif')

tcws
summary(tcws)
plot(tcws)

tcwl
summary(tcwl)

setwd(figs)
pdf('Tasseled_Cap.pdf')
plot(tcwl, box = F, axes = F)
dev.off()


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat 
ts_plot$tcws <- extract(tcws, gps)
ts_plot$tcwl <- extract(tcwl, gps)
summary(ts_plot$tcws) 
summary(ts_plot$tcwl) 


# Check data distribution
par(mfrow = c(3,2))
hist(ts_plot$tcws, breaks = 100)
hist(ts_plot$tcwl, breaks = 100)
dotchart(ts_plot$tcws, labels = '') 
dotchart(ts_plot$tcwl, labels = '') 
plot(tcws ~ ndwi, data = ts_plot, main = paste('cor =', round(cor(ts_plot$tcws, ts_plot$ndwi), 2)))
plot(tcwl ~ ndwi, data = ts_plot, main = paste('cor =', round(cor(ts_plot$tcwl, ts_plot$ndwi), 2)))


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # SPECTRAL DATA # # 

####################################################################################################

# Load data
setwd(topo)
ndwi <- raster('NDWI_nuuk.tif')
ndwi
summary(ndwi) #NDWI range should be = (-1, 1)
plot(ndwi, box = F, axes = F)

ndvi <- raster('NDVI_nuuk.tif')
ndvi
summary(ndvi)
plot(ndvi)

nir <- raster('sentinelB8Anir_nuuk.tif')
nir
summary(nir)
plot(nir)

swir <- raster('sentinelB11swir_nuuk.tif')
swir
summary(swir)
plot(swir, box = F, axes = F)


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat 
ts_plot$ndwi <- extract(ndwi, gps)
summary(ts_plot$ndwi) 
ts_plot$ndvi <- extract(ndvi, gps)
summary(ts_plot$ndvi) 
ts_plot$nir <- extract(nir, gps)
summary(ts_plot$nir) 
ts_plot$swir <- extract(swir, gps)
summary(ts_plot$swir) 


# Check data distribution
par(mfrow = c(4,3))
hist(ts_plot$ndwi, breaks = 100)
boxplot(ts_plot$ndwi, horizontal = T) 
dotchart(ts_plot$ndwi, labels = '') 

hist(ts_plot$ndvi, breaks = 100)
boxplot(ts_plot$ndvi, horizontal = T) 
dotchart(ts_plot$ndvi, labels = '') 

hist(ts_plot$nir, breaks = 100)
boxplot(ts_plot$nir, horizontal = T) 
dotchart(ts_plot$nir, labels = '') 

hist(ts_plot$swir, breaks = 100)
boxplot(ts_plot$swir, horizontal = T) 
dotchart(ts_plot$swir, labels = '') 


# Correlations with other variables
cor(ts_plot[c('nir', 'ndwi', 'ndvi', 'swir')])
    
setwd(figs)

pdf('ndwi_env-params.pdf')
par(mfrow = c(3,2), cex.lab = 1.3, cex.axis = 1.3)

plot(ndwi ~ inclin_dir, data = ts_plot, xlab = 'Aspect (º)', ylab = 'NDWI',
     main = paste('cor =', round(cor(ts_plot$ndwi, ts_plot$inclin_dir), 2)))
plot(ndwi ~ tempjja_ts_30, data = ts_plot, xlab = 'Mean Summer Temperature (Cº)', ylab = 'NDWI',
     main = paste('cor =', round(cor(ts_plot$ndwi, ts_plot$tempjja_ts_30), 2)))

plot(ndwi ~ inclin_down, data = ts_plot[which(ts_plot$inclin_down < 90),], xlab = 'Slope (º)', ylab = 'NDWI',
     main = paste('cor =', round(cor(ts_plot[which(ts_plot$inclin_down < 90),]$ndwi, ts_plot[which(ts_plot$inclin_down < 90),]$inclin_dir), 2)))
plot(ndwi ~ precipjja_ts_30, data = ts_plot, xlab = 'Mean Summer Cumulative Precipitation (mm)', ylab = 'NDWI',
     main = paste('cor =', round(cor(ts_plot$ndwi, ts_plot$precipjja_ts_30), 2)))

plot(ndwi ~ alt, data = ts_plot, xlab = 'Elevation (m)', ylab = 'NDWI',
     main = paste('cor =', round(cor(ts_plot$ndwi, ts_plot$alt), 2)))
plot(ndwi ~ insoljja_ts_30, data = ts_plot, xlab = 'Mean Summer Insolation', ylab = 'NDWI',
     main = paste('cor =', round(cor(ts_plot$ndwi, ts_plot$insoljja_ts_30), 2)))

dev.off()


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # SLOPE AND ASPECT FROM 90 M DEM # # 

####################################################################################################

# Load data
setwd(topo)
slope <- raster('Slope_nuuk.tif')
slope
summary(slope) 
par(mfrow = c(1, 2))
plot(slope)

aspect <- raster('Aspect_nuuk.tif')
aspect
summary(aspect) 
plot(aspect)


# Extract data from rasters
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat 
ts_plot$slope_90m <- extract(slope, gps)
summary(ts_plot$slope_90m) 

ts_plot$asp_90m <- extract(aspect, gps)
summary(ts_plot$asp_90m) 


# Check data distribution
par(mfrow = c(3,2))
hist(ts_plot$slope_90m, breaks = 100)
hist(ts_plot$asp_90m, breaks = 100)
boxplot(ts_plot$slope_90m, horizontal = T) 
boxplot(ts_plot$asp_90m, horizontal = T) 
dotchart(ts_plot$slope_90m, labels = '') 
dotchart(ts_plot$asp_90m, labels = '') 




####################################################################################################

# # ANNUAL SOLAR INSOLATION # # 

####################################################################################################

# Outlier values for inclin_down in P338-P342 corrected with raw data from JNN (17 April 2020)
# (Values had been switched between inclin_down and inclin_dir)
ts_plot[which(ts_plot$plot == 'P338'),]$inclin_dir <- 235
ts_plot[which(ts_plot$plot == 'P338'),]$inclin_down <- 5
ts_plot[which(ts_plot$plot == 'P339'),]$inclin_dir <- 277
ts_plot[which(ts_plot$plot == 'P339'),]$inclin_down <- 40
ts_plot[which(ts_plot$plot == 'P340'),]$inclin_dir <- 266
ts_plot[which(ts_plot$plot == 'P340'),]$inclin_down <- 12
ts_plot[which(ts_plot$plot == 'P341'),]$inclin_dir <- 268
ts_plot[which(ts_plot$plot == 'P341'),]$inclin_down <- 8
ts_plot[which(ts_plot$plot == 'P342'),]$inclin_dir <- 260
ts_plot[which(ts_plot$plot == 'P342'),]$inclin_down <- 8

hist(ts_plot$inclin_down, breaks = 100)
hist(ts_plot$inclin_dir, breaks = 100)


# Function for approximation of mean annual solar insolation from 
# McCune and Keon 2002 J. Veg. Sci. 13: 603-606, used in Chardon et al. 2015
load('~/Desktop/Research/Pinus coulteri/R files/insolation.Rdata')


# Calculation
for (i in 1:nrow(ts_plot)) {
  # field data
  ts_plot$field_rad[i] <- insolation(ts_plot$lat[i], ts_plot$inclin_down[i], ts_plot$inclin_dir[i])
 
  # DEM data
  ts_plot$dem_rad[i] <- insolation(ts_plot$lat[i], ts_plot$slope_90m[i], ts_plot$asp_90m[i])
}
summary(ts_plot$field_rad)
summary(ts_plot$dem_rad)


# Check data distribution
par(mfrow = c(3,2))
hist(ts_plot$field_rad, breaks = 100)
hist(ts_plot$dem_rad, breaks = 100)
dotchart(ts_plot$field_rad, labels = '') 
dotchart(ts_plot$dem_rad, labels = '') 
plot(dem_rad ~ field_rad, data = ts_plot, main = paste('cor =', round(cor(ts_plot$dem_rad, ts_plot$field_rad), 2)))
plot(insoljja_ts_30 ~ field_rad, data = ts_plot, main = paste('cor =', round(cor(ts_plot$insoljja_ts_30, ts_plot$field_rad), 2)))


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')



####################################################################################################

# # SOLAR RADIATION INDEX # # 

####################################################################################################

# Code adapted from J. vonOppen (sri_calcuation.R in https://gitlab.com/nathaliechardon/gl_microclim)
# Based on Keating et al. 2007. Journal of Wildlife Management, 71:1344-1348

library(GeoLight) #zenith angles
library(insol) #declination

# Calculate solar time and declination
doy <- 196 #Julian day in middle of JJA
time <- as.POSIXct("2011-07-15 12:00:00 UTC") #POSIXct time
sun <- solar(time)

# Calculate solar zenith angle
zen <- zenith(sun, ts_plot$long, ts_plot$lat)

# Calculate declination
declin <- declination(doy)

# Radial angle of earth to sun at day of year (for calculation of ecventricity factor)
day_angle <- 2*pi*(doy-1)/365

# Calculate excentricity correction factor following Eq. 1.2.1 in Iqbal 1983
# https://doi.org/10.1016/B978-0-12-373750-2.50006-9
# https://www.sciencedirect.com/topics/physics-and-astronomy/elliptical-orbits
E0 <- 1.000110 + 0.034221 * cos(day_angle) + 0.00128 * sin(day_angle) + 0.000719 * 
  cos(2 * day_angle) + 0.000077 * sin(2 * day_angle) 

# calculate SRI (here creating sri column in tms_meta dataframe, adjust as needed)
sinDeg <- function(angle) sin(angle*pi/180)
cosDeg <- function(angle) cos(angle*pi/180)

for (i in 1:nrow(ts_plot)) {
  ts_plot$sri[i] <- E0 * 
    ((sinDeg(ts_plot$lat[i]) * cosDeg(ts_plot$inclin_down[i]) - cosDeg(ts_plot$lat[i]) *
        sinDeg(ts_plot$inclin_down[i]) * cosDeg(180 - ts_plot$inclin_dir[i] - declin)) *
       sinDeg(zen[i]) + 
       (cosDeg(ts_plot$lat[i]) * cosDeg(ts_plot$inclin_down[i]) + sinDeg(ts_plot$lat[i]) *
          sinDeg(ts_plot$inclin_down[i]) * cosDeg(180 - ts_plot$inclin_dir[i] - declin)) *
       (cosDeg(zen[i]) * cosDeg(0)) +
       cosDeg(zen[i]) * sinDeg(ts_plot$inclin_down[i]) *
       sinDeg(180 - ts_plot$inclin_dir[i] - declin) * sinDeg(0))
}

# Check data distribution
summary(ts_plot$sri)

par(mfrow = c(3, 2))
hist(ts_plot$sri, breaks = 100)
dotchart(ts_plot$sri, labels = '') 
plot(sri ~ field_rad, data = ts_plot, main = paste('cor =', round(cor(ts_plot$sri, ts_plot$field_rad), 2)))
plot(sri ~ insoljja_ts_30, data = ts_plot, main = paste('cor =', round(cor(ts_plot$sri, ts_plot$insoljja_ts_30), 2)))
plot(sri ~ inclin_down, data = ts_plot)
plot(sri ~ inclin_dir, data = ts_plot)


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # COMPETITION # # 

####################################################################################################

# Load data
setwd(my.dat)
load('sp_plot.RData')
load('ts_plot.RData')

# Most abundant shrubs: 
# Salix glauca, Betula nana, Empetrum nigrum, Vaccinium uliginosum, Rhododendron groenlandicum
par(mfrow = c(3,3))
hist(sp_plot[which(sp_plot$taxon == 'Salix glauca'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Betula nana'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Cassiope tetragona'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Empetrum nigrum'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Rhododendron groenlandicum'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Rhododendron tomentosum'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Phyllodoce caerulea'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Salix arctophila'),]$pins, breaks = 25)
hist(sp_plot[which(sp_plot$taxon == 'Vaccinium uliginosum'),]$pins, breaks = 25)

# % of 0s in pins data per species
nrow(sp_plot[which(sp_plot$taxon == 'Salix glauca' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Salix glauca'),])
nrow(sp_plot[which(sp_plot$taxon == 'Betula nana' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Betula nana'),])
nrow(sp_plot[which(sp_plot$taxon == 'Empetrum nigrum' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Empetrum nigrum'),])
nrow(sp_plot[which(sp_plot$taxon == 'Vaccinium uliginosum' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Vaccinium uliginosum'),])
nrow(sp_plot[which(sp_plot$taxon == 'Rhododendron groenlandicum' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Rhododendron groenlandicum'),])
nrow(sp_plot[which(sp_plot$taxon == 'Cassiope tetragona' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Cassiope tetragona'),])
nrow(sp_plot[which(sp_plot$taxon == 'Rhododendron tomentosum' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Rhododendron tomentosum'),])
nrow(sp_plot[which(sp_plot$taxon == 'Phyllodoce caerulea' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Phyllodoce caerulea'),])
nrow(sp_plot[which(sp_plot$taxon == 'Salix arctophila' & sp_plot$pins == 0),]) / 
  nrow(sp_plot[which(sp_plot$taxon == 'Salix arctophila'),])


# # Biotic competition: % cover of non-focal shrubs # #

# Salix glauca
foo <- sp_plot[!sp_plot$taxon == 'Salix glauca',] #DF for all non-Salix species
pp <- data.frame(plot = unique(foo$plot)) #DF with list of plots
pp$comp <- NA #initialize new column for competition values

for (i in 1:nrow(pp)) { #loop through each plot
  boo <- foo[foo$plot == pp$plot[i],] #DF for plot_i
  pp$comp[i] <- sum(boo$pins) #sum of pins for all species in plot_i
}

pp2ts <- match(ts_plot$plot, pp$plot) #match to ts_plot & transfer competition values
ts_plot$comp_sagl <- pp$comp[pp2ts]

# Betula nana
foo <- sp_plot[!sp_plot$taxon == 'Betula nana',] 
pp <- data.frame(plot = unique(foo$plot)) 
pp$comp <- NA 
for (i in 1:nrow(pp)) { 
  boo <- foo[foo$plot == pp$plot[i],] 
  pp$comp[i] <- sum(boo$pins) 
}
pp2ts <- match(ts_plot$plot, pp$plot)
ts_plot$comp_bena <- pp$comp[pp2ts]

# Empetrum nigrum
foo <- sp_plot[!sp_plot$taxon == 'Empetrum nigrum',] 
pp <- data.frame(plot = unique(foo$plot)) 
pp$comp <- NA 
for (i in 1:nrow(pp)) { 
  boo <- foo[foo$plot == pp$plot[i],] 
  pp$comp[i] <- sum(boo$pins) 
}
pp2ts <- match(ts_plot$plot, pp$plot)
ts_plot$comp_emni <- pp$comp[pp2ts]

# Vaccinium uliginosum
foo <- sp_plot[!sp_plot$taxon == 'Vaccinium uliginosum',] 
pp <- data.frame(plot = unique(foo$plot)) 
pp$comp <- NA 
for (i in 1:nrow(pp)) { 
  boo <- foo[foo$plot == pp$plot[i],] 
  pp$comp[i] <- sum(boo$pins) 
}
pp2ts <- match(ts_plot$plot, pp$plot)
ts_plot$comp_vaul <- pp$comp[pp2ts]

# Phyllodoce caerulea
foo <- sp_plot[!sp_plot$taxon == 'Phyllodoce caerulea',] 
pp <- data.frame(plot = unique(foo$plot)) 
pp$comp <- NA 
for (i in 1:nrow(pp)) { 
  boo <- foo[foo$plot == pp$plot[i],] 
  pp$comp[i] <- sum(boo$pins) 
}
pp2ts <- match(ts_plot$plot, pp$plot)
ts_plot$comp_phca <- pp$comp[pp2ts]


# Check data distribution
dat <- ts_plot[c('comp_sagl', 'comp_bena', 'comp_emni', 'comp_vaul', 'comp_phca')]
summary(dat)
par(mfrow = c(5,2))
for (i in 1:ncol(dat)) {
  hist(dat[,i], breaks = 40)
  dotchart(dat[,i])
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




