## N. Chardon
## Start: 15 April 2020



rm(list=ls()) #clear environment
showTmpFiles() #show temporary files
removeTmpFiles(h = 0) #clear all temporary files


## LIBRARIES ##
library(raster)
library(rgdal)


## WORKING DIRECTORIES ##

  # (removed, containing sensible data - JvO)


## DATA INPUT ##

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
# individual environmental rasters loaded separately below




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




# end of script ----
