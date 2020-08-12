## N. Chardon
## Start: 10 Feb 2020
## Aims:
## 1. Process downscaled climate data (received 7.1.2020) for long-term trends over time series (TS)
## 2. Match ts clim data to sampling years

rm(list=ls()) #clear environment
showTmpFiles() #show temporary files
removeTmpFiles(h = 0) #clear all temporary files


## LIBRARIES ##
library(raster)
library(rgdal)
library(snow) #parallelization


## WORKING DIRECTORIES ##

# Climate data input
temp <- '/Volumes/Nat_Ecoinformatics/B_Read/Arctic/Microclimate/NuukFjord/FINAL_LST_TS_PAST/'
precip <- '/Volumes/Nat_Ecoinformatics/B_Read/Arctic/Microclimate/NuukFjord/FINAL_Precipitation_TS_PAST/'
ins <- '/Volumes/ST_Ecoinformatics/B_Read/Arctic/Microclimate/NuukFjord/FINAL_SolarRadiation/'

# DEM in Stere proj
output <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/maps/nuuk_dem/'

# Output rasters
out.ras <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/input_data/processed/'

# Plot-level Rdata  
my.dat <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/scripts/RData/'

# Local directory independent of AU server
loc <- '/Volumes/MILKBONE/Greenland/processed_data/'

# Figures
figs <- '/Volumes/Nat_Ecoinformatics/C_Write/_User/NathalieChardon_au653181/figs/stats_Apr2020/'



## INITAL DATA INPUT ##

# setwd(my.dat) 
# load('env_plot.RData') #field data by plot (plot_data.R; NC)
# # downscaled climate data input loaded separately in each section below


## DATA OUTPUT ##

setwd(out.ras)
dem_crop <- raster('dem_crop.tif') #main file for DEM raster (ts_90m.R; NC)
main_ras <- raster('main_ras.tif') #main raster to project climate rasters to (ts_90m.R; NC)

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
load('ts_pattern.RData') #reference DF used in time series computation (ts_90m.R; NC)

# Intermediate DFs that store extracted raster data
load('vals_temp_jja.RData') #summer temps (ts_90m.R; NC)
load('vals_temp_jfma.RData') #winter temps (ts_90m.R; NC)
load('vals_precip_jja.RData') #summer precips (ts_90m.R; NC)
load('vals_precip_jfmam.RData') #winter precips (ts_90m.R; NC)
load('vals_insol_jja.RData') #summer insolation (ts_90m.R; NC)




####################################################################################################

# # DEFINE RASTER TO USE IN ALL RASTER REPROJECTIONS # # 

####################################################################################################

# DEM raster needed to project TO
setwd(output)
dem_stere <- raster('dem_stere.tif') #nuuk_clim.R


# Check different raster data to ID extent to crop main raster to
setwd(temp)
ex <- raster('LST_MAX_Greenland_Mercator_resolution90_1979_01.tif')
ex_stere <- projectRaster(from = ex, to = dem_stere)
plot(ex_stere)
click() #ID x_max for map

setwd(precip)
ex <- raster('PREC_Greenland_ETRS89_resolution90_1979_01_regression.tif')
ex_stere <- projectRaster(from = ex, to = dem_stere)
plot(ex_stere)

setwd(ins)
ex <- raster('SolarRadiation_Greenland_Mercator_resolution90_01_1979.tif')
ex_stere <- projectRaster(from = ex, to = dem_stere)
plot(ex_stere)


# Crop DEM
base <- data.frame(longitude = c(extent(dem_stere)[1], -234600), #avoid cutoff area in temp
                   latitude = c(extent(dem_stere)[3], extent(dem_stere)[4]))

spdf <- SpatialPointsDataFrame(base[ c('longitude', 'latitude') ], data = base, #SPDF w/ extent
                               proj4string = CRS(paste(crs(dem_stere))))

e <- extent(spdf) #spatial extent to crop to

dem_crop <- crop(dem_stere, e, snap = 'out')

dem_crop
plot(dem_crop)


# Make empty template raster for reprojections
main_ras <- projectExtent(dem_crop, crs(dem_crop)) #create empty raster to project to


# Save rasters
setwd(out.ras)
# setwd(loc)
writeRaster(dem_crop, filename = 'dem_crop.tif', overwrite = T)
writeRaster(main_ras, filename = 'main_ras.tif') #save for future use




####################################################################################################

# # SUMMER TEMPERATURE: DATA # # 

####################################################################################################

# Specify multiple conditions to create file list
ts <- paste(c(1982:2013), collapse = '|') #years of interest

temps <- Reduce(intersect, list(list.files(temp, pattern = 'MAX'),
                                list.files(temp, pattern = '_06|_07|_08'),
                                list.files(temp, pattern = ts)))


# Read in data for this section 
setwd(temp)
t_stack <- stack(lapply(temps, raster)) #summer temp rasters as stack
t_stack
nlayers(t_stack)

setwd(my.dat) 
load('env_plot.RData') #field data by plot (plot_data.R; NC)
setwd(out.ras)
main_ras <- raster('main_ras.tif') #main raster to project climate rasters to (ts_90m.R; NC)


# Change R temporary file location to local drive in case of AU server connection disruption
rasterOptions(tmpdir = loc)


# Parallel computing to reproject Stack to template raster
start_time <- Sys.time()
beginCluster()

temps_ts <- projectRaster(from = t_stack, to = main_ras)

endCluster()
end_time <- Sys.time()
end_time - start_time #2.7 hrs


# Extract data from raster
gps <- as.matrix(env_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat to use for clim data extraction
vals <- data.frame(id = env_plot$id, year = env_plot$year, extract(temps_ts, gps)) #extract clim values for each raster layer to new DF


# Check data distribution
colMax <- function(data) sapply(data, max, na.rm = TRUE) #max value per column
mx <- colMax(vals[3:ncol(vals)])

colMin <- function(data) sapply(data, min, na.rm = TRUE) #min value per column
mn <- colMin(vals[3:ncol(vals)])

par(mfrow = c(4,2), mar = c(5,4,2,1))
hist(mx, breaks = 100)
hist(mn, breaks = 100)
boxplot(mx, horizontal = T) #following Zuur et al. 2009
boxplot(mn, horizontal = T)
dotchart(mx, labels = '') 
dotchart(mn, labels = '')

hist(vals[,3], breaks = nrow(vals), xlim = c(min(mn), max(mx)), ylim = c(0, 15), 
     main = 'All Values', xlab = '')
for (i in ncol(vals)) {
  hist(vals[,i], breaks = nrow(vals), add = T)
}


# Save all extracted values
setwd(my.dat) 
save(vals, file = 'vals_temp_jja.RData')




####################################################################################################

# # SUMMER TEMPERATURE: COMPUTE MEAN OF YEARLY JJA MAXS & MEAN OF YEARLY MAX # # 

####################################################################################################

# Load data for this section
setwd(my.dat) 
load('vals_temp_jja.RData')
load('env_plot.RData') #environmental data by plot (plot_data.R; NC)
ts_plot <- env_plot #create new DF to store time series data


# Create dataframe to reference in loop
ts_pattern <- data.frame(ts = rep(c(4, 9, 14, 19, 29), 3), #time series - 1
                         yr = c(rep(2011, 5), rep(2012, 5), rep(2013, 5))) #sampling years

ts_pattern$patt <- 'NA'
for (i in 1:nrow(ts_pattern)) {
  ts_pattern$patt[i] <- paste((ts_pattern$yr[i] - ts_pattern$ts[i]) : ts_pattern$yr[i], collapse = '|')
}
head(ts_pattern)
save(ts_pattern, file = 'ts_pattern.RData')


## Calculate yearly summer means and means of yearly max ##

for (j in 1:nrow(ts_pattern)) {
  
  df <- 'NA' #initialize intermediary DF to dump values into
  
  # ID and subset appropriate columns
  for (i in 3:ncol(vals)) { #loop through columns = clim data from diff rasters
    
    nn <- grep(ts_pattern[j, 3], colnames(vals[i]), value = T) #extract colname_i from ts_j
    cc <- vals[which(colnames(vals) == nn)] #extract corresponding values
    df <- cbind(df, cc) #dump into new DF
  }
  
  df2 <- df[-1] #delete first column with NAs
  
  # ID yearly max
  index <- gl(ncol(df2)/3, 3) #generate factor levels to loop through JJA of each year
  mm <- as.data.frame(t(apply(df2, 1, tapply, index, max))) #maximum temp per JJA series per plot
  
  for (h in 1:nrow(ts_plot)) { #loop through rows = plots
    if (ts_plot$year[h] == ts_pattern[j, 2]) { #if sampling year matches current ts
      
      # Calculate JJA avgs per row_h and add to main DF
      ts_plot[h, paste('tempjja_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- rowMeans(df2[h,])
      
      # Calculate max avgs per row_h and add to main DF
      ts_plot[h, paste('tempmax_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- rowMeans(mm[h,])
    }
  }
}
head(ts_plot)


# Check data distribution
nstart <- 14 #first column with new clim data
cend <- 23 #last column with new clim data

par(mfrow = c(5, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i], xlim = c(6, 15))
}

    
# Save new main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # WINTER TEMPERATURE: DATA # #

####################################################################################################

# Specify multiple conditions to create file list
ts <- paste(c(1982:2013), collapse = '|') #years of interest

temps <- Reduce(intersect, list(list.files(temp, pattern = 'MIN'),
                                list.files(temp, pattern = '_01|_02|_03|_04'),
                                list.files(temp, pattern = ts)))


# Read in data for this section 
setwd(temp)
t_stack <- stack(lapply(temps, raster)) #winter temp rasters as stack
t_stack
nlayers(t_stack)

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
setwd(out.ras)
main_ras <- raster('main_ras.tif') #main raster to project climate rasters to (ts_90m.R; NC)


# Change R temporary file location to local drive in case of AU server connection disruption
rasterOptions(tmpdir = loc)


# Parallel computing to reproject Stack to template raster
start_time <- Sys.time()
beginCluster()

temps_ts <- projectRaster(from = t_stack, to = main_ras)

endCluster()
end_time <- Sys.time()
end_time - start_time #4.7 hrs


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat to use for clim data extraction
vals <- data.frame(id = ts_plot$id, year = ts_plot$year, extract(temps_ts, gps)) #extract clim values for each raster layer to new DF


# Check data distribution
colMax <- function(data) sapply(data, max, na.rm = TRUE) #max value per column
mx <- colMax(vals[3:ncol(vals)])

colMin <- function(data) sapply(data, min, na.rm = TRUE) #min value per column
mn <- colMin(vals[3:ncol(vals)])

par(mfrow = c(4,2), mar = c(5,4,2,1))
hist(mx, breaks = 100)
hist(mn, breaks = 100)
boxplot(mx, horizontal = T) #following Zuur et al. 2009
boxplot(mn, horizontal = T)
dotchart(mx, labels = '') 
dotchart(mn, labels = '')

hist(vals[,3], breaks = nrow(vals), xlim = c(min(mn), max(mx)), ylim = c(0, 15), 
     main = 'All Values', xlab = '')
for (i in ncol(vals)) {
  hist(vals[,i], breaks = nrow(vals), add = T)
}


# Save all extracted values
setwd(my.dat) 
save(vals, file = 'vals_temp_jfma.RData')




####################################################################################################

# # WINTER TEMPERATURE: COMPUTE MEAN OF YEARLY MINS # # 

####################################################################################################

# Load data for this section
setwd(my.dat) 
load('vals_temp_jfma.RData') #winter temps (ts_90m.R; NC)
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
load('ts_pattern.RData') #reference DF used in time series computation (ts_90m.R; NC)


## Calculate means of yearly min ##

for (j in 1:nrow(ts_pattern)) {
  
  df <- 'NA' #initialize intermediary DF to dump values into
  
  # ID and subset appropriate columns
  for (i in 3:ncol(vals)) { #loop through columns = clim data from diff rasters
    
    nn <- grep(ts_pattern[j, 3], colnames(vals[i]), value = T) #extract colname_i from ts_j
    cc <- vals[which(colnames(vals) == nn)] #extract corresponding values
    df <- cbind(df, cc) #dump into new DF
  }
  
  df2 <- df[-1] #delete first column with NAs
  
  # ID yearly min 
  index <- gl(ncol(df2)/4, 4) #generate factor levels to loop through JMFA of each year
  mm <- as.data.frame(t(apply(df2, 1, tapply, index, min))) #minimum temp per JMFA series per plot
  
  for (h in 1:nrow(ts_plot)) { #loop through rows = plots
    if (ts_plot$year[h] == ts_pattern[j, 2]) { #if sampling year matches current ts
      
      # Calculate min avgs per row_h and add to main DF
      ts_plot[h, paste('tempmin_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- rowMeans(mm[h,])
    }
  }
}
head(ts_plot)


# Check data distribution
nstart <- 24 #first column with new clim data
cend <- ncol(ts_plot) #last column with new clim data

par(mfrow = c(3, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i], xlim = c(-23, -11))
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # CONTINENTALITY # #

####################################################################################################

# Data for this section
setwd(my.dat)
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)


# Subset max and min temp columns
nn <- colnames(ts_plot)
mx <- ts_plot[,grep('max', nn)]
mn <- ts_plot[,grep('min', nn)]


## Calculate temperature continentality

ts_plot[, paste('tempcont_ts', '5', sep = '_')] <- mx[1] - mn[1]
ts_plot[, paste('tempcont_ts', '10', sep = '_')] <- mx[2] - mn[2]
ts_plot[, paste('tempcont_ts', '15', sep = '_')] <- mx[3] - mn[3]
ts_plot[, paste('tempcont_ts', '20', sep = '_')] <- mx[4] - mn[4]
ts_plot[, paste('tempcont_ts', '30', sep = '_')] <- mx[5] - mn[5]

head(ts_plot)


# Check data distribution
nstart <- 29 #first column with new clim data
cend <- ncol(ts_plot) #last column with new clim data

par(mfrow = c(3, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i],xlim = c(22.5, 32.5))
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # SUMMER PRECIP: DATA # # 

####################################################################################################

# Specify multiple conditions to create file list
ts <- paste(c(1982:2013), collapse = '|') #years of interest

precips <- Reduce(intersect, list(list.files(precip, pattern = 'regression.tif'),
                                  list.files(precip, pattern = '_06|_07|_08'),
                                  list.files(precip, pattern = ts)))


# Read in data for this section 
setwd(precip)
p_stack <- stack(lapply(precips, raster)) #JJA precipitation
p_stack
nlayers(p_stack)

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
setwd(out.ras)
main_ras <- raster('main_ras.tif') #main raster to project climate rasters to (ts_90m.R; NC)


# Change R temporary file location to local drive in case of AU server connection disruption
rasterOptions(tmpdir = loc)


# Parallel computing to reproject Stack to template raster
start_time <- Sys.time()
beginCluster()

precips_ts <- projectRaster(from = p_stack, to = main_ras)

endCluster()
end_time <- Sys.time()
end_time - start_time #2.7 hrs


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat to use for clim data extraction
vals <- data.frame(id = ts_plot$id, year = ts_plot$year, extract(precips_ts, gps)) #extract clim values for each raster layer to new DF


# Check data distribution
colMax <- function(data) sapply(data, max, na.rm = TRUE) #max value per column
mx <- colMax(vals[3:ncol(vals)])

colMin <- function(data) sapply(data, min, na.rm = TRUE) #min value per column
mn <- colMin(vals[3:ncol(vals)])

par(mfrow = c(4,2), mar = c(5,4,2,1))
hist(mx, breaks = 100)
hist(mn, breaks = 100)
boxplot(mx, horizontal = T) #following Zuur et al. 2009
boxplot(mn, horizontal = T)
dotchart(mx, labels = '') 
dotchart(mn, labels = '')

hist(vals[,3], breaks = nrow(vals), xlim = c(min(mn), max(mx)), 
     main = 'All Values', xlab = '')
for (i in ncol(vals)) {
  hist(vals[,i], breaks = nrow(vals), add = T)
}


# Save all extracted values
setwd(my.dat) 
save(vals, file = 'vals_precip_jfmam.RData')




####################################################################################################

# # SUMMER PRECIP: COMPUTE CUMULATIVE SUMMER MEAN # # 

####################################################################################################

# Load data for this section
setwd(my.dat) 
load('vals_precip_jja.RData') #summer precips (ts_90m.R; NC)
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
load('ts_pattern.RData') #reference DF used in time series computation (ts_90m.R; NC)


## Calculate average summer cumulative precip ##

for (j in 1:nrow(ts_pattern)) {
  
  df <- 'NA' #initialize intermediary DF to dump values into
  
  # ID and subset appropriate columns
  for (i in 3:ncol(vals)) { #loop through columns = clim data from diff rasters
    
    nn <- grep(ts_pattern[j, 3], colnames(vals[i]), value = T) #extract colname_i from ts_j
    cc <- vals[which(colnames(vals) == nn)] #extract corresponding values
    df <- cbind(df, cc) #dump into new DF
  }
  
  df2 <- df[-1] #delete first column with NAs
  
  for (h in 1:nrow(ts_plot)) { #loop through rows = plots
    if (ts_plot$year[h] == ts_pattern[j, 2]) { #if sampling year matches current ts
      
      # Calculate avg JJA cumulative per row_h and add to main DF
      ts_plot[h, paste('precipjja_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- 
        rowSums(df2[h,])/(ts_pattern$ts[j] + 1) #divide by years in time series
    }
  }
}
head(ts_plot)


# Check data distribution
nstart <- 34 #first column with new clim data
cend <- ncol(ts_plot) #last column with new clim data

par(mfrow = c(3, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i], xlim = c(130, 240))
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # WINTER PRECIP: DATA # # 

####################################################################################################

# Specify multiple conditions to create file list
ts <- paste(c(1982:2013), collapse = '|') #years of interest

precips <- Reduce(intersect, list(list.files(precip, pattern = 'regression.tif'),
                                  list.files(precip, pattern = '_01|_02|_03|_04|_05'),
                                  list.files(precip, pattern = ts)))


# Read in data for this section 
setwd(precip)
p_stack <- stack(lapply(precips, raster)) #JFMAM precipitation
p_stack
nlayers(p_stack)

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
setwd(out.ras)
main_ras <- raster('main_ras.tif') #main raster to project climate rasters to (ts_90m.R; NC)


# Change R temporary file location to local drive in case of AU server connection disruption
rasterOptions(tmpdir = loc)


# Parallel computing to reproject Stack to template raster
start_time <- Sys.time()
beginCluster()

precips_ts <- projectRaster(from = p_stack, to = main_ras)

endCluster()
end_time <- Sys.time()
end_time - start_time #6.8 hrs


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat to use for clim data extraction
vals <- data.frame(id = ts_plot$id, year = ts_plot$year, extract(precips_ts, gps)) #extract clim values for each raster layer to new DF


# Check data distribution
colMax <- function(data) sapply(data, max, na.rm = TRUE) #max value per column
mx <- colMax(vals[3:ncol(vals)])

colMin <- function(data) sapply(data, min, na.rm = TRUE) #min value per column
mn <- colMin(vals[3:ncol(vals)])

par(mfrow = c(4,2), mar = c(5,4,2,1))
hist(mx, breaks = 100)
hist(mn, breaks = 100)
boxplot(mx, horizontal = T) #following Zuur et al. 2009
boxplot(mn, horizontal = T)
dotchart(mx, labels = '') 
dotchart(mn, labels = '')

hist(vals[,3], breaks = nrow(vals), xlim = c(min(mn), max(mx)), ylim = c(0, 15), 
     main = 'All Values', xlab = '')
for (i in ncol(vals)) {
  hist(vals[,i], breaks = nrow(vals), add = T)
}

# Check outliers
outs <- names(which(mx > 200)) #index for columns with high precip values

setwd(figs)
pdf('precip_outliers.pdf')
par(mfrow = c(4, 2)) #plot full range of column values
for(i in 1:length(outs)) {
  dotchart(vals[, c(outs[i])], xlab = outs[i])
}
dev.off()


# Save all extracted values
setwd(my.dat) 
save(vals, file = 'vals_precip_jfmam.RData')





####################################################################################################

# # WINTER PRECIP: COMPUTE CUMULATIVE WINTER MEAN # # 

####################################################################################################

# Load data for this section
setwd(my.dat) 
load('vals_precip_jfmam.RData') #winter precips (ts_90m.R; NC)
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
load('ts_pattern.RData') #reference DF used in time series computation (ts_90m.R; NC)


## Calculate average winter cumulative precip ##

for (j in 1:nrow(ts_pattern)) {
  
  df <- 'NA' #initialize intermediary DF to dump values into
  
  # ID and subset appropriate columns
  for (i in 3:ncol(vals)) { #loop through columns = clim data from diff rasters
    
    nn <- grep(ts_pattern[j, 3], colnames(vals[i]), value = T) #extract colname_i from ts_j
    cc <- vals[which(colnames(vals) == nn)] #extract corresponding values
    df <- cbind(df, cc) #dump into new DF
  }
  
  df2 <- df[-1] #delete first column with NAs
  
  for (h in 1:nrow(ts_plot)) { #loop through rows = plots
    if (ts_plot$year[h] == ts_pattern[j, 2]) { #if sampling year matches current ts
      
      # Calculate avg JJA cumulative per row_h and add to main DF
      ts_plot[h, paste('precipjfmam_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- 
        rowSums(df2[h,], na.rm = T)/(ts_pattern$ts[j] + 1) #divide by years in time series
    }
  }
}
head(ts_plot)


# Check data distribution
nstart <- 54 #first column with new clim data
cend <- 58 #last column with new clim data

par(mfrow = c(3, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i], xlim = c(150, 400))
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # SPRING PRECIP: COMPUTE CUMULATIVE SPRING MEAN # # 

####################################################################################################

# Load data for this section
setwd(my.dat) 
load('vals_precip_jfmam.RData') #winter precips (ts_90m.R; NC)
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
load('ts_pattern.RData') #reference DF used in time series computation (ts_90m.R; NC)


## Calculate average spring cumulative precip ##
vals <- vals[, grep('_03_|_04_|_05_', colnames(vals))] #subset March, April, May data
  
for (j in 1:nrow(ts_pattern)) {
  
  df <- 'NA' #initialize intermediary DF to dump values into
  
  # ID and subset appropriate columns
  for (i in 3:ncol(vals)) { #loop through columns = clim data from diff rasters
    
    nn <- grep(ts_pattern[j, 3], colnames(vals[i]), value = T) #extract colname_i from ts_j
    cc <- vals[which(colnames(vals) == nn)] #extract corresponding values
    df <- cbind(df, cc) #dump into new DF
  }
  
  df2 <- df[-1] #delete first column with NAs
  
  for (h in 1:nrow(ts_plot)) { #loop through rows = plots
    if (ts_plot$year[h] == ts_pattern[j, 2]) { #if sampling year matches current ts
      
      # Calculate avg MAM cumulative per row_h and add to main DF
      ts_plot[h, paste('precipmam_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- 
        rowSums(df2[h,], na.rm = T)/(ts_pattern$ts[j] + 1) #divide by years in time series
    }
  }
}
head(ts_plot)


# Check data distribution
nstart <- 61 #first column with new clim data
cend <- 65 #last column with new clim data

par(mfrow = c(3, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i], xlim = c(70, 200))
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')




####################################################################################################

# # SUMMER INSOLATION: DATA # # 

####################################################################################################

# Specify multiple conditions to create file list
ts <- paste(c(1982:2013), collapse = '|') #years of interest

insols <- Reduce(intersect, list(list.files(ins, pattern = '_06|_07|_08'),
                                 list.files(ins, pattern = ts)))


# Read in data for this section 
setwd(ins)
i_stack <- stack(lapply(insols, raster)) #JJA insolation
i_stack
nlayers(i_stack)

setwd(my.dat) 
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
setwd(out.ras)
main_ras <- raster('main_ras.tif') #main raster to project climate rasters to (ts_90m.R; NC)


# Change R temporary file location to local drive in case of AU server connection disruption
rasterOptions(tmpdir = loc)


# Parallel computing to reproject Stack to template raster
start_time <- Sys.time()
beginCluster()

insols_ts <- projectRaster(from = i_stack, to = main_ras)

endCluster()
end_time <- Sys.time()
end_time - start_time #


# Extract data from raster
gps <- as.matrix(ts_plot[,c('x_stere', 'y_stere')]) #matrix of long, lat to use for clim data extraction
vals <- data.frame(id = ts_plot$id, year = ts_plot$year, extract(insols_ts, gps)) #extract clim values for each raster layer to new DF


# Check data distribution
colMax <- function(data) sapply(data, max, na.rm = TRUE) #max value per column
mx <- colMax(vals[3:ncol(vals)])

colMin <- function(data) sapply(data, min, na.rm = TRUE) #min value per column
mn <- colMin(vals[3:ncol(vals)])

par(mfrow = c(4,2), mar = c(5,4,2,1))
hist(mx, breaks = 100)
hist(mn, breaks = 100)
boxplot(mx, horizontal = T) #following Zuur et al. 2009
boxplot(mn, horizontal = T)
dotchart(mx, labels = '') 
dotchart(mn, labels = '')

hist(vals[,3], breaks = nrow(vals), xlim = c(min(mn), max(mx)), ylim = c(0, 15), 
     main = 'All Values', xlab = '')
for (i in ncol(vals)) {
  hist(vals[,i], breaks = nrow(vals), add = T)
}

ms <- rowMeans(vals[,3:ncol(vals)])
plot(ms ~ ts_plot$inclin_dir) #mean insolation ~ aspect


# Save all extracted values
setwd(my.dat) 
save(vals, file = 'vals_insol_jja.RData')




####################################################################################################

# # SUMMER INSOLATION: COMPUTE SUMMER MEAN # # in progress: check adjustments before running

####################################################################################################

# Load data for this section
setwd(my.dat) 
load('vals_insol_jja.RData') #summer insolation (ts_90m.R; NC)
load('ts_plot.RData') #plot metadata + downscaled clim time series (ts_90m.R; NC)
load('ts_pattern.RData') #reference DF used in time series computation (ts_90m.R; NC)


## Calculate average JJA insolation ##

for (j in 1:nrow(ts_pattern)) {
  
  df <- 'NA' #initialize intermediary DF to dump values into
  
  # ID and subset appropriate columns
  for (i in 3:ncol(vals)) { #loop through columns = clim data from diff rasters
    
    nn <- grep(ts_pattern[j, 3], colnames(vals[i]), value = T) #extract colname_i from ts_j
    cc <- vals[which(colnames(vals) == nn)] #extract corresponding values
    df <- cbind(df, cc) #dump into new DF
  }
  
  df2 <- df[-1] #delete first column with NAs
  
  for (h in 1:nrow(ts_plot)) { #loop through rows = plots
    if (ts_plot$year[h] == ts_pattern[j, 2]) { #if sampling year matches current ts
      
      # Calculate yearly JJA avgs per row_h and add to main DF
      ts_plot[h, paste('insoljja_ts', (ts_pattern$ts[j] + 1), sep = '_')] <- rowMeans(df2[h,])
    }
  }
}
head(ts_plot)


# Check data distribution
nstart <- 39 #first column with new clim data
cend <- ncol(ts_plot) #last column with new clim data

par(mfrow = c(3, 2))
for (i in nstart:cend) {
  dotchart(ts_plot[,i])
}


# Save main DF
setwd(my.dat)
save(ts_plot, file = 'ts_plot.RData')



