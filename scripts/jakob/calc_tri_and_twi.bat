REM Calculation of wetness indices for von Oppen et al. (in review)

REM ###############################################
REM Preparations 

REM Download ArcitDEM tiles
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/12_37/12_37_1_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\12_37_1_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/12_37/12_37_2_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\12_37_2_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/12_38/12_38_1_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\12_38_1_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/12_38/12_38_2_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\12_38_2_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_37/13_37_1_1_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_37_1_1_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_37/13_37_1_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_37_1_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_37/13_37_2_1_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_37_2_1_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_37/13_37_2_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_37_2_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_38/13_38_1_1_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_38_1_1_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_38/13_38_1_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_38_1_2_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_38/13_38_2_1_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_38_2_1_2m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/2m/13_38/13_38_2_2_2m_v3.0.tar.gz','D:\Jakob\ArcticDEM\nathalie_nuuk\2m\13_38_2_2_2m_v3.0.tar.gz')

REM Extract ArcticDEM tiles
cd D:\Jakob\ArcticDEM\nathalie_nuuk\2m\
"C:\Program Files\7-Zip\7z" e -y *2m_v3.0.tar.gz 
"C:\Program Files\7-Zip\7z" e -y *2m_v3.0.tar

REM Mosaic ArcticDEM
dir /b *dem.tif > list_of_files.txt
C:\OSGeo4W64\OSGeo4W.bat gdalbuildvrt -input_file_list list_of_files.txt nuuk_fjord_ArcticDEM_mosaic_2m.vrt

REM Download GIMP MEaSUREs DEM 30 m
REM Tiles were retrieved manually from https://nsidc.org/data/nsidc-0715/versions/1
REM required tiles are:
REM tile_0_1_fit_30m_dem_v01.1.tif
REM tile_1_1_fit_30m_dem_v01.1.tif
REM tile_2_1_fit_30m_dem_v01.1.tif

REM Mosaic GIMP MEaSURES DEM 30 m 
cd D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\dem
dir /b *dem_v01.1.tif > list_of_files.txt
C:\OSGeo4W64\OSGeo4W.bat gdalbuildvrt -input_file_list list_of_files.txt nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt

REM ################################################
REM Calculate TRI for Arctic DEM 2m  
C:\OSGeo4W64\OSGeo4W.bat gdaldem TRI D:\Jakob\ArcticDEM\nathalie_nuuk\2m\nuuk_fjord_ArcticDEM_mosaic_2m.vrt  D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\tri\nuuk_fjord_ArcticDEM_mosaic_2m_tri.tif

REM ################################################
REM TWI following Kopecky et al. 2020 for GIMP MEaSURES DEM 30 m

REM Fill Sinks in DEM (slopes > 0.1)
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_preprocessor 5 -ELEV D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt -FILLED D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\sink_filled\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_sink_filled.sdat -MINSLOPE 0.01 

REM Calculate Flow Accumulation
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_hydrology 0 -ELEVATION D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\sink_filled\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_sink_filled.sdat -METHOD 4 -CONVERGENCE 1.0 -FLOW D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\flow\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd.sdat

REM Calculate Width / Catchment
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_hydrology 19 -DEM D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\sink_filled\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_sink_filled.sdat -TCA D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\flow\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd.sdat -WIDTH D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\flow_width\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_width.sdat -SCA D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\sca\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_sca.sdat  

REM Slope
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_morphometry 0 -ELEVATION D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\sink_filled\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_sink_filled.sdat -METHOD 7 -SLOPE D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\slope\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_slope.sdat

REM Calculate TWI
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_hydrology 20 -SLOPE D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\slope\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_slope.sdat -AREA D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\sca\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_sca.sdat -TWI D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_twi.sgrd  

REM Translate to geotiff
C:\OSGeo4W64\OSGeo4W.bat gdal_translate D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_twi.sdat D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_flow_mfd_twi.tif

REM ################################################
REM SAGA Wetness Index for GIMP MEaSURES DEM 30 m

REM One-step command to calculate SAGA Wetness Index
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_hydrology 20 -DEM D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt -TWI D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.sdat

REM Translate to geotiff
C:\OSGeo4W64\OSGeo4W.bat gdal_translate D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.sdat D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.tif
