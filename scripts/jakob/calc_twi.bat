REM Download Nuuk Fjord ArcticDEM and process wetness indices

REM Download ArcticDEM
REM Repeat for 10 m resolution - not used in the end!
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/10m/12_37/12_37_10m_v3.0.tar.gz','D:\Jakob\ArcticDEM\jonathan_wet\10m\12_37_10m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/10m/12_38/12_38_10m_v3.0.tar.gz','D:\Jakob\ArcticDEM\jonathan_wet\10m\12_38_10m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/10m/13_37/13_37_10m_v3.0.tar.gz','D:\Jakob\ArcticDEM\jonathan_wet\10m\13_37_10m_v3.0.tar.gz')
C:\Windows\System32\WindowsPowerShell\v1.0\PowerShell.exe (new-object System.Net.WebClient).DownloadFile('http://data.pgc.umn.edu/elev/dem/setsm/ArcticDEM/mosaic/v3.0/10m/13_38/13_38_10m_v3.0.tar.gz','D:\Jakob\ArcticDEM\jonathan_wet\10m\13_38_10m_v3.0.tar.gz')

REM Extract using 7zip
REM --------------------------
cd D:\Jakob\ArcticDEM\jonathan_wet\10m\


"C:\Program Files\7-Zip\7z" e -y *10m_v3.0.tar.gz 
"C:\Program Files\7-Zip\7z" e -y *10m_v3.0.tar

REM  Mosaic ArcticDEM

dir /b *dem.tif > list_of_files.txt
C:\OSGeo4W64\OSGeo4W.bat gdalbuildvrt -input_file_list list_of_files.txt nuuk_fjord_ArcticDEM_mosaic_2m.vrt

REM Mosaic GIMP MEaSUREs DEM 30 m
cd D:\Jakob\ArcticDEM\jonathan_wet\10m\
dir /b *dem_v01.1.tif > list_of_files.txt
C:\OSGeo4W64\OSGeo4W.bat gdalbuildvrt -input_file_list list_of_files.txt nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt

REM ################################################
REM TWI following Kopecky et al. 2020

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
REM TWI following SAGA Standard Workflow

REM One-step command to calculate TWI
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_compound 0 -ELEVATION D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt -WETNESS D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_standard_twi.sdat

REM Translate to geotiff
C:\OSGeo4W64\OSGeo4W.bat gdal_translate D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_standard_twi.sdat D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_standard_twi.tif

REM ################################################
REM SAGA Wetness Index

REM One-step command to calculate SAGA Wetness Index
D:\Jakob\saga-7.8.2_x64\saga_cmd.exe --cores=54 ta_hydrology 20 -DEM D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\nuuk_fjord_GIMP_MEaSUREs_30m_DEM.vrt -TWI D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.sdat

REM Translate to geotiff
C:\OSGeo4W64\OSGeo4W.bat gdal_translate D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.sdat D:\Jakob\ArcticDEM\jonathan_wet\GIMP_MEaSUREs_30m\twi\nuuk_fjord_GIMP_MEaSUREs_30m_DEM_saga_twi.tif