# Data preparation code

# Authors: Rob Cooke, Steven White, Colin Harrower

#### Covariates ####

# install terra from github to avoid this bug https://github.com/rspatial/terra/issues/361
# remotes::install_github("rspatial/terra")

# load packages
library(ncdf4)
library(sf)
library(dplyr)
library(terra)
library(sp)
library(tidyr)
library(gstat)
library(janitor)

# remotes::install_github("colinharrower/BRCmap")
library(BRCmap)

# set location of temporary directory for terra package
terra::terraOptions(tmpdir = "temp")

# template raster of UK
uk_1000 <- terra::rast("data/uk_1000.tif")

# ## terrain ruggedness index - tri
# 
# # load data
# tri_orig <- terra::rast("data/tri_1KMmd_GMTEDmd.tif") 
# # FROM https://doi.pangaea.de/10.1594/PANGAEA.867115
# 
# # crop to ~UK
# tri <- terra::crop(tri_orig, terra::ext(-10, 4, 49, 60))
# 
# # match projection and resolution to UK 1km
# # If the origin and crs are the same, you should consider using these other functions instead: aggregate, disagg, expand or crop.
# tri <- terra::resample(tri, uk_1000, method = "bilinear")
# 
# # save as .tif
# terra::writeRaster(tri, "data/tri.tif")
# 
# ## terrain wetness index - twi
# 
# # load data
# twi_orig <- terra::rast("data/twi/ga2.nc")
# # FROM https://catalogue.ceh.ac.uk/documents/6b0c4358-2bf3-4924-aa8f-793d468b92be
# 
# # crop to ~UK
# twi <- terra::crop(twi_orig, terra::ext(-10, 4, 49, 60))
# 
# # match projection and resolution to UK 1km
# twi <- terra::resample(twi, uk_1000, method = "bilinear")
# 
# # save as .tif
# terra::writeRaster(twi, "data/twi.tif")
# 
# ## Soil
#
# soil <- terra::vect("data/soil/5dd624a9-55c9-4cc0-b366-d335991073c7/5dd624a9-55c9-4cc0-b366-d335991073c7/data/CS_topsoil_pH_bulkDensity.shp")
# # FROM https://catalogue.ceh.ac.uk/documents/4b0e364d-61e6-48fb-8973-5eb18fb454cd 
#
# # ph
# 
# ph <- terra::rasterize(soil, uk_1000, field = "PH_07")
# 
# terra::writeRaster(ph, "data/ph.tif")
# 
# # load: ph
# ph <- raster::raster("data/ph.tif")
# 
# # IDW interpolation
# 
# ph_sf <- sf::st_as_sf(raster::as.data.frame(ph, xy = TRUE), coords = c("x", "y"), crs = raster::crs(uk_1000)) %>% 
#   # add coordinates as explicit columns
#   cbind(sf::st_coordinates(.)) %>% 
#   # drop missing data
#   tidyr::drop_na()
# 
# ph_sp <- as(ph_sf, "Spatial")
# 
# # grid to interpolate
# sp_grid <- raster::rasterToPoints(uk_1000, spatial = TRUE)
# sp::gridded(sp_grid) <- TRUE
# sp_grid <- as(sp_grid, "SpatialPixels")
# 
# idw_int <- gstat::gstat(formula = ph ~ 1, data = ph_sp, nmax = length(ph_sf$ph), set = list(idp = 2))
# 
# grid_ph <- predict(object = idw_int, newdata = sp_grid)
# 
# ph_idw <- raster::raster(grid_ph)
# 
# raster::writeRaster(ph_idw, "data/ph_idw.tif")

#### CLIMATE ####

# ALL FROM https://catalogue.ceda.ac.uk/uuid/8194b416cbee482b89e0dfbe17c5786c/

## climate data

# 1980 - 2010 # years ending Nov 81 - Nov 2020 # BASELINE
start_yr <- 1
max_yr <- (2010-1980)

# 2020 - 2040 # years ending Nov 2021 - Nov 2040
start_yr <- (2020-1980) + 1
max_yr <- (2040-1980)

# 2040 - 2060
start_yr <- (2040-1980) + 1
max_yr <- (2060-1980)

# 2060 - 2080
start_yr <- (2060-1980) + 1
max_yr <- (2080-1980)

#### annual precipitation - prec ####

# # RCP 2.6
# prec_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_pr_uk_1km_annual_19801201-20801130.nc"') 
# # RCP 4.5
# prec_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_pr_uk_1km_annual_19801201-20801130.nc"')
# # RCP 6.0
# prec_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_pr_uk_1km_annual_19801201-20801130.nc"')
# # RCP 8.5
# prec_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_pr_uk_1km_annual_19801201-20801130.nc"')
crs(prec_orig) <- "epsg:27700"

prec_yrs <- prec_orig[[start_yr:max_yr]]

# mean across years
prec <- terra::app(prec_yrs, fun = mean, na.rm = TRUE)

# # RCP 2.6 1980 - 2010
# terra::writeRaster(prec, "data/prec_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(prec, "data/prec_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(prec, "data/prec_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(prec, "data/prec_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(prec, "data/prec_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(prec, "data/prec_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(prec, "data/prec_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(prec, "data/prec_rcp45_2060-2080.tif")
# # RCP 6.0 1980 - 2010
# terra::writeRaster(prec, "data/prec_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(prec, "data/prec_rcp60_2020-2040.tif")
# # RCP 6.0 2040 - 2060
# terra::writeRaster(prec, "data/prec_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(prec, "data/prec_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(prec, "data/prec_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(prec, "data/prec_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(prec, "data/prec_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(prec, "data/prec_rcp85_2060-2080.tif")

# tidy up
rm(prec_orig, prec_yrs)

#### annual minimum temperature - tmin ####

# # RCP 2.6
# tmin_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_tasmin_uk_1km_annual_19801201-20801130.nc"')
# # RCP 4.5
# tmin_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_tasmin_uk_1km_annual_19801201-20801130.nc"')
# # RCP 6.0
# tmin_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_tasmin_uk_1km_annual_19801201-20801130.nc"')
# # RCP 8.5
# tmin_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_tasmin_uk_1km_annual_19801201-20801130.nc"')
crs(tmin_orig) <- "epsg:27700"

tmin_yrs <- tmin_orig[[start_yr:max_yr]]

# mean across years
tmin <- terra::app(tmin_yrs, fun = mean, na.rm = TRUE)

# # RCP 2.6 1980 - 2010
# terra::writeRaster(tmin, "data/tmin_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(tmin, "data/tmin_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(tmin, "data/tmin_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(tmin, "data/tmin_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(tmin, "data/tmin_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(tmin, "data/tmin_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(tmin, "data/tmin_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(tmin, "data/tmin_rcp45_2060-2080.tif")
# # RCP 6.0 1980 - 2010
# terra::writeRaster(tmin, "data/tmin_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(tmin, "data/tmin_rcp60_2020-2040.tif")
# # # RCP 6.0 2040 - 2060
# terra::writeRaster(tmin, "data/tmin_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(tmin, "data/tmin_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(tmin, "data/tmin_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(tmin, "data/tmin_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(tmin, "data/tmin_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(tmin, "data/tmin_rcp85_2060-2080.tif")

# tidy up
rm(tmin_orig, tmin_yrs)

#### annual maximum temperature - tmax ####

# # RCP 2.6
# tmax_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_tasmax_uk_1km_annual_19801201-20801130.nc"')
# # RCP 4.5
# tmax_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_tasmax_uk_1km_annual_19801201-20801130.nc"')
# # RCP 6.0
# tmax_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_tasmax_uk_1km_annual_19801201-20801130.nc"')
# # RCP 8.5
# tmax_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_tasmax_uk_1km_annual_19801201-20801130.nc"')
crs(tmax_orig) <- "epsg:27700"

tmax_yrs <- tmax_orig[[start_yr:max_yr]]

# mean across years
tmax <- terra::app(tmax_yrs, fun = mean, na.rm = TRUE)

# # RCP 2.6 1980 - 2010
# terra::writeRaster(tmax, "data/tmax_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(tmax, "data/tmax_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(tmax, "data/tmax_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(tmax, "data/tmax_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(tmax, "data/tmax_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(tmax, "data/tmax_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(tmax, "data/tmax_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(tmax, "data/tmax_rcp45_2060-2080.tif")
# # RCP 6.0 1980 - 2010
# terra::writeRaster(tmax, "data/tmax_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(tmax, "data/tmax_rcp60_2020-2040.tif")
# # RCP 6.0 2040 - 2060
# terra::writeRaster(tmax, "data/tmax_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(tmax, "data/tmax_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(tmax, "data/tmax_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(tmax, "data/tmax_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(tmax, "data/tmax_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(tmax, "data/tmax_rcp85_2060-2080.tif")

# tidy up
rm(tmax_orig, tmax_yrs)

#### maximum monthly diurnal temperature range - trngx ####

# # RCP 2.6
# trngx_max_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_tasmax_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 4.5
# trngx_max_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_tasmax_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 6.0
# trngx_max_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_tasmax_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 8.5
# trngx_max_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_tasmax_uk_1km_monthly_19801201-20801130.nc"')
crs(trngx_max_orig) <- "epsg:27700"

# # RCP 2.6
# trngx_min_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_tasmin_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 4.5
# trngx_min_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_tasmin_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 6.0
# trngx_min_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_tasmin_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 8.5
# trngx_min_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_tasmin_uk_1km_monthly_19801201-20801130.nc"')
crs(trngx_min_orig) <- "epsg:27700"

# 1980-2010 12 months (also for other years - SMW)
trngx_max_yrs <- trngx_max_orig[[(((start_yr - 1) * 12) + 1):(max_yr * 12)]]

trngx_min_yrs <- trngx_min_orig[[(((start_yr - 1) * 12) + 1):(max_yr * 12)]]

# monthly difference between max and min
trngx_range <- trngx_max_yrs - trngx_min_yrs

# max range per year
trngx_maxi <- terra::tapp(trngx_range, index = rep(1:(max_yr - start_yr + 1), each = 12), fun = max, na.rm = TRUE)

# mean across years
trngx <- terra::app(trngx_maxi, fun = mean, na.rm = TRUE)

# # RCP 2.6 1980 - 2010
# terra::writeRaster(trngx, "data/trngx_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(trngx, "data/trngx_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(trngx, "data/trngx_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(trngx, "data/trngx_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(trngx, "data/trngx_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(trngx, "data/trngx_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(trngx, "data/trngx_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(trngx, "data/trngx_rcp45_2060-2080.tif")
# # RCP 6.0 1980 - 2010
# terra::writeRaster(trngx, "data/trngx_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(trngx, "data/trngx_rcp60_2020-2040.tif")
# # RCP 6.0 2040 - 2060
# terra::writeRaster(trngx, "data/trngx_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(trngx, "data/trngx_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(trngx, "data/trngx_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(trngx, "data/trngx_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(trngx, "data/trngx_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(trngx, "data/trngx_rcp85_2060-2080.tif")

rm(trngx_max_orig, trngx_min_orig, trngx_max_yrs, trngx_min_yrs, trngx_range, trngx_maxi)

#### precipitation seasonality - pseas ####

# # RCP 2.6
# prec_month_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_pr_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 4.5
# prec_month_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_pr_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 6.0
# prec_month_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_pr_uk_1km_monthly_19801201-20801130.nc"')
# # RCP 8.5
# prec_month_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_pr_uk_1km_monthly_19801201-20801130.nc"')
crs(prec_month_orig) <- "epsg:27700"

prec_mths <- prec_month_orig[[(((start_yr - 1) * 12) + 1):(max_yr * 12)]] # monthly data across the baseline years

prec_mths_py_sd <- terra::tapp(prec_mths, index = rep(1:(max_yr - start_yr + 1), each = 12), fun = sd, na.rm = TRUE) # sd per year
prec_mths_py_mean <- terra::tapp(prec_mths, index = rep(1:(max_yr - start_yr + 1), each = 12), fun = mean, na.rm = TRUE) # mean per year

# CV per year
prec_pseas_py <- (prec_mths_py_sd / prec_mths_py_mean) * 100

# CV mean across years
prec_pseas <- terra::app(prec_pseas_py, fun = mean, na.rm = TRUE) # CV mean across years

# # RCP 2.6 1980 - 2010
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp45_2060-2080.tif")
# # # RCP 6.0 1980 - 2010
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp60_2020-2040.tif")
# # RCP 6.0 2040 - 2060
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(prec_pseas, "data/prec_pseas_rcp85_2060-2080.tif")

# tidy up
rm(prec_month_orig, prec_mths, prec_mths_py_mean, prec_mths_py_sd, prec_pseas, prec_pseas_py)

#### potential evapotranspiration with interception correction of driest month - peti ####

# # RCP 2.6
# peti_month_orig <- terra::rast('NETCDF:"data/ukcp18_rcp26_land-rcm_uk_1km_01_v20190731_peti_bias_corrected_monthly_198012-208011.nc"') 
# # RCP 4.5
# peti_month_orig <- terra::rast('NETCDF:"data/ukcp18_rcp45_land-rcm_uk_1km_01_v20190731_peti_bias_corrected_monthly_198012-208011.nc"')
# # RCP 6.0
# peti_month_orig <- terra::rast('NETCDF:"data/ukcp18_rcp60_land-rcm_uk_1km_01_v20190731_peti_bias_corrected_monthly_198012-208011.nc"')
# # RCP 8.5
# peti_month_orig <- terra::rast('NETCDF:"data/ukcp18_rcp85_land-rcm_uk_1km_01_v20190731_peti_bias_corrected_monthly_198012-208011.nc"')
crs(peti_month_orig) <- "epsg:27700"

peti_mths <- peti_month_orig[[(((start_yr - 1) * 12) + 1):(max_yr * 12)]] # monthly data across the selected years

peti_mths_py_min <- terra::tapp(peti_mths, index = rep(1:(max_yr - start_yr + 1), each = 12), fun = min, na.rm = TRUE) # minimum monthly estimate per year

peti <- terra::app(peti_mths_py_min, fun = mean, na.rm = TRUE) # peti mean across years

# # RCP 2.6 1980 - 2010
# terra::writeRaster(peti, "data/peti_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(peti, "data/peti_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(peti, "data/peti_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(peti, "data/peti_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(peti, "data/peti_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(peti, "data/peti_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(peti, "data/peti_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(peti, "data/peti_rcp45_2060-2080.tif")
# # RCP 6.0 1980 - 2010
# terra::writeRaster(peti, "data/peti_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(peti, "data/peti_rcp60_2020-2040.tif")
# # RCP 6.0 2040 - 2060
# terra::writeRaster(peti, "data/peti_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(peti, "data/peti_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(peti, "data/peti_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(peti, "data/peti_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(peti, "data/peti_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(peti, "data/peti_rcp85_2060-2080.tif")

# tidy up
rm(peti_month_orig)

#### annual surface downwelling shortwave radiation - sun ####

# # RCP 2.6
# sun_orig <- terra::rast('NETCDF:"data/chess-scape_rcp26_bias-corrected_01_rsds_uk_1km_annual_19801201-20801130.nc"') 
# # RCP 4.5
# sun_orig <- terra::rast('NETCDF:"data/chess-scape_rcp45_bias-corrected_01_rsds_uk_1km_annual_19801201-20801130.nc"')
# # RCP 6.0
# sun_orig <- terra::rast('NETCDF:"data/chess-scape_rcp60_bias-corrected_01_rsds_uk_1km_annual_19801201-20801130.nc"')
# # RCP 8.5
# sun_orig <- terra::rast('NETCDF:"data/chess-scape_rcp85_bias-corrected_01_rsds_uk_1km_annual_19801201-20801130.nc"')
crs(sun_orig) <- "epsg:27700"

sun_yrs <- sun_orig[[start_yr:max_yr]]

# mean across years
sun <- terra::app(sun_yrs, fun = mean, na.rm = TRUE)

# # RCP 2.6 1980 - 2010
# terra::writeRaster(sun, "data/sun_rcp26_1980-2010.tif")
# # RCP 2.6 2020 - 2040
# terra::writeRaster(sun, "data/sun_rcp26_2020-2040.tif")
# # RCP 2.6 2040 - 2060
# terra::writeRaster(sun, "data/sun_rcp26_2040-2060.tif")
# # RCP 2.6 2060 - 2080
# terra::writeRaster(sun, "data/sun_rcp26_2060-2080.tif")
# # RCP 4.5 1980 - 2010
# terra::writeRaster(sun, "data/sun_rcp45_1980-2010.tif")
# # RCP 4.5 2020 - 2040
# terra::writeRaster(sun, "data/sun_rcp45_2020-2040.tif")
# # RCP 4.5 2040 - 2060
# terra::writeRaster(sun, "data/sun_rcp45_2040-2060.tif")
# # RCP 4.5 2060 - 2080
# terra::writeRaster(sun, "data/sun_rcp45_2060-2080.tif")
# # RCP 6.0 1980 - 2010
# terra::writeRaster(sun, "data/sun_rcp60_1980-2010.tif")
# # RCP 6.0 2020 - 2040
# terra::writeRaster(sun, "data/sun_rcp60_2020-2040.tif")
# # RCP 6.0 2040 - 2060
# terra::writeRaster(sun, "data/sun_rcp60_2040-2060.tif")
# # RCP 6.0 2060 - 2080
# terra::writeRaster(sun, "data/sun_rcp60_2060-2080.tif")
# # RCP 8.5 1980 - 2010
# terra::writeRaster(sun, "data/sun_rcp85_1980-2010.tif")
# # RCP 8.5 2020 - 2040
# terra::writeRaster(sun, "data/sun_rcp85_2020-2040.tif")
# # RCP 8.5 2040 - 2060
# terra::writeRaster(sun, "data/sun_rcp85_2040-2060.tif")
# # RCP 8.5 2060 - 2080
# terra::writeRaster(sun, "data/sun_rcp85_2060-2080.tif")

# tidy up
rm(sun_orig, sun_yrs)

#### Plants ####

# FROM https://catalogue.ceh.ac.uk/documents/e742c94f-82a4-43e7-af14-36b131afe81b

# Load different datasets
# domin scores
domin = read.csv("data/plants/raw_data/dominscores.csv")
# this file seems to have extra blank columns after the data
rm_col = which(grepl("X",names(domin)))
if(length(rm_col) > 0){
  domin = domin[,-rm_col]
}
# Location types
loc_type = read.csv("data/plants/raw_data/locations_2015to2021.csv")
# habitat lookup
hab_lkup = read.csv("data/plants/raw_data/npmshabitatlookup.csv")
# Occurrences
occ_raw = read.csv("data/plants/raw_data/occurrences_2015to2021.csv")
# Samples attributes
samp_att = read.csv("data/plants/raw_data/sampleattributes_2015to2021.csv")
# Sample latlon
samp_latlon = read.csv("data/plants/raw_data/sampleinfowithlatlong_2015to2021.csv")
# This data file seems to have row.names saved to the csv file as there is an extra unnamed (X) column at the start of the data.frame
rm_col = which(grepl("X",names(samp_latlon)))
if(length(rm_col) > 0){
  samp_latlon = samp_latlon[,-rm_col]
}

# indicator level
samp_latlon_fil <- samp_latlon %>% 
  # 87 = Wildflower survey; 154 = Inventory survey; 155 = Indicator survey
  # nested so choose indicator level and higher inventory level
  dplyr::filter(survey_id == 154)

# join occurrence data and location data
npms_df_first <- dplyr::inner_join(dplyr::select(occ_raw, -id), samp_latlon_fil, by = c("sample_id" = "id")) %>% 
  # convert date into year
  dplyr::mutate(year = substr(date_start, 1, 4))

monad_xy <- dplyr::distinct(npms_df_first, monad) %>% 
  dplyr::bind_cols(BRCmap::OSgrid2GB_EN(.$monad, centre = TRUE)) %>% 
  dplyr::rename(x = "EASTING", y = "NORTHING")

npms_df <- npms_df_first %>% 
  dplyr::left_join(monad_xy, by = "monad") %>% 	
  # filter to Great Britain
  dplyr::filter(country %in% c("Britain")) %>% 
  # convert to incidence
  dplyr::mutate(sind = 1) %>% 
  dplyr::select(preferred_taxon, sind, year, monad, x, y)

# unique species
npms_spp <- dplyr::distinct(npms_df, preferred_taxon) %>% 
  dplyr::arrange(preferred_taxon)

# write.csv(npms_spp, "data/plants/raw_data/npms_spp.csv")

npms_spp_lookup <- read.csv("data/plants/raw_data/npms_spp_lookup.csv")

# correct species names
npms_df_corr <- dplyr::left_join(npms_df, npms_spp_lookup, by = "preferred_taxon") %>%
  # ~species level only
  dplyr::filter(!is.na(species))

# are there any duplicates
npms_dup <- janitor::get_dupes(npms_df_corr)
# yes duplicates within monads due to multiple samples per year

# distinct combinations
npms_spp_yr <- npms_df_corr %>% 
  dplyr::select(monad, x, y, year, species, sind) %>% 
  # remove duplicates
  dplyr::distinct()
# npms_dup <- janitor::get_dupes(npms_spp_yr) # no duplicates

# # save
# saveRDS(npms_spp_yr, "data/plants/prep_data/npms_spp_yr.rds")

#### Butterflies ####

# FROM https://catalogue.ceh.ac.uk/documents/b77561d8-20ee-45ad-9221-7a8885d5ac8e

## load butterflies

site_orig <- read.csv("data/butterflies/raw_data/ukbmsSiteLocationData1976-2019.csv", header = TRUE)

ind_orig <- read.csv("data/butterflies/raw_data/ukbmsSiteIndices1976-2019.csv", header = TRUE)

spe_orig <- read.delim("data/butterflies/raw_data/species_data.txt", header = TRUE, sep = ",")

## prep butterflies

bms_prep <- ind_orig %>% 
  # remove NAs (-2)
  dplyr::filter(SITE.INDEX != -2) %>% 
  # remove zeros (smaller dataset)
  dplyr::filter(SITE.INDEX != 0) %>% 
  # add species names
  dplyr::left_join(dplyr::select(spe_orig, SPECIES, STRATEGY), by = c("SPECIES.CODE" = "SPECIES")) %>%
  # drop Essex Skipper and Small Skipper - use aggregate instead
  dplyr::filter(!COMMON.NAME %in% c("Essex Skipper", "Small Skipper")) %>% 
  dplyr::ungroup() %>% 
  # add site data
  dplyr::left_join(site_orig, by = c("SITE.CODE" = "Site.Number", "COUNTRY" = "Country")) %>% 
  # remove sites without transect length data
  dplyr::filter(!is.na(Length) & !Length == 0) %>% 
  # adjust abundance by transect length - i.e. convert abundance to density
  dplyr::mutate(SITE.INDEX = SITE.INDEX / (Length / 1000)) %>%
  # just Britain - England, Scotland, Wales
  dplyr::filter(COUNTRY %in% c("England", "Scotland", "Wales")) %>% 
  # just UKBMS transects (exclude wider countryside transects)
  dplyr::filter(Survey.type == "UKBMS") %>% 
  # simplify names
  dplyr::select(site = SITE.CODE, species_no = SPECIES.CODE, species = SPECIES, common_name = COMMON.NAME, year = YEAR, strategy = STRATEGY, sind = SITE.INDEX, site_name = Site.Name, gridref = Gridreference, east = Easting, north = Northing, length = Length)

# # check for duplicates
# bms_dup <- bms_prep %>% 
#   janitor::get_dupes(site, species, year)
# # no duplicates

# coordinates for sites as UK grid
bms_xy <- dplyr::select(bms_prep, site, east, north) %>%
  # unique sites
  dplyr::distinct(site, .keep_all = TRUE) %>%
  # convert eastings and northings to geometry
  sf::st_as_sf(coords = c("east", "north"), crs = 27700) %>% 
  # add geometry as explicit column
  dplyr::mutate(X = sf::st_coordinates(.)[,1],
                Y = sf::st_coordinates(.)[,2])

# saveRDS(bms_xy, "data/butterflies/prep_data/bms_xy.rds")

# identify raster cells per site
cellID <- as.data.frame(terra::cellFromXY(covars, as.matrix(sf::st_set_geometry(dplyr::select(bms_xy, X, Y), NULL)))) %>%
  setNames("cellName")

# get x,y coordinates for each cell
cellLocs <- as.data.frame(terra::xyFromCell(covars, cellID$cellName)) %>% 
  # add bms transect information
  dplyr::bind_cols(bms_xy) %>% 
  # rename to prevent confusion with transect X and Y
  dplyr::rename(x_cell = x, y_cell = y) %>% 
  dplyr::select(site, x_cell, y_cell)

bms_grid <- bms_prep %>% 
  # join cell locations
  dplyr::left_join(cellLocs, by = c("site")) %>% 
  # per cell, year, and species
  dplyr::group_by(x_cell, y_cell, year, species) %>% 
  # summarise presence across transects
  dplyr::summarise(sind = mean(sind, na.rm = TRUE)) %>% 
  # create cell identifier
  dplyr::mutate(cell = paste0(x_cell, y_cell)) %>% 
  dplyr::ungroup()

# saveRDS(bms_grid, "data/butterflies/prep_data/bms_spp_yr.rds")

#### Birds ####

# REQUEST FROM https://www.bto.org/our-science/data/request-data-reports

# Read in BTO csv files
# Records
bto_raw = read.csv("data/birds/raw_data/BBS9410_no_fly-overs.csv", stringsAsFactors = FALSE)
# Fix Irish grid refs that have I added to the start
# Find rows affected
fix_inds = which(grepl("^[Ii](.*$)", bto_raw$square))
# Now fix these by removing the I
if(length(fix_inds) > 0){
  bto_raw$square = gsub("^[Ii](.*$)","\\1",bto_raw$square)
}
# Square coordinates
monad_info = read.csv("data/birds/raw_data/coordinates_1km_squares.csv", stringsAsFactors = FALSE)
# Change ONEKMREF column name to monad
names(monad_info)[grepl("ONEKMREF",names(monad_info))] = "monad"
# Fix Irish grid refs that have I added to the start
# Find rows affected
fix_inds = which(grepl("^[Ii](.*$)", monad_info$monad))
# Now fix by removing the Ii
if(length(fix_inds) > 0){
  monad_info$monad[fix_inds] = gsub("^[Ii](.*$)","\\1",monad_info$monad[fix_inds])
}
# Add british grid ref x,y
monad_info[,c("x","y")] = BRCmap::OSgrid2GB_EN(monad_info$monad,centre = TRUE)
# Set row.names to gridref
row.names(monad_info) = monad_info$monad

# Species names
spp_names = read.csv("data/birds/raw_data/species_codes.csv", stringsAsFactors = FALSE)
# set row names to the codes
row.names(spp_names) = spp_names$code2ltr

# Squares surveyed in each year
sq_yr = read.csv("data/birds/raw_data/surveyed_squares_per_year.csv", stringsAsFactors = FALSE)

# Fix Irish grid refs that have I added to the start
# Find rows affected
fix_inds = which(grepl("^[Ii](.*$)", sq_yr$square))
# Now fix these by removing the I
if(length(fix_inds) > 0){
  sq_yr$square = gsub("^[Ii](.*$)","\\1",sq_yr$square)
}

# Filter squares to only England, Scotland, Wales
# Convert to spatial objects to check which country it overlaps with (use polygon rather than points to maximise assignment of country info (single monad point, centre or bottom left, may be outside of of country polygon but 
monad_sp = BRCmap::gr2sp_poly(monad_info$monad)
# The projetions of monad_sp is OSGB (27700) which is the same as UK_countries but the proj4 strings are formatted slightly differently so don't match exactly so need to change them to be exact
proj4string(monad_sp) = proj4string(UK_countries)

temp_cty = cbind(monad_sp@data, over(monad_sp, UK_countries))

# Add country to monad_info
monad_info[as.character(temp_cty$GRIDREF), "country"] = as.character(temp_cty$COUNTRY)

# Find monads from Scotland,England, Wales
kp_monads = subset(monad_info, country %in% c("Scotland","England","Wales"))$monad

# Filter monad_info to just these monads
monad_info = subset(monad_info, monad %in% kp_monads)

# Add numerical site "ID" to monad_info table
monad_info[,"site"] = as.numeric(factor(monad_info$monad))

# Reset row.names
row.names(monad_info) = NULL

# Filter bto raw to only locations in monad_info (to remove Ireland, IoM, CI)
bto_raw = subset(bto_raw, square %in% monad_info$monad)

# Filter sq_yr to keep only monads in monad_info (to remove Ireland, IoM, CI, etc)
sq_yr = subset(sq_yr, square %in% monad_info$monad)

# Add species names to bto_raw
bto_raw[,c("species","common_name")] = spp_names[as.character(bto_raw$sp2),c("scientific_name","english_name")]

# Look for unmatched (there are none with the current dataset)
miss_inds = which(is.na(bto_raw$species) | bto_raw$species == "")

spp_abc <- read.csv("data/birds/raw_data/BOU_British_List_10th.csv", stringsAsFactors = FALSE, header = TRUE) %>% 
  dplyr::mutate(cat_pri = substr(Category, 1, 1)) %>% 
  dplyr::filter(cat_pri == "A")

# filter to only 'natives' 
# Species that have been recorded in an apparently natural state at least once since 1 January 1950.
spp_nat <- dplyr::filter(spp_names, scientific_name %in% spp_abc$Scientific.name)

bto_spp <- dplyr::filter(bto_raw, species %in% spp_nat$scientific_name)

saveRDS(bto_spp, "data/birds/prep_data/bto_spp_yr.rds")

# Save monad_info
saveRDS(monad_info[,c("site","monad","x","y","country")], file = "data/birds/prep_data/bto_site_loc.rds")

#### Habitat condition ####

# bii data for predicts land uses
bii_df <- read.csv("data/bii.csv", stringsAsFactors = FALSE)

# lookup to match crafty and predicts land uses
lu_look <- read.csv("data/lu_look.csv", stringsAsFactors = FALSE)

bii_crafty <- dplyr::left_join(lu_look, bii_df, by = "predicts_lu")

# groups of years
years_grps <- list(c("2020", "2030", "2040"), c("2040", "2050", "2060"), c("2060", "2070", "2080"))

# RCP-SSP combinations
rcp_ssps <- c("RCP2_6-SSP1", "RCP4_5-SSP2", "RCP4_5-SSP4", "RCP6_0-SSP3", "RCP8_5-SSP2", "RCP8_5-SSP5")

# function to convert crafty land uses into bii estimates 
conv_bii <- function(years_grps, df, rcp_ssp) {
  
  lapply(1:length(years_grps), function(y) {
    
    years <- years_grps[[y]]
    
    # load rasters
    r1 <- readRDS(paste0("data/landuse/crafty_raw/", rcp_ssp, "/", rcp_ssp, "_", years[[1]], "_raster.rds")) %>% 
      # convert to terra
      terra::rast(.)
    
    r2 <- readRDS(paste0("data/landuse/crafty_raw/", rcp_ssp, "/", rcp_ssp, "_", years[[2]], "_raster.rds")) %>% 
      # convert to terra
      terra::rast(.)
  
    r3 <- readRDS(paste0("data/landuse/crafty_raw/", rcp_ssp, "/", rcp_ssp, "_", years[[3]], "_raster.rds")) %>%
      # convert to terra
      terra::rast(.)
  
    # convert values from land use class to bii coefficient
    bii1 <- terra::subst(r1, from = df$land_use_num, to = df$bii)
    bii2 <- terra::subst(r2, from = df$land_use_num, to = df$bii)
    bii3 <- terra::subst(r3, from = df$land_use_num, to = df$bii)
  
    # average across years
    bii_mean <- terra::app(c(bii1, bii2, bii3), mean)
    
    # save file
    if (rcp_ssp == "RCP8_5-SSP5") {
      
      terra::writeRaster(bii_mean, paste0("data/landuse/bii/", rcp_ssp, "/", rcp_ssp, "_", years[[1]], "-", years[[3]], "_noextrap.tif"), overwrite = TRUE)
      
    } else {
      
      terra::writeRaster(bii_mean, paste0("data/landuse/bii/", rcp_ssp, "/", rcp_ssp, "_", years[[1]], "-", years[[3]], ".tif"), overwrite = TRUE)
      
      }
  
  }) # close lapply
  
}

# run for all RCP-SSP combinations
lapply(1:length(rcp_ssps), function(x) conv_bii(years_grps = years_grps, df = bii_crafty, rcp_ssp = rcp_ssps[[x]]))

## SSP5 (extrapolated intensive agriculture and pastoral)

# # linear interpolation
# years <- 2037:2070
# agr <- seq(from = 0.4326028, to = 0.385904, length.out = length(years))
# agr_yrs <- data.frame(bii = agr, year = years)
# past <- seq(from = 0.6295697, to = 0.4962877, length.out = length(years))
# past_yrs <- data.frame(bii = past, year = years)

# bii data for predicts land uses
bii_extrap_df <- read.csv("data/bii_extrap.csv", stringsAsFactors = FALSE)

bii_extrap <- dplyr::left_join(lu_look, bii_extrap_df, by = "predicts_lu")

# function to convert crafty land uses into bii estimates 
conv_bii_extrap <- function(years_grps, df, df_yrs, rcp_ssp) {
  
  lapply(1:length(years_grps), function(y) {
    
    years <- years_grps[[y]]
    
    # load rasters
    r1 <- readRDS(paste0("data/landuse/crafty_raw/", rcp_ssp, "/", rcp_ssp, "_", years[[1]], "_raster.rds")) %>% 
      # convert to terra
      terra::rast(.)
    
    r2 <- readRDS(paste0("data/landuse/crafty_raw/", rcp_ssp, "/", rcp_ssp, "_", years[[2]], "_raster.rds")) %>% 
      # convert to terra
      terra::rast(.)
    
    r3 <- readRDS(paste0("data/landuse/crafty_raw/", rcp_ssp, "/", rcp_ssp, "_", years[[3]], "_raster.rds")) %>%
      # convert to terra
      terra::rast(.)
    
    # filter df to get correct bii values for extrapolated years
    if (years[[1]] >= 2037) {
      
      df1 <- dplyr::filter(df_yrs, year == years[[1]]) %>% 
        dplyr::select(-year)
      
    } else {df1 <- df}
    
    if (years[[2]] >= 2037) {
      
      df2 <- dplyr::filter(df_yrs, year == years[[2]]) %>% 
        dplyr::select(-year)
      
    } else {df2 <- df}
    
    if (years[[3]] >= 2037) {
      
      df3 <- dplyr::filter(df_yrs, year == years[[3]]) %>% 
        dplyr::select(-year)
      
    } else {df3 <- df}
    
    # convert values from land use class to bii coefficient
    bii1 <- terra::subst(r1, from = df$land_use_num, to = df1$bii)
    bii2 <- terra::subst(r2, from = df$land_use_num, to = df2$bii)
    bii3 <- terra::subst(r3, from = df$land_use_num, to = df3$bii)
    
    # average across years
    bii_mean <- terra::app(c(bii1, bii2, bii3), mean)
    
    # save file
    terra::writeRaster(bii_mean, paste0("data/landuse/bii/", rcp_ssp, "/", rcp_ssp, "_", years[[1]], "-", years[[3]], ".tif"), overwrite = TRUE)
    
  }) # close lapply
  
}

# run for SSP5
conv_bii_extrap(years_grps = years_grps, df = bii_crafty, df_yrs = bii_extrap, rcp_ssp = "RCP8_5-SSP5")

## baseline
baseline_prep <- readRDS("data/landuse/crafty_raw/Baseline/Baseline_2020_raster.rds") %>% 
  # convert to terra
  terra::rast(.) %>% 
  terra::subst(., from = bii_crafty$land_use_num, to = bii_crafty$bii)

terra::writeRaster(baseline_prep, paste0("data/landuse/bii/Baseline/Baseline_2020.tif"), overwrite = TRUE)
