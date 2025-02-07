# ---
# title: "gdm_start"
# author: "Rob Cooke"
# date: "08/06/2021"
# output: html_notebook
# ---
  
#### Set-up ####
  
# Here we load the necessary packages

library(dplyr)
library(raster)
library(terra)
library(ggplot2)
library(rphylopic)
library(gdm)
library(cowplot)
library(corrplot)
library(iNEXT)
library(tibble)

# commit packages to renv library
# renv::snapshot()
# renv::status()

# # set location of temporary directory for terra and raster package
# terra::terraOptions(tmpdir = "temp")
# raster::rasterOptions(tmpdir = "temp")

# set cowplot theme for ggplots
ggplot2::theme_set(cowplot::theme_cowplot())

#### Load covariate data ####

# see data_prep.R for preparation of covariates

# template

# load: uk_1000
uk_1000 <- terra::rast("data/uk_1000.tif") %>% 
  terra::crop(., terra::ext(0, 656000, 0, 1057000))
# these have the 500s so are the centre of the grid cells not the bottom left corner

# data frame of uk raster - useful as a base map
uk_1000_df <- as.data.frame(uk_1000, xy = TRUE) %>% 
  setNames(., c("x", "y", "z")) %>% 
  dplyr::mutate(z = ifelse(z >= 1, 1, NA))

# terrain ruggedness index

# load: tri
tri <- terra::rast("data/tri.tif") %>% 
  terra::crop(., uk_1000)

# terrain wetness index

# load: twi
twi <- terra::rast("data/twi.tif") %>% 
  terra::crop(., uk_1000)

## soil ph

# load: ph
ph <- terra::rast("data/ph_idw.tif") %>% 
  terra::crop(., uk_1000)

# annual minimum temperature

# load: tmin
# NB climate RCPs are ~equivalent for 1980-2010 (baseline)
tmin <- terra::rast("data/tmin_rcp26_1980-2010.tif") %>% 
  terra::crop(., uk_1000)

# annual maximum temperature

# load: tmax
tmax <- terra::rast("data/tmax_rcp26_1980-2010.tif") %>% 
  terra::crop(., uk_1000)

# maximum monthly diurnal temperature range

# load: trngx
trngx <- terra::rast("data/trngx_rcp26_1980-2010.tif") %>%
  terra::crop(., uk_1000)

# annual precipitation

# load: prec
prec <- terra::rast("data/prec_rcp26_1980-2010.tif") %>% 
  terra::crop(., uk_1000)

# precipitation seasonality

# load: pseas
pseas <- terra::rast("data/prec_pseas_rcp26_1980-2010.tif") %>% 
  terra::crop(., uk_1000)

# potential evapotranspiration with interception correction of driest month

# load: peti
peti <- terra::rast("data/peti_rcp26_1980-2010.tif") %>% 
  terra::crop(., uk_1000)

# annual surface downwelling shortwave radiation

# load: sun
sun <- terra::rast("data/sun_rcp26_1980-2010.tif") %>% 
  terra::crop(., uk_1000)

# covariates combined
covarso <- c(tri, twi, ph, tmin, tmax, trngx, prec, pseas, peti, sun)
names(covarso) <- c("tri", "twi", "ph", "tmin", "tmax", "trngx", "prec", "pseas", "peti", "sun")

# replace nans with na
covarso[is.nan(covarso)] <- NA

# remove Ireland
covarso <- terra::mask(covarso, terra::as.polygons(terra::ext(0, 150000, 200000, 630000)), inverse = TRUE)
covarso <- terra::mask(covarso, terra::as.polygons(terra::ext(100000, 190000, 400000, 600000)), inverse = TRUE)

# remove France
covarso <- terra::mask(covarso, terra::as.polygons(terra::ext(600000, 750000, 0, 100000)), inverse = TRUE)
covarso <- terra::mask(covarso, terra::as.polygons(terra::ext(640000, 750000, 0, 135000)), inverse = TRUE)

# remove Isle of Man
covarso <- terra::mask(covarso, terra::as.polygons(terra::ext(190000, 260000, 450000, 520000)), inverse = TRUE)

# remove Isles of Scilly
covarso <- terra::mask(covarso, terra::as.polygons(terra::ext(0, 120000, 0, 300000)), inverse = TRUE)

# plot(covarso)

# correlations between covariates across all grid cells
M <- cor(terra::values(covarso), use = "na.or.complete")

corrplot::corrplot.mixed(M, lower = "number", upper = "ellipse", tl.pos = "lt", tl.col = "black")

# match nas across rasters within stack
covars <- terra::mask(covarso, terra::app(covarso, fun = sum))

#### Prepare future climates ####

# function to prepare covariates for future
fut_covars <- function(rcp, years) {
  
  # annual minimum temperature
  tmin <- terra::rast(paste0("data/tmin_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # annual maximum temperature
  tmax <- terra::rast(paste0("data/tmax_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # maximum monthly diurnal temperature range
  trngx <- terra::rast(paste0("data/trngx_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # annual precipitation
  prec <- terra::rast(paste0("data/prec_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # precipitation seasonality
  pseas <- terra::rast(paste0("data/prec_pseas_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # potential evapotranspiration with interception correction of driest month
  peti <- terra::rast(paste0("data/peti_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # annual surface downwelling shortwave radiation
  sun <- terra::rast(paste0("data/sun_", rcp, "_", years, ".tif")) %>%
    terra::crop(., uk_1000)
  
  # covariates combined
  covarso_rcp <- c(tri, twi, ph, tmin, tmax, trngx, prec, pseas, peti, sun)
  names(covarso_rcp) <- c("tri", "twi", "ph", "tmin", "tmax", "trngx", "prec", "pseas", "peti", "sun")
  
  # replace nans with na
  covarso_rcp[is.nan(covarso_rcp)] <- NA
  
  # remove Ireland
  covarso_rcp <- terra::mask(covarso_rcp, terra::as.polygons(terra::ext(0, 150000, 200000, 630000)), inverse = TRUE)
  covarso_rcp <- terra::mask(covarso_rcp, terra::as.polygons(terra::ext(100000, 190000, 400000, 600000)), inverse = TRUE)
  
  # remove France
  covarso_rcp <- terra::mask(covarso_rcp, terra::as.polygons(terra::ext(600000, 750000, 0, 100000)), inverse = TRUE)
  covarso_rcp <- terra::mask(covarso_rcp, terra::as.polygons(terra::ext(640000, 750000, 0, 135000)), inverse = TRUE)
  
  # remove Isle of Man
  covarso_rcp <- terra::mask(covarso_rcp, terra::as.polygons(terra::ext(190000, 260000, 450000, 520000)), inverse = TRUE)
  
  # remove Isles of Scilly
  covarso_rcp <- terra::mask(covarso_rcp, terra::as.polygons(terra::ext(0, 120000, 0, 300000)), inverse = TRUE)
  
  return(covarso_rcp)
  
}

# RCP 2.6 2020-2040
covarso_rcp26_2030 <- fut_covars(rcp = "rcp26", years = "2020-2040")

# RCP 2.6 2040-2060
covarso_rcp26_2050 <- fut_covars(rcp = "rcp26", years = "2040-2060")

# RCP 2.6 2060-2080
covarso_rcp26_2070 <- fut_covars(rcp = "rcp26", years = "2060-2080")

# RCP 4.5 2020-2040
covarso_rcp45_2030 <- fut_covars(rcp = "rcp45", years = "2020-2040")

# RCP 4.5 2040-2060
covarso_rcp45_2050 <- fut_covars(rcp = "rcp45", years = "2040-2060")

# RCP 4.5 2060-2080
covarso_rcp45_2070 <- fut_covars(rcp = "rcp45", years = "2060-2080")

# RCP 6.0 2020-2040
covarso_rcp60_2030 <- fut_covars(rcp = "rcp60", years = "2020-2040")

# RCP 6.0 2040-2060
covarso_rcp60_2050 <- fut_covars(rcp = "rcp60", years = "2040-2060")

# RCP 6.0 2060-2080
covarso_rcp60_2070 <- fut_covars(rcp = "rcp60", years = "2060-2080")

# RCP 8.5 2020-2040
covarso_rcp85_2030 <- fut_covars(rcp = "rcp85", years = "2020-2040")

# RCP 8.5 2040-2060
covarso_rcp85_2050 <- fut_covars(rcp = "rcp85", years = "2040-2060")

# RCP 8.5 2060-2080
covarso_rcp85_2070 <- fut_covars(rcp = "rcp85", years = "2060-2080")

#### Match env data for future ####

# RCP 2.6 2020-2040
covars_rcp26_2030 <- terra::mask(covarso_rcp26_2030, terra::app(covarso_rcp26_2030, fun = sum))

# RCP 2.6 2040-2060
covars_rcp26_2050 <- terra::mask(covarso_rcp26_2050, terra::app(covarso_rcp26_2050, fun = sum))

# RCP 2.6 2060-2080
covars_rcp26_2070 <- terra::mask(covarso_rcp26_2070, terra::app(covarso_rcp26_2070, fun = sum))

# RCP 4.5 2020-2040
covars_rcp45_2030 <- terra::mask(covarso_rcp45_2030, terra::app(covarso_rcp45_2030, fun = sum))

# RCP 4.5 2040-2060
covars_rcp45_2050 <- terra::mask(covarso_rcp45_2050, terra::app(covarso_rcp45_2050, fun = sum))

# RCP 4.5 2060-2080
covars_rcp45_2070 <- terra::mask(covarso_rcp45_2070, terra::app(covarso_rcp45_2070, fun = sum))

# RCP 6.0 2020-2040
covars_rcp60_2030 <- terra::mask(covarso_rcp60_2030, terra::app(covarso_rcp60_2030, fun = sum))

# RCP 6.0 2040-2060
covars_rcp60_2050 <- terra::mask(covarso_rcp60_2050, terra::app(covarso_rcp60_2050, fun = sum))

# RCP 6.0 2060-2080
covars_rcp60_2070 <- terra::mask(covarso_rcp60_2070, terra::app(covarso_rcp60_2070, fun = sum))

# RCP 8.5 2020-2040
covars_rcp85_2030 <- terra::mask(covarso_rcp85_2030, terra::app(covarso_rcp85_2030, fun = sum))

# RCP 8.5 2040-2060
covars_rcp85_2050 <- terra::mask(covarso_rcp85_2050, terra::app(covarso_rcp85_2050, fun = sum))

# RCP 8.5 2060-2080
covars_rcp85_2070 <- terra::mask(covarso_rcp85_2070, terra::app(covarso_rcp85_2070, fun = sum))

rm(covarso, covarso_rcp26_2030, covarso_rcp26_2050, covarso_rcp26_2070, covarso_rcp45_2030, covarso_rcp45_2050, covarso_rcp45_2070, covarso_rcp60_2030, covarso_rcp60_2050, covarso_rcp60_2070, covarso_rcp85_2030, covarso_rcp85_2050, covarso_rcp85_2070)
# rm(covarso, covarso_rcp85_2070)

rm(tri, twi, ph, tmin, tmax, trngx, prec, pseas, peti, sun)

#### Load habitat condition data ####

# see data_prep.R for preparation of habitat condition layers
# basically bii coefficients applied to the crafty land use categories (averaged for each period)

# function to load habitat condition layers
hc_load <- function(rcp_ssp, years) {
  
  hc <- terra::rast(paste0("data/landuse/bii/", rcp_ssp, "/", rcp_ssp, "_", years, ".tif"))
  
}

# RCP 2.6 SSP1 2020-2040
hc_rcp26_ssp1_2030 <- hc_load(rcp_ssp = "RCP2_6-SSP1", years = "2020-2040")

# RCP 2.6 SSP1 2040-2060
hc_rcp26_ssp1_2050 <- hc_load(rcp_ssp = "RCP2_6-SSP1", years = "2040-2060")

# RCP 2.6 SSP1 2060-2080
hc_rcp26_ssp1_2070 <- hc_load(rcp_ssp = "RCP2_6-SSP1", years = "2060-2080")

# RCP 4.5 SSP2 2020-2040
hc_rcp45_ssp2_2030 <- hc_load(rcp_ssp = "RCP4_5-SSP2", years = "2020-2040")

# RCP 4.5 SSP2 2040-2060
hc_rcp45_ssp2_2050 <- hc_load(rcp_ssp = "RCP4_5-SSP2", years = "2040-2060")

# RCP 4.5 SSP2 2060-2080
hc_rcp45_ssp2_2070 <- hc_load(rcp_ssp = "RCP4_5-SSP2", years = "2060-2080")

# RCP 4.5 SSP4 2020-2040
hc_rcp45_ssp4_2030 <- hc_load(rcp_ssp = "RCP4_5-SSP4", years = "2020-2040")

# RCP 4.5 SSP4 2040-2060
hc_rcp45_ssp4_2050 <- hc_load(rcp_ssp = "RCP4_5-SSP4", years = "2040-2060")

# RCP 4.5 SSP4 2060-2080
hc_rcp45_ssp4_2070 <- hc_load(rcp_ssp = "RCP4_5-SSP4", years = "2060-2080")

# RCP 6.0 SSP3 2020-2040
hc_rcp60_ssp3_2030 <- hc_load(rcp_ssp = "RCP6_0-SSP3", years = "2020-2040")

# RCP 6.0 SSP3 2040-2060
hc_rcp60_ssp3_2050 <- hc_load(rcp_ssp = "RCP6_0-SSP3", years = "2040-2060")

# RCP 6.0 SSP3 2060-2080
hc_rcp60_ssp3_2070 <- hc_load(rcp_ssp = "RCP6_0-SSP3", years = "2060-2080")

# RCP 8.5 SSP2 2020-2040
hc_rcp85_ssp2_2030 <- hc_load(rcp_ssp = "RCP8_5-SSP2", years = "2020-2040")

# RCP 8.5 SSP2 2040-2060
hc_rcp85_ssp2_2050 <- hc_load(rcp_ssp = "RCP8_5-SSP2", years = "2040-2060")

# RCP 8.5 SSP2 2060-2080
hc_rcp85_ssp2_2070 <- hc_load(rcp_ssp = "RCP8_5-SSP2", years = "2060-2080")

# RCP 8.5 SSP5 2020-2040
hc_rcp85_ssp5_2030 <- hc_load(rcp_ssp = "RCP8_5-SSP5", years = "2020-2040")

# RCP 8.5 SSP5 2040-2060
hc_rcp85_ssp5_2050 <- hc_load(rcp_ssp = "RCP8_5-SSP5", years = "2040-2060")

# RCP 8.5 SSP5 2060-2080
hc_rcp85_ssp5_2070 <- hc_load(rcp_ssp = "RCP8_5-SSP5", years = "2060-2080")

# Baseline 2020
hc_baseline_2020 <- terra::rast("data/landuse/bii/Baseline/Baseline_2020.tif")
