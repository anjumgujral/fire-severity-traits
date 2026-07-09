
library(sf)
library(ggplot2)
library(terra)
library(tidyterra)

# download 30 year climate normals for ppt, cwd, pet, aet, vpd
years <- 1994:2023

ppt_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_ppt_", years, ".nc")
)

# calculate total annual precip for each year by taking monthly precip and grouping it by 12 months and summing
# precip across those 12 months. Then create a raster for each year, and stack 30 years
ppt_annual_by_year <- tapp(
  ppt_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)
# take the average of the 30 annual total precip rasters
ppt_mean <- mean(ppt_annual_by_year, na.rm = TRUE)

# save raster as .tif for loading later if needed
#writeRaster(ppt_mean, "ppt_30yr_mean.tif", overwrite = TRUE)
ppt_mean <- rast("data/terra_climate/ppt_30yr_mean.tif")


## CWD
# now run through the same code for climatic water deficit
def_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_def_", years, ".nc")
)

def_annual_by_year <- tapp(
  def_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)

def_mean <- mean(def_annual_by_year, na.rm = TRUE)
#writeRaster(def_mean, "def_30yr_mean.tif", overwrite = TRUE)
def_mean <- rast("data/terra_climate/def_30yr_mean.tif")


## VPD
vpd_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_vpd_", years, ".nc")
)

vpd_annual_by_year <- tapp(
  vpd_stack_stable,
  index = rep(1:30, each = 12),
  fun = mean,
  na.rm = TRUE
)

vpd_mean <- mean(vpd_annual_by_year, na.rm = TRUE)
#writeRaster(vpd_mean, "vpd_30yr_mean.tif", overwrite = TRUE)
vpd_mean <- rast("data/terra_climate/vpd_30yr_mean.tif")


# AET
aet_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_aet_", years, ".nc")
)

aet_annual_by_year <- tapp(
  aet_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)

aet_mean <- mean(aet_annual_by_year, na.rm = TRUE)
#writeRaster(aet_mean, "aet_30yr_mean.tif", overwrite = TRUE)
aet_mean <- rast("data/terra_climate/aet_30yr_mean.tif")


# PET
pet_stack_stable <- rast(
  paste0("https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_pet_", years, ".nc")
)

pet_annual_by_year <- tapp(
  pet_stack_stable,
  index = rep(1:30, each = 12),
  fun = sum,
  na.rm = TRUE
)

pet_mean <- mean(pet_annual_by_year, na.rm = TRUE)
#writeRaster(pet_mean, "pet_30yr_mean.tif", overwrite = TRUE)
pet_mean <- rast("data/terra_climate/pet_30yr_mean.tif")


# Tmax
tmax_stack_stable <- rast(
  paste0(
    "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmax_", years, ".nc"))
nlyr(tmax_stack_stable)

tmax_annual_by_year <- tapp(
  tmax_stack_stable,
  index = rep(1:30, each = 12),
  fun = mean,
  na.rm = TRUE
)
nlyr(tmax_annual_by_year)

tmax_mean <- mean(tmax_annual_by_year, na.rm = TRUE)
#writeRaster(tmax_mean, "tmax_30yr_mean.tif", overwrite = TRUE)
tmax_mean <- rast("data/terra_climate/tmax_30yr_mean.tif")

# Tmin
tmin_stack_stable <- rast(
  paste0(
    "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_tmin_", years, ".nc"))
nlyr(tmin_stack_stable)

tmin_annual_by_year <- tapp(
  tmin_stack_stable,
  index = rep(1:30, each = 12),
  fun = mean,
  na.rm = TRUE
)
nlyr(tmin_annual_by_year)

tmin_mean <- mean(tmin_annual_by_year, na.rm = TRUE)
#writeRaster(tmin_mean, "tmin_30yr_mean.tif", overwrite = TRUE)
tmin_mean <- rast("data/terra_climate/tmin_30yr_mean.tif")



