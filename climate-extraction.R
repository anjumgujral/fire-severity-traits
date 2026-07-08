library(tmap)
library(AOI)
library(sf)
library(climateR)
library(ggplot2)
library(terra)
library(ggspatial)
library(prettymapr)
library(maps)
library(elevatr)
library(ggnewscale)
library(tidyterra)

# download 30 year climate normals for ppt
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
writeRaster(ppt_mean, "ppt_30yr_mean.tif", overwrite = TRUE)

# extract climate values for specific site locations, first make sure CRS matches between site points and climate data
sites_vect <- terra::project(vect(sites_sf), crs(ppt_mean))
ppt_vals <- terra::extract(ppt_mean, sites_vect)

ppt_table <- cbind(site_boundaries, ppt_vals[,-1])
colnames(ppt_table)[ncol(ppt_table)] <- "ppt_30yr_mean"

ppt_table

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
writeRaster(def_mean, "def_30yr_mean.tif", overwrite = TRUE)


