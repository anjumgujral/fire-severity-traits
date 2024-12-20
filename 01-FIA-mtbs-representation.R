
library(sf)
library(terra)
library(dplyr)
library(raster)

#load points for fuzzed FIA plots

FIA <- read.csv("/Users/anjumgujral/Documents/repos/fire-severity-traits/data/FIA/CA_PLOT.csv")

#load fire perimeters




#load mtbs

# Define the main folder path
MTBS_BSmosaics <- "/Users/anjumgujral/Documents/repos/fire-severity-traits/data/MTBS_BSmosaics"

all_tif_files <- character()

# Create an empty list to store rasters
rasters_list <- list()


# Loop over the years from 1984 to 2021
for (year in 1984:2021) {
  year_folder <- file.path("/Users/anjumgujral/Documents/repos/fire-severity-traits/data/MTBS_BSmosaics", as.character(year))
  subfolder <- file.path(year_folder, paste0("mtbs_CA_", year))

  if (dir.exists(subfolder)) {
    tif_files <- list.files(subfolder, pattern = "\\.tif$", full.names = TRUE)

    if (length(tif_files) > 0) {
      # Read and process the .tif files here
      print(paste("Processing", length(tif_files), "tif files for year", year))
      all_tif_files <- c(all_tif_files, tif_files)
    } else {
      print(paste("No .tif files found for year", year))
    }
  } else {
    print(paste("Folder for year", year, "does not exist"))
  }
}

# before compiling all tif files, we have to make sure they are aligned
#to the same resolution, extent, and CRS

# aligning the extent of rasters

# Define the extent of California (approximate lat/lon values)
california_extent <- extent(-125.0, -113.0, 32.5, 42.0)

# Crop and resample all rasters to the California extent
aligned_rasters <- lapply(rasters, function(r) {
  r_cropped <- crop(r, california_extent)  # Crop to California extent
  resample(r_cropped, rasters[[1]], method = "bilinear")  # Resample to match resolution
})


# Get the CRS of the rasters
rasters_crs <- crs(rasters[[1]])

# Reproject California extent to the same CRS as the rasters
california_extent_proj <- spTransform(as(california_extent_geo, "SpatialPolygons"), rasters_crs)

# Convert the reprojected California extent to a bounding box (extent object)
california_extent_proj <- extent(california_extent_proj)


# Now stack the aligned rasters
raster_stack <- stack(aligned_rasters)




# After loading all rasters, check if all have the same extent
rasters_extent_check <- lapply(rasters, extent)

# Check if all extents are the same
if (length(unique(rasters_extent_check)) == 1) {
  print("All rasters have the same extent")
  # Proceed with stacking
  raster_stack <- stack(rasters)
} else {
  print("Rasters have different extents. Aligning to a common extent.")




# After the loop: Process all .tif files
if (length(all_tif_files) > 0) {
  # Example: Read in all the .tif files as raster objects
  library(raster)  # Or use terra::rast() if preferred
  rasters <- lapply(all_tif_files, raster)

  # stack all rasters together into a single object
  raster_stack <- stack(rasters)

  # save the combined raster stack to a file
  writeRaster(raster_stack, filename = "combined_fire_severity.tif", overwrite = TRUE)

  # Print the number of rasters processed
  print(paste("Processed", length(rasters), "rasters"))
} else {
  print("No .tif files were found in any year folder.")
}


raster_stack <- stack(rasters_list)




