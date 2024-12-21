
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
CA_extent <- extent(-125.0, -113.0, 32.5, 42.0)

# Convert to a spatial object
CA_extent_geo_sp <- as(CA_extent, "SpatialPolygons")

# Assign the WGS84 CRS to the extent object
proj4string(CA_extent_geo_sp) <- CRS("+proj=longlat +datum=WGS84")

# Get the CRS of the rasters
rasters_crs <- crs(rasters[[1]])

# Reproject California extent to the same CRS as the rasters
CA_extent_proj_sp <- spTransform(CA_extent_geo_sp, rasters_crs)

# Convert the reprojected California extent to a bounding box (extent object)
CA_extent_proj <- extent(CA_extent_proj_sp)

# Load the rasters with terra::rast
rasters <- lapply(rasters, rast)


# Resample and crop rasters using terra

# Define the resolution of the first raster as reference
reference_raster <- rasters[[1]]
res_reference <- res(reference_raster)  # Get the resolution of the first raster

# Function to crop and resample rasters to match the California extent and resolution of the first raster
aligned_rasters <- lapply(rasters, function(r) {
  # Crop each raster to the California extent
  r_cropped <- crop(r, CA_extent_proj)  # Crop to the California extent

  # Check if the cropped raster has valid data (i.e., non-empty)
  if (ncell(r_cropped) > 0) {
    # Resample the cropped raster to match the resolution of the first raster
    r_resampled <- resample(r_cropped, reference_raster, method = "bilinear")
    return(r_resampled)  # Return the resampled raster
  } else {
    return(NULL)  # Return NULL if the raster has no valid data after cropping
  }
})


# Remove NULL values (rasters without valid data)
aligned_rasters <- aligned_rasters[!sapply(aligned_rasters, is.null)]

# Stack the remaining rasters into a single object
if (length(aligned_rasters) > 0) {
  raster_stack <- rast(aligned_rasters)
  print("Rasters successfully stacked.")
} else {
  print("No rasters overlap with the California extent.")
}


# Function to check if rasters have the same CRS, extent, and resolution
raster_properties <- function(aligned_rasters) {
  # Get the CRS, extent, and resolution of the first raster as reference
  crs_ref <- crs(aligned_rasters[[1]])
  extent_ref <- ext(aligned_rasters[[1]])
  res_ref <- res(aligned_rasters[[1]])

  # Check if all rasters have the same CRS, extent, and resolution
  for (i in 2:length(aligned_rasters)) {
    if (crs(aligned_rasters[[i]]) != crs_ref) {
      cat("Raster", i, "has a different CRS.\n")
    }
    if (!identical(ext(aligned_rasters[[i]]), extent_ref)) {
      cat("Raster", i, "has a different extent.\n")
    }
    if (!identical(res(aligned_rasters[[i]]), res_ref)) {
      cat("Raster", i, "has a different resolution.\n")
    }
  }

  cat("CRS, extent, and resolution check completed.\n")
}

# Check if all rasters have the same CRS, extent, and resolution
raster_properties(aligned_rasters)


# extract data from mtbs raster stack for all FIA points

# Replace these with your actual lat/long coordinates
lat_long_points <- data.frame(
  lat = c(37.7749, 36.7783, 34.0522),  # Example latitudes (San Francisco, Sacramento, Los Angeles)
  lon = c(-122.4194, -119.4179, -118.2437)  # Example longitudes
)

# Convert the lat/long points into a SpatVector (spatial object)
points <- vect(lat_long_points, geom = c("lon", "lat"), crs = "EPSG:4326")

# Extract raster values at these points for each layer in the raster stack
extracted_values <- extract(raster_stack, points)

# Print the extracted values
print(extracted_values)

# If you want the values as a data frame, you can do the following:
extracted_df <- as.data.frame(extracted_values)
print(extracted_df)

