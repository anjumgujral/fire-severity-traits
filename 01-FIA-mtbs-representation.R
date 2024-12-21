
library(sf)
library(terra)
library(dplyr)
library(raster)
library(terra)
#load points for fuzzed FIA plots

FIA <- read.csv("/Users/anjumgujral/Documents/repos/fire-severity-traits/data/FIA/CA_PLOT.csv")

#load mtbs
MTBS_BSmosaics <- "/Users/anjumgujral/Documents/repos/fire-severity-traits/data/MTBS_BSmosaics"

# Initialize an empty list to store rasters
rasters_list <- list()

# Loop over the years from 1984 to 2021
for (year in 1984:2021) {
  year_folder <- file.path(MTBS_BSmosaics, as.character(year))
  subfolder <- file.path(year_folder, paste0("mtbs_CA_", year))

  if (dir.exists(subfolder)) {
    tif_files <- list.files(subfolder, pattern = "\\.tif$", full.names = TRUE)

    if (length(tif_files) > 0) {
      print(paste("Processing", length(tif_files), "tif files for year", year))

      # Read and process each .tif file
      for (tif_file in tif_files) {
        raster_layer <- terra::rast(tif_file)  # Use terra::rast to read the .tif file
        rasters_list[[tif_file]] <- raster_layer  # Store the raster in the list
      }
    } else {
      print(paste("No .tif files found for year", year))
    }
  } else {
    print(paste("Folder for year", year, "does not exist"))
  }
}

# Reproject to WGS84 (lat/lon)
raster_layer_wgs84 <- terra::project(raster_layer, "EPSG:4326")

raster_extent <- ext(raster_layer_wgs84)

# Print the extent
print(raster_extent)




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




# extract data from mtbs raster stack for all FIA points
plot_points <- FIA[, c("LAT", "LON")]
plot_points <- na.omit(plot_points)


# Convert the lat/long points into a SpatVector (spatial object)
plot_points <- terra::vect(plot_points, geom = c("LON", "LAT"), crs = "EPSG:4326")

# Extract the raster's extent bounds
raster_extent <- ext(raster_layer_wgs84)
xmin_raster <- raster_extent[1]
xmax_raster <- raster_extent[2]
ymin_raster <- raster_extent[3]
ymax_raster <- raster_extent[4]

# Check if points are within the raster's extent
inside_extent <- with(FIA, LAT >= ymin_raster & LAT <= ymax_raster & LON >= xmin_raster & LON <= xmax_raster)

# Filter points that are inside the extent
plot_points <- plot_points[inside_extent, ]

# Print the remaining points (inside the extent)
print(plot_points)

extracted_values[[1]] <- terra::extract(raster_layer_wgs84, plot_points)


# Loop over all rasters in the rasters_list and extract values
for (raster_file in names(rasters_list)) {
  raster_layer <- rasters_list[[raster_file]]
  extracted_values[[raster_file]] <- terra::extract(raster_layer, plot_points)
}



# If you want the values as a data frame, you can do the following:
extracted_df <- as.data.frame(extracted_values)
print(extracted_df)

