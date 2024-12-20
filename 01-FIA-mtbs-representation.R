
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

  # Dynamically create the path to the year folder (e.g., '1984')
  year_folder <- file.path("/Users/anjumgujral/Documents/repos/fire-severity-traits/data/MTBS_BSmosaics", as.character(year))

  # Create the path to the 'mtbs_CA_year' folder (e.g., 'mtbs_CA_1984')
  subfolder <- file.path(year_folder, paste0("mtbs_CA_", year))

  # Check if the subfolder exists
  if (dir.exists(subfolder)) {

    # List all .tif files in the 'mtbs_CA_year' subfolder
    tif_files <- list.files(subfolder, pattern = "\\.tif$", full.names = TRUE)

    # Check if any .tif files are found
    if (length(tif_files) > 0) {
      # Read and process the .tif files here
      print(paste("Processing", length(tif_files), "tif files for year", year))
      all_tif_files <- c(all_tif_files, tif_files)
      # Example: Read in the .tif files
      # rasters <- lapply(tif_files, raster)  # Or use terra::rast() if using the terra package
      # You can add further processing here for the rasters...

    } else {
      print(paste("No .tif files found for year", year))
    }
  } else {
    print(paste("Folder for year", year, "does not exist"))
  }
}

# before compiling all tif files, we have to make sure they are aligned
#to the same resolution, extent, and CRS






# After the loop: Process all collected .tif files
if (length(all_tif_files) > 0) {
  # Example: Read in all the .tif files as raster objects
  library(raster)  # Or use terra::rast() if preferred
  rasters <- lapply(all_tif_files, raster)

  # Optionally, stack all rasters together into a single object
  raster_stack <- stack(rasters)

  # Example of saving the combined raster stack to a file
  writeRaster(raster_stack, filename = "combined_fire_severity.tif", overwrite = TRUE)

  # Print the number of rasters processed
  print(paste("Processed", length(rasters), "rasters"))
} else {
  print("No .tif files were found in any year folder.")
}


raster_stack <- stack(rasters_list)




