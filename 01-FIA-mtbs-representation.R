
library(sf)
library(terra)
library(dplyr)
library(raster)

#load points for fuzzed FIA plots

FIA <- read.csv("/Users/anjumgujral/Documents/repos/fire-severity-traits/data/FIA/CA_PLOT.csv")

#load fire perimeters




#load mtbs

# Define the main folder path
main_folder <- "C:/Users/YourUserName/Desktop/MTBS_BSmosaics"

# Create an empty list to store rasters
rasters_list <- list()

# Loop through each year folder from 1984 to 2021
for (year in 1984:2021) {

  # Create the path to the year's folder
  year_folder <- file.path(main_folder, as.character(year))

  # Check if the folder exists
  if (dir.exists(year_folder)) {

    # Get the list of .tif files in the current year's folder
    tif_files <- list.files(year_folder, pattern = "\\.tif$", full.names = TRUE)

    # Check if any .tif files are found
    if (length(tif_files) > 0) {

      # Loop through each .tif file and read it into R
      for (tif in tif_files) {

        # Read the .tif file (you can also use terra::rast() if using the terra package)
        raster_layer <- raster(tif)

        # Add raster to the list with the file name as key (optional)
        rasters_list[[paste0(year, "_", basename(tif))]] <- raster_layer
      }
    } else {
      print(paste("No .tif files found in year folder:", year))
    }
  } else {
    print(paste("Year folder does not exist:", year))
  }
}

# You now have a list of raster layers in rasters_list





#find the intersection of points between FIA plots and mtbs within
```


