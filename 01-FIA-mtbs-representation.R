library(sf)
library(terra)
library(dplyr)
library(raster)
library(terra)
library(tidyverse)
library(here)

# Load points for fuzzed FIA plots

FIA <- read.csv("./data/FIA/CA_PLOT.csv")

# Convert the FIA locations to an st point object
plot_points <- FIA[, c("LAT", "LON")]
plot_points <- na.omit(plot_points)
plot_points <- st_as_sf(plot_points, coords = c("LON", "LAT"), crs = 4326)

# Load mtbs rasters
MTBS_BSmosaics <- here("data/MTBS_BSmosaics")

# Create a raster name list from all the files in the data folder
rasters_list <- list.files(MTBS_BSmosaics, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

# Reproject FIA location points to the CRS of the raster layer
plot_points <- st_transform(plot_points, crs(rast(rasters_list[[1]])))

# Function to read in rasters, set them to a common extent based on the sample points, then write them back to file
# Note this is kind of slow to run (~5 seconds per raster)
extend_raster <- function(raster_file, new_extent, new_directory, overwrite = TRUE) {
  r <- rast(raster_file)
  r <- extend(r, new_extent)
  r <- crop(r, new_extent)
  # Create output file path
  output_file <- file.path(new_directory, basename(raster_file))
  if (file.exists(output_file) && !overwrite) {
    message("File exists and overwrite is set to FALSE. Skipping raster: ", raster_file)
    return(NULL)  # Skip saving if overwrite is FALSE
  }
  # Write the raster with overwrite option
  writeRaster(r, filename = output_file, overwrite = overwrite)
  return(output_file)  # Return the path to the saved raster
}


# Run the function on all the MTBS rasters
new_directory <- here("data/MTBS_same_extent")
dir.create(new_directory, showWarnings = FALSE)
new_extent <- ext(plot_points)
extended_rasters <- lapply(rasters_list, extend_raster, new_extent = new_extent, new_directory = new_directory)

# Create a raster stack from the list of extended rasters
rasters_list_extended <- list.files(new_directory, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)
raster_stack <- rast(rasters_list_extended)

# Check to make sure extraction works on the first layer of the stack
test_extract <- terra::extract(raster_stack[[1]], plot_points)
length(test_extract)
sum(!is.na(test_extract$mtbs_CA_1984))
hist(test_extract$mtbs_CA_1984)

# Extract the severity values from the raster layers
extracted_values <- terra::extract(raster_stack, plot_points)
dim(extracted_values)
head(extracted_values)

# Check how many non-NA values there are
count_nonNAs <- function(x) {return(sum(!is.na(x)))}
nonNAs_by_year <- apply(extracted_values[2:ncol(extracted_values)], 2, count_nonNAs)
# plot number of nonNAs_by_year, using ggplot, and rotating the x labels to vertical
nonNAs_df <- data.frame(year = names(nonNAs_by_year), nonNAs = nonNAs_by_year)
ggplot(nonNAs_df, aes(x = year, y = nonNAs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of non-NA values by year", x = "Year", y = "Number of non-NA values")

