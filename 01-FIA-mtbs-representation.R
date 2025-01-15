
library(sf)
library(terra)
library(dplyr)
library(raster)
library(terra)

#load points for fuzzed FIA plots
FIA <- read.csv("/Users/anjumgujral/Documents/repos/fire-severity-traits/data/FIA/CA_PLOT.csv")

# Convert the FIA locations to an st point object
plot_points <- FIA[, c("LAT", "LON")]
plot_points <- na.omit(plot_points)
plot_points <- st_as_sf(plot_points, coords = c("LON", "LAT"), crs = 4326)

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

# Reproject FIA location points to the CRS of the raster layer
plot_points <- st_transform(plot_points, crs(raster_layer))

# Extract raster values at the plot points
extracted_values <- terra::extract(raster_layer, plot_points)

# Loop over all rasters in the rasters_list and extract values
for (raster_file in names(rasters_list)) {
  raster_layer <- rasters_list[[raster_file]]
  extracted_values[[raster_file]] <- terra::extract(raster_layer, plot_points)
}

# Look at the extracted values as a data frame, getting rid of the duplicative "ID" columns
extracted_df <- extracted_values  |>
  as.data.frame() |>
  dplyr::select(!ends_with(".ID"))

# rename the columns to a more readable column name
new_names <- substr(names(extracted_df), 58, 69)
names(extracted_df) <- new_names

# Check how many non-NA values there are
count_nonNAs <- function(x) {return(sum(!is.na(x)))}
nonNAs_by_year <- apply(extracted_df, 2, count_nonNAs)

# plot number of nonNAs_by_year, using ggplot, and rotating the x labels to vertical
nonNAs_df <- data.frame(year = names(nonNAs_by_year), nonNAs = nonNAs_by_year)

ggplot(nonNAs_df, aes(x = year, y = nonNAs)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of non-NA values by year", x = "Year", y = "Number of non-NA values")



