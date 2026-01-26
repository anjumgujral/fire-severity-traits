library(sf)
library(terra)
library(dplyr)
library(raster)
library(terra)
library(tidyverse)
library(here)

# load in FIA tree data for western North America (California, Oregon, Washington, Colorado,
# Utah, Nevada, Wyoming) to encompass the Sierra Nevada, Northern and Southern Coast Ranges,
# maybe also include New Mexico and Montana.

# Load points for fuzzed FIA plots
CA <- read.csv("./data/FIA/CA_PLOT.csv")
CO <- read.csv("./data/FIA/CO_PLOT.csv")
NV <- read.csv("./data/FIA/NV_PLOT.csv")
OR <- read.csv("./data/FIA/OR_PLOT.csv")
UT <- read.csv("./data/FIA/UT_PLOT.csv")
WA <- read.csv("./data/FIA/WA_PLOT.csv")
WY <- read.csv("./data/FIA/WY_PLOT.csv")
ID <- read.csv("./data/FIA/ID_PLOT.csv")
MT <- read.csv("./data/FIA/MT_PLOT.csv")

# then load in condition code files for each state
CA_cond <- read.csv("./data/FIA/CA_COND.csv")
CO_cond <- read.csv("./data/FIA/CO_COND.csv")
NV_cond <- read.csv("./data/FIA/NV_COND.csv")
OR_cond <- read.csv("./data/FIA/OR_COND.csv")
UT_cond <- read.csv("./data/FIA/UT_COND.csv")
WA_cond <- read.csv("./data/FIA/WA_COND.csv")
WY_cond <- read.csv("./data/FIA/WY_COND.csv")
ID_cond <- read.csv("./data/FIA/ID_COND.csv")
MT_cond <- read.csv("./data/FIA/MT_COND.csv")

# then load in TREE level data for each state
CA_TREE <- read.csv("./data/FIA/CA_TREE.csv")
CO_TREE <- read.csv("./data/FIA/CO_TREE.csv")
NV_TREE <- read.csv("./data/FIA/NV_TREE.csv")
OR_TREE <- read.csv("./data/FIA/OR_TREE.csv")
UT_TREE <- read.csv("./data/FIA/UT_TREE.csv")
WA_TREE <- read.csv("./data/FIA/WA_TREE.csv")
WY_TREE <- read.csv("./data/FIA/WY_TREE.csv")
ID_TREE <- read.csv("./data/FIA/ID_TREE.csv")
MT_TREE <- read.csv("./data/FIA/MT_TREE.csv")

# Load in reference species names
REF_SPECIES <- read.csv("./data/FIA/REF_SPECIES.csv")


condition_codes <- list(CA_cond, CO_cond, NV_cond, OR_cond, UT_cond, WA_cond, WY_cond, ID_cond, .id = "state")

# filter plots to DSTRBCD1 == 30, 31, or 32, this is selecting all plots with any fire damage [30],
# (from crown and ground fire, either prescribed or natural).
# [31] = ground fire, [32] = canopy fire

# plots with fire only, use a vector of plots from this df to link with tree and veg subplot data
#CA_fire_only <- CA_cond %>%
  #filter(DSTRBCD1 %in% c(30, 31, 32))

# trying this to make sure im pulling plots with at least one fire

CA_fire_only <- CA_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))


# we want to also filter for plots that have one pre fire survey

CA_fire_only_yr <- CA_cond %>%
  dplyr::filter(DSTRBCD1 %in% c(30, 31, 32), !is.na(DSTRBYR1)) %>%
  dplyr::select(PLT_CN, DSTRBYR1, DSTRBCD1, INVYR) %>%
  dplyr::distinct()

all_fire_plot_inventories <- CA_cond %>%
  filter(PLT_CN %in% CA_fire_only_yr$PLT_CN)

all_fire_plot_inventories <- all_fire_plot_inventories %>%
  left_join(CA_fire_only_yr, by = "PLT_CN")

# Flag pre- and post-fire records
CA_fire_only_pre_post_fire_surveys <- all_fire_plot_inventories %>%
  mutate(
    pre_fire = INVYR.x < DSTRBYR1.x,
    post_fire = INVYR.x > DSTRBYR1.x
  )

CA_fire_only_pre_post_fire_surveys2 <- CA_fire_only_pre_post_fire_surveys %>%
  group_by(PLT_CN) %>%
  filter(any(pre_fire) & any(post_fire)) %>%
  ungroup()


# what is the most common forest type in the burned plots?
sort(table(CA_fire_only$FORTYPCD), decreasing = TRUE) # mixed conifer [371]

# extract just plots that burned in the mixed-conifer forest type
forest_type_371 <- CA_fire_only %>%
  filter(FORTYPCD == 371)

# 300+ plots in ground fire, and ~100 plots canopy fire in CA mixed conifer
ggplot(dplyr::filter(forest_type_371, DSTRBCD1 %in% c(30, 31, 32)),
       aes(x = factor(DSTRBCD1))) +
  geom_bar() +
  labs(x = "Disturbance Code", y = "Count",
       title = "Fire-Related Disturbances (Codes 30–32)") +
  theme_minimal()

sum(forest_type_371$DSTRBCD1 == 31, na.rm = TRUE)
sum(forest_type_371$DSTRBCD1 == 32, na.rm = TRUE)

forest_type_371 %>%
  summarise(unique_plots = n_distinct(PLT_CN))

# expand to also include CA mixed conifer, douglas-fir, ponderosa pine,
#incense cedar, sugar pine, jefferyi, pine groups, and white fir, and
# CA black oak groups
forest_type_combined <- CA_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

sum(forest_type_combined$DSTRBCD1 == 31, na.rm = TRUE)
sum(forest_type_combined$DSTRBCD1 == 32, na.rm = TRUE)

forest_type_combined %>%
  summarise(unique_plots = n_distinct(PLT_CN))


# take the PLT_CN codes from forest_type_combined and filter CA_TREE
# forest_type_combined is our key dataframe with unique PLT_CN and their disturbance codes
CA_TREE_fire_only <- CA_TREE %>%
  filter(PLT_CN %in% forest_type_combined$PLT_CN)
#467 plots burned in mixed conifer forests


CA_TREE_fire_only <- CA_TREE %>%
  filter(PLT_CN %in% CA_fire_only$PLT_CN)

# attach SCIENTIFIC_NAME from REF_SPECIES to SPCD in CA_TREE_fire_only_spp_ID
CA_TREE_fire_only_spp_ID <- CA_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

# create a list of tree species in fire only plots
CA_TREE_fire_only_spp_list <- unique(CA_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

write.csv(CA_TREE_fire_only_spp_list, file = "CA_TREE_fire_only_spp_list.csv")


# ok --  lets add some more mixed conifer states and run through the filtering process again
# filter for plots with fire -- ground or canopy fires
WA_fire_only <- WA_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

OR_fire_only <- OR_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

ID_fire_only <- ID_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

MT_fire_only <- MT_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

CO_fire_only <- CO_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

UT_fire_only <- UT_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

WY_fire_only <- WY_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

NV_fire_only <- NV_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

# now filter for all related forest types (see above in CA section for full list)
WA_forest_type_combined <- WA_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

OR_forest_type_combined <- OR_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

ID_forest_type_combined <- ID_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

MT_forest_type_combined <- MT_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

CO_forest_type_combined <- CO_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

UT_forest_type_combined <- UT_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

WY_forest_type_combined <- WY_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

NV_forest_type_combined <- NV_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))


sum(OR_forest_type_combined$DSTRBCD1 == 31, na.rm = TRUE)
sum(OR_forest_type_combined$DSTRBCD1 == 32, na.rm = TRUE)

# now lets aggregate a species list for each state --- bleh!

# take the PLT_CN codes from forest_type_combined and filter STATE_TREE
WA_TREE_fire_only <- WA_TREE %>%
  filter(PLT_CN %in% WA_forest_type_combined$PLT_CN)

OR_TREE_fire_only <- OR_TREE %>%
  filter(PLT_CN %in% OR_forest_type_combined$PLT_CN)

ID_TREE_fire_only <- ID_TREE %>%
  filter(PLT_CN %in% ID_forest_type_combined$PLT_CN)

MT_TREE_fire_only <- MT_TREE %>%
  filter(PLT_CN %in% MT_forest_type_combined$PLT_CN)

CO_TREE_fire_only <- CO_TREE %>%
  filter(PLT_CN %in% CO_forest_type_combined$PLT_CN)

NV_TREE_fire_only <- NV_TREE %>%
  filter(PLT_CN %in% NV_forest_type_combined$PLT_CN)

UT_TREE_fire_only <- UT_TREE %>%
  filter(PLT_CN %in% UT_forest_type_combined$PLT_CN)

WY_TREE_fire_only <- WY_TREE %>%
  filter(PLT_CN %in% WY_forest_type_combined$PLT_CN)

# attach SCIENTIFIC_NAME from REF_SPECIES to SPCD in CA_TREE_fire_only_spp_ID
WA_TREE_fire_only_spp_ID <- WA_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

OR_TREE_fire_only_spp_ID <- OR_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

ID_TREE_fire_only_spp_ID <- ID_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

MT_TREE_fire_only_spp_ID <- MT_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

CO_TREE_fire_only_spp_ID <- CO_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

NV_TREE_fire_only_spp_ID <- NV_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

UT_TREE_fire_only_spp_ID <- UT_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

WY_TREE_fire_only_spp_ID <- WY_TREE_fire_only %>%
  left_join(REF_SPECIES %>% dplyr::select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

# create a list of tree species in fire only plots
WA_TREE_fire_only_spp_list <- unique(WA_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

OR_TREE_fire_only_spp_list <- unique(OR_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

ID_TREE_fire_only_spp_list <- unique(ID_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

MT_TREE_fire_only_spp_list <- unique(MT_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

CO_TREE_fire_only_spp_list <- unique(CO_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

NV_TREE_fire_only_spp_list <- unique(NV_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

UT_TREE_fire_only_spp_list <- unique(UT_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

WY_TREE_fire_only_spp_list <- unique(WY_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

# can we now take the species lists for each state and find the intersection?
#(i.e. we want a list of total unique species across states)

# merge all states species lists and keep only unique species
total_unique_species <- unique(c(
  CA_TREE_fire_only_spp_list,
  WA_TREE_fire_only_spp_list,
  OR_TREE_fire_only_spp_list,
  ID_TREE_fire_only_spp_list,
  MT_TREE_fire_only_spp_list,
  CO_TREE_fire_only_spp_list,
  NV_TREE_fire_only_spp_list,
  UT_TREE_fire_only_spp_list,
  WY_TREE_fire_only_spp_list
))

write.csv(total_unique_species, file = "fire-severity-unique-species-list.csv")

# try plotting locations on a map of the US
# color code by burn severity to see the spatial distribution of high and low severity

#extract plot locations from CA_PLOT or 'CA' for the PLT_CN's on CA_fire_only

CA_burned_plot_coordinates <- CA_TREE_fire_only %>%
  left_join(
    CA %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Convert to sf object using LAT and LON columns
CA_burned_sf <- st_as_sf(
  CA_burned_plot_coordinates,
  coords = c("LON", "LAT"),  # Order matters: (lon, lat)
  crs = 4326  # WGS84 coordinate system
)

# Get US states shapefile
usa_shapefile <- ne_states(country = "United States of America", returnclass = "sf")

# Filter for western states
western_states <- usa_shapefile %>%
  filter(name %in% c(
    "California", "Oregon", "Washington", "Nevada", "Idaho", "Utah",
    "Arizona", "New Mexico", "Montana", "Wyoming", "Colorado"
  ))


# Join disturbance code into your plots dataframe
CA_burned_plot_coordinates_DSTRBCD1 <- CA_burned_plot_coordinates %>%
  left_join(
    forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1, DSTRBYR1),
    by = "PLT_CN"
  )

# Convert to sf with disturbance code included
CA_burned_sf <- st_as_sf(
  CA_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)

ggplot() +
  geom_sf(data = western_states, fill = "gray95", color = "black") +
  geom_sf(
    data = CA_burned_sf %>% filter(DSTRBCD1 %in% c(31, 32)),
    aes(color = factor(DSTRBCD1)),
    size = 1, alpha = 0.8
  ) +
  scale_color_manual(
    values = c("31" = "orange", "32" = "red"),
    labels = c("31" = "Disturbance 31", "32" = "Disturbance 32"),
    name = "Disturbance Code"
  ) +
  coord_sf(xlim = c(-125, -100), ylim = c(30, 45)) +
  theme_minimal() +
  labs(
    title = "Burned plots in low-moderate and high severity",
    x = "Longitude",
    y = "Latitude"
  )

# lets add other states now

# 1. Join coordinates from the state-level plot table
OR_burned_plot_coordinates <- OR_TREE_fire_only %>%
  left_join(
    OR %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
OR_burned_plot_coordinates_DSTRBCD1 <- OR_burned_plot_coordinates %>%
  left_join(
    OR_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1, DSTRBYR1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
OR_burned_sf <- st_as_sf(
  OR_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)


# 1. Join coordinates from the state-level plot table
WA_burned_plot_coordinates <- WA_TREE_fire_only %>%
  left_join(
    WA %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
WA_burned_plot_coordinates_DSTRBCD1 <- WA_burned_plot_coordinates %>%
  left_join(
    WA_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1, DSTRBYR1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
WA_burned_sf <- st_as_sf(
  WA_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)

# 1. Join coordinates from the state-level plot table
ID_burned_plot_coordinates <- ID_TREE_fire_only %>%
  left_join(
    ID %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
ID_burned_plot_coordinates_DSTRBCD1 <- ID_burned_plot_coordinates %>%
  left_join(
    ID_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
ID_burned_sf <- st_as_sf(
  ID_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)


# 1. Join coordinates from the state-level plot table
MT_burned_plot_coordinates <- MT_TREE_fire_only %>%
  left_join(
    MT %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
MT_burned_plot_coordinates_DSTRBCD1 <- MT_burned_plot_coordinates %>%
  left_join(
    MT_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
MT_burned_sf <- st_as_sf(
  MT_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)

# 1. Join coordinates from the state-level plot table
CO_burned_plot_coordinates <- CO_TREE_fire_only %>%
  left_join(
    CO %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
CO_burned_plot_coordinates_DSTRBCD1 <- CO_burned_plot_coordinates %>%
  left_join(
    CO_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
CO_burned_sf <- st_as_sf(
  CO_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)

# 1. Join coordinates from the state-level plot table
UT_burned_plot_coordinates <- UT_TREE_fire_only %>%
  left_join(
    UT %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
UT_burned_plot_coordinates_DSTRBCD1 <- UT_burned_plot_coordinates %>%
  left_join(
    UT_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
UT_burned_sf <- st_as_sf(
  UT_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)


# 1. Join coordinates from the state-level plot table
WY_burned_plot_coordinates <- WY_TREE_fire_only %>%
  left_join(
    WY %>% dplyr::select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
WY_burned_plot_coordinates_DSTRBCD1 <- WY_burned_plot_coordinates %>%
  left_join(
    WY_forest_type_combined %>% dplyr::select(PLT_CN, DSTRBCD1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
WY_burned_sf <- st_as_sf(
  WY_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)

###### this chunk for NV didnt work, see below instead
# 1. Join coordinates from the state-level plot table
NV_burned_plot_coordinates <- NV_TREE_fire_only %>%
  left_join(
    NV %>% select(CN, LAT, LON),
    by = c("PLT_CN" = "CN")
  )

# 2. Join in the disturbance code from forest_type_combined
NV_burned_plot_coordinates_DSTRBCD1 <- NV_burned_plot_coordinates %>%
  left_join(
    NV_forest_type_combined %>% select(PLT_CN, DSTRBCD1),
    by = "PLT_CN"
  )

# 3. Convert to sf object with disturbance info
NV_burned_sf <- st_as_sf(
  NV_burned_plot_coordinates_DSTRBCD1,
  coords = c("LON", "LAT"),
  crs = 4326
)

######
NV_fire_coords <- NV_fire_only %>%
  left_join(NV %>% dplyr::select(CN, LAT, LON), by = c("PLT_CN" = "CN"))

NV_fire_sf <- st_as_sf(NV_fire_coords, coords = c("LON", "LAT"), crs = 4326)



NV_burned_plot_coordinates <- NV_fire_only %>%
  left_join(NV %>% dplyr::select(CN, LAT, LON), by = c("PLT_CN" = "CN"))

# Step 3: Convert to sf object
NV_burned_sf <- st_as_sf(NV_burned_plot_coordinates, coords = c("LON", "LAT"), crs = 4326)

normalize_types <- function(x) {
  dplyr::mutate(
    x,
    dplyr::across(
      where(~ !inherits(.x, "sfc")),
      as.character
    )
  )
}



all_burned_sf <- dplyr::bind_rows(
  normalize_types(CA_burned_sf),
  normalize_types(OR_burned_sf),
  normalize_types(WA_burned_sf),
  normalize_types(ID_burned_sf),
  normalize_types(MT_burned_sf),
  normalize_types(CO_burned_sf),
  normalize_types(UT_burned_sf),
  normalize_types(NV_burned_sf),
  normalize_types(WY_burned_sf)
)




ggplot() +
  geom_sf(data = western_states, fill = "gray95", color = "black") +
  geom_sf(
    data = all_burned_sf %>% filter(DSTRBCD1 %in% c(31, 32)),
    aes(color = factor(DSTRBCD1)),
    size = 1, alpha = 0.8
  ) +
  scale_color_manual(
    values = c("31" = "orange", "32" = "red"),
    name = "Disturbance Code"
  ) +
  coord_sf(xlim = c(-125, -100), ylim = c(30, 50)) +
  theme_minimal() +
  labs(
    x = "Longitude",
    y = "Latitude"
  )


# lets visualize some summary stats about our burned and unburned plots

h1 <- ggplot(
  data = CA_burned_plot_coordinates_DSTRBCD1 %>%
    filter(DSTRBYR1 <= 2025) %>%
    group_by(PLT_CN) %>%
    slice_min(DSTRBYR1, with_ties = FALSE) %>%
    ungroup(),  # keep one row per unique plot
  aes(x = DSTRBYR1)
) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "black", fill = "steelblue") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 1)) +
  labs(
    title = "Histogram of Disturbance Years in CA (using first year fire was recorded)",
    x = "Disturbance Year",
    y = "Count (Unique PLT_CN)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

h1

h2 <- ggplot(
  data = OR_burned_plot_coordinates_DSTRBCD1 %>%
    filter(DSTRBYR1 >= 1990, DSTRBYR1 <= 2025) %>%
    group_by(PLT_CN) %>%
    slice_min(DSTRBYR1, with_ties = FALSE) %>%
    ungroup(),
  aes(x = DSTRBYR1)
) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "black", fill = "steelblue") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 1)) +
  labs(
    title = "Histogram of Disturbance Years in OR (using first year fire was recorded)",
    x = "Disturbance Year",
    y = "Count (Unique Plots)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


h2



h3 <- ggplot(
  data = WA_burned_plot_coordinates_DSTRBCD1 %>%
    filter(DSTRBYR1 <= 2025) %>%
    group_by(PLT_CN) %>%
    slice_min(DSTRBYR1, with_ties = FALSE) %>%
    ungroup(),  # keep one row per unique plot
  aes(x = DSTRBYR1)
) +
  geom_histogram(binwidth = 1, boundary = 1990, color = "black", fill = "steelblue") +
  scale_x_continuous(breaks = seq(1990, 2025, by = 1)) +
  labs(
    title = "Histogram of Disturbance Years in WA (using first year fire was recorded)",
    x = "Disturbance Year",
    y = "Count (Unique Plots)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



h3
library('gridExtra')
grid.arrange (h1, h2, h3)

h4 <- ggplot(
  data = CA_burned_plot_coordinates_DSTRBCD1 %>%
    filter(!is.na(DSTRBYR1), DSTRBYR1 >= 1990, DSTRBYR1 <= 2025) %>%
    group_by(PLT_CN) %>%
    slice_min(DSTRBYR1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(TimeSinceFire = 2025 - DSTRBYR1),
  aes(x = TimeSinceFire)
) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", fill = "darkorange") +
  scale_x_continuous(breaks = seq(0, 35, by = 1)) +
  labs(
    title = "CA",
    x = "Time Since Fire (Years)",
    y = "Count (Unique Plots)"
  ) +
  theme_minimal()
h4

h5 <- ggplot(
  data = OR_burned_plot_coordinates_DSTRBCD1 %>%
    filter(!is.na(DSTRBYR1), DSTRBYR1 >= 1990, DSTRBYR1 <= 2025) %>%
    group_by(PLT_CN) %>%
    slice_min(DSTRBYR1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(TimeSinceFire = 2025 - DSTRBYR1),
  aes(x = TimeSinceFire)
) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", fill = "darkorange") +
  scale_x_continuous(breaks = seq(0, 35, by = 1)) +
  labs(
    title = "OR",
    x = "Time Since Fire (Years)",
    y = "Count (Unique Plots)"
  ) +
  theme_minimal()

h5

h6 <- ggplot(
  data = WA_burned_plot_coordinates_DSTRBCD1 %>%
    filter(!is.na(DSTRBYR1), DSTRBYR1 >= 1990, DSTRBYR1 <= 2025) %>%
    group_by(PLT_CN) %>%
    slice_min(DSTRBYR1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(TimeSinceFire = 2025 - DSTRBYR1),
  aes(x = TimeSinceFire)
) +
  geom_histogram(binwidth = 1, boundary = 0, color = "black", fill = "darkorange") +
  scale_x_continuous(breaks = seq(0, 35, by = 1)) +
  labs(
    title = "WA",
    x = "Time Since Fire (Years)",
    y = "Count (Unique Plots)"
  ) +
  theme_minimal()

h6

grid.arrange (h4, h5, h6)


# 600+ plots in ground fire and 400+ plots in canopy fire in CA
ggplot(dplyr::filter(CA_cond, DSTRBCD1 %in% c(30, 31, 32)),
       aes(x = factor(DSTRBCD1))) +
  geom_bar() +
  labs(x = "Disturbance Code", y = "Count",
       title = "Fire-Related Disturbances (Codes 30–32)") +
  theme_minimal()





# lets develop a workflow for extracting climate for filtered plot locations











# lets look at the understory too...

# attach SCIENTIFIC_NAME from REF_SPECIES to CA_veg_understory_fire_only by matching VEG_SPCD
# to SPECIES_SYMBOL
CA_veg_understory_fire_only_spp_ID <- CA_veg_understory_fire_only %>%
  left_join(REF_SPECIES %>%
              select(SPECIES_SYMBOL, SCIENTIFIC_NAME),
            by = c("VEG_SPCD" = "SPECIES_SYMBOL")) ## this has the be redone, only gives trees

# take the PLT_CN codes from forest_type_combined and filter CA_veg_understory to those PLT_CN's
CA_veg_understory <- CA_P2VEG_SUBPLOT_SPP

CA_veg_understory_fire_only <- CA_veg_understory %>%
  filter(PLT_CN %in% forest_type_combined$PLT_CN)

# CA_veg_understory_fire_only uses VEG_SPCD instead of SPCD



# create a list of understory species in fire only plots



# how many resurveys do fire only mixed conifer plots in CA have?
resurveyed_fire_only_plots <- CA_TREE_fire_only %>%
  group_by(PLT_CN) %>%
  summarise(n_years = n_distinct(INVYR))
# all plots only have 1 visit?

resurveyed_fire_only_plots <- CA_TREE_fire_only %>%
  group_by(PLT_CN) %>%
  summarise(INVYR_list = list(sort(unique(INVYR))))






