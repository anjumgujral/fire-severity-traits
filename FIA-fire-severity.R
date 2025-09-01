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

# then load in condition code files for each state
CA_cond <- read.csv("./data/FIA/CA_COND.csv")
CO_cond <- read.csv("./data/FIA/CO_COND.csv")
NV_cond <- read.csv("./data/FIA/NV_COND.csv")
OR_cond <- read.csv("./data/FIA/OR_COND.csv")
UT_cond <- read.csv("./data/FIA/UT_COND.csv")
WA_cond <- read.csv("./data/FIA/WA_COND.csv")
WY_cond <- read.csv("./data/FIA/WY_COND.csv")

condition_codes <- list(CA_cond, CO_cond, NV_cond, OR_cond, UT_cond, WA_cond, WY_cond, .id = "state")
condition_codes <- as.data.frame(condition_codes)

# filter plots to DSTRBCD1 == 30, 31, or 32, this is selecting all plots with any fire damage [30],
# (from crown and ground fire, either prescribed or natural).
# [31] = ground fire, [32] = canopy fire

# plots with fire only, use a vector of plots from this df to link with tree and veg subplot data
#CA_fire_only <- CA_cond %>%
  #filter(DSTRBCD1 %in% c(30, 31, 32))

# trying this to make sure im pulling plots with at least one fire

CA_fire_only <- CA_cond %>%
  filter(if_any(c(DSTRBCD1, DSTRBCD2, DSTRBCD3), ~ .x %in% c(30, 31, 32)))

# actually lets make sure to pull burned plots across all disturbance codes (DSTRBCD2 and DSTRBCD3 for when
# there are multiple disturbances)
#CA_fire_only <- CA_cond %>%
  #filter(DSTRBCD1 %in% c(30, 31, 32) |
           #DSTRBCD2 %in% c(30, 31, 32) |
           #DSTRBCD3 %in% c(30, 31, 32))

# 600+ plots in ground fire and 400+ plots in canopy fire in CA
ggplot(dplyr::filter(CA_cond, DSTRBCD1 %in% c(30, 31, 32)),
       aes(x = factor(DSTRBCD1))) +
  geom_bar() +
  labs(x = "Disturbance Code", y = "Count",
       title = "Fire-Related Disturbances (Codes 30–32)") +
  theme_minimal()


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

# expand to also include CA mixed conifer, douglas-fir, ponderosa pine,
#incense cedar, sugar pine, jefferyi, pine groups, and white fir, and
# CA black oak groups
forest_type_combined <- CA_fire_only %>%
  filter(FORTYPCD %in% c(371, 201, 221, 222, 224, 225, 261, 922))

sum(forest_type_combined$DSTRBCD1 == 31, na.rm = TRUE)
sum(forest_type_combined$DSTRBCD1 == 32, na.rm = TRUE)

# take the PLT_CN codes from forest_type_combined and filter CA_veg_understory to those PLT_CN's
CA_veg_understory <- CA_P2VEG_SUBPLOT_SPP

CA_veg_understory_fire_only <- CA_veg_understory %>%
  filter(PLT_CN %in% forest_type_combined$PLT_CN)

# take the PLT_CN codes from forest_type_combined and filter CA_TREE
CA_TREE_fire_only <- CA_TREE %>%
  filter(PLT_CN %in% forest_type_combined$PLT_CN)
#467 plots burned in mixed conifer forests



CA_TREE_fire_only <- CA_TREE %>%
  filter(PLT_CN %in% CA_fire_only$PLT_CN)

# attach SCIENTIFIC_NAME from REF_SPECIES to SPCD in CA_TREE_fire_only_spp_ID
CA_TREE_fire_only_spp_ID <- CA_TREE_fire_only %>%
  left_join(REF_SPECIES %>% select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

# create a list of tree species in fire only plots
CA_TREE_fire_only_spp_list <- unique(CA_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

write.csv(CA_TREE_fire_only_spp_list, file = "CA_TREE_fire_only_spp_list.csv")

# attach SCIENTIFIC_NAME from REF_SPECIES to CA_veg_understory_fire_only by matching VEG_SPCD
# to SPECIES_SYMBOL
CA_veg_understory_fire_only_spp_ID <- CA_veg_understory_fire_only %>%
  left_join(REF_SPECIES %>%
              select(SPECIES_SYMBOL, SCIENTIFIC_NAME),
            by = c("VEG_SPCD" = "SPECIES_SYMBOL")) ## this has the be redone, only gives trees


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







