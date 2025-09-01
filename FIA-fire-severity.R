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

condition_codes <- list(CA_cond, CO_cond, NV_cond, OR_cond, UT_cond, WA_cond, WY_cond, ID_cond, .id = "state")
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


sum(WA_forest_type_combined$DSTRBCD1 == 31, na.rm = TRUE)
sum(WA_forest_type_combined$DSTRBCD1 == 32, na.rm = TRUE)

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

# attach SCIENTIFIC_NAME from REF_SPECIES to SPCD in CA_TREE_fire_only_spp_ID
WA_TREE_fire_only_spp_ID <- WA_TREE_fire_only %>%
  left_join(REF_SPECIES %>% select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

OR_TREE_fire_only_spp_ID <- OR_TREE_fire_only %>%
  left_join(REF_SPECIES %>% select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

ID_TREE_fire_only_spp_ID <- ID_TREE_fire_only %>%
  left_join(REF_SPECIES %>% select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

MT_TREE_fire_only_spp_ID <- MT_TREE_fire_only %>%
  left_join(REF_SPECIES %>% select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

CO_TREE_fire_only_spp_ID <- CO_TREE_fire_only %>%
  left_join(REF_SPECIES %>% select(SPCD, SCIENTIFIC_NAME), by = "SPCD")

# create a list of tree species in fire only plots
WA_TREE_fire_only_spp_list <- unique(WA_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

OR_TREE_fire_only_spp_list <- unique(OR_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

ID_TREE_fire_only_spp_list <- unique(ID_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

MT_TREE_fire_only_spp_list <- unique(MT_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

CO_TREE_fire_only_spp_list <- unique(CO_TREE_fire_only_spp_ID$SCIENTIFIC_NAME)

# can we now take the species lists for each state and find the intersection?
#(i.e. we want a list of total unique species across states)

# merge all states species lists and keep only unique species
total_unique_species <- unique(c(
  CA_TREE_fire_only_spp_list,
  WA_TREE_fire_only_spp_list,
  OR_TREE_fire_only_spp_list,
  ID_TREE_fire_only_spp_list,
  MT_TREE_fire_only_spp_list,
  CO_TREE_fire_only_spp_list
))


#but we want to further filter for plots that have one pre fire survey as well
# lets try this with CA plots first

burned_plots <- cond %>%
  filter(DSTRBCD1 %in% c(30, 31, 32), !is.na(DSTRBYR1)) %>%
  select(PLT_CN, DSTRBYR1) %>%
  distinct()

burned_with_surveys <- burned_plots %>%
  inner_join(plot, by = "PLT_CN") %>%
  filter(INVYR < DSTRBYR1)  # keep only inventories before the burn

preburn_plots <- burned_with_surveys %>%
  distinct(PLT_CN)


# lets look at the understorey too...

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







