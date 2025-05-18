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

CA_fire_only <- CA_cond %>%
  filter(DSTRBCD1 %in% c(30, 31, 32))

# extract just plots that burned in the mixed-conifer forest type
forest_type_371 <- CA_fire_only %>%
  filter(FORTYPCD == 371)

length(CA_cond$MIXEDCONFCD[CA_cond$MIXEDCONFCD != ""])


# eventually filter tree- or vegetation-level tables using the filtered PLOT data



