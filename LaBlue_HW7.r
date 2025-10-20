###################
### ZOO800: HW7 ###
###################

# Author: Rebekkah LaBlue (Partner: Samantha Summerfield)
# Assignment: Maps with ggplot
# Due: October 20, 2025


##############
### SET UP ###
##############

### --- LOAD PACKAGES --- ###

library(dplyr)
library(ggplot2)
library(viridis)
library(lubridate)
library(readr)
library(sf)
library(magrittr)
library(tidyr)
library(purrr)
library(units)


###############
### MAPPING ###
###############

### --- SUMMARY --- ###

### My data comes from my MS project exploring the efficacy of protected areas in the Upper Great Lakes Region
# under climate change. My data set is avian detection/non-detection data from the Wisconsin Breeding Bird Atlas
# project, a participatory science initiative for which there are two Atlas periods: Atlas 1 (1995-2000) and
# Atlas 2 (2015-2019). The Atlas sampling protocol parcels the entire state into standardized grid cells: "blocks"
# with unique alphanumeric IDs each approx. 25 km^2 in area; species detection data is aggregated at the block level.

### My workflow 1) takes the full Atlas block grid map and filters it to only the blocks that I've deemed comparable
# (through other analysis) across the two Atlas periods; 2) creates flexible code to filter my complete, zero-filled
# data set of 200+ species to a single species; 3) uses a single species' detection/non-detection data to map 
# "transition" across blocks from the first to the second Atlas period. (ie. Colonization: not detected in block 'A' in
# Atlas 1, detected in Atlas 2 [0, 1]; Extinction: detected in block 'B' in Atlas 1, not detected in Atlas 2 [1, 0], etc.).


### --- MAP DATA --- ###

# Load, create maps
blocks_shp <- st_read("data/maps/wibba blocks/Wisconsin_Breeding_Bird_Atlas_Blocks.shp") %>% # load full block map
  rename(atlas_block = BLOCK_ID)

wibba_summary_comp <- read.csv("data/wibba_summary.csv")
blocks_comp <- wibba_summary_comp$atlas_block # create list of designated comparable blocks from full set

blocks_comp_shp <- blocks_shp %>% # create sf object of filtered comparable blocks
  filter(atlas_block %in% blocks_comp)
st_write(blocks_comp_shp, "outputs/maps/blocks_comp.shp", delete_dsn = TRUE) # create shp file for comp blocks


### --- SPECIES DATA --- ###

# Reproducible for any species 
species_to_plot <- "Bobolink"

# Join species data to blocks shp data by block ID
blocks_species <- blocks_comp_shp %>%
  left_join(
    breeders_zf_summary %>%
      filter(common_name == species_to_plot),
    by = "atlas_block"
  )


### --- PLOT --- ###

vir_colors <- viridis::viridis(3) # make color blind-friendly palette

ggplot(blocks_species) +
  geom_sf(aes(fill = transition_state), size = 0.1) +
  scale_fill_manual( # hacking manual fill with viridis palette
    values = c(
      "Colonization" = vir_colors[3], # assign accessible colors to states of interest
      "Persistence"  = vir_colors[2],
      "Extinction"   = vir_colors[1],
      "Absence"      = "white" # force absence to white so it doesn't distract
    ),
    breaks = c("Colonization", "Persistence", "Extinction"), # show only states of relevance
    drop = FALSE
  ) +
  labs(
    fill = "Transition State",
    caption = "Figure 1. Map of state transitions for the Bobolink (Dolichonyx oryzivorus) across comparable survey blocks using data from the Wisconsin Breeding Bird Atlas. \nTransitions reflect detection/non-detection data aggregated at the block level (~25 km^2) across two survey periods: 1995-2000 (Atlas 1) and 2015-2019 (Atlas 2). \nWhite blocks denote those grid cells part of the comparable pool but in which the species was not detected in either Atlas period."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 13, margin = margin(b = 10)),
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 20))
  )