# load required libraries
library(tidyverse)
library(ggplot2)


################################################################################
# # 1 - Load inventory
################################################################################

# Load required data and shapefiles for plotting occurrence maps and data tables
inventory <- read.csv("./Input_Data_and_Files/inventory.csv") %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) %>% 
  mutate(SPECIES = str_replace(SPECIES, "×", "")) 

inventory_finest_taxon_resolution <- inventory %>%
  filter(FINEST_TAXON_RESOLUTION == "Y")

################################################################################
# # 2 - SPECIES AND TAXA IN TIER 1, TIER 2, and WUS   
################################################################################

# inventory summary stats
# number of unique taxa and distinct species
unique_taxa <- nrow(inventory_finest_taxon_resolution)
unique_species <- inventory_finest_taxon_resolution %>%
  distinct(SPECIES, .keep_all = TRUE)
# and again within the 3 main groupings
# number of distinct taxa and distinct species
CWR1 <- inventory_finest_taxon_resolution %>%
  filter(TIER == 1)
CWR1_sp <- unique_species %>%
  filter(TIER == 1)
CWR2 <- inventory_finest_taxon_resolution %>%
  filter(TIER == 2)
CWR2_sp <- unique_species %>%
  filter(TIER == 2)
WUS <- inventory_finest_taxon_resolution %>%
  filter(WUS == "Y")
WUS_sp <- unique_species %>%
  filter(WUS == "Y")

#############################################################
# TABLE 1 - GENEPOOLS, SPECIES, TAXA BY CATEGORY (TIER 1)   #
#############################################################

CWR_inventory_summary <- inventory_finest_taxon_resolution %>%
  filter(CWR == "Y") %>%
  filter(TIER == 1) %>%
  group_by(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) %>%
  add_tally() %>%
  distinct(SPECIES, .keep_all = TRUE) %>%
  add_tally() %>%
  distinct(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, .keep_all = TRUE ) %>%
  arrange(desc(n)) %>%
  dplyr::select(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, n, nn) %>%
  # change level name to fit on the figure page better
  transform(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1=plyr::revalue(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                                                     c("Cereals and pseudocereals"="Cereals, Pseudocereals")))

par(mar=c(4,15,4,4))
barplot(CWR_inventory_summary$n, #main = "Native CWR Taxa in Broad Crop Categories",
        names.arg = CWR_inventory_summary$PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, xlab = "", ylab = "",
        cex.names=1.5, cex.axis=1.5, horiz=T, las=1, xlim = c(0,140))

