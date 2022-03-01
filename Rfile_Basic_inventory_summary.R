# load required libraries
library(tidyverse)
library(ggplot2)

# Jens Ulrich
# Updated Feb 28, 2022

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
  mutate(difference = n - nn) %>%
  # change level name to fit on the figure page better
  transform(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1=plyr::revalue(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                                                     c("Cereals and pseudocereals"="Cereals, Pseudocereals")))


par(mar=c(4,15,4,4))
barplot(CWR_inventory_summary$n, #main = "Native CWR Taxa in Broad Crop Categories",
        names.arg = CWR_inventory_summary$PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, xlab = "", ylab = "",
        cex.names=1.5, cex.axis=1.5, horiz=T, las=1, xlim = c(0,140))

CWR_inventory_summary_long <- CWR_inventory_summary %>%
  select(-n) %>%
  gather(type, count, difference:nn) %>%
  transform(
    type=plyr::revalue(type, c("difference"="taxa", "nn" = "distinct species"))) %>%
  arrange(desc("taxa")) 

cbp1 <- c("#44AA99", # Cereals
          "#117733", # Fruits
          "#332288", # Herbs and Spices
          "#882255", # Nuts
          "#88CCEE", # Oils
          "#AA4499", # Pulses
          "#CC6677", # Sugars
          "#DDCC77") # Vegetables
  
(ggplot(CWR_inventory_summary_long, aes(
  x = reorder(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, count),
  y = count,
  fill = as.factor(desc(type))))
  #color = as.factor(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1))) 
  +
  geom_bar(position="stack", stat="identity", lwd=1) +
  scale_fill_manual(values = c("gray93", "gray71"), 
                    labels = c("total taxa", "distinct species")) +
  #scale_color_manual(values = cbp1) +
  theme_bw() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14, vjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = c(0.7, 0.2),
        #legend.position = "none") +
        legend.text=element_text(size=12)) +
  labs(y="", x="") + 
  guides(fill=guide_legend(title="")) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  
  
  #, subtitle="Taxon counts for Tier 1 CWR food crop functional categories"
)
