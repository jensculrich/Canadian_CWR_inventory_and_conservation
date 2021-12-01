# INTRO (EDIT THIS)
# Use the species distribution data and the garden/genebank accession data
# to explore the inventory itself
# and more particularly
# the strengths and gaps in ex situ conservation of the CWR and WUS in the inventory


# load required libraries
library(tidyverse)
library(ggplot2)
library(sf) # the base package manipulating shapes
library(geojsonio) # geo json input and output
library(ggridges) # needed for ridgeline plot in fig 4
library(tmap) # libary for drawing spatial figures - i.e. fig 2
library(tigris) # spatial joins between sf's and df
library(gridExtra) # panelling figures
library(scatterpie) # pie charts for map
library(ggnewscale) # for mixing continuous and discrete fill scales on a map

############
# CONTENTS #
############

# 1 - Load inventory and spatial data; species distribution and garden accessions 
# 2 - Compute some basic summary stats about the inventory   
# 3 - Determine patterns of species distributions  
# 4 - Load garden data format for gap analyses
# Figure 1: Ex situ conservation of each crop group, and in gardens v genebanks
# Figure 2: National geographic gaps of wild-orgin accessions; A = CWR, B = WUS
            # (wild origin accessions per region versus species distr density per region)
# Supplementary Figure: Figure 2 but by province (or ecoregion, whichever is not used)
# Figure 3: Geographic gap analysis: proportion of range conserved in different groups
            # A: CWR Tier 1
            # B: CWR Tier 2
            # C: WUS
# Supplementary Figure: Figure 3 but by province (or ecoregion, whichever is not used) 
# Figure 4: Case Study
# Figure 5: ? Or maybe we add a table of some sort?

################################################################################
# # 1 - Load inventory and spatial data     
################################################################################

# Load required data and shapefiles for plotting occurrence maps and data tables
inventory <- read.csv("./Input_Data_and_Files/inventory.csv") %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) %>% 
  mutate(SPECIES = str_replace(SPECIES, "×", ""))

canada_ecoregions_geojson <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE)
# rename PROVINCE in shapefile (needs to match the species distribution table)
canada_provinces_geojson <- canada_provinces_geojson %>%
  rename("PROVINCE" = "name")


######################################################################
# Reformat (add taxon info) and Project Species Distribution Tables  #

sp_distr_ecoregion <- as_tibble(read.csv("./GBIF_download_outputs/species_distributions_ecoregion_trimmed.csv"))
sp_distr_province <- as_tibble(read.csv("./GBIF_download_outputs/species_distributions_province_trimmed.csv"))

# add geometry to the species distribution table
sp_distr_province_sf <- merge(x = sp_distr_province, 
                               y = canada_provinces_geojson[ , c("PROVINCE", "geometry")], 
                               by = "PROVINCE", all.x=TRUE)

# join with inventory to add taxon information (category, crop relative, IUCN, etc.)
sp_distr_province_sf <- merge(x = sp_distr_province_sf,
                               y = inventory[ , c("TAXON",
                                            "PRIMARY_ASSOCIATED_CROP_COMMON_NAME",
                                            "SECONDARY_ASSOCIATED_CROP_COMMON_NAME",
                                            "CWR", "WUS", "NATIVE",
                                            "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1",
                                            "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2",
                                            "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1",
                                            "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2",
                                            "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3",
                                            "SECONDARY_CROP_OR_WUS_USE_1",
                                            "CATEGORY", "TIER", "GENEPOOL")],
                               by = c("TAXON"),
                               all.x=TRUE)

# Now ecoregion species distributions
# add geometry to the species distribution table
sp_distr_ecoregion_sf <- merge(x = sp_distr_ecoregion, 
                              y = canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")], 
                              by = "ECO_NAME", all.x=TRUE)

# join with inventory to add taxon information (category, crop relative, IUCN, etc.)
sp_distr_ecoregion_sf <- merge(x = sp_distr_ecoregion_sf,
                              y = inventory[ , c("TAXON",
                                                 "PRIMARY_ASSOCIATED_CROP_COMMON_NAME",
                                                 "SECONDARY_ASSOCIATED_CROP_COMMON_NAME",
                                                 "CWR", "WUS", "NATIVE",
                                                 "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1",
                                                 "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2",
                                                 "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1",
                                                 "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2",
                                                 "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3",
                                                 "SECONDARY_CROP_OR_WUS_USE_1",
                                                 "CATEGORY", "TIER", "GENEPOOL")],
                              by = c("TAXON"),
                              all.x=TRUE)


################################################################################
# # 2 - Compute some basic summary stats about the inventory   
################################################################################

##############################
# CWRs in each category ######
##############################
# end result is a bar chart that displays the number of CWR in each category
# how to show relative to the number of crop groups?

CWR_inventory_summary <- inventory %>%
  filter(CWR == "Y") %>%
  filter(TIER == 1) %>%
  group_by(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) %>%
  add_tally() %>%
  distinct(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, .keep_all = TRUE ) %>%
  arrange(desc(n)) %>%
  dplyr::select(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, n) %>%
  # change level name to fit on the figure page better
  transform(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1=plyr::revalue(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
    c("Cereals and pseudocereals"="Cereals, Pseudocereals")))
 
par(mar=c(4,15,4,4))
barplot(CWR_inventory_summary$n, #main = "Native CWR Taxa in Broad Crop Categories",
        names.arg = CWR_inventory_summary$PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, xlab = "", ylab = "",
        cex.names=1.5, cex.axis=1.5, horiz=T, las=1, xlim = c(0,140))

################################################################################
# # 3 - Determine patterns of species distributions (re-used later for Fig 2)   
################################################################################
# end result -> identify richness of total and endemic CWR and WUS
# in each region. Will be used later for Figure 2
# the histograms plots (P, Q, R, and S) are just to confirm that the data manipulation
# produced the results we want to see i.e. a number of taxa per region
# can also identify the 'hotspots' - areas with the most CWR and WUS using the resulting

# CWR per ecoregion
total_and_endemic_CWRs_ecoregion <- sp_distr_ecoregion_sf %>%
  # count total CWRs (unique TAXON in each province) - only tier 1?
  # want the rows where CWR is Y (just the CWR)
  filter(CWR == "Y") %>%
  filter(TIER == "1") %>%
  # group by ecoregion
  group_by(ECO_NAME) %>%
  # tally the number of unique CWR TAXA
  distinct(TAXON, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(total_CWRs_in_ecoregion = "n") %>%
  mutate(total_CWRs_in_ecoregion = as.numeric(total_CWRs_in_ecoregion)) %>%
  ungroup() %>%
  
  # count endemic CWRs (species that occurs in only 1 ecoregion)
  group_by(TAXON) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_ecoregions_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_ecoregions_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(ECO_NAME) %>%
  mutate(endemic_CWRs_in_ecoregion = sum(is_endemic))

# just want number of CWRS in each region
# for a histogram and to easily see ranked list of top ecoregions
# by total CWRs:
total_CWRs_group_by_ecoregion <- total_and_endemic_CWRs_ecoregion %>% 
  distinct(ECO_NAME, .keep_all = TRUE ) %>%
  arrange(desc(total_CWRs_in_ecoregion))
# and by endemic CWRs:
total_CWRs_group_by_ecoregion <- total_CWRs_group_by_ecoregion %>% 
  arrange(desc(endemic_CWRs_in_ecoregion))

# Plot number CWRs in each province (as a histogram)
P <- ggplot(total_CWRs_group_by_ecoregion, aes(x = total_CWRs_in_ecoregion)) + theme_bw() + 
  geom_histogram()
P

# CWR per province
total_and_endemic_CWRs_province <- sp_distr_province_sf %>%
  # count total CWRs (unique TAXON in each province) - only tier 1?
  # want the rows where CWR is Y (just the CWR)
  filter(CWR == "Y") %>%
  filter(TIER == "1") %>%
  # group by province
  group_by(PROVINCE) %>%
  # tally the number of unique CWR species
  distinct(TAXON, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(total_CWRs_in_province = "n") %>%
  mutate(total_CWRs_in_province = as.numeric(total_CWRs_in_province)) %>%
  ungroup() %>%
  
  # count endemic CWRs (species that occurs in only 1 province)
  group_by(TAXON) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_provinces_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_provinces_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(PROVINCE) %>%
  mutate(endemic_CWRs_in_province = sum(is_endemic))

# just want number of CWRS in each region
# for a histogram and to easily see ranked list of top ecoregions
# by total CWRs:
total_CWRs_group_by_province <- total_and_endemic_CWRs_province %>% 
  distinct(PROVINCE, .keep_all = TRUE ) %>%
  arrange(desc(total_CWRs_in_province))
# and by endemic CWRs:
total_CWRs_group_by_province <- total_CWRs_group_by_province %>% 
  arrange(desc(endemic_CWRs_in_province))

# Plot number CWRs in each province (as a histogram)
Q <- ggplot(total_CWRs_group_by_province, aes(x = total_CWRs_in_province)) + theme_bw() + 
  geom_histogram()
Q

#########################################
# repeat for WUS 

# WUS per ecoregion
total_and_endemic_WUS_ecoregion <- sp_distr_ecoregion_sf %>%
  # count total WUS (unique TAXON in each province) - only tier 1?
  # want the rows where CWR is Y (just the WUS)
  filter(WUS == "Y") %>%
  filter(TIER == "1") %>%
  # group by ecoregion
  group_by(ECO_NAME) %>%
  # tally the number of unique CWR TAXA
  distinct(TAXON, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(total_WUS_in_ecoregion = "n") %>%
  mutate(total_WUS_in_ecoregion = as.numeric(total_WUS_in_ecoregion)) %>%
  ungroup() %>%
  
  # count endemic WUS (species that occurs in only 1 ecoregion)
  group_by(TAXON) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_ecoregions_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_ecoregions_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(ECO_NAME) %>%
  mutate(endemic_WUS_in_ecoregion = sum(is_endemic))

# just want number of WUS in each region
# for a histogram and to easily see ranked list of top ecoregions
# by total WUS:
total_WUS_group_by_ecoregion <- total_and_endemic_WUS_ecoregion %>% 
  distinct(ECO_NAME, .keep_all = TRUE ) %>%
  arrange(desc(total_WUS_in_ecoregion))
# and by endemic WUS:
total_WUS_group_by_ecoregion <- total_WUS_group_by_ecoregion %>% 
  arrange(desc(endemic_WUS_in_ecoregion))

# Plot number WUS in each province (as a histogram)
R <- ggplot(total_WUS_group_by_ecoregion, aes(x = total_WUS_in_ecoregion)) + theme_bw() + 
  geom_histogram()
R

# WUS per province
total_and_endemic_WUS_province <- sp_distr_province_sf %>%
  # count total WUS (unique TAXON in each province) - only tier 1?
  # want the rows where WUS is Y (just the WUS)
  filter(WUS == "Y") %>%
  filter(TIER == "1") %>%
  # group by province
  group_by(PROVINCE) %>%
  # tally the number of unique WUS species
  distinct(TAXON, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(total_WUS_in_province = "n") %>%
  mutate(total_WUS_in_province = as.numeric(total_WUS_in_province)) %>%
  ungroup() %>%
  
  # count endemic WUS (species that occurs in only 1 province)
  group_by(TAXON) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_provinces_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_provinces_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(PROVINCE) %>%
  mutate(endemic_WUS_in_province = sum(is_endemic))

# just want number of WUS in each region
# for a histogram and to easily see ranked list of top ecoregions
# by total WUS:
total_WUS_group_by_province <- total_and_endemic_WUS_province %>% 
  distinct(PROVINCE, .keep_all = TRUE ) %>%
  arrange(desc(total_WUS_in_province))
# and by endemic WUS:
total_WUS_group_by_province <- total_WUS_group_by_province %>% 
  arrange(desc(endemic_WUS_in_province))

# Plot number WUS in each province (as a histogram)
S <- ggplot(total_WUS_group_by_province, aes(x = total_WUS_in_province)) + theme_bw() + 
  geom_histogram()
S


# inventory summary stats
unique_taxa <- nrow(inventory)
unique_species <- inventory %>%
  distinct(SPECIES, .keep_all = TRUE)
CWR1 <- inventory %>%
  filter(TIER == 1)
CWR2 <- inventory %>%
  filter(TIER == 2)
WUS <- inventory %>%
  filter(WUS == "Y")
CWR1_sp <- unique_species %>%
  filter(TIER == 1)
CWR2_sp <- unique_species %>%
  filter(TIER == 2)
WUS_sp <- unique_species %>%
  filter(WUS == "Y")

################################################################################
# # 4 - Load garden data format for gap analyses
################################################################################

##########
# compile garden data and append ecoregion or province of the origin for
# wild-collected accessions,
# when lat/long was given

# load data from garden collections (already filtered to only CWRs)
# update and add new gardens as we receive additional datasets
cwr_ubc <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_UBC.csv", na.strings=c("","NA"))
cwr_rbg <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_RBG.csv", na.strings=c("","NA"))
cwr_montreal <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_montreal.csv", na.strings=c("","NA"))
cwr_guelph <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_guelph.csv", na.strings=c("","NA"))
cwr_mountp <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_mpg.csv", na.strings=c("","NA"))
cwr_vandusen <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_van_dusen.csv", na.strings=c("","NA"))
cwr_usask <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_sask.csv") 
cwr_readerrock <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_reader_rock.csv", na.strings=c("","NA"))
cwr_PGRC <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_pgrc.csv", na.strings=c("","NA"))
cwr_NPGS <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_usda.csv", na.strings=c("","NA"))
cwr_PGRC_usa <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_pgrc_usa.csv", na.strings=c("","NA"))
cwr_NPGS_usa <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_usda_usa.csv", na.strings=c("","NA"))


# join all garden data into one long table
# update and add new gardens as we receive additional datasets
garden_accessions <- rbind(cwr_ubc, cwr_rbg, cwr_guelph, cwr_montreal, 
                           cwr_mountp, cwr_vandusen, cwr_usask,
                           cwr_readerrock, 
                           cwr_PGRC, cwr_NPGS, 
                           cwr_PGRC_usa, cwr_NPGS_usa)

garden_accessions <- garden_accessions %>% # format columns
  mutate(latitude = as.numeric(LATITUDE), 
         longitude = as.numeric(LONGITUDE)) # %>%

# Transform garden data into a projected shape file
sf_garden_accessions <- garden_accessions %>%
  # na.fail = FALSE to keep all of the accessions (about 80% don't have lat long,
  # but many of these have province at least)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE)

###########################################################
# Join with species distributions to conduct gap analyses #
###########################################################

# Append Province to accession using lat and longitude
# spatial join to add accession province
points_polygon <- st_join(sf_garden_accessions, canada_provinces_geojson, left = TRUE)
# spatial join to add accession ecoregion
points_polygon_2 <- st_join(points_polygon, canada_ecoregions_geojson, left = TRUE)

# GO BACK AND REDO THIS JOIN BY NEAREST NEIGHBOR!!! 
# WILL ALSO NEED TO REMOVE COLLECTIONS FROM OUTSIDE CANADA FOR THE GAP ANALYSIS FIGUREs/APP

# break out new latitude and longitude columns and reformat
all_garden_accessions_shapefile <- points_polygon_2 %>%
  # break coordinates into lat/long
  mutate(longitude=gsub("\\,.*","", geometry)) %>%
  mutate(latitude=gsub(".*,","",geometry)) %>%
  # format to remove "c(" and  ")"
  mutate(longitude = as.numeric(str_sub(longitude, 3)))  %>% 
  mutate(latitude = as.numeric(str_remove(latitude, "[)]"))) %>% 
  
  # select columns that match garden accessions
  dplyr::select(TAXON, 
                #GENUS, SPECIES, RANK, INFRASPECIFIC, 
                #PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                #SECONDARY_ASSOCIATED_CROP_COMMON_NAME,
                #PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1,
                #PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2,
                #SECONDARY_ASSOCIATED_CROP_TYPE_GENERAL,
                #PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                #PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                #PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                #FINEST_TAXON_RESOLUTION, CWR, WUS, CATEGORY, TIER, 
                PROVENANCE, COUNTRY, LOCALITY,
                PROVINCE.x, PROVINCE.y, ECO_NAME,
                latitude, longitude,
                GARDEN_CODE, INSTITUTION) %>%
                # IUCNRedList/conservation_status) %>%
  # take province from cd_canada unless was already provided by garden (just want one column)
  mutate(province = ifelse(is.na(PROVINCE.x), PROVINCE.y, PROVINCE.x)) %>%
  dplyr::select(-PROVINCE.y, - PROVINCE.x) %>%
  left_join(inventory[,c("TAXON", "SPECIES", "TIER", "CWR", "WUS")])


# gardens often give province but no lat/long (including all PGRC)
accessions_w_province_but_no_geo_data <- all_garden_accessions_shapefile %>%
  filter(!is.na(province)) %>%
  filter(is.na(latitude))
# the province layers don't always catch coastal/island collections bounded by ecoregion
# manually edit these accessions afterwards?
accessions_w_ecoregion_but_no_province <- all_garden_accessions_shapefile %>%
  filter(!is.na(ECO_NAME)) %>%
  filter(is.na(province))
accessions_lat_long <- all_garden_accessions_shapefile %>%
  filter(!is.na(latitude))

TIER1 <- all_garden_accessions_shapefile %>%
  filter(TIER == 1)
TIER2 <- all_garden_accessions_shapefile %>%
  filter(TIER == 2)
WUS <- all_garden_accessions_shapefile %>%
  filter(WUS == "Y")
BG <- all_garden_accessions_shapefile %>%
  filter(INSTITUTION == "BG")
G <- all_garden_accessions_shapefile %>%
  filter(INSTITUTION == "G")
unique_CWR_repped_species <- TIER1 %>%
  distinct(SPECIES)
unique_CWR2_repped_species <- TIER2 %>%
  distinct(SPECIES)
unique_WUS_repped_species <- WUS %>%
  distinct(SPECIES)


# for now, want to get rid of collections from outside Canada
# all_garden_accessions_shapefile <- all_garden_accessions_shapefile %>%
#  filter(COUNTRY == "Canada" | COUNTRY == "United States" |
#           COUNTRY == "United States of America" |
#           is.na(COUNTRY))

# now join the garden data with the sp distributions by province
# to expand each row where a taxon/ species occurs
# for taxon level use these:
province_gap_table <- sp_distr_province[ , c("TAXON", "PROVINCE")] %>%
  rename("province" = "PROVINCE") %>%
  merge(x = ., y = all_garden_accessions_shapefile,
        by = c("TAXON", "province"),
        all = TRUE) %>%
  select(-ECO_NAME, -COUNTRY) %>%
  full_join(inventory)

ecoregion_gap_table <- sp_distr_ecoregion[ , c("TAXON", "ECO_NAME")] %>%
  merge(x = ., y = all_garden_accessions_shapefile,
        by = c("TAXON", "ECO_NAME"),
        all = TRUE) %>%
  select(-province, -COUNTRY) %>%
  full_join(inventory)

# for species level use these:
inventory_sp <- inventory %>%
  select(-TAXON, -GENUS, -RANK, -INFRASPECIFIC) %>%
  distinct(SPECIES, .keep_all = TRUE)
sp_distr_province_sp <- sp_distr_province %>%
  # remove duplicates at province within species
  distinct(SPECIES, PROVINCE, .keep_all=TRUE)
province_gap_table_species <- sp_distr_province_sp[ , c("SPECIES", "PROVINCE")] %>%
  rename("province" = "PROVINCE") %>%
  merge(x = ., y = all_garden_accessions_shapefile,
        by = c("SPECIES", "province"),
        all = TRUE) %>%
  select(-ECO_NAME, -COUNTRY, -TAXON) %>%
  full_join(inventory_sp)

sp_distr_ecoregion_sp <- sp_distr_ecoregion %>%
  # remove duplicates at province within species
  distinct(SPECIES, ECO_NAME, .keep_all=TRUE)
ecoregion_gap_table_species <- sp_distr_ecoregion_sp[ , c("SPECIES", "ECO_NAME")] %>%
  merge(x = ., y = all_garden_accessions_shapefile,
        by = c("SPECIES", "ECO_NAME"),
        all = TRUE) %>%
  select(-province, -COUNTRY) %>%
  full_join(inventory_sp)

##################################################################
# # FIGURE 1 # # # # # # # # # # # # # # #
##################################################################

# group by species (or taxon), calc number of rows where GARDEN_CODE !(is.na)
# GARDEN_CODE !(is.na) reduces the gap table to just the garden accessions 
# then group by INSTITUTION (within species or taxon)
num_accessions <- province_gap_table_species %>%
  group_by(SPECIES) %>%
  mutate(total_accessions = sum(!is.na(GARDEN_CODE))) %>%
  mutate(garden_accessions = sum(!is.na(GARDEN_CODE) & 
                                   INSTITUTION == "BG")) %>%
  mutate(genebank_accessions = sum(!is.na(GARDEN_CODE) & 
                                   INSTITUTION == "G")) %>%
  distinct(SPECIES, .keep_all = TRUE)
  
num_accessions_cwr <- num_accessions %>%
  filter(TIER == 1) %>%
  mutate(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = as.factor(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) %>%
  group_by(PRIMARY_ASSOCIATED_CROP_COMMON_NAME) %>%
  mutate(mean = mean(total_accessions))



F1A <- ggplot(num_accessions_cwr, 
             aes(x = reorder(PRIMARY_ASSOCIATED_CROP_COMMON_NAME, total_accessions), 
                 y = total_accessions,
                 color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_jitter(shape=16, position=position_jitter(0.1)) +
  facet_grid(cols = vars(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1), scales = "free_x", space = "free_x") +
  stat_summary(fun=mean, geom="point", shape='-', size= 8, color="black", fill="black") +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.spacing.x = unit(.1, "cm"),
        strip.text.x = element_blank(),
        axis.text.y  = element_text(angle=90, vjust = 1, hjust=0.5, size = 12), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 12)) +
  ylab("Accessions per CWR")
F1A

F1A_legend <- ggplot(num_accessions_cwr, 
             aes(x = reorder(PRIMARY_ASSOCIATED_CROP_COMMON_NAME, total_accessions), 
                 y = total_accessions,
                 color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_jitter(shape=16, position=position_jitter(0.1)) +
  theme_bw()

F1A_legend

# some summary statistics
num_accessions_cwr_summary <- num_accessions_cwr

##########
# another way to appraoch?
# function:
# filter by group
# make plot (all the same axis scales?)
# arrange plots on grid
category_names <- c("Sugars", "Vegetables", 
                    "Cereals and pseudocereals", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

make_a_plot_accessions_by_category <- function(category) {
  num_accessions_cwr_filtered <- num_accessions_cwr %>%
    filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == category) 
  
  plot <- ggplot(num_accessions_cwr_filtered, 
                 aes(x = reorder(PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                                 total_accessions), 
                     y = total_accessions)) +
    # geom_boxplot() +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    stat_summary(fun=mean, geom="point", shape='|', size= 8, color="red", fill="red") +
    theme_bw() +
    theme(legend.position = 'none',
          axis.title.y = element_blank()) +
    coord_flip()
    
    return(plot)
}

plot_accessions = list()
q = 1 # make an empty plot object
for(i in 1:8) {
  
  selected_category <- category_names[[i]] # make a vector of cat names
  plot_accessions[[q]] <- make_a_plot_accessions_by_category(selected_category)
  
  q = q+1
  
} 

# plot
plot_accessions
print(do.call(grid.arrange, plot_accessions))

#############
# Figure 1B

num_accessions_cwr_long <- gather(num_accessions_cwr, INSTITUTION_TYPE, accessions,
                                  garden_accessions,genebank_accessions, 
                                  factor_key=TRUE)
num_accessions_cwr_long <- num_accessions_cwr_long %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
    c("Cereals and pseudocereals"="Cereals, pseudo-"))) 
# %>%
  #mutate(binary = ifelse(
   # accessions > 0, 1, 0)))

F1B <- ggplot(num_accessions_cwr_long, aes(x = INSTITUTION_TYPE, 
                                           y = log(accessions), 
                                           fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.text.x = element_text(margin = margin(.4, 0, .1, 0, "cm")),
        strip.text = element_text(size = 12),
        axis.text.y  = element_text(size = 12), 
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(labels = c('BG','G')) +
  ylab("log(Accessions per CWR)")
F1B

####################
# statistical difference between bg and g for each group?

# to log transform?
p1 <- ggplot(num_accessions_cwr_long, aes(accessions)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ INSTITUTION_TYPE)

p2 <- ggplot(num_accessions_cwr_long, aes(log(accessions))) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ INSTITUTION_TYPE) +
  scale_x_log10()

grid.arrange(p1, p2, nrow = 2)

# I would say it is better to compare the log-transformed data,
# not do a t-test at all, given the pretty enormous right-skew of the data distributions
# non-parametric test?


category_names <- c("Sugars", "Vegetables", 
                    "Cereals, pseudo-", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

ttest_accessions_by_category <- function(category){
  num_accessions_cwr_long_filtered <- num_accessions_cwr_long %>%
    filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == category) %>%
    mutate(accessions = as.numeric(accessions))
  
  res <- t.test(num_accessions_cwr_long_filtered$accessions ~
                num_accessions_cwr_long_filtered$INSTITUTION_TYPE)
  
  return(res)
}

ttest_accessions = list()
q = 1 # specify a list element position
for(i in 1:8) {
  
  selected_category <- category_names[i] # make a vector of cat names
  ttest_accessions[[q]] <- ttest_accessions_by_category(selected_category)
  
  q = q+1
  
} 

print(ttest_accessions)

# wilcoxon ranked sum test
ttest_accessions_by_category <- function(category){
  num_accessions_cwr_long_filtered <- num_accessions_cwr_long %>%
    filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == category) %>%
    mutate(accessions = as.numeric(accessions))
  
  res <- wilcox.test(num_accessions_cwr_long_filtered$accessions ~
                  num_accessions_cwr_long_filtered$INSTITUTION_TYPE,
                  exact = FALSE)
  
  return(res)
}

ttest_accessions = list()
q = 1 # specify a list element position
for(i in 1:8) {
  
  selected_category <- category_names[i] # make a vector of cat names
  ttest_accessions[[q]] <- ttest_accessions_by_category(selected_category)
  
  q = q+1
  
} 

print(ttest_accessions)

##############################
# # FIGURE 2 CWR # # # # # # # 
##############################

# Geographic gaps in accessions versus species distr density
# total accessions collected from each area, proportion of species in each region that are
# conserved ex situ via wild-origin accessions collected from that region

# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Figure 2 Supplement # CWR richness by TAXON 
province_gap_table_fig2 <- province_gap_table %>%
  filter(!is.na(province)) %>% # filter for those from Canada AND were able to join w a province
  # essentially is filtering for the wild origins, but may be capturing some garden origin plants?
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  # join with CWR per province to get all provinces possible
  full_join(total_CWRs_group_by_province[ , c("PROVINCE", "total_CWRs_in_province")],
            by = c("province" = "PROVINCE")) %>%
  # now tally the number of CWR accessions from the province
  group_by(province) %>%
  mutate(total_accessions = sum(!is.na(province))) %>%
  mutate(garden_accessions = sum(!is.na(province) & 
                                   INSTITUTION == "BG")) %>%
  mutate(genebank_accessions = sum(!is.na(province) & 
                                     INSTITUTION == "G")) %>%
  distinct(province, .keep_all = TRUE) %>%
  filter(province != "Canada") %>%
  select(province, latitude, longitude, total_CWRs_in_province,
         total_accessions, garden_accessions, genebank_accessions) %>%
  full_join(canada_provinces_geojson[ , c("PROVINCE", "geometry")],
            by = c("province" = "PROVINCE"))
########### 
# old version of this figure
legend_title_CWR = expression("CWR")
# breaks = c(0, 40, 80, 120, 160, 200)
CWR_sf_provinces_fig2 <- st_as_sf(province_gap_table_fig2)
(map_provinces <- tm_shape(CWR_sf_provinces_fig2,
                           projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_province",
                style = "jenks",
                title = legend_title_CWR) + 
    tm_bubbles(size="total_accessions", 
               title.size = "CWR accessions",
               scale = 3, col = "black", 
               border.col = "white") +
    tm_layout(frame = FALSE,
              legend.outside = TRUE)
)

# province by SPECIES
province_species_gaps <- province_gap_table_species %>%
  filter(!is.na(province))%>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  # join with CWR per province to get all provinces possible
  full_join(total_CWRs_group_by_province[ , c("PROVINCE", "total_CWRs_in_province")],
            by = c("province" = "PROVINCE")) %>%
  # now tally the number of CWR accessions from the province
  group_by(province) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  # not sure why but having an issue with calculating garden_accessions
  # works fine if using ecoregion_gap_table df to start with
  # but produces NAs if using the same script but on the 
  # ecoregion_gap_table_species df 
  #mutate(garden_accessions = sum(!is.na(province) & 
  #                                  INSTITUTION == "BG")) %>%
  #mutate(genebank_accessions = sum(!is.na(province) & 
  #                                  INSTITUTION == "G")) %>%
  group_by(province, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation
  # could be better to just do which in each ecoregion are repped at all (w/ or without geo)
  ungroup() %>%
  group_by(province) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_CWRs_in_province)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_CWRs_in_province)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_CWRs_in_province))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_CWRs_in_province)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(province, .keep_all = TRUE) %>%
  filter(province != "Canada") %>%
  select(province, latitude, longitude, total_CWRs_in_province,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_provinces_geojson[ , c("PROVINCE", "geometry")],
            by = c("province" = "PROVINCE"))


crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

legend_title_CWR = expression("CWR species richness")

province_species_gaps_sf <- st_as_sf(province_species_gaps)

(map_provinces <- tm_shape(province_species_gaps_sf,
                            projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_province",
                style = "cont",
                title = legend_title_CWR) + 
    tm_symbols(size = "total_accessions", col = "proportion_in_any",
               title.size = "Accessions from region",
               title.col = "CWR species conserved (%)",
               sizes.legend=c(0, 25, 50, 100, 500),
               scale = 4,
               palette = rev(RColorBrewer::brewer.pal(5,"Greys")), alpha = 0.9,
               legend.format = list(text.align="right", text.to.columns = TRUE)) +
    # tm_bubbles(size="total_accessions", 
    #         title.size = "CWR accessions",
    #        scale = 3, col = "black", 
    #       border.col = "white") +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1)
)


# Figure 2A
# by taxon
ecoregion_gap_table_fig2 <- ecoregion_gap_table %>%
  filter(!is.na(ECO_NAME)) %>% # filter for those from Canada AND were able to join w a province
  # essentially is filtering for the wild origins, but may be capturing some garden origin plants?
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  # join with CWR per province to get all provinces possible
  full_join(total_CWRs_group_by_ecoregion[ , c("ECO_NAME", "total_CWRs_in_ecoregion")]) %>%
  # now tally the number of CWR accessions from the province
  group_by(ECO_NAME) %>%
  mutate(total_accessions = sum(!is.na(ECO_NAME))) %>%
  mutate(garden_accessions = sum(!is.na(ECO_NAME) & 
                                   INSTITUTION == "BG")) %>%
  mutate(genebank_accessions = sum(!is.na(ECO_NAME) & 
                                     INSTITUTION == "G")) %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  filter(ECO_NAME != "Canada") %>%
  select(ECO_NAME, latitude, longitude, total_CWRs_in_ecoregion,
         total_accessions, garden_accessions, genebank_accessions) %>%
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")])

# by species
ecoregion_species_gaps <- ecoregion_gap_table_species %>%
  filter(!is.na(ECO_NAME)) %>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  # join with CWR per province to get all provinces possible
  full_join(total_CWRs_group_by_ecoregion[ , c("ECO_NAME", "total_CWRs_in_ecoregion")]) %>%
  # now tally the number of CWR accessions from the province
  group_by(ECO_NAME) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  # not sure why but having an issue with calculating garden_accessions
  # works fine if using ecoregion_gap_table df to start with
  # but produces NAs if using the same script but on the 
  # ecoregion_gap_table_species df 
  #mutate(garden_accessions = sum(!is.na(ECO_NAME) & 
  #                                  INSTITUTION == "BG")) %>%
  #mutate(genebank_accessions = sum(!is.na(ECO_NAME) & 
  #                                  INSTITUTION == "G")) %>%
  group_by(ECO_NAME, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation
  # could be better to just do which in each ecoregion are repped at all (w/ or without geo)
  ungroup() %>%
  group_by(ECO_NAME) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_CWRs_in_ecoregion)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_CWRs_in_ecoregion)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_CWRs_in_ecoregion))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_CWRs_in_ecoregion)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  filter(ECO_NAME != "Canada") %>%
  select(ECO_NAME, latitude, longitude, total_CWRs_in_ecoregion,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")])


crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

legend_title_CWR = expression("CWR species richness")

ecoregion_species_gaps_sf <- st_as_sf(ecoregion_species_gaps)

(map_ecoregions <- tm_shape(ecoregion_species_gaps_sf,
                            projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_ecoregion",
                style = "cont",
                title = legend_title_CWR) + 
    tm_symbols(size = "total_accessions", col = "proportion_in_any",
               title.size = "Accessions from region",
               title.col = "CWR species conserved (%)",
               sizes.legend=c(0, 25, 50, 100, 500),
               scale = 4,
               palette = rev(RColorBrewer::brewer.pal(5,"Greys")), alpha = 0.9,
               legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1)
)

#########
# not using this, old version of the figure
legend_title_CWR = expression("CWR")
# breaks = c(0, 40, 80, 120, 160, 200)
CWR_sf_ecoregions_fig2 <- st_as_sf(ecoregion_gap_table_fig2)
(map_ecoregions <- tm_shape(CWR_sf_ecoregions_fig2,
                           projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_ecoregion",
                style = "jenks",
                title = legend_title_CWR) + 
    tm_bubbles(size="total_accessions", 
               title.size = "CWR accessions",
               scale = 3, col = "black", 
               border.col = "white") +
    tm_layout(frame = FALSE,
              legend.outside = TRUE)
)


# Alternate alternate
test <- ecoregion_species_gaps_sf %>%
  filter(!is.na(latitude)) %>%
  mutate(radius = 1/2 * log(total_accessions)) %>%
  as_tibble() %>%
  mutate(ECON_NAME = as.factor(ECO_NAME)) 

crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=10, base_family="") { # 3
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

(pies <- ggplot() +
  geom_scatterpie(data = test,
                  aes(x=longitude, y=latitude, group = ECO_NAME,
                      r = radius),
                  cols = c("proportion_in_any", "proportion_in_neither"),
                  color = NA)
)
  
(pies_plot <- ggplot(ecoregion_species_gaps_sf) +
  geom_sf(data = ecoregion_species_gaps_sf, aes(fill = as.numeric(total_CWRs_in_ecoregion))) +
  scale_fill_viridis_c(option = "B") +
  
  theme_map() +
  new_scale("fill") +
  geom_scatterpie(data = test,
                  aes(x = longitude,
                      y = latitude,
                      r = radius), alpha = 0.5,
                  cols = c("proportion_in_any", "proportion_in_neither")
                  ) + 
  scale_fill_manual(breaks = c("proportion_in_any", "proportion_in_neither"), 
                    values=c("black", "white")) +
  #coord_sf(crs = crs_string)
  theme(panel.grid.major = element_line(color = "white"),
        plot.title = element_text(color="black",
                                  size=14, face="bold.italic", hjust = 0.5),
        plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
        legend.position = "none") # , legend.text = element_text(size=10))
  
)
# +



############################################
############ FIGURE 2 WUS ##################
############################################

# note that these figures emphasize taxa NOT species

# Figure 3A
province_gap_table_fig3 <- province_gap_table %>%
  filter(!is.na(province)) %>% # filter for those from Canada AND were able to join w a province
  # essentially is filtering for the wild origins, but may be capturing some garden origin plants?
  # filter for tier 1 CWR
  filter(WUS == "Y") %>%
  # join with CWR per province to get all provinces possible
  full_join(total_WUS_group_by_province[ , c("PROVINCE", "total_WUS_in_province")],
            by = c("province" = "PROVINCE")) %>%
  # now tally the number of CWR accessions from the province
  group_by(province) %>%
  mutate(total_accessions = sum(!is.na(province))) %>%
  mutate(garden_accessions = sum(!is.na(province) & 
                                   INSTITUTION == "BG")) %>%
  mutate(genebank_accessions = sum(!is.na(province) & 
                                     INSTITUTION == "G")) %>%
  distinct(province, .keep_all = TRUE) %>%
  filter(province != "Canada") %>%
  filter(province %in% province_names) %>% # later, filter for - province = NA or any of the Canadian
  select(province, latitude, longitude, total_WUS_in_province,
         total_accessions, garden_accessions, genebank_accessions) %>%
  full_join(canada_provinces_geojson[ , c("PROVINCE", "geometry")],
            by = c("province" = "PROVINCE"))

legend_title_WUS = expression("WUS")
# breaks = c(0, 40, 80, 120, 160, 200)
WUS_sf_provinces_fig3 <- st_as_sf(province_gap_table_fig3)
(map_provinces <- tm_shape(WUS_sf_provinces_fig3,
                           projection = crs_string) +
    tm_polygons(col = "total_WUS_in_province",
                style = "jenks",
                title = legend_title_CWR,
                palette = "Blues") + 
    tm_bubbles(size="total_accessions", 
               title.size = "WUS accessions",
               scale = 3, col = "black", 
               border.col = "white") +
    tm_layout(frame = FALSE,
              legend.outside = TRUE)
)

# filter for points in provinces
province_names <- c("British Columbia", "Alberta", "Newfoundland and Labrador",
                    "Northwest Territories", "Nunavut", "Saskatchewan", 
                    "Manitoba", "Quebec", "Nova Scotia", "Ontario",
                    "New Brunswick", "Prince Edward Island", "Yukon")

province_species_gaps_WUS <- province_gap_table_species %>%
  filter(!is.na(province))%>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(WUS == "Y") %>%
  # join with CWR per province to get all provinces possible
  full_join(total_WUS_group_by_province[ , c("PROVINCE", "total_WUS_in_province")],
            by = c("province" = "PROVINCE")) %>%
  # now tally the number of CWR accessions from the province
  group_by(province) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  # not sure why but having an issue with calculating garden_accessions
  # works fine if using ecoregion_gap_table df to start with
  # but produces NAs if using the same script but on the 
  # ecoregion_gap_table_species df 
  #mutate(garden_accessions = sum(!is.na(province) & 
  #                                  INSTITUTION == "BG")) %>%
  #mutate(genebank_accessions = sum(!is.na(province) & 
  #                                  INSTITUTION == "G")) %>%
  group_by(province, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation
  # could be better to just do which in each ecoregion are repped at all (w/ or without geo)
  ungroup() %>%
  group_by(province) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_WUS_in_province)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_WUS_in_province)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_WUS_in_province))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_WUS_in_province)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(province, .keep_all = TRUE) %>%
  filter(province %in% province_names) %>%
  select(province, latitude, longitude, total_WUS_in_province,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_provinces_geojson[ , c("PROVINCE", "geometry")],
            by = c("province" = "PROVINCE"))


crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

legend_title_WUS = expression("WUS species richness")

province_species_gaps_WUS_sf <- st_as_sf(province_species_gaps_WUS)

(map_provinces_WUS <- tm_shape(province_species_gaps_WUS_sf,
                           projection = crs_string) +
    tm_polygons(col = "total_WUS_in_province",
                style = "cont",
                title = legend_title_WUS,
                palette = "Blues") + 
    tm_symbols(size = "total_accessions", col = "proportion_in_any",
               title.size = "Accessions from region",
               title.col = "WUS species conserved (%)",
               sizes.legend=c(0, 25, 50, 100, 500, 1000),
               scale = 4,
               palette = rev(RColorBrewer::brewer.pal(5,"Greys")), alpha = 0.9,
               legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1)
)

# Figure 2B
# by species
ecoregion_species_gaps_WUS <- ecoregion_gap_table_species %>%
  filter(!is.na(ECO_NAME)) %>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  # join with CWR per province to get all provinces possible
  full_join(total_WUS_group_by_ecoregion[ , c("ECO_NAME", "total_WUS_in_ecoregion")]) %>%
  # now tally the number of CWR accessions from the province
  group_by(ECO_NAME) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  # not sure why but having an issue with calculating garden_accessions
  # works fine if using ecoregion_gap_table df to start with
  # but produces NAs if using the same script but on the 
  # ecoregion_gap_table_species df 
  #mutate(garden_accessions = sum(!is.na(ECO_NAME) & 
  #                                  INSTITUTION == "BG")) %>%
  #mutate(genebank_accessions = sum(!is.na(ECO_NAME) & 
  #                                  INSTITUTION == "G")) %>%
  group_by(ECO_NAME, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation
  # could be better to just do which in each ecoregion are repped at all (w/ or without geo)
  ungroup() %>%
  group_by(ECO_NAME) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_WUS_in_ecoregion)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_WUS_in_ecoregion)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_WUS_in_ecoregion))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_WUS_in_ecoregion)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  filter(ECO_NAME != "Canada") %>%
  select(ECO_NAME, latitude, longitude, total_WUS_in_ecoregion,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")])


crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

legend_title_WUS = expression("WUS species richness")

ecoregion_species_gaps_WUS_sf <- st_as_sf(ecoregion_species_gaps_WUS)

(map_ecoregions_WUS <- tm_shape(ecoregion_species_gaps_WUS_sf,
                            projection = crs_string) +
    tm_polygons(col = "total_WUS_in_ecoregion",
                style = "cont",
                title = legend_title_WUS,
                palette = "Blues") + 
    tm_symbols(size = "total_accessions", col = "proportion_in_any",
               title.size = "Accessions from region",
               title.col = "WUS species conserved (%)",
               sizes.legend=c(0, 25, 50, 100, 500, 1000),
               scale = 4,
               border.col = "white",
               palette = rev(RColorBrewer::brewer.pal(5,"Greys")), alpha = 0.9,
               legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1)
)



ecoregion_gap_table_fig3 <- ecoregion_gap_table %>%
  filter(!is.na(ECO_NAME)) %>% # filter for those from Canada AND were able to join w a province
  # essentially is filtering for the wild origins, but may be capturing some garden origin plants?
  # filter for tier 1 CWR
  filter(WUS == "Y") %>%
  # join with CWR per province to get all provinces possible
  full_join(total_WUS_group_by_ecoregion[ , c("ECO_NAME", "total_WUS_in_ecoregion")]) %>%
  # now tally the number of CWR accessions from the province
  group_by(ECO_NAME) %>%
  mutate(total_accessions = sum(!is.na(ECO_NAME))) %>%
  mutate(garden_accessions = sum(!is.na(ECO_NAME) & 
                                   INSTITUTION == "BG")) %>%
  mutate(genebank_accessions = sum(!is.na(ECO_NAME) & 
                                     INSTITUTION == "G")) %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  filter(ECO_NAME != "Canada") %>%
  select(ECO_NAME, latitude, longitude, total_WUS_in_ecoregion,
         total_accessions, garden_accessions, genebank_accessions) %>%
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")])

# breaks = c(0, 40, 80, 120, 160, 200)
WUS_sf_ecoregions_fig3 <- st_as_sf(ecoregion_gap_table_fig3)
(map_ecoregions <- tm_shape(WUS_sf_ecoregions_fig3,
                            projection = crs_string) +
    tm_polygons(col = "total_WUS_in_ecoregion",
                style = "jenks",
                title = legend_title_WUS,
                palette = "Blues") + 
    tm_bubbles(size="total_accessions", 
               title.size = "WUS accessions",
               scale = 3, col = "black", 
               border.col = "white") +
    tm_layout(frame = FALSE,
              legend.outside = TRUE)
)


#####################################################
# Conduct Geographic Conservation Gap Analyses      #
#####################################################
# we determined the proportion of native regions (province or ecoregion) where 
# each CWR baturally occurs that were represented in garden collections

# note that these figures emphasize species NOT taxa
# filter inventory to Tier 1 species
inventory_sp_T1 <- inventory_sp %>%
filter(TIER == 1)

# Gap Analysis By Province

# filter to the garden occurrence points that were in Canada
province_gap_table_T1 <- province_gap_table_species %>%
  filter(TIER == 1)  # filter for tier 1 CWR
# filter to the garden occurrence points that were in Canada

# define the gap analysis function
province_gap_analysis <- function(species) {
  
  provinceTableData <- province_gap_table_T1 %>%
    # filter the table to the selected CWR
    filter(province_gap_table_T1$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(province) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                  num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province <- data.frame("SPECIES" = character(),
                                          "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(),
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2" = character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3" = character(), 
                                          "native provinces" = character(), 
                                          "covered provinces" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_T1)) {
  
  selected_taxon <- inventory_sp_T1[[i,1]] # column 1 is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis(
    species = selected_taxon)) 
  gap_analysis_df_by_province <- rbind(
    gap_analysis_df_by_province, temp)
  
} 

# summary stats
gap_analysis_df_by_province <- gap_analysis_df_by_province %>%
  mutate(mean = mean(perc_province_range_covered))

# Compute the analysis of variance
res.aov <- aov(perc_province_range_covered ~ 
                 PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
               data = gap_analysis_df_by_province)
# Summary of the analysis
summary(res.aov)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
# not normally distributed!

res.kruskal <- kruskal.test(perc_province_range_covered ~ 
               PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
             data = gap_analysis_df_by_province)
(res.kruskal) # medians of one or more group-pairs are significantly different

DT = dunnTest(perc_province_range_covered ~ PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
              data=gap_analysis_df_by_province,
              method="bh")

DT

PT = DT$res
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

# 
### now make the figure
#

gap_analysis_df_by_province_f4 <- gap_analysis_df_by_province %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
  c("Cereals and pseudocereals"="Cereals, pseudo-")))

(fig4a <- ggplot(gap_analysis_df_by_province_f4, 
               aes(x = perc_province_range_covered, 
                   y = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                   fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                   height = ..density..)) +
            geom_density_ridges(alpha = 0.6, trim = TRUE,
                                scale = 1.5, size = 0.5,
                                jittered_points = TRUE,
                                position = position_points_jitter(width = 0.01, height = 0),
                                point_shape = '|', point_size = 3, point_alpha = 1) +
            theme_ridges() +
            scale_fill_manual(values = c("grey87", "grey38",
                                         "grey87", "grey38",
                                         "grey87", "grey38",
                                         "grey87", "grey38")) +
            xlab(expression(paste(
              ""))) +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 14, 
                                              margin = margin(t = 20, r = 50))) +
            scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                               limits = c(0,1))
)  

######
# test new for T2
T2_levels = c("Forage and Feed", "Forest Resources", "Ornamentals")
inventory_sp_T2 <- inventory_sp %>%
  filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 %in% T2_levels | 
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2 %in% T2_levels |
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3 %in% T2_levels)

province_gap_table_T2 <- province_gap_table_species %>%
  filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 %in% T2_levels | 
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2 %in% T2_levels |
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3 %in% T2_levels)

# define the gap analysis function
province_gap_analysis_T2 <- function(species) {
  
  provinceTableData <- province_gap_table_T2 %>%
    # filter the table to the selected CWR
    filter(province_gap_table_T2$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(province) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                  num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}
 
# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province_T2 <- data.frame("SPECIES" = character(),
                                          "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(),
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2" = character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3" = character(), 
                                          "native provinces" = character(), 
                                          "covered provinces" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_T2)) {
  
  selected_taxon <- inventory_sp_T2[[i,1]] # column 1 is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis_T2(
    species = selected_taxon)) 
  gap_analysis_df_by_province_T2 <- rbind(
    gap_analysis_df_by_province_T2, temp)
  
} 

# 
### now make the figure
#

gap_analysis_df_by_province_T2_f4 <- gap_analysis_df_by_province_T2 %>%
 filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 %in% c(
   "Forage and Feed", "Ornamentals", "Forest Resources"))
# need to figure out how to include groups from multipple species
# because species have multiple levels in different columns 
# make the data long???
(fig4b <- ggplot(gap_analysis_df_by_province_T2_f4, 
                aes(x = perc_province_range_covered, 
                    y = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                    fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                    height = ..density..)) +
    geom_density_ridges(alpha = 0.6, trim = TRUE,
                        scale = 2, size = 0.5,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.01, height = 0),
                        point_shape = '|', point_size = 3, point_alpha = 1) +
    theme_ridges() + 
    scale_fill_manual(values = c("grey38", "grey87", "grey38")) +
    xlab(expression(paste(
      ""))) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 14, 
                                      margin = margin(t = 20, r = 50))) +
    scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(0,1))
)  

######
# test new for WUS
inventory_sp_WUS <- inventory_sp %>%
  filter(WUS == "Y")

province_gap_table_WUS <- province_gap_table_species %>%
  filter(WUS == "Y")

# define the gap analysis function
province_gap_analysis_WUS <- function(species) {
  
  provinceTableData <- province_gap_table_WUS %>%
    # filter the table to the selected CWR
    filter(province_gap_table_WUS$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(province) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                  PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1, PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                  num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province_WUS <- data.frame("SPECIES" = character(),
                                             "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1"= character(),
                                             "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2"= character(),
                                             "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                             "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(),
                                             "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2" = character(), 
                                             "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3" = character(), 
                                             "native provinces" = character(), 
                                             "covered provinces" = character(), 
                                             "accessions with geographic data" = character(),
                                             "accessions lacking geographic data" = character(),
                                             stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_WUS)) {
  
  selected_taxon <- inventory_sp_WUS[[i,1]] # column 1 is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis_WUS(
    species = selected_taxon)) 
  gap_analysis_df_by_province_WUS <- rbind(
    gap_analysis_df_by_province_WUS, temp)
  
} 

# 
### now make the figure
#

gap_analysis_df_by_province_WUS_f4 <- gap_analysis_df_by_province_WUS %>%
 filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 %in%
          c("Food", "Cultural"))

(fig4 <- ggplot(gap_analysis_df_by_province_WUS_f4, 
                aes(x = perc_province_range_covered, 
                    y = PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1, 
                    fill = PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1,
                    height = ..density..)) +
    geom_density_ridges(alpha = 0.6, trim = TRUE,
                        scale = 2, size = 0.5,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.01, height = 0),
                        point_shape = '|', point_size = 3, point_alpha = 1) +
    theme_ridges() + 
    scale_fill_manual(values = c("grey38", "grey87")) +
    xlab(expression(paste(
      "Proportion of range in ex situ collections"))) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 14, 
                                      margin = margin(t = 20, r = 50),
                                      hjust = 0.5)) +
    scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(0,1))  +
    scale_y_discrete(labels=c("Cultural" = "WUS - Cultural", 
                              "Food" = "WUS - Food"))
)


# Gap Analysis By Ecoregion

# filter to the garden occurrence points that were in Canada
ecoregion_gap_table_T1 <- ecoregion_gap_table_species %>%
  filter(TIER == 1)  # filter for tier 1 CWR

# define the gap analysis function
ecoregion_gap_analysis <- function(species) {
  
  ecoregionTableData <- ecoregion_gap_table_T1 %>%
    # filter the table to the selected CWR
    filter(ecoregion_gap_table_T1$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(ECO_NAME) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregion = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             (num_covered_ecoregion / num_native_ecoregion)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                  num_native_ecoregion, num_covered_ecoregion, perc_ecoregion_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_ecoregion = as.integer(num_covered_ecoregion)) %>%
    rename("native ecoregions" = num_native_ecoregion,
           "covered ecoregions" = num_covered_ecoregion,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_ecoregion <- data.frame("SPECIES" = character(),
                                          "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(), 
                                          "native ecoregions" = character(), 
                                          "covered ecoregions" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_T1)) {
  
  selected_taxon <- inventory_sp_T1[[i,1]] # column 1 is species ("sci_name")
  as.data.frame(temp <- ecoregion_gap_analysis(
    species = selected_taxon)) 
  gap_analysis_df_by_ecoregion <- rbind(
    gap_analysis_df_by_ecoregion, temp)
  
} 

# 
### now make the figure
#

gap_analysis_df_by_ecoregion_f4b <- gap_analysis_df_by_ecoregion %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
    c("Cereals and pseudocereals"="Cereals, pseudo-")))

fig4b <- ggplot(gap_analysis_df_by_ecoregion_f4b, 
               aes(x = perc_ecoregion_range_covered, 
                   y = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                   fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                   height = ..density..)) +
  geom_density_ridges(alpha = 0.6,
                      stat = "density", trim = TRUE,
                      scale = 4.5, size = 0.5) +
  theme_ridges() + 
xlab(expression(paste(
    "Proportion of native ecoregions of Tier 1 CWR \n represented in ex situ conservation systems"))) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, 
                                    margin = margin(t = 20, r = 50))) +
  scale_x_continuous(labels = function(x) paste0(x*100, "%"))

fig4b

####################################
######## GAP ANALYSIS CASE STUDY   #
####################################

# potential figure 5
# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=10, base_family="") { # 3
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

##############
# By Ecoregion
saskatoon_cwr_list <- inventory_sp %>%
  filter(PRIMARY_ASSOCIATED_CROP_COMMON_NAME == "Saskatoon")

accessions_sf <- st_as_sf(ecoregion_gap_table_species, 
         coords = c("longitude", "latitude"), 
         crs = 4326, 
         na.fail = FALSE)

# Function to get plot data by taxon
plotData_ecoregion <- function(species){
  # filter province_gap_table frame and calculate species specific stats
  ecoregionTableData <- ecoregion_gap_table_species %>%
    # filter the table to the selected CWR
    filter(ecoregion_gap_table_species$SPECIES == species) %>%
    
    # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
    group_by(ECO_NAME) %>%
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(total_accessions_for_species = sum(!is.na(GARDEN_CODE))) %>%
    mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
    
    # convert number of accessions to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregion = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             num_covered_ecoregion / num_native_ecoregion) 
  
  # join plot data with the spatial data frame necessary for projecting the plot  
  tigris::geo_join(canada_ecoregions_geojson, ecoregionTableData,  
                   by_sp = "ECO_NAME", by_df = "ECO_NAME")
  
} 

make_a_plot_ecoregion <- function(species) {
  accessions_sf_filtered <- accessions_sf %>%
    filter(SPECIES == species) 
  
  plot <- ggplot(plotData_ecoregion(species)) +
    geom_sf(aes(fill = as.factor(binary)),
            color = "gray60", size = 0.1) +
    geom_sf(data = accessions_sf_filtered, color = 'skyblue', alpha = 0.5, size = 2) +
    coord_sf(crs = crs_string) +
    scale_fill_manual(values = c("gray80", "gray18"), 
                      labels = c("No accessions with geographic data held in collection", 
                                 ">1 accession with geographic data held in collection", 
                                 "Outside of native range")) +
    # guides(fill = guide_legend(title = "Conservation Status in Botanic Gardens", 
    #                            title.position = "top",
    #                           title.theme = element_text(size = 10, face = "bold")
    # )) +
    theme_map() +
    ggtitle(species) +
    theme(panel.grid.major = element_line(color = "white"),
          plot.title = element_text(color="black",
                                    size=14, face="bold.italic", hjust = 0.5),
          plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
          legend.position = "none") # , legend.text = element_text(size=10)),
  return(plot)
}

test <- make_a_plot_ecoregion("Amelanchier alnifolia")
test

test1 <- make_a_plot_ecoregion("Corylus americana")
test1

test2 <- make_a_plot_ecoregion("Helianthus nuttallii")
test2

test3 <- make_a_plot_ecoregion("Allium tricoccum")
test3

test4 <- make_a_plot_ecoregion("Fragaria virginiana")
test4

