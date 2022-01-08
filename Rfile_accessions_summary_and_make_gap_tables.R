# load required libraries
library(tidyverse)
library(ggplot2)
library(sf) # the base package manipulating shapes


################################################################################
# # 1 - Load inventory and spatial data     
################################################################################

# Load required data and shapefiles for plotting occurrence maps and data tables
inventory <- read.csv("./Input_Data_and_Files/inventory.csv") %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) %>% 
  mutate(SPECIES = str_replace(SPECIES, "×", "")) 

inventory_finest_taxon_resolution <- inventory %>%
  filter(FINEST_TAXON_RESOLUTION == "Y")

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
                              y = inventory_finest_taxon_resolution[ , c("TAXON",
                                                                         "PRIMARY_ASSOCIATED_CROP_COMMON_NAME",
                                                                         # "SECONDARY_ASSOCIATED_CROP_COMMON_NAME",
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
                               y = inventory_finest_taxon_resolution[ , c("TAXON",
                                                                          "PRIMARY_ASSOCIATED_CROP_COMMON_NAME",
                                                                          # "SECONDARY_ASSOCIATED_CROP_COMMON_NAME",
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

# some taxa were added to the inventory later (after GBIF download) and don't have species dist
# which taxa are missing species distributions?
# distinct_sp_distributions <- sp_distr_ecoregion %>%
#  distinct(TAXON)

# missing <- anti_join(inventory, distinct_sp_distributions) %>%
#  select(TAXON)

# write.csv(missing, "GBIF_download_inputs/Missed_species.csv")

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
  #filter(CWR == "Y") %>%
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

################################################################################
# - Load garden data format for gap analyses
################################################################################

##########
# compile garden data and append ecoregion or province of the origin for
# wild-collected accessions,
# when lat/long was given

# load data from garden collections (already filtered to only CWRs)
# update and add new gardens as we receive additional datasets
cwr_BG1 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG1.csv", na.strings=c("","NA"))
cwr_BG2 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG2.csv", na.strings=c("","NA"))
cwr_BG3 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG3.csv", na.strings=c("","NA"))
cwr_BG4 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG4.csv", na.strings=c("","NA"))
cwr_BG5 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG5.csv", na.strings=c("","NA"))
cwr_BG6 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG6.csv", na.strings=c("","NA"))
cwr_BG7 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG7.csv", na.strings=c("","NA")) 
cwr_BG8 <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_BG8.csv", na.strings=c("","NA"))
cwr_PGRC <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_PGRC.csv", na.strings=c("","NA"))
cwr_NTSC <- read.csv("./Garden_PGRC_Data/filtered_data/cwr_NTSC.csv", na.strings=c("","NA"))
cwr_NPGS <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_NPGS.csv", na.strings=c("","NA"))
cwr_missed <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_missed_taxa.csv", na.strings=c("","NA"))


# join all garden data into one long table
# update and add new gardens as we receive additional datasets
garden_accessions <- rbind(cwr_BG1, cwr_BG2, cwr_BG3, cwr_BG4, 
                           cwr_BG5, cwr_BG6, cwr_BG7, cwr_BG8, 
                           cwr_PGRC, cwr_NTSC,
                           cwr_NPGS,
                           cwr_missed)

garden_accessions <- garden_accessions %>% # format columns
  mutate(latitude = as.numeric(LATITUDE), 
         longitude = as.numeric(LONGITUDE)) %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) # R can't match this symbol in joins

# Transform garden data into a projected shape file
sf_garden_accessions <- garden_accessions %>%
  # na.fail = FALSE to keep all of the accessions (about 80% don't have lat long,
  # but many of these have province at least)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE)

################################################################
# TABLE 3 - SUMMARY OF THE TOTAL ACCESSIONS                    #
################################################################
# below, this is repeated for only the Candian wild-origin accessions

# sf_garden_accessions needs to be joined with inventory
sf_garden_accessions_all <- sf_garden_accessions %>% 
  left_join(inventory[,c("TAXON", "SPECIES", "TIER", "CWR", "WUS")])

## summary of accessions
# how many for each category
all_distinct <- sf_garden_accessions_all %>%
  distinct(SPECIES)

TIER1 <- sf_garden_accessions_all %>%
  filter(TIER == 1)
TIER1_distinct <- TIER1 %>%
  distinct(SPECIES)
TIER2 <- sf_garden_accessions_all %>%
  filter(TIER == 2)
TIER2_distinct <- TIER2 %>%
  distinct(SPECIES)
WUS <- sf_garden_accessions_all %>%
  filter(WUS == "Y")
WUS_distinct <- WUS %>%
  distinct(SPECIES)

# and broken down by those in gardens, those in genebanks (and within the two genebanks types)
BG_accessions <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG") # %>%

# accessions in individual gardens (>10,000 in garden 1)
BG <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG") %>%
  group_by(GARDEN_CODE, TIER) %>%
  count()

BG_WUS <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG",
         WUS == "Y") %>%
  group_by(GARDEN_CODE) %>%
  count()

BG_distinct <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG") %>%
  distinct(SPECIES)

BG_distinct_1 <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG",
         TIER == 1) %>%
  distinct(SPECIES)

# TIER2 in each BG
BG_WUS <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG",
         TIER == 2) %>%
  group_by(GARDEN_CODE) %>%
  count()

# WUS in each BG
BG_WUS <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "BG",
         WUS == "Y") %>%
  group_by(GARDEN_CODE) %>%
  count()

# count accessions in G
G <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "G") # %>%

# G species Tier 1
G_distinct <- G  %>%
  distinct(SPECIES)
# G species Tier 1
G_distinct_1 <- G %>%
  filter(TIER == 1) %>%
  distinct(SPECIES)

# count accessions in PGRC
PGRC_accessions <- G %>%
  filter(GARDEN_CODE == "PGRC")
PGRC_accessions_1 <- PGRC_accessions %>%
  filter(TIER == 1)
PGRC_accessions_2 <- PGRC_accessions %>%
  filter(TIER == 2)
PGRC_accessions_WUS <- PGRC_accessions %>%
  filter(WUS == "Y")

# distinct species
PGRC_distinct <- PGRC_accessions %>%
  distinct(SPECIES)
# PGRC species Tier 1
PGRC_distinct_1 <- PGRC_accessions %>%
  filter(TIER == 1) %>%
  distinct(SPECIES)
PGRC_distinct_2 <- PGRC_accessions %>%
  filter(TIER == 2) %>%
  distinct(SPECIES)
PGRC_distinct_WUS <- PGRC_accessions %>%
  filter(WUS == "Y") %>%
  distinct(SPECIES)

# count accessions in NPGS
NPGS_accessions <- G %>%
  filter(GARDEN_CODE == "NPGS")
NPGS_accessions_1 <- NPGS_accessions %>%
  filter(TIER == 1)
NPGS_accessions_2 <- NPGS_accessions %>%
  filter(TIER == 2)
NPGS_accessions_WUS <- NPGS_accessions %>%
  filter(WUS == "Y")

# distinct species
NPGS_distinct <- NPGS_accessions %>%
  distinct(SPECIES)
# PGRC species Tier 1
NPGS_distinct_1 <- NPGS_accessions %>%
  filter(TIER == 1) %>%
  distinct(SPECIES)
NPGS_distinct_2 <- NPGS_accessions %>%
  filter(TIER == 2) %>%
  distinct(SPECIES)
NPGS_distinct_WUS <- NPGS_accessions %>%
  filter(WUS == "Y") %>%
  distinct(SPECIES)

# TIER2 in genebanks
G_TIER2 <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "G",
         TIER == 2)

# WUS in genebanks
G_WUS <- sf_garden_accessions_all %>%
  filter(INSTITUTION == "G",
         WUS == "Y")

# which species in one type of institution but not the other
left <- anti_join(PGRC_distinct, NPGS_distinct) # in x but not in y
left2 <- anti_join(NPGS_distinct, PGRC_distinct)
left3 <- anti_join(G_distinct, BG_distinct)
left4 <- anti_join(BG_distinct, G_distinct)

# which species (tier1) in one type of institution but not the other
left_TIER1 <- anti_join(PGRC_distinct_1, NPGS_distinct_1) # in x but not in y
left2_TIER1 <- anti_join(NPGS_distinct_1, PGRC_distinct_1)
left3_TIER1 <- anti_join(G_distinct_1, BG_distinct_1)
left4_TIER1 <- anti_join(BG_distinct_1, G_distinct_1)


##################################################################
# Join with species distributions to get province and ecoregion, #
# and add inventory data, e.g. TIER, G versus BG, etc.           #
##################################################################

# first, group by species and add column with total accessions (will need this data later)
# then add accessions in each type of institution
# then bind the two dfs back together
BG_accessions <- sf_garden_accessions %>%
  left_join(inventory[,c("TAXON", "SPECIES")]) %>%
  # total accessions by species
  group_by(SPECIES) %>%
  add_tally() %>%
  # accessions by species in genebanks and gardens
  filter(INSTITUTION == "BG") %>%
  add_tally()

G_accessions <- sf_garden_accessions %>%
  left_join(inventory[,c("TAXON", "SPECIES")]) %>%
  # total accessions by species
  group_by(SPECIES) %>%
  add_tally() %>%
  # accessions by species in genebanks and gardens
  filter(INSTITUTION == "G") %>%
  add_tally()

sf_garden_accessions_counts <- rbind(
  BG_accessions, G_accessions) 

sf_garden_accessions_counts <- sf_garden_accessions_counts %>%
  rename(total_accessions = n, 
         total_accessions_BG_or_G = nn)

# THEN
# remove points from outside of Canada 
# code here
canada <- st_read("./Geo_Data/canada.geojson", quiet = TRUE) # 1
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# create a buffer of ~>100km - capture all points on weird coastlines
# will inevitably capture a few accessions from the border with the U.S.
canada_buff = st_buffer(canada, dist = 1)

plot <- ggplot(canada_buff) +
  geom_sf(fill = "white", color = "gray60", size = 0.1) +
  coord_sf(crs = crs_string)
plot

# project points to same as canada shapefile
sp <- st_transform(sf_garden_accessions_counts, proj4string=CRS(proj4string(canada)))
# and then clip to the layer (the buffered layer)
sp_subset <- sp[canada_buff, ]


# Append Province to accession using lat and longitude
# spatial join to add accession province
points_polygon_wild <- st_join(sp_subset, canada_provinces_geojson, left = TRUE,
                          join = st_nearest_feature, maxdist = 1000)

# spatial join to add accession ecoregion
points_polygon_wild_2 <- st_join(points_polygon_wild, canada_ecoregions_geojson, left = TRUE,
                            join = st_nearest_feature, maxdist = 1000)


# break out new latitude and longitude columns and reformat
summary_all_garden_accessions_wild <- points_polygon_wild_2 %>%
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
                GARDEN_CODE, INSTITUTION,
                total_accessions, 
                total_accessions_BG_or_G) %>%
  # IUCNRedList/conservation_status) %>%
  # take province from cd_canada unless was already provided by garden (just want one column)
  mutate(province = ifelse(is.na(PROVINCE.x), PROVINCE.y, PROVINCE.x)) %>%
  dplyr::select(-PROVINCE.y, - PROVINCE.x) %>%
  left_join(inventory[,c("TAXON", "SPECIES", "TIER", "CWR", "WUS")])

############################################################################
# TABLE 3 - SUMMARY OF THE Wild-ORIGIN CANADIAN ACCESSIONS (W/ Provenance) #
############################################################################

## summary of accessions
# how many for each category
TIER1_W <- summary_all_garden_accessions_wild %>%
  filter(TIER == 1)
TIER1_W_distinct <- TIER1 %>%
  distinct(SPECIES)
TIER2_W <- summary_all_garden_accessions_wild %>%
  filter(TIER == 2)
TIER2_W_distinct <- TIER2_W %>%
  distinct(SPECIES)
WUS_W <- summary_all_garden_accessions_wild %>%
  filter(WUS == "Y")
WUS_W_distinct <- WUS_W %>%
  distinct(SPECIES)

# and broken down by those in gardens, those in genebanks (and within the two genebanks types)
BG_accessions_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "BG") # %>%

# accessions in individual gardens (>10,000 in garden 1)
BG_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "BG") %>%
  group_by(GARDEN_CODE, TIER) %>%
  count()

BG_distinct_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "BG",
         TIER == 1) %>%
  distinct(SPECIES)

# TIER2 in each BG
BG_TIER2_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "BG",
         TIER == 2) %>%
  group_by(GARDEN_CODE) %>%
  count()

# WUS in each BG
BG_WUS_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "BG",
         WUS == "Y") %>%
  group_by(GARDEN_CODE) %>%
  count()

# count accessions in G
G_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "G") # %>%

# G species Tier 1
G_distinct_W <- G_W %>%
  filter(TIER == 1) %>%
  distinct(SPECIES)

# count accessions in PGRC
PGRC_accessions_W <- G_W %>%
  filter(GARDEN_CODE == "PGRC")

# PGRC species Tier 1
PGRC_distinct_W <- G_W %>%
  filter(TIER == 1,
         GARDEN_CODE == "PGRC") %>%
  distinct(SPECIES)

# count accessions in NPGS
NPGS_accessions_W <- G_W %>%
  filter(GARDEN_CODE == "NPGS")

# NPGS species Tier 1
NPGS_distinct_W <- G_W %>%
  filter(TIER == 1,
         GARDEN_CODE == "NPGS") %>%
  distinct(SPECIES)

# TIER2 in genebanks
G_WUS_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "G",
         TIER == 2)

# WUS in genebanks
G_WUS_W <- summary_all_garden_accessions_wild %>%
  filter(INSTITUTION == "G",
         WUS == "Y")

# which species in one type of institution but not the other
left_W <- anti_join(PGRC_distinct_W, NPGS_distinct_W) # in x but not in y
left2_W <- anti_join(NPGS_distinct_W, PGRC_distinct_W)
left3_W <- anti_join(G_distinct_W, BG_distinct_W)
left4_W <- anti_join(BG_distinct_W, G_distinct_W)

only_PGRC_or_gardens <- rbind(left4_W, left_W)
only_PGRC_or_gardens <- only_PGRC_or_gardens %>% 
  distinct()


################################################################
# NOW MAKE THE GAP TABLES                                      #
################################################################

# now join the garden data with the sp distributions by province
# to expand each row where a taxon/ species occurs
# for taxon level use these:
province_gap_table <- sp_distr_province[ , c("TAXON", "PROVINCE")] %>%
  rename("province" = "PROVINCE") %>%
  merge(x = ., y = summary_all_garden_accessions_wild,
        by = c("TAXON", "province"),
        all = TRUE) %>%
  dplyr::select(-ECO_NAME, -COUNTRY) %>%
  full_join(inventory)

ecoregion_gap_table <- sp_distr_ecoregion[ , c("TAXON", "ECO_NAME")] %>%
  merge(x = ., y = summary_all_garden_accessions_wild,
        by = c("TAXON", "ECO_NAME"),
        all = TRUE) %>%
  dplyr::select(-province, -COUNTRY) %>%
  full_join(inventory)

# for species level use these:
inventory_sp <- inventory %>%
  dplyr::select(-TAXON, -RANK, -INFRASPECIFIC) %>%
  distinct(SPECIES, .keep_all = TRUE)

sp_distr_province_sp <- sp_distr_province %>%
  # remove duplicates at province within species
  distinct(SPECIES, PROVINCE, .keep_all=TRUE)

province_gap_table_species <- sp_distr_province_sp[ , c("SPECIES", "PROVINCE")] %>%
  rename("province" = "PROVINCE") %>%
  merge(x = ., y = summary_all_garden_accessions_wild,
        by = c("SPECIES", "province"),
        all = TRUE) %>%
  dplyr::select(-ECO_NAME, -COUNTRY, -TAXON, -TIER, -CWR, -WUS) %>%
  full_join(inventory_sp, by = "SPECIES") %>%
  rename(PROVINCE = province)

sp_distr_ecoregion_sp <- sp_distr_ecoregion %>%
  # remove duplicates at province within species
  distinct(SPECIES, ECO_NAME, .keep_all=TRUE)

ecoregion_gap_table_species <- sp_distr_ecoregion_sp[ , c("SPECIES", "ECO_NAME")] %>%
  merge(x = ., y = summary_all_garden_accessions_wild,
        by = c("SPECIES", "ECO_NAME"),
        all = TRUE) %>%
  dplyr::select(-province, -COUNTRY, -TAXON, -TIER, -CWR, -WUS) %>%
  full_join(inventory_sp, by = "SPECIES") 

# Last, join with total and endemic CWR per ecoregion/province
# and total and endemic WUS per ecoregion/province
province_gap_table_species_out <- province_gap_table_species %>%
  full_join(total_CWRs_group_by_province[, c(
    "PROVINCE", "total_CWRs_in_province", "endemic_CWRs_in_province")]) %>%
  full_join(total_WUS_group_by_province[, c(
  "PROVINCE", "total_WUS_in_province", "endemic_WUS_in_province")])

ecoregion_gap_table_species_out <- ecoregion_gap_table_species %>%
  full_join(total_CWRs_group_by_ecoregion[, c(
    "ECO_NAME", "total_CWRs_in_ecoregion", "endemic_CWRs_in_ecoregion")]) %>%
  full_join(total_WUS_group_by_ecoregion[, c(
    "ECO_NAME", "total_WUS_in_ecoregion", "endemic_WUS_in_ecoregion")])

# write.csv(province_gap_table_species_out, "Garden_PGRC_Data/province_gap_table_species.csv")
# write.csv(ecoregion_gap_table_species_out, "Garden_PGRC_Data/ecoregion_gap_table_species.csv")


# GOING TO NEED TO MAKE ANOTHER ONE FOR ALL ACCESSIONS (FOR FIG 1)
# can be by province, just need all accessions
province_gap_table_species_all_accessions <- sp_distr_province_sp[ , c("SPECIES", "PROVINCE")] %>%
  merge(x = ., y = sf_garden_accessions_all,
        by = c("SPECIES", "PROVINCE"),
        all = TRUE) %>%
  dplyr::select(-COUNTRY, -TAXON) %>%
  full_join(inventory_sp) 


# group by species (or taxon), calc number of rows where GARDEN_CODE !(is.na)
# GARDEN_CODE !(is.na) reduces the gap table to just the garden accessions 
# then group by INSTITUTION (within species or taxon)
num_accessions <- garden_accessions %>%
  full_join(inventory) %>%
  group_by(SPECIES) %>%
  mutate(total_accessions = sum(!is.na(GARDEN_CODE))) %>%
  mutate(garden_accessions = sum(!is.na(GARDEN_CODE) & 
                                   INSTITUTION == "BG")) %>%
  mutate(genebank_accessions = sum(!is.na(GARDEN_CODE) & 
                                     INSTITUTION == "G")) %>%
  distinct(SPECIES, .keep_all = TRUE) %>%
  select(-GARDEN_CODE, -INSTITUTION, -PROVENANCE, -COUNTRY,
         -LOCALITY, -LATITUDE, -LONGITUDE, -PROVINCE, -QUANTITY,
         -latitude, -longitude)

# write.csv(num_accessions, "Garden_PGRC_Data/summary_accessions_all_species.csv")

##################################################
# TABLE 4 - SPECIES WITH MOST ACCESSIONS         #
##################################################

table_4 <- num_accessions %>%
  filter(TIER == 1)
# write.csv(table_4, "accessions_tier1_CWR.csv")
