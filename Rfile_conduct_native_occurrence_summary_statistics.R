# INTRO (EDIT THIS)

# load required packages
library(tidyverse)
library(ggplot2)
library(sf) # the base package manipulating shapes
library(geojsonio) # geo json input and output
library(ggridges) # ridgeline plot in fig 4

######################################################################################

######################################################################################

# Load required data and shapefiles for plotting occurrence maps and data tables
inventory <- read.csv("./Input_Data_and_Files/inventory.csv") %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) %>% 
  mutate(SPECIES = str_replace(SPECIES, "×", ""))

canada_ecoregions_geojson <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE)
# rename PROVINCE in shapefile
canada_provinces_geojson <- canada_provinces_geojson %>%
  rename("PROVINCE" = "name")

sp_distr_ecoregion <- as_tibble(read.csv("./GBIF_download_outputs/species_distributions_ecoregion_trimmed.csv"))
sp_distr_province <- as_tibble(read.csv("./GBIF_download_outputs/species_distributions_province_trimmed.csv"))


######################################################################
# Reformat (add taxon info) and Project Species Distribution Tables  #
######################################################################

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


##############################
# Explore Summary Statistics #
##############################

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

#########################################
# regional richness and endemics of CWR #
#########################################
# end result -> heat map of CWR per province and ecoregion
# could overlay with gardens/PGRC and accessions or some representation of accessions?

# find ecoregions with the most total native CWRs
# and most endemic native CWRs
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

# Go ahead and add leaflet and/or heatmap here for ecoregions
# using the total_and_endemic_CWRs_ecoregion table?


# find provinces with the most total native CWRs
# and most endemic native CWRs
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
# regional richness and endemics of WUS #
#########################################
# end result -> heat map of CWR per province and ecoregion
# could overlay with gardens/PGRC and accessions or some representation of accessions?


# find ecoregions with the most total native WUS
# and most endemic native WUS
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
P <- ggplot(total_WUS_group_by_ecoregion, aes(x = total_WUS_in_ecoregion)) + theme_bw() + 
  geom_histogram()
P

# Go ahead and add leaflet and/or heatmap here for ecoregions
# using the total_and_endemic_WUS_ecoregion table?


# find provinces with the most total native WUS
# and most endemic native WUS
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
Q <- ggplot(total_WUS_group_by_province, aes(x = total_WUS_in_province)) + theme_bw() + 
  geom_histogram()
Q

############
# MAPPING ##
############
library(tmap) 

# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

legend_title_CWR = expression("CWR")
# breaks = c(0, 40, 80, 120, 160, 200)
CWR_sf_provinces <- st_as_sf(total_CWRs_group_by_province)
(map_provinces <- tm_shape(CWR_sf_provinces,
                           projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_province",
          style = "jenks",
          title = legend_title_CWR) + 
    tm_layout(frame = FALSE,
          legend.position = c("right", "top"))
)

# breaks2 = c(0, 30, 60, 90, 120, 150)
CWR_sf_ecoregions <- st_as_sf(total_CWRs_group_by_ecoregion)
(map_ecoregions <- tm_shape(CWR_sf_ecoregions,
                            projection = crs_string) +
  tm_polygons(col = "total_CWRs_in_ecoregion",
          style = "jenks",
          title = legend_title_CWR) + 
  tm_layout(frame = FALSE,
          legend.position = c("right", "top"))
)

legend_title_WUS = expression("WUS")
# breaks = c(0, 40, 80, 120, 160, 200)
WUS_sf_provinces <- st_as_sf(total_WUS_group_by_province)
(map_provinces_WUS <- tm_shape(WUS_sf_provinces,
                           projection = crs_string) +
    tm_polygons(col = "total_WUS_in_province",
                style = "jenks",
                title = legend_title_WUS) +
    tm_style("col_blind") + 
    tm_layout(frame = FALSE,
              legend.position = c("right", "top"))
)

# breaks2 = c(0, 30, 60, 90, 120, 150)
WUS_sf_ecoregions <- st_as_sf(total_WUS_group_by_ecoregion)
(map_ecoregions <- tm_shape(WUS_sf_ecoregions,
                            projection = crs_string) +
    tm_polygons(col = "total_WUS_in_ecoregion",
                style = "jenks",
                title = legend_title_WUS) +
    tm_style("col_blind") + 
    tm_layout(frame = FALSE,
              legend.position = c("right", "top"))
)


###############################
# GAP ANALYSIS ################
###############################

#########################################################################################
# Load and format garden collection data                                
#########################################################################################


##########
# compile garden data and append ecoregion or province
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
# all tables need to have the same columns!  
#rbind(cwr_ubc, cwr_rbg, cwr_montreal, cwr_guelph, cwr_mountp, cwr_vandusen,
   #                        cwr_readerrock, cwr_pgrc)
garden_accessions <- garden_accessions %>% # format columns
  mutate(latitude = as.numeric(LATITUDE), 
         longitude = as.numeric(LONGITUDE)) # %>%
# for now, we want to filter our data for coverage of ONLY CANADIAN ecoregions/admin districts
# delete the follwoing line of code if the focus expands to North America or world
# filter(country == "Canada")

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
  #rename(new = province) %>% # add a dummy name for province 
  # take province from cd_canada unless was already provided by garden (just want one column)
  mutate(province = ifelse(is.na(PROVINCE.x), PROVINCE.y, PROVINCE.x)) %>%
  dplyr::select(-PROVINCE.y, - PROVINCE.x) %>%
  left_join(inventory[,c("TAXON", "SPECIES")])


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
################## FIGURE 1 ######################################
##################################################################

# group by species (or taxon), calc number of rows where GARDEN_CODE !(is.na)
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
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) 


F1 <- ggplot(num_accessions_cwr, 
             aes(x = reorder(PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                             total_accessions), 
                 y = total_accessions,
                 color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank()) +
  coord_flip() +
  ylab("Total accessions per CWR")
F1

#############
# Figure 1B

num_accessions_cwr_long <- gather(num_accessions_cwr, INSTITUTION_TYPE, accessions,
                                  garden_accessions,genebank_accessions, 
                                  factor_key=TRUE)
num_accessions_cwr_long <- num_accessions_cwr_long %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
    c("Cereals and pseudocereals"="Cereals, pseudo-")))

F1B <- ggplot(num_accessions_cwr_long, aes(x = INSTITUTION_TYPE, 
                                           y = log(accessions), 
                                           color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.text.x = element_text(margin = margin(.4, 0, .1, 0, "cm")),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(labels = c('BG','G')) +
  ylab("log(Total accessions per CWR)")
F1B
F1B + ggpubr::stat_pvalue_manual(stat_pvalue, label = "p.signif") 
F1B

# I don't like this version but attempted to reapproach to add pvalues (need to use ggploxplot)
p <- ggboxplot(num_accessions_cwr_long, x = "INSTITUTION_TYPE", 
               y = "accessions",
               color = "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1",
               add = "jitter",
               facet.by = "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1", short.panel.labs = TRUE) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.text.x = element_text(margin = margin(.4, 0, .1, 0, "cm")),
        strip.text = element_text(size = 12)) +
  scale_x_discrete(labels = c('BG','G')) +
  # yscale("log10", .format = FALSE) +
  facet_wrap(ncol = 4)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format")

##############################
###### FIGURE 2 ##############
##############################

# Figure 2A 
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


# Figure 2B

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

########################################
############ FIGURE 3 ##################
########################################

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

# Figure 3B
# Figure 2B

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
inventory_sp_T1 <- inventory_sp
filter(TIER == 1)

# Gap Analysis By Province

# filter to the garden occurrence points that were in Canada
province_gap_table_T1 <- province_gap_table_species %>%
  filter(TIER == 1)  # filter for tier 1 CWR

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
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
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

# 
### now make the figure
#

gap_analysis_df_by_province_f4 <- gap_analysis_df_by_province %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
  c("Cereals and pseudocereals"="Cereals, pseudo-")))

fig4 <- ggplot(gap_analysis_df_by_province_f4, 
               aes(x = perc_province_range_covered, 
                   y = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                   fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                   height = ..density..)) +
            geom_density_ridges(alpha = 0.6,
                                stat = "density", trim = TRUE,
                                scale = 4.5, size = 0.5) +
            theme_ridges() + 
            #xlab(
            #  "Proportion of native provinces of Tier 1 CWR represented in ex situ conservation systems") +
            xlab(expression(paste(
              "Proportion of native provinces of Tier 1 CWR \n represented in ex situ conservation systems"))) +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 14, 
                                              margin = margin(t = 20, r = 50))) +
            scale_x_continuous(labels = function(x) paste0(x*100, "%"))
  
fig4


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

##############
# By Ecoregion
saskatoon_cwr_list <- inventory_sp %>%
  filter(PRIMARY_ASSOCIATED_CROP_COMMON_NAME == "Saskatoon")

saskatoon_ecoregion_gap_table <- ecoregion_gap_table_species %>%
  filter(PRIMARY_ASSOCIATED_CROP_COMMON_NAME == "Saskatoon")

# Function to get plot data by taxon
saskatoon_plotData_ecoregion <- function(species){
  # filter province_gap_table frame and calculate species specific stats
  ecoregionTableData <- saskatoon_ecoregion_gap_table %>%
    # filter the table to the selected CWR
    filter(saskatoon_ecoregion_gap_table$SPECIES == species) %>%
    
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
  subset_ecoregion_gap_table_sf <- ecoregion_gap_table_species %>%
    filter(SPECIES == species)
  
  plot <- ggplot(saskatoon_plotData_ecoregion(species = SPECIES)) +
    geom_sf(aes(fill = as.factor(binary)),
            color = "gray60", size = 0.1) +
    geom_sf(data = subset_ecoregion_gap_table_sf, color = 'skyblue', alpha = 0.5, size = 2) +
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
    ggtitle(taxon) +
    theme(panel.grid.major = element_line(color = "white"),
          plot.title = element_text(color="black",
                                    size=14, face="bold.italic", hjust = 0.5),
          plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
          legend.position = "none") # , legend.text = element_text(size=10)),
  return(plot)
}

test <- make_a_plot_ecoregion("Amelanchier arborea")
test

plot_ecoregions = list()
q = 1
for(i in 1:nrow(saskatoon_cwr_list)) {
  
  selected_taxon <- saskatoon_cwr_list[[i,3]] # r is species ("sci_name")
  plot_ecoregions[[q]] <- make_a_plot_ecoregion(selected_taxon)
  
  q = q+1
  
} 

# plot
plot_ecoregions[c(5, 6, 8, 11, 12, 15)] <- NULL # remove species w no range data
print(do.call(grid.arrange,plot_ecoregions))


##############################
# identify regional CWR richness and endemics by category
# by ecoregion

# function for identifying the native CWRs in each region given a category (x)
find_native_cwrs_by_group_ecoregion <- function(x) {
  temp <- ecoregion_gap_table %>%
    # one unique row per province for each species
    # this removes the duplicate rows when there are more than one accession per species
    group_by(species) %>%
    distinct(ECO_NAME, .keep_all=TRUE) %>%
    ungroup() %>%
    
    filter(Group == x)  %>%
    
    group_by(ECO_NAME) %>%
    # tally the number of unique CWR species
    distinct(species, .keep_all = TRUE) %>%
    add_tally() %>%
    rename(total_CWRs_in_ecoregion = "n") %>%
    mutate(total_CWRs_in_ecoregion = as.numeric(total_CWRs_in_ecoregion)) %>%
    ungroup() %>%
    
    # count endemic CWRs (species that occurs in only 1 ecoregion)
    group_by(species) %>%
    # if group is only one row, endemic = 1, else endemic = 0
    add_tally() %>%
    rename("native_ecoregions_for_species" = "n") %>%
    mutate(is_endemic = ifelse(
      native_ecoregions_for_species == 1, 1, 0)) %>%
    ungroup() %>%
    group_by(ECO_NAME) %>%
    mutate(endemic_CWRs_in_ecoregion = sum(is_endemic)) %>%
    
    distinct(ECO_NAME, .keep_all = TRUE ) %>%
    arrange(desc(total_CWRs_in_ecoregion))
    
  return(as_tibble(temp))
  
} 

# create an empty dataframe to be filled in by the function
native_cwrs_by_group_ecoregion <- data.frame("species" = character(),
                 "ECO_CODE"= character(), 
                 "ECO_NAME" = character(), 
                 "Group" = character(), 
                 "crop" = character(), 
                 "garden" = character(),
                 "variant" = character(),
                 "country" = character(), 
                 "IUCNRedList" = character(), 
                 "geometry" = character(), 
                 "total_CWRs_in_ecoregion" = character(),
                 "native_ecoregions_for_species" = character(),
                 "is_endemic" = character(), 
                 "endemic_CWRs_in_ecoregion" = character(),
                 stringsAsFactors=FALSE)

# run the function across all categories (there are 9 crop categories)
for(i in 1:9) {
  
  group_name <- cwr_list_summary[[i,1]]
  as.data.frame(temp <- find_native_cwrs_by_group_ecoregion(
    x = group_name)) 
  native_cwrs_by_group_ecoregion <- rbind(
    native_cwrs_by_group_ecoregion, temp)

} 

# find "hotspot" regions (regions with most CWR species) for each CWR category
hotspots_by_crop_category_ecoregion <- native_cwrs_by_group_ecoregion %>%
  dplyr::select(-species, -crop, -variant, -latitude, -longitude, 
                -country, -garden, -IUCNRedList, -ECO_CODE) %>%
  group_by(Group) %>%
  filter(!is.na(ECO_NAME)) %>%
  # keep row with max total_CWRs_in_ecoregion
  slice(which.max(total_CWRs_in_ecoregion))
  
#################
# by province

# function for identifying the native CWRs in each region given a category (x)
find_native_cwrs_by_group_province <- function(x) {
  temp <- province_gap_table %>%
    # one unique row per province for each species
    # this removes the duplicate rows when there are more than one accession per species
    group_by(TAXON) %>%
    distinct(PROVINCE, .keep_all=TRUE) %>%
    ungroup() %>%
    
    filter(Group == x)  %>%
    
    # group by province
    group_by(PROVINCE) %>%
    # tally the number of unique CWR species
    distinct(species, .keep_all = TRUE) %>%
    add_tally() %>%
    rename(total_CWRs_in_province = "n") %>%
    mutate(total_CWRs_in_province = as.numeric(total_CWRs_in_province)) %>%
    ungroup() %>%
    
    # count endemic CWRs (species that occurs in only 1 province)
    group_by(TAXON) %>%
    # if group is only one row, endemic = 1, else endemic = 0
    add_tally() %>%
    rename("native_province_for_species" = "n") %>%
    mutate(is_endemic = ifelse(
      native_province_for_species == 1, 1, 0)) %>%
    ungroup() %>%
    group_by(PROVINCE) %>%
    mutate(endemic_CWRs_in_province = sum(is_endemic)) %>%
    
    distinct(province, .keep_all = TRUE ) %>%
    arrange(desc(total_CWRs_in_province))
  
  return(as_tibble(temp))
  
} 

# create an empty dataframe to be filled in by the function
native_cwrs_by_group_province <- data.frame("species" = character(),
                                             "province"= character(), 
                                             "Group" = character(), 
                                             "crop" = character(), 
                                             "garden" = character(),
                                             "variant" = character(),
                                             "country" = character(), 
                                             "IUCNRedList" = character(), 
                                             "geometry" = character(), 
                                             "total_CWRs_in_ecoregion" = character(),
                                             "native_ecoregions_for_species" = character(),
                                             "is_endemic" = character(), 
                                             "endemic_CWRs_in_ecoregion" = character(),
                                             stringsAsFactors=FALSE)

# run the function across all categories (there are 9 crop categories)
for(i in 1:9) {
  
  group_name <- cwr_list_summary[[i,1]]
  as.data.frame(temp <- find_native_cwrs_by_group_province(
    x = group_name)) 
  native_cwrs_by_group_province <- rbind(
    native_cwrs_by_group_province, temp)
  
} 

# find "hotspot" regions (regions with most CWR species) for each CWR category
hotspots_by_crop_category_province <- native_cwrs_by_group_province %>%
  dplyr::select(-species, -crop, -variant, -latitude, -longitude, 
                -country, -garden, -IUCNRedList) %>%
  group_by(Group) %>%
  filter(!is.na(province)) %>%
  # keep row with max total_CWRs_in_province
  slice(which.max(total_CWRs_in_province))
