# INTRO (EDIT THIS)

# load required packages
library(tidyverse)
library(ggplot2)
library(sf) # the base package manipulating shapes
library(geojsonio) # geo json input and output

#library(rgeos)
#library(rgdal) # geo data abstraction library
#library(spdplyr) # the `dplyr` counterpart for shapes
#library(rmapshaper) # the package that allows geo shape transformation
#library(magrittr) # data wrangling
#library(dplyr)
#library(tigris)

######################################################################################

######################################################################################

# Load required data and shapefiles for plotting occurrence maps and data tables
inventory <- read.csv("./Input_Data_and_Files/inventory.csv")

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
# cwr_ubc <- read.csv("./Garden_Data/CWR_of_UBC.csv", na.strings=c("","NA"))
# cwr_rbg <- read.csv("./Garden_Data/CWR_of_RBG.csv", na.strings=c("","NA"))
cwr_montreal <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_montreal.csv", na.strings=c("","NA"))
cwr_guelph <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_guelph.csv", na.strings=c("","NA"))
# cwr_mountp <- read.csv("./Garden_Data/CWR_of_MountPleasantGroup.csv", na.strings=c("","NA"))
# cwr_vandusen <- read.csv("./Garden_Data/CWR_of_VanDusenBG.csv", na.strings=c("","NA"))
# cwr_pgrc <- read.csv("./Garden_Data/CWR_Amelanchier_PGRC.csv") # removing these subsetted data sets for now
# cwr_usask <- read.csv("Amelanchier_UofSask.csv") # removing these subsetted data sets for now
# cwr_readerrock <- read.csv("./Garden_Data/CWR_of_ReaderRock.csv", na.strings=c("","NA"))
cwr_PGRC <- read.csv("./Garden_PGRC_Data/filtered_data/CWR_of_pgrc.csv", na.strings=c("","NA"))

# join all garden data into one long table
# update and add new gardens as we receive additional datasets
garden_accessions <- rbind(cwr_guelph, cwr_montreal, cwr_PGRC)
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
  dplyr::select(-PROVINCE.y, - PROVINCE.x) 


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
all_garden_accessions_shapefile <- all_garden_accessions_shapefile %>%
  filter(COUNTRY == "Canada" | is.na(COUNTRY))

# now join the garden data with the sp distributions by province
# to expand each row where a 
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

# fix so inventory isn't filtering out hybrid x's!!

##################################################################
##################
##################################################################








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
