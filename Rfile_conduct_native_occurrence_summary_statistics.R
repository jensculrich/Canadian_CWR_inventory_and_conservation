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

sp_distr_ecoregion <- as_tibble(read.csv("./GBIF_download_outputs/species_distributions_ecoregion_trimmed.csv"))
sp_distr_province <- as_tibble(read.csv("./GBIF_download_outputs/species_distributions_province_trimmed.csv"))

# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=9, base_family="") { # 3
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

#########################################################################
# Reformat Gap Tables So That Garden Points Can Be Projected alternates #
#########################################################################

# rename PROVINCE in shapefile
canada_provinces_geojson <- canada_provinces_geojson %>%
  rename("PROVINCE" = "name")

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
total_CWRs_group_by_ecoregion <- total_WUS_group_by_ecoregion %>% 
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

# Create a color palette for the map:
mypalette <- colorNumeric( palette="YlOrBr", domain=mydat$variable, na.color="transparent")
mypalette(c(45,43))

leaflet(plotDataNativeRanges()) %>% 
  addTiles()  %>% 
  setView( lat=60, lng=-98 , zoom=3) %>%
  addPolygons(fillOpacity = 0.5, 
              smoothFactor = 0.5, 
              color = ~colorNumeric("YlOrBr", variable)(variable),
              label = mytext,
              layerId = ~region) %>%
  addLegend( pal=mypalette, values=~variable, opacity=0.9, title = "CWRs", position = "bottomleft" )












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
