library(tidyverse)
library(readr)
library(rgbif)

# Enter your GBIF user info here before proceeding
# user=user 
# pwd=pwd 
# email=email
# I will delete this before pushing this file to protect my account privacy
# must be rewritten every time running this script
user="" 
pwd="" 
email=""

# load separate lists of taxon names. GBIF has a limit 100 000 observations that it's able to
#download at once, which is well under the number for the species combined
# I partitioned into groups of 100 taxon names
csvlist = c("part1.csv","part2.csv","part3.csv","part4.csv","part5.csv", 
            "part6.csv", "part7.csv", "part8.csv", "part9.csv")
# note, had to re-enter these without the "x" character in hybrid taxa, because GBIF cannot
# match the name for these and moves the speciesKey to the genus,
# e.g. Fragaria "x"ananassa becomes Fragaria sp.
# note, some taxa returned as synonyms. Not sure if it is going to return all of the
# occurrence ID points for the GBIF standardized name OR no occurrences at all.


# Download data from GBIF looping through csvlist
# need to point to the working directory with the csv files
setwd("GBIF_download_inputs/")
for (i in csvlist){
  print(i)
  df = read.csv(i) 
  keys = unique(df$speciesKey, na.last = TRUE)
  keys = keys[!is.na(keys)]
  down_code = occ_download(
    pred_in("taxonKey", keys),
    pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
    pred("country", "CA"),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV",
    user=user, pwd=pwd, email=email)
  Sys.sleep(180) # need to pause the loop to give GBIF time to gather data
  occ_download_get(down_code[1], overwrite = TRUE)
}  

##########################
# POST PROCESSING        #
##########################
# First unzip the output files
# I renamed them all afterwards to part1, part2, etc.
# Text to columns by tab delimiter for each (did manually)
# Want to know bind them all together,
# then spatially project the df
# then conduct a geo join by province
# then conduct a geo join by ecoregion to add ecoregion data
library(geojsonsf)
library(jsonlite)
library(sf)

setwd("~/R/Canadian_CWR_inventory_and_conservation/Input_Data_and_Files/")
inventory <- read.csv("inventory.csv")

setwd("~/R/Canadian_CWR_inventory_and_conservation/GBIF_download_outputs/")
# Bind the 9 seperate csv's into one
df1 <- read.csv("part1.csv") 
df2 <- read.csv("part2.csv")
df3 <- read.csv("part3.csv")
df4 <- read.csv("part4.csv")
df5 <- read.csv("part5.csv")
df6 <- read.csv("part6.csv")
df7 <- read.csv("part7.csv")
df8 <- read.csv("part8.csv")
df9 <- read.csv("part9.csv")

df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9)
df <- df %>%
  select(taxonKey, genus, species, infraspecificEpithet, taxonRank, scientificName, 
         verbatimScientificName, stateProvince, decimalLatitude, decimalLongitude
         )

# Convert to geoJSON for spatial projection
spatial_df <- df_geojson(df = df, lon = "decimalLongitude", lat = "decimalLatitude")
sf <- geojson_sf(spatial_df)

# Now join with province boundaries
setwd("~/R/Canadian_CWR_inventory_and_conservation/Geo_Data/")
provinces <- st_read("canada_provinces.geojson")
str(provinces)

shape_joined_1 <- st_join(provinces, sf)
output_sf_provinces <- shape_joined_1 %>%
  select(taxonKey, genus, species, infraspecificEpithet, taxonRank, scientificName, 
         verbatimScientificName, name
  )

# Now filter it down so there's one row per province per species
# need to change it back into a df first? 
output_df_provinces <- as.data.frame(output_sf_provinces) %>%
  select(-geometry)
str(output_df_provinces)

GBIF_province <- output_df_provinces %>%
  mutate(taxonRank = str_replace(taxonRank, "VARIETY", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SUBSPECIES", "subsp.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "FORM", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SPECIES", "")) %>%
  mutate(TAXON = paste(species, taxonRank, infraspecificEpithet, sep=' ')) %>%
  distinct(TAXON, name, .keep_all = TRUE) %>%
  select(TAXON, genus, species, taxonRank, infraspecificEpithet, name) %>%
  rename("GENUS" = "genus",
         "SPECIES" = "species",
         "RANK" = "taxonRank",
         "INFRASPECIFIC" = "infraspecificEpithet",
         "PROVINCE" = "name") %>%
  filter(!str_detect(TAXON, 'GENUS'))


GBIF_province$TAXON <- trimws(GBIF_province$TAXON, which = c("right"))
GBIF_province_filtered <- semi_join(GBIF_province, inventory, by="TAXON")

# need to figure out a way to add all infraspecific range information that might be missing at the species level
# for the species themselves



# Now join with ecoregion boundaries
setwd("~/R/Canadian_CWR_inventory_and_conservation/Geo_Data/")
ecoregions <- st_read("canada_ecoregions_clipped.geojson")
str(ecoregions)

shape_joined_2 <- st_join(ecoregions, sf)
output_sf_ecoregion <- shape_joined_2 %>%
  select(taxonKey, genus, species, infraspecificEpithet, taxonRank, scientificName, 
         verbatimScientificName, ECO_NAME
  )

# Now filter it down so there's one row per province per species
# need to change it back into a df first? 
output_df_ecoregion <- as.data.frame(output_sf_ecoregion) %>%
  select(-geometry)
str(output_df_ecoregion)

GBIF_ecoregion <- output_df_ecoregion %>%
  mutate(taxonRank = str_replace(taxonRank, "VARIETY", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SUBSPECIES", "subsp.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "FORM", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SPECIES", "")) %>%
  mutate(TAXON = paste(species, taxonRank, infraspecificEpithet, sep=' ')) %>%
  distinct(TAXON, ECO_NAME, .keep_all = TRUE) %>%
  select(TAXON, genus, species, taxonRank, infraspecificEpithet, ECO_NAME) %>%
  rename("GENUS" = "genus",
         "SPECIES" = "species",
         "RANK" = "taxonRank",
         "INFRASPECIFIC" = "infraspecificEpithet") %>%
  filter(!str_detect(TAXON, 'GENUS'))


GBIF_ecoregion$TAXON <- trimws(GBIF_ecoregion$TAXON, which = c("right"))
GBIF_ecoregion_filtered <- semi_join(GBIF_ecoregion, inventory, by="TAXON")

setwd("~/R/Canadian_CWR_inventory_and_conservation/GBIF_download_outputs/")
write.csv(GBIF_province_filtered, "species_distributions_province.csv")
write.csv(GBIF_ecoregion_filtered, "species_distributions_ecoregion.csv")

# test to see length (should be 845)
test <- GBIF_province_filtered %>%
  distinct(TAXON)
# so which taxa are missing?
test2 <- anti_join(inventory, GBIF_province_filtered, by="TAXON")

# make separate files that take all missing range areas that were found
# from the subsp and varietals, but drop the subsp. info
