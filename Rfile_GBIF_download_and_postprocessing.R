library(tidyverse)
library(readr)
library(rgbif)

# Enter your GBIF user info here before proceeding
# user=user 
# pwd=pwd 
# email=email
# I will delete this before pushing this file to protect my account privacy
# must be rewritten every time running this script
user="jensj27" 
pwd="Ceratina_1802" 
email="jensj27@gmail.com"

# load separate lists of taxon names. GBIF has a limit 100 000 observations that it's able to
#download at once, which is well under the number for the species combined
# I partitioned into groups of 100 taxon names
csvlist = c("part1.csv","part2.csv","part3.csv","part4.csv","part5.csv", 
            "part6.csv", "part7.csv", "part8.csv", "part9.csv")
# csvlist = "speciesKeys_extra_taxa.csv"
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
df10 <- read.csv("problemTaxa.csv")
df11 <- read.csv("problemTaxa_manual.csv")

# replace NAs with "" in df11
df11 <- df11 %>%
  mutate(taxonRank = replace_na(taxonRank, "")) %>%
  mutate(infraspecificEpithet = replace_na(infraspecificEpithet, ""))

df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11)
df <- df %>%
  select(taxonKey, genus, species, infraspecificEpithet, taxonRank, scientificName, 
         verbatimScientificName, stateProvince, decimalLatitude, decimalLongitude
         )

# issues with problem species that need to be fixed to match the inventory
df <- within(df, infraspecificEpithet[species == 'Amelanchier pumila'] <- '')
df <- within(df, taxonRank[species == 'Amelanchier pumila'] <- 'species')
df <- within(df, infraspecificEpithet[species == 'Leymus villosissimus'] <- '')
df <- within(df, taxonRank[species == 'Leymus villosissimus'] <- 'species')
df[nrow(df) + 1,] = c("NA","Zizania", "Zizania aquatica", "interior", "var.",
                            "Zizania aquatica var. interior", "Zizania aquatica var. interior", 
                            "Manitoba", "49.6", "-95.3")
df$decimalLatitude <- as.numeric(df$decimalLatitude)
df$decimalLongitude <- as.numeric(df$decimalLongitude)

# R is having difficulty matching the odd hybrid x symbols
df <- df %>% 
  mutate(species = str_replace(species, "×", ""))
# R is having difficulty matching the odd hybrid x symbols
inventory <- inventory %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) %>% 
  mutate(SPECIES = str_replace(SPECIES, "×", ""))

# Convert to geoJSON for spatial projection
spatial_df <- df_geojson(df = df, lon = "decimalLongitude", lat = "decimalLatitude")
points <- geojson_sf(spatial_df)

# Now join with province boundaries
setwd("./Geo_Data/")
provinces <- st_read("canada_provinces.geojson")
str(provinces)

shape_joined_1 <- st_join(points, provinces, join = st_nearest_feature, maxdist = 10000)
# join = st_nearest_feature joins each point to nearest polygon (province).
# REALLY important because many collections along coastlines, 
# and the polygone edges can be quite wonky
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
  mutate(infraspecificEpithet = replace_na(infraspecificEpithet, "")) %>%
  mutate(taxonRank = str_replace(taxonRank, "VARIETY", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SUBSPECIES", "subsp.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "FORM", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SPECIES", "")) %>%
  mutate(taxonRank = str_replace(taxonRank, "species", "")) %>%
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
GBIF_province_2 <- GBIF_province[!is.na(GBIF_province$PROVINCE),] 

# join with inventory to remove taxa with incorrect names 
# This should be the one we want
GBIF_province_filtered_2 <- semi_join(GBIF_province_2, inventory, by="TAXON", all.x = TRUE)

# is it possible to match and update the taxon names that don't match those in the inventory?
anti_join <- anti_join(GBIF_province_2, inventory, by="TAXON", all.x = TRUE)
# these are names where the GBIF name at the time of submission was incorrect, mostly
# where the subsp. or variety is synonymous. Just coerce them back to plain speces,
# then rebind with the GBIF_province_filtered_2
# then filter eagain to remove any more potential mismatches
anti_join_prepped <- anti_join %>%
  mutate(TAXON = SPECIES) %>%
  mutate(RANK = "") %>%
  mutate(INFRASPECIFIC = "")

GBIF_province_filtered_3 <- rbind(GBIF_province_filtered_2, anti_join_prepped)
GBIF_province_filtered_4 <- semi_join(GBIF_province_filtered_3, inventory, by="TAXON", all.x = TRUE)

# those taxa with incorrect names that were reverted back to species level
# may now have multiple rows for unique TAXON x region, use distinct again 
# to get just one per unique combination
GBIF_province_filtered_4 <- GBIF_province_filtered_4 %>%
  distinct(TAXON, PROVINCE, .keep_all = TRUE)

# need to figure out a way to add all infraspecific range information that might be missing at the species level
# for the species themselves (in case any are missing)

# test to see length (should be 790)
test <- GBIF_province_filtered_4 %>%
  distinct(TAXON)
# so which taxa are missing?
test2 <- anti_join(inventory, GBIF_province_filtered_4, by="TAXON")
# no occurrence points on GBIF for these ones
# write.csv(test2, "species_distributions_taxa_w_issues_2.csv")

# Now join with ecoregion boundaries
ecoregions <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson")
str(ecoregions)

shape_joined_2 <- st_join(points, ecoregions, join = st_nearest_feature, maxdist = 10000)
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
  mutate(infraspecificEpithet = replace_na(infraspecificEpithet, "")) %>%
  mutate(taxonRank = str_replace(taxonRank, "VARIETY", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SUBSPECIES", "subsp.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "FORM", "var.")) %>%
  mutate(taxonRank = str_replace(taxonRank, "SPECIES", "")) %>%
  mutate(taxonRank = str_replace(taxonRank, "species", "")) %>%
  mutate(TAXON = paste(species, taxonRank, infraspecificEpithet, sep=' ')) %>%
  distinct(TAXON, ECO_NAME, .keep_all = TRUE) %>%
  select(TAXON, genus, species, taxonRank, infraspecificEpithet, ECO_NAME) %>%
  rename("GENUS" = "genus",
         "SPECIES" = "species",
         "RANK" = "taxonRank",
         "INFRASPECIFIC" = "infraspecificEpithet") %>%
  filter(!str_detect(TAXON, 'GENUS'))


GBIF_ecoregion$TAXON <- trimws(GBIF_ecoregion$TAXON, which = c("right"))
GBIF_ecoregion_2 <- GBIF_ecoregion[!is.na(GBIF_ecoregion$ECO_NAME),] 


# join with inventory to remove taxa with incorrect names 
# This should be the one we want
GBIF_ecoregion_filtered_2 <- semi_join(GBIF_ecoregion_2, inventory, by="TAXON", all.x = TRUE)

# is it possible to match and update the taxon names that don't match those in the inventory?
anti_join_ecoregion <- anti_join(GBIF_ecoregion_2, inventory, by="TAXON", all.x = TRUE)
# these are names where the GBIF name at the time of submission was incorrect, mostly
# where the subsp. or variety is synonymous. Just coerce them back to plain speces,
# then rebind with the GBIF_ecoregion_filtered_2
# then filter eagain to remove any more potential mismatches
anti_join_prepped_ecoregion <- anti_join_ecoregion %>%
  mutate(TAXON = SPECIES) %>%
  mutate(RANK = "") %>%
  mutate(INFRASPECIFIC = "")

GBIF_ecoregion_filtered_3 <- rbind(GBIF_ecoregion_filtered_2, anti_join_prepped_ecoregion)
GBIF_ecoregion_filtered_4 <- semi_join(GBIF_ecoregion_filtered_3, inventory, by="TAXON", all.x = TRUE)

# those taxa with incorrect names that were reverted back to species level
# may now have multiple rows for unique TAXON x region, use distinct again 
# to get just one per unique combination
GBIF_ecoregion_filtered_4 <- GBIF_ecoregion_filtered_4 %>%
  distinct(TAXON, ECO_NAME, .keep_all = TRUE)

write.csv(GBIF_province_filtered_4, "./GBIF_download_outputs/species_distributions_province.csv")
write.csv(GBIF_ecoregion_filtered_4, "./GBIF_download_outputs/species_distributions_ecoregion.csv")


