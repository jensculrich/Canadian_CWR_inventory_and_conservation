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
# then conduct a geo join by province (don't need to do this because province is there)
# then conduct a geo join by ecoregion to add ecoregion data
library(geojsonsf)
library(jsonlite)
library(sf)

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
  select(genus, species, infraspecificEpithet, taxonRank, scientificName, 
         verbatimScientificName, stateProvince, decimalLatitude, decimalLongitude
         )

# Convert to geoJSON for spatial projection
spatial_df <- df_geojson(df = df, lon = "decimalLongitude", lat = "decimalLatitude")
sf <- geojson_sf(spatial_df)

# Now join with ecoregion boundaries
setwd("~/R/Canadian_CWR_inventory_and_conservation/Geo_Data/")
ecoregions <- st_read("canada_ecoregions_clipped.geojson")
str(ecoregions)

shape_df <- st_join(ecoregions, sf)
output_df <- shape_df %>%
  select(genus, species, infraspecificEpithet, taxonRank, scientificName, 
         verbatimScientificName, stateProvince, geometry,
         ECO_NAME
  )
# replace "QuÃ©bec" with "Quebec"
output_df <- output_df %>% 
  mutate(stateProvince = str_replace(stateProvince, "QuÃ©bec", "Quebec"))

GBIF_province <- output_df %>%
  group_by(scientificName) %>%
  distinct(stateProvince, .keep_all = TRUE) 

%>%
  dplyr::select(sci_nam, PRENAME) %>%
  # distinct(sci_nam) # this line just to see how many species (remove for actual processing)
  # has a bunch of extra species?
  # join with cwr_list to pair it down?
  left_join(cwr_list, .)