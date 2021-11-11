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
# then conduct a geo join by province
# reduce to one unique row per taxon per province
# write an output csv
# repeat by ecoregion