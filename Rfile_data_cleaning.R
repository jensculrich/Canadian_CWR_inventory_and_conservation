library(tidyverse)

########################################################
# DATA TIDYING FILE FOR MULTIPLE DATA MANAGEMENT TASKS #
########################################################
# setwd() <- Navigate to a subfolder if needed
# CONTENTS:
# 1 - COMBINE TAXON LISTS
# 2 - PARSE TAXON NAMES
# 3 - CONVERT TO DECIMAL DEGREES
# 4 - FORMAT SHAPEFILES
# 5 - TRIM OUT OF BOUNDS SPECIES DISTRIBUTIONS
# 6 - FILTER GARDEN ACCESSIONS

##################################################
# COMBINE TAXON LISTS FOR THE INVENTORY BACKBONE #
##################################################

# To combine Davidson and GRIN taxon lists
# needed to clean the Davidson data
# wanted one column with genus species names rather than genus and species in separate columns
df <- read.csv("Davidson_Canada_raw_data.csv")

df2 <- df %>% 
  unite("TAXON", Wild_Relative_Genus:Wild_Relative_species, 
        sep = " ", remove = FALSE) %>%
  select(-Wild_Relative_Genus, -Wild_Relative_species) %>%
  rename(PRIMARY_ASSOCIATED_CROP_COMMON_NAME = Crop)
# write.csv(df2, "Davidson_Canada_cleaned_data.csv")

###########################################################
# SEPERATE TAXON NAMES INTO GENUS, SPECIES, INFRASPECIFIC #
###########################################################

# To break names of taxa in inventory so that each species has 
# one column with just the species name "SPECIES",
# second column with "RANK" (subsp. or var.),
# third column with "INFRASPECIFIC" (subspecies or varietal name)
# want to keep the full name "TAXON" column
df <- read.csv("inventory.csv")

df2 <- df %>% 
  mutate(GENUS = word(TAXON, 1)) %>%
  mutate(TAXON1 = TAXON) %>%
  separate(TAXON1, c("SPECIES1", "SPECIES2", 
                    "RANK", "INFRASPECIFC"), 
           sep = "\\ ") %>%
  unite("SPECIES", "SPECIES1", "SPECIES2", sep = " ")

# write.csv(df2, "inventory.csv")  

####################################################
# CONVERT MANUAL GBIF DOWNLOADS TO DECIMAL DEGREES #
####################################################
df <- read.csv("problemTaxa_manual_table_downloads.csv")

df <- separate(df, 'DMS', into =c("latitude", "longitude"), sep = ",")
df <- df %>%
  mutate(decimalLatitude = as.numeric(substring(latitude, 1, nchar(latitude)-1))) %>%
  mutate(decimalLongitude = as.numeric(substring(longitude, 1, nchar(longitude)-1))) %>%
  mutate(decimalLongitude = decimalLongitude * -1)

# write.csv(df, "problemTaxa_manual.csv")

df <- read.csv("problemTaxa_manual.csv")
df <- df %>% 
  mutate(GENUS = word(scientificName, 1)) %>%
  mutate(TAXON1 = scientificName) %>%
  separate(TAXON1, c("SPECIES1", "SPECIES2", 
                     "RANK", "INFRASPECIFC"), 
           sep = "\\s+") %>%
  unite("SPECIES", "SPECIES1", "SPECIES2", sep = " ")

# write.csv(df, "problemTaxa_manual.csv")

####################################################################################
# Load and format shapefile data
####################################################################################

# CRS
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# add geojson map with province boundaries 
canada_cd <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE) # 1
canada_cd <- canada_cd %>%
  rename("province" = "name")

# add geojson map with all of canada (no inner boundaries)
# we will use this as a boundary for trimming all the ecoregion maps
canada <- st_read("./Geo_Data/canada.geojson", quiet = TRUE) # 1

# add geojson map with ecoregion boundaries
world_eco <- st_read("./Geo_Data/world_ecoregions.geojson", quiet = TRUE)
# Trim geojson world map to canada ecoregions from native_occurrence_df
canada_eco <- semi_join(world_eco, native_occurrence_df_ecoregion_formatted, by=("ECO_NAME")) 

# clip ecoregions to canada national border
canada_eco_subset <- st_intersection(canada_eco, canada)
#geojsonio::geojson_write(canada_eco_subset, file = "canada_ecoregions_clipped.geojson")


############################################
# TRIM OUT OF BOUNDS SPECIES DISTRIBUTIONS #
############################################
# Feed in the ecoregion species distributions
# read in the out of bounds provinces
# if any species and ecoregion combo from out of bounds is found in 
# ecoregion distribution, then remove that row
out_of_bounds <- read.csv("out_of_bound_ranges_ecoregions.csv")
sp_distr_eco <- read.csv("species_distributions_ecoregion.csv")

sp_distr_eco <- anti_join(sp_distr_eco, out_of_bounds, by = c('SPECIES', 'ECO_NAME'))
write.csv(sp_distr_eco, "species_distributions_ecoregion_trimmed.csv")

# repeat by province
out_of_bounds <- read.csv("out_of_bound_ranges_provincial.csv")
sp_distr_province <- read.csv("species_distributions_province.csv")

sp_distr_province <- anti_join(sp_distr_province, out_of_bounds, by = c('SPECIES', 'PROVINCE'))
write.csv(sp_distr_province, "species_distributions_province_trimmed.csv")

############################
# FILTER GARDEN ACCESSIONS #
############################
##GOAL: Filter Garden/repository accessions for CWR and WUS taxa
##END-RESULT: A list of all CWR accessions from Garden/repository
library(stringr)

# setwd()
master_list <- read.csv("inventory.csv")

#read data for whichever garden you want to compare
rbg<- read.csv("UBC_bg.csv")

#Most gardens have variant info in their species name so we need to seperate that out in order to compare with our Master List

#rbg_replace <- gsub("\\[", "(", rbg$NAME)  #messy work-around: 
#replace '[' with '(' because str_split_fixed function can't deal with '['
#rebinded <- cbind(rbg_replace, rbg) #rebind them together so it's recursive
#is.recursive(rebinded) #check to see if the data is recursive, str_split_fixed won't work with atomic only recursive


#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(rbg$TaxonName, " var. | cv.| subsp.|[']|[(]|sp.", 2)  #seperate names into 2 columns
# based off of different ways 'variant' 
#categories were written in the in the database

total<-cbind(rbg, split_var) #put split columns back into original RBG database with proper headings
colnames(total)[43] <- "species" 
colnames(total)[44] <- "variant"

total$species <- trimws(total$species, which = c("right"))  #remove trailing white space on species names

CWR_of_RBG<-merge(master_list,total, by.x = "sci_name", by.y = "species") #Finally, cross-reference RBG list with Master list
#getting a list of all CWR plants in RBG

# write.csv(CWR_of_RBG, "CWR_of_UBC.csv")  

#####################################
# TIDY PGRC and USDA ACCESSION DATA #
#####################################
### NOV 1 - Tidying and seperating column names of PGRC accession data PGRC DATASETS ####
pgrc_US<- read.csv("PGRC_accessions_USA.csv")

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(pgrc_US$Taxonomy, "\\s+", 3)  #seperate names into 3 columns, \\s+ is any sized white space

total<-cbind(pgrc_US, split_var) #put split columns back into original database and rename new columns
colnames(total)[6] <- "Genus" 
colnames(total)[7] <- "Species_only" 
colnames(total)[8] <- "The_rest" 


split_var <- str_split_fixed(total$The_rest, " var.\\s+", 2)
total<-cbind(total, split_var) #split off text for names with variants
colnames(total)[9] <- "not_needed" 
colnames(total)[10] <- "Variant_interm" 


split_var <- str_split_fixed(total$The_rest, "subsp.\\s+", 2)
total<-cbind(total, split_var) #split off text for names with subspecies
colnames(total)[11] <- "not_needed2" 
colnames(total)[12] <- "Subspecies_interm" 

#now get rid of all the columns and text within columns that aren't needed
total <- subset(total, select=-c(not_needed,not_needed2, The_rest))

#now splitting the newly created columns again to get rid of text after the variant or subspecies names
split_var <- str_split_fixed(total$Variant_interm, "\\s+", 2)  #seperate names into 2 columns, \\s+ is any sized white space
total<-cbind(total, split_var) 
colnames(total)[10] <- "Variant" 
colnames(total)[11] <- "garbage" 
split_var <- str_split_fixed(total$Subspecies_interm, "\\s+", 2)  #seperate names into 2 columns, \\s+ is any sized white space
total<-cbind(total, split_var) 
colnames(total)[12] <- "Subspecies" 
colnames(total)[13] <- "garbage_2" 

total <- subset(total, select=-c(Variant_interm, Subspecies_interm, garbage, garbage_2)) #removing unneeded columns

#giving correct name in Rank Column using iselse statement 
total$Rank <- ifelse(total$Variant!="", "Var.",
                     ifelse(total$Subspecies!="", "Subsp.", ""))
total$Infraspecific <- paste(total$Variant, total$Subspecies)
total$Infraspecific <- trimws(total$Infraspecific, which = c("both"))  #remove trailing white space on species names

total$Species<- paste(total$Genus, total$Species_only)
total$Taxon <-  paste(total$Species, total$Rank, total$Infraspecific)

total <- subset(total, select=-c(Variant, Subspecies, Species_only))
total <- total[, c(1,2,3,4,5,10,6,9,7,8)] #reorder into the order we want

# write.csv(total, "Cleaned_PGRC_US.csv")  


### NOV 1 - Tidying and seperating column names of PGRC accession data USDA DATASETS####
USDA_cad<- read.csv("USDA_NPGS_accessions_Canada.csv")

split_var <- str_split_fixed(USDA_cad$TAXONOMY, "\\s+", 3)  #seperate names into 3 columns, \\s+ is any sized white space

total<-cbind(USDA_cad, split_var) #put split columns back into database then rename the columns
colnames(total)[8] <- "Genus" 
colnames(total)[9] <- "Species_only" 
colnames(total)[10] <- "The_rest" 


split_var <- str_split_fixed(total$The_rest, " var.\\s+", 2) #split again to get variant info
total<-cbind(total, split_var) #put split columns back 
colnames(total)[11] <- "not_needed" 
colnames(total)[12] <- "Variant_interm" 


split_var <- str_split_fixed(total$The_rest, "subsp.\\s+", 2) #split for subspecies info
total<-cbind(total, split_var) 
colnames(total)[13] <- "not_needed2" 
colnames(total)[14] <- "Subspecies_interm" 

#now get rid of all the columns and text within columns that aren't needed
total <- subset(total, select=-c(not_needed,not_needed2, The_rest))

#now we're removing all unneeded text after the variant + subspecies information
split_var <- str_split_fixed(total$Variant_interm, "\\s+", 2)  #seperate names into 2 columns, \\s+ is any sized white space
total<-cbind(total, split_var) 
colnames(total)[12] <- "Variant" 
colnames(total)[13] <- "garbage" 
split_var <- str_split_fixed(total$Subspecies_interm, "\\s+", 2)  #seperate names into 2 columns, \\s+ is any sized white space
total<-cbind(total, split_var) 
colnames(total)[14] <- "Subspecies" 
colnames(total)[15] <- "garbage_2" 

total <- subset(total, select=-c(Variant_interm, Subspecies_interm, garbage, garbage_2))
#give proper name to Rank Column using iselse statement
total$Rank <- ifelse(total$Variant!="", "Var.",
                     ifelse(total$Subspecies!="", "Subsp.", ""))
total$Infraspecific <- paste(total$Variant, total$Subspecies)
total$Infraspecific <- trimws(total$Infraspecific, which = c("both"))  #remove trailing white space on species names

total$Species<- paste(total$Genus, total$Species_only)
total$Taxon <-  paste(total$Species, total$Rank, total$Infraspecific)

total <- subset(total, select=-c(Variant, Subspecies, Species_only))
total <- total[, c(1,2,3,4,5,6,7,12,8,11,9,10)] #reorder into the order we want

# write.csv(total, "Cleaned_USDA_CAD.csv")  

