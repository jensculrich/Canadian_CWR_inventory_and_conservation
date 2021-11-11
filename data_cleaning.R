library(tidyverse)

########################################################
# DATA TIDYING FILE FOR MULTIPLE DATA MANAGEMENT TASKS #
########################################################
# setwd() <- Navigate to a subfolder if needed

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
