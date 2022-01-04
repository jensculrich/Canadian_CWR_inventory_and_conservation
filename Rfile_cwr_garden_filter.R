##GOAL: Compare Botanical Garden accession list with our Master Canadian CWR list
##END-RESULT: A list of all CWR accessions from any botanical garden you upload and filter against
##last updated: Nov 17, 2021, by Kephra Beckett

##WORK FLOW: Repeat of similar (but tweaked to each garden's particular dataset) code. There is a sub-header for each seperate garden

#load packages
library(stringr)
library(dplyr)

master_list <- read.csv("inventory.csv") #complete inventory of all identified CWRs in Canada, this is what we will be using to filter each botanical garden with

#########Royal Botanical Garden###############

#Read data for whichever garden you want to compare
rbg<- read.csv("rbg_all.csv") #ROYAL BOTANICAL GARDEN
rbg<- rbg %>% 
  mutate(NAME = str_replace(NAME, "x", "")) #removing 'x' symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(rbg$NAME, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

rbg_cleaned<-cbind(rbg, split_var) #put split columns back into original database and rename new columns
colnames(rbg_cleaned)[25] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(rbg_cleaned)[26] <- "Species_only" 
colnames(rbg_cleaned)[27] <- "The_rest" 


split_var <- str_split_fixed(rbg_cleaned$The_rest, "var.\\s+", 2)
rbg_cleaned<-cbind(rbg_cleaned, split_var) #split off text for names with variants
colnames(rbg_cleaned)[28] <- "not_needed" 
colnames(rbg_cleaned)[29] <- "Variant" 


split_var <- str_split_fixed(rbg_cleaned$The_rest, "subsp.\\s+", 2)
rbg_cleaned<-cbind(rbg_cleaned, split_var) #split off text for names with subspecies
colnames(rbg_cleaned)[30] <- "not_needed2" 
colnames(rbg_cleaned)[31] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
rbg_cleaned$Rank <- ifelse(rbg_cleaned$Variant!="", "var.",
                     ifelse(rbg_cleaned$Subspecies!="", "subsp.", ""))
rbg_cleaned$Infraspecific <- paste(rbg_cleaned$Variant, rbg_cleaned$Subspecies)
rbg_cleaned$Infraspecific <- trimws(rbg_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

rbg_cleaned$Species<- paste(rbg_cleaned$Genus, rbg_cleaned$Species_only) #now join columns to get full species name
rbg_cleaned$Taxon <-  paste(rbg_cleaned$Species, rbg_cleaned$Rank, rbg_cleaned$Infraspecific) #now join columns to get full taxon name
rbg_cleaned <- subset(rbg_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
rbg_cleaned$Taxon <- trimws(rbg_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_RBG<-merge(master_list,rbg_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference RBG list with Master list
#getting a list of all CWR plants in RBG

write.csv(CWR_of_RBG, "CWR_of_RBG_nov17.csv")  
rm(CWR_of_RBG, bg, rbg, rbg_cleaned, split_var)

############Botanical Garden of Montreal#################

#Read data for whichever garden you want to compare
montreal<- read.csv("Montreal_bg.csv") 
montreal<- montreal %>% 
  mutate(NAME = str_replace(TAXON, "x", "")) #removing 'x' symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(montreal$TAXON, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

montreal_cleaned<-cbind(montreal, split_var) #put split columns back into original database and rename new columns
colnames(montreal_cleaned)[15] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(montreal_cleaned)[16] <- "Species_only" 
colnames(montreal_cleaned)[17] <- "The_rest" 


split_var <- str_split_fixed(montreal_cleaned$The_rest, "var.\\s+", 2)
montreal_cleaned<-cbind(montreal_cleaned, split_var) #split off text for names with variants
colnames(montreal_cleaned)[18] <- "not_needed" 
colnames(montreal_cleaned)[19] <- "Variant" 


split_var <- str_split_fixed(montreal_cleaned$The_rest, "subsp.\\s+", 2)
montreal_cleaned<-cbind(montreal_cleaned, split_var) #split off text for names with subspecies
colnames(montreal_cleaned)[20] <- "not_needed2" 
colnames(montreal_cleaned)[21] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
montreal_cleaned$Rank <- ifelse(montreal_cleaned$Variant!="", "var.",
                           ifelse(montreal_cleaned$Subspecies!="", "subsp.", ""))
montreal_cleaned$Infraspecific <- paste(montreal_cleaned$Variant, montreal_cleaned$Subspecies)
montreal_cleaned$Infraspecific <- trimws(montreal_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

montreal_cleaned$Species<- paste(montreal_cleaned$Genus, montreal_cleaned$Species_only) #now join columns to get full species name
montreal_cleaned$Taxon <-  paste(montreal_cleaned$Species, montreal_cleaned$Rank, montreal_cleaned$Infraspecific) #now join columns to get full taxon name
montreal_cleaned <- subset(montreal_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
montreal_cleaned$Taxon <- trimws(montreal_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_montreal<-merge(master_list,montreal_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference montreal list with Master list
#getting a list of all CWR plants in montreal

write.csv(CWR_of_montreal, "CWR_of_montreal_nov17.csv")  
rm(CWR_of_montreal, montreal, montreal_cleaned, split_var)


############University of Guelph Botanical Garden#################

#Read data for whichever garden you want to compare
guelph<- read.csv("UofGuelphBG.csv") 
guelph<- guelph %>% 
  mutate(NAME = str_replace(TaxonName, "×", "")) #removing 'x' symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(guelph$TaxonName, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

guelph_cleaned<-cbind(guelph, split_var) #put split columns back into original database and rename new columns
colnames(guelph_cleaned)[16] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(guelph_cleaned)[17] <- "Species_only" 
colnames(guelph_cleaned)[18] <- "The_rest" 


split_var <- str_split_fixed(guelph_cleaned$The_rest, "var.\\s+", 2)
guelph_cleaned<-cbind(guelph_cleaned, split_var) #split off text for names with variants
colnames(guelph_cleaned)[19] <- "not_needed" 
colnames(guelph_cleaned)[20] <- "Variant" 


split_var <- str_split_fixed(guelph_cleaned$The_rest, "subsp.\\s+", 2)
guelph_cleaned<-cbind(guelph_cleaned, split_var) #split off text for names with subspecies
colnames(guelph_cleaned)[21] <- "not_needed2" 
colnames(guelph_cleaned)[22] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
guelph_cleaned$Rank <- ifelse(guelph_cleaned$Variant!="", "var.",
                                ifelse(guelph_cleaned$Subspecies!="", "subsp.", ""))
guelph_cleaned$Infraspecific <- paste(guelph_cleaned$Variant, guelph_cleaned$Subspecies)
guelph_cleaned$Infraspecific <- trimws(guelph_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

guelph_cleaned$Species<- paste(guelph_cleaned$Genus, guelph_cleaned$Species_only) #now join columns to get full species name
guelph_cleaned$Taxon <-  paste(guelph_cleaned$Species, guelph_cleaned$Rank, guelph_cleaned$Infraspecific) #now join columns to get full taxon name
guelph_cleaned <- subset(guelph_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
guelph_cleaned$Taxon <- trimws(guelph_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_guelph<-merge(master_list,guelph_cleaned, by.x = "TAXON", by.y = "TaxonName") #Finally, cross-reference guelph list with Master list
#getting a list of all CWR plants in guelph

write.csv(CWR_of_guelph, "CWR_of_guelph_nov17.csv")  
rm(CWR_of_guelph, guelph, guelph_cleaned, split_var)

############University of Saskatchewan Botanical Garden#################

#Read data for whichever garden you want to compare
sask<- read.csv("UofSaskatchewan.csv") 
#This dataset does not have any variant or subspecies information in the taxonomic name, so no need to properly clean 
#it before filtering against the master list

CWR_of_sask<-merge(master_list,sask, by.x = "TAXON", by.y = "Species") #cross-reference sask list with Master list
#getting a list of all CWR plants in sask

write.csv(CWR_of_sask, "CWR_of_sask_nov17.csv")  
rm(CWR_of_sask, sask)

############Reader Rock Garden#################

#Read data for whichever garden you want to compare
reader_rock<- read.csv("Reader Rock's plant accessions.csv") 
reader_rock<- reader_rock %>% 
  mutate(Scientific.Name = str_replace(Scientific.Name, "x", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

reader_rock$Scientific.Name<-gsub("[[:punct:]]","",as.character(reader_rock$Scientific.Name)) #remove astericks symbol (plus anything other punctuation from names)

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(reader_rock$Scientific.Name, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

reader_rock_cleaned<-cbind(reader_rock, split_var) #put split columns back into original database and rename new columns
colnames(reader_rock_cleaned)[124] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(reader_rock_cleaned)[125] <- "Species_only" 
colnames(reader_rock_cleaned)[126] <- "The_rest" 


split_var <- str_split_fixed(reader_rock_cleaned$The_rest, "var\\s+", 2)
reader_rock_cleaned<-cbind(reader_rock_cleaned, split_var) #split off text for names with variants
colnames(reader_rock_cleaned)[127] <- "not_needed" 
colnames(reader_rock_cleaned)[128] <- "Variant" 


split_var <- str_split_fixed(reader_rock_cleaned$The_rest, "ssp\\s+", 2)
reader_rock_cleaned<-cbind(reader_rock_cleaned, split_var) #split off text for names with subspecies
colnames(reader_rock_cleaned)[129] <- "not_needed2" 
colnames(reader_rock_cleaned)[130] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
reader_rock_cleaned$Rank <- ifelse(reader_rock_cleaned$Variant!="", "var.",
                              ifelse(reader_rock_cleaned$Subspecies!="", "subsp.", ""))
reader_rock_cleaned$Infraspecific <- paste(reader_rock_cleaned$Variant, reader_rock_cleaned$Subspecies)
reader_rock_cleaned$Infraspecific <- trimws(reader_rock_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

reader_rock_cleaned$Species<- paste(reader_rock_cleaned$Genus, reader_rock_cleaned$Species_only) #now join columns to get full species name
reader_rock_cleaned$Taxon <-  paste(reader_rock_cleaned$Species, reader_rock_cleaned$Rank, reader_rock_cleaned$Infraspecific) #now join columns to get full taxon name
reader_rock_cleaned <- subset(reader_rock_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
reader_rock_cleaned$Taxon <- trimws(reader_rock_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_reader_rock<-merge(master_list,reader_rock_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference reader_rock list with Master list
#getting a list of all CWR plants in reader_rock

write.csv(CWR_of_reader_rock, "CWR_of_reader_rock_nov17.csv")  
rm(CWR_of_reader_rock, reader_rock, reader_rock_cleaned, split_var)

############Van Dusen Botanical Garden#################

#Read data for whichever garden you want to compare
van_dusen<- read.csv("VanDusen_bg.csv") 
van_dusen<- van_dusen %>% 
  mutate(NAME = str_replace(NAME, "x", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(van_dusen$NAME, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

van_dusen_cleaned<-cbind(van_dusen, split_var) #put split columns back into original database and rename new columns
colnames(van_dusen_cleaned)[12] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(van_dusen_cleaned)[13] <- "Species_only" 
colnames(van_dusen_cleaned)[14] <- "The_rest" 


split_var <- str_split_fixed(van_dusen_cleaned$The_rest, "var.\\s+", 2)
van_dusen_cleaned<-cbind(van_dusen_cleaned, split_var) #split off text for names with variants
colnames(van_dusen_cleaned)[15] <- "not_needed" 
colnames(van_dusen_cleaned)[16] <- "Variant" 


split_var <- str_split_fixed(van_dusen_cleaned$The_rest, "ssp.\\s+", 2)
van_dusen_cleaned<-cbind(van_dusen_cleaned, split_var) #split off text for names with subspecies
colnames(van_dusen_cleaned)[17] <- "not_needed2" 
colnames(van_dusen_cleaned)[18] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
van_dusen_cleaned$Rank <- ifelse(van_dusen_cleaned$Variant!="", "var.",
                                   ifelse(van_dusen_cleaned$Subspecies!="", "subsp.", ""))
van_dusen_cleaned$Infraspecific <- paste(van_dusen_cleaned$Variant, van_dusen_cleaned$Subspecies)
van_dusen_cleaned$Infraspecific <- trimws(van_dusen_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

van_dusen_cleaned$Species<- paste(van_dusen_cleaned$Genus, van_dusen_cleaned$Species_only) #now join columns to get full species name
van_dusen_cleaned$Taxon <-  paste(van_dusen_cleaned$Species, van_dusen_cleaned$Rank, van_dusen_cleaned$Infraspecific) #now join columns to get full taxon name
van_dusen_cleaned <- subset(van_dusen_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
van_dusen_cleaned$Taxon <- trimws(van_dusen_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_van_dusen<-merge(master_list,van_dusen_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference van_dusen list with Master list
#getting a list of all CWR plants in van_dusen

write.csv(CWR_of_van_dusen, "CWR_of_van_dusen_nov17.csv")  
rm(CWR_of_van_dusen, van_dusen, van_dusen_cleaned, split_var)

############ University of British Columbia (UBC) Botanical Garden#################

#Read data for whichever garden you want to compare
ubc<- read.csv("UBC_bg.csv") 
ubc<- ubc %>% 
  mutate(TaxonName = str_replace(TaxonName, "x", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(ubc$TaxonName, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

ubc_cleaned<-cbind(ubc, split_var) #put split columns back into original database and rename new columns
colnames(ubc_cleaned)[43] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(ubc_cleaned)[44] <- "Species_only" 
colnames(ubc_cleaned)[45] <- "The_rest" 


split_var <- str_split_fixed(ubc_cleaned$The_rest, "var.\\s+", 2)
ubc_cleaned<-cbind(ubc_cleaned, split_var) #split off text for names with variants
colnames(ubc_cleaned)[46] <- "not_needed" 
colnames(ubc_cleaned)[47] <- "Variant" 


split_var <- str_split_fixed(ubc_cleaned$The_rest, "subsp.\\s+", 2)
ubc_cleaned<-cbind(ubc_cleaned, split_var) #split off text for names with subspecies
colnames(ubc_cleaned)[48] <- "not_needed2" 
colnames(ubc_cleaned)[49] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
ubc_cleaned$Rank <- ifelse(ubc_cleaned$Variant!="", "var.",
                                 ifelse(ubc_cleaned$Subspecies!="", "subsp.", ""))
ubc_cleaned$Infraspecific <- paste(ubc_cleaned$Variant, ubc_cleaned$Subspecies)
ubc_cleaned$Infraspecific <- trimws(ubc_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

ubc_cleaned$Species<- paste(ubc_cleaned$Genus, ubc_cleaned$Species_only) #now join columns to get full species name
ubc_cleaned$Taxon <-  paste(ubc_cleaned$Species, ubc_cleaned$Rank, ubc_cleaned$Infraspecific) #now join columns to get full taxon name
ubc_cleaned <- subset(ubc_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
ubc_cleaned$Taxon <- trimws(ubc_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_ubc<-merge(master_list,ubc_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference ubc list with Master list
#getting a list of all CWR plants in ubc

write.csv(CWR_of_ubc, "CWR_of_ubc_nov17.csv")  
rm(CWR_of_ubc, ubc, ubc_cleaned, split_var)

############ Mount Pleasant Group Cemetery Garden#################

#Read data for whichever garden you want to compare
mpg<- read.csv("MountPleasantGroup.csv") #Note that there are no repeat names, instead they have a count column!
mpg<- mpg %>% 
  mutate(Botanical.Name = str_replace(Botanical.Name, "x", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(mpg$Botanical.Name, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

mpg_cleaned<-cbind(mpg, split_var) #put split columns back into original database and rename new columns
colnames(mpg_cleaned)[4] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(mpg_cleaned)[5] <- "Species_only" 
colnames(mpg_cleaned)[6] <- "The_rest" 


split_var <- str_split_fixed(mpg_cleaned$The_rest, "var.\\s+", 2)
mpg_cleaned<-cbind(mpg_cleaned, split_var) #split off text for names with variants
colnames(mpg_cleaned)[7] <- "not_needed" 
colnames(mpg_cleaned)[8] <- "Variant" 


split_var <- str_split_fixed(mpg_cleaned$The_rest, "subsp\\s+", 2)
mpg_cleaned<-cbind(mpg_cleaned, split_var) #split off text for names with subspecies
colnames(mpg_cleaned)[9] <- "not_needed2" 
colnames(mpg_cleaned)[10] <- "Subspecies" 


#giving correct name in Rank Column using iselse statement 
mpg_cleaned$Rank <- ifelse(mpg_cleaned$Variant!="", "var.",
                           ifelse(mpg_cleaned$Subspecies!="", "subsp.", ""))
mpg_cleaned$Infraspecific <- paste(mpg_cleaned$Variant, mpg_cleaned$Subspecies)
mpg_cleaned$Infraspecific <- trimws(mpg_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

mpg_cleaned$Species<- paste(mpg_cleaned$Genus, mpg_cleaned$Species_only) #now join columns to get full species name
mpg_cleaned$Taxon <-  paste(mpg_cleaned$Species, mpg_cleaned$Rank, mpg_cleaned$Infraspecific) #now join columns to get full taxon name
mpg_cleaned <- subset(mpg_cleaned, select=-c(The_rest, not_needed, not_needed2)) #get rid of columns we don't need
mpg_cleaned$Taxon <- trimws(mpg_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_mpg<-merge(master_list,mpg_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference mpg list with Master list
#getting a list of all CWR plants in mpg

write.csv(CWR_of_mpg, "CWR_of_mpg_nov17.csv")  
rm(CWR_of_mpg, mpg, mpg_cleaned, split_var)


############ PGRC USA #################

#Read data for whichever garden you want to compare
pgrc_us<- read.csv("PGRC_accessions_USA.csv") #Note that there are no repeat names, instead they have a count column!
pgrc_us<- pgrc_us %>% 
  mutate(Taxonomy = str_replace(Taxonomy, "<d7>", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

pgrc_us$Taxonomy<-gsub("[[:punct:]]","",as.character(pgrc_us$Taxonomy))
#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(pgrc_us$Taxonomy, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

pgrc_us_cleaned<-cbind(pgrc_us, split_var) #put split columns back into original database and rename new columns
colnames(pgrc_us_cleaned)[6] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(pgrc_us_cleaned)[7] <- "Species_only" 
colnames(pgrc_us_cleaned)[8] <- "The_rest" 

split_var <- str_split_fixed(pgrc_us_cleaned$The_rest, "var\\s+", 2)
pgrc_us_cleaned<-cbind(pgrc_us_cleaned, split_var) #split off text for names with variants
colnames(pgrc_us_cleaned)[9] <- "not_needed" 
colnames(pgrc_us_cleaned)[10] <- "Variant_interm" 
split_var <- str_split_fixed(pgrc_us_cleaned$Variant_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
pgrc_us_cleaned<-cbind(pgrc_us_cleaned, split_var) 
colnames(pgrc_us_cleaned)[11] <- "Variant" 
colnames(pgrc_us_cleaned)[12] <- "not_needed2" 


split_var <- str_split_fixed(pgrc_us_cleaned$The_rest, "subsp\\s+", 2)
pgrc_us_cleaned<-cbind(pgrc_us_cleaned, split_var) #split off text for names with subspecies
colnames(pgrc_us_cleaned)[13] <- "not_needed3" 
colnames(pgrc_us_cleaned)[14] <- "Subspecies_interm" 
split_var <- str_split_fixed(pgrc_us_cleaned$Subspecies_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
pgrc_us_cleaned<-cbind(pgrc_us_cleaned, split_var) 
colnames(pgrc_us_cleaned)[15] <- "Subspecies" 
colnames(pgrc_us_cleaned)[16] <- "not_needed4" 


#giving correct name in Rank Column using iselse statement 
pgrc_us_cleaned$Rank <- ifelse(pgrc_us_cleaned$Variant!="", "var.",
                           ifelse(pgrc_us_cleaned$Subspecies!="", "subsp.", ""))
pgrc_us_cleaned$Infraspecific <- paste(pgrc_us_cleaned$Variant, pgrc_us_cleaned$Subspecies)
pgrc_us_cleaned$Infraspecific <- trimws(pgrc_us_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

pgrc_us_cleaned$Species<- paste(pgrc_us_cleaned$Genus, pgrc_us_cleaned$Species_only) #now join columns to get full species name
pgrc_us_cleaned$Taxon <-  paste(pgrc_us_cleaned$Species, pgrc_us_cleaned$Rank, pgrc_us_cleaned$Infraspecific) #now join columns to get full taxon name
pgrc_us_cleaned <- subset(pgrc_us_cleaned, select=-c(The_rest, not_needed, not_needed2, Variant_interm, Subspecies_interm, not_needed3, not_needed4)) #get rid of columns we don't need
pgrc_us_cleaned$Taxon <- trimws(pgrc_us_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_pgrc_us<-merge(master_list,pgrc_us_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference pgrc_us list with Master list
#getting a list of all CWR plants in pgrc_us

write.csv(CWR_of_pgrc_us, "CWR_of_pgrc_us_nov17.csv")  
rm(CWR_of_pgrc_us, pgrc_us, pgrc_us_cleaned, split_var)

############ PGRC CANADA #################

#Read data for whichever garden you want to compare
pgrc_cad<- read.csv("PGRC_accessions_Canada.csv") #Note that there are no repeat names, instead they have a count column!
pgrc_cad<- pgrc_cad %>% 
  mutate(Taxonomy = str_replace(Taxonomy, "×", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(pgrc_cad$Taxonomy, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

pgrc_cad_cleaned<-cbind(pgrc_cad, split_var) #put split columns back into original database and rename new columns
colnames(pgrc_cad_cleaned)[6] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(pgrc_cad_cleaned)[7] <- "Species_only" 
colnames(pgrc_cad_cleaned)[8] <- "The_rest" 

split_var <- str_split_fixed(pgrc_cad_cleaned$The_rest, "var.\\s+", 2)
pgrc_cad_cleaned<-cbind(pgrc_cad_cleaned, split_var) #split off text for names with variants
colnames(pgrc_cad_cleaned)[9] <- "not_needed" 
colnames(pgrc_cad_cleaned)[10] <- "Variant_interm" 
split_var <- str_split_fixed(pgrc_cad_cleaned$Variant_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
pgrc_cad_cleaned<-cbind(pgrc_cad_cleaned, split_var) 
colnames(pgrc_cad_cleaned)[11] <- "Variant" 
colnames(pgrc_cad_cleaned)[12] <- "not_needed2" 


split_var <- str_split_fixed(pgrc_cad_cleaned$The_rest, "subsp.\\s+", 2)
pgrc_cad_cleaned<-cbind(pgrc_cad_cleaned, split_var) #split off text for names with subspecies
colnames(pgrc_cad_cleaned)[13] <- "not_needed3" 
colnames(pgrc_cad_cleaned)[14] <- "Subspecies_interm" 
split_var <- str_split_fixed(pgrc_cad_cleaned$Subspecies_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
pgrc_cad_cleaned<-cbind(pgrc_cad_cleaned, split_var) 
colnames(pgrc_cad_cleaned)[15] <- "Subspecies" 
colnames(pgrc_cad_cleaned)[16] <- "not_needed4" 


#giving correct name in Rank Column using iselse statement 
pgrc_cad_cleaned$Rank <- ifelse(pgrc_cad_cleaned$Variant!="", "var.",
                               ifelse(pgrc_cad_cleaned$Subspecies!="", "subsp.", ""))
pgrc_cad_cleaned$Infraspecific <- paste(pgrc_cad_cleaned$Variant, pgrc_cad_cleaned$Subspecies)
pgrc_cad_cleaned$Infraspecific <- trimws(pgrc_cad_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

pgrc_cad_cleaned$Species<- paste(pgrc_cad_cleaned$Genus, pgrc_cad_cleaned$Species_only) #now join columns to get full species name
pgrc_cad_cleaned$Taxon <-  paste(pgrc_cad_cleaned$Species, pgrc_cad_cleaned$Rank, pgrc_cad_cleaned$Infraspecific) #now join columns to get full taxon name
pgrc_cad_cleaned <- subset(pgrc_cad_cleaned, select=-c(The_rest, not_needed, not_needed2, Variant_interm, Subspecies_interm, not_needed3, not_needed4)) #get rid of columns we don't need
pgrc_cad_cleaned$Taxon <- trimws(pgrc_cad_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_pgrc_cad<-merge(master_list,pgrc_cad_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference pgrc_cad list with Master list
#getting a list of all CWR plants in pgrc_cad

write.csv(CWR_of_pgrc_cad, "CWR_of_pgrc_cad_nov17.csv")  
rm(CWR_of_pgrc_cad, pgrc_cad, pgrc_cad_cleaned, split_var)


############ USDA NPGS CANADA #################

#Read data for whichever garden you want to compare
usda_cad<- read.csv("USDA_NPGS_accessions_Canada.csv") #Note that there are no repeat names, instead they have a count column!

#no hybrids in this dataset

#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(usda_cad$TAXONOMY, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

usda_cad_cleaned<-cbind(usda_cad, split_var) #put split columns back into original database and rename new columns
colnames(usda_cad_cleaned)[8] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(usda_cad_cleaned)[9] <- "Species_only" 
colnames(usda_cad_cleaned)[10] <- "The_rest" 

split_var <- str_split_fixed(usda_cad_cleaned$The_rest, "var.\\s+", 2)
usda_cad_cleaned<-cbind(usda_cad_cleaned, split_var) #split off text for names with variants
colnames(usda_cad_cleaned)[11] <- "not_needed" 
colnames(usda_cad_cleaned)[12] <- "Variant_interm" 
split_var <- str_split_fixed(usda_cad_cleaned$Variant_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
usda_cad_cleaned<-cbind(usda_cad_cleaned, split_var) 
colnames(usda_cad_cleaned)[13] <- "Variant" 
colnames(usda_cad_cleaned)[14] <- "not_needed2" 


split_var <- str_split_fixed(usda_cad_cleaned$The_rest, "subsp.\\s+", 2)
usda_cad_cleaned<-cbind(usda_cad_cleaned, split_var) #split off text for names with subspecies
colnames(usda_cad_cleaned)[15] <- "not_needed3" 
colnames(usda_cad_cleaned)[16] <- "Subspecies_interm" 
split_var <- str_split_fixed(usda_cad_cleaned$Subspecies_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
usda_cad_cleaned<-cbind(usda_cad_cleaned, split_var) 
colnames(usda_cad_cleaned)[17] <- "Subspecies" 
colnames(usda_cad_cleaned)[18] <- "not_needed4" 


#giving correct name in Rank Column using iselse statement 
usda_cad_cleaned$Rank <- ifelse(usda_cad_cleaned$Variant!="", "var.",
                                ifelse(usda_cad_cleaned$Subspecies!="", "subsp.", ""))
usda_cad_cleaned$Infraspecific <- paste(usda_cad_cleaned$Variant, usda_cad_cleaned$Subspecies)
usda_cad_cleaned$Infraspecific <- trimws(usda_cad_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

usda_cad_cleaned$Species<- paste(usda_cad_cleaned$Genus, usda_cad_cleaned$Species_only) #now join columns to get full species name
usda_cad_cleaned$Taxon <-  paste(usda_cad_cleaned$Species, usda_cad_cleaned$Rank, usda_cad_cleaned$Infraspecific) #now join columns to get full taxon name
usda_cad_cleaned <- subset(usda_cad_cleaned, select=-c(The_rest, not_needed, not_needed2, Variant_interm, Subspecies_interm, not_needed3, not_needed4)) #get rid of columns we don't need
usda_cad_cleaned$Taxon <- trimws(usda_cad_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_usda_cad<-merge(master_list,usda_cad_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference usda_cad list with Master list
#getting a list of all CWR plants in usda_cad

write.csv(CWR_of_usda_cad, "CWR_of_usda_cad_nov17.csv")  
rm(CWR_of_usda_cad, usda_cad, usda_cad_cleaned, split_var)

############ USDA NPGS USA #################

#Read data for whichever garden you want to compare
usda_usa<- read.csv("USDA_NPGS_accessions_USA.csv") #Note that there are no repeat names, instead they have a count column!

usda_usa<- usda_usa %>% 
  mutate(TAXONOMY = str_replace(TAXONOMY, "×", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!


#Before we can filter the garden's collection against the master CWR list, we must put it into the same format as our master list 
#This means, having separate columns for the Taxon, Genus, Species, Rank and Intraspecific

#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(usda_usa$TAXONOMY, "\\s+", 3)  #separate any names with spaces into 3 columns , \\s+ is any sized white space

usda_usa_cleaned<-cbind(usda_usa, split_var) #put split columns back into original database and rename new columns
colnames(usda_usa_cleaned)[8] <- "Genus" # the column numbers are the last 3 of the dataframe
colnames(usda_usa_cleaned)[9] <- "Species_only" 
colnames(usda_usa_cleaned)[10] <- "The_rest" 

split_var <- str_split_fixed(usda_usa_cleaned$The_rest, "var.\\s+", 2)
usda_usa_cleaned<-cbind(usda_usa_cleaned, split_var) #split off text for names with variants
colnames(usda_usa_cleaned)[11] <- "not_needed" 
colnames(usda_usa_cleaned)[12] <- "Variant_interm" 
split_var <- str_split_fixed(usda_usa_cleaned$Variant_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
usda_usa_cleaned<-cbind(usda_usa_cleaned, split_var) 
colnames(usda_usa_cleaned)[13] <- "Variant" 
colnames(usda_usa_cleaned)[14] <- "not_needed2" 


split_var <- str_split_fixed(usda_usa_cleaned$The_rest, "subsp.\\s+", 2)
usda_usa_cleaned<-cbind(usda_usa_cleaned, split_var) #split off text for names with subspecies
colnames(usda_usa_cleaned)[15] <- "not_needed3" 
colnames(usda_usa_cleaned)[16] <- "Subspecies_interm" 
split_var <- str_split_fixed(usda_usa_cleaned$Subspecies_interm, "\\s+", 2)  #now we need to remove excess writing after the variant name
usda_usa_cleaned<-cbind(usda_usa_cleaned, split_var) 
colnames(usda_usa_cleaned)[17] <- "Subspecies" 
colnames(usda_usa_cleaned)[18] <- "not_needed4" 


#giving correct name in Rank Column using iselse statement 
usda_usa_cleaned$Rank <- ifelse(usda_usa_cleaned$Variant!="", "var.",
                                ifelse(usda_usa_cleaned$Subspecies!="", "subsp.", ""))
usda_usa_cleaned$Infraspecific <- paste(usda_usa_cleaned$Variant, usda_usa_cleaned$Subspecies)
usda_usa_cleaned$Infraspecific <- trimws(usda_usa_cleaned$Infraspecific, which = c("both"))  #remove trailing white space on species names

usda_usa_cleaned$Species<- paste(usda_usa_cleaned$Genus, usda_usa_cleaned$Species_only) #now join columns to get full species name
usda_usa_cleaned$Taxon <-  paste(usda_usa_cleaned$Species, usda_usa_cleaned$Rank, usda_usa_cleaned$Infraspecific) #now join columns to get full taxon name
usda_usa_cleaned <- subset(usda_usa_cleaned, select=-c(The_rest, not_needed, not_needed2, Variant_interm, Subspecies_interm, not_needed3, not_needed4)) #get rid of columns we don't need
usda_usa_cleaned$Taxon <- trimws(usda_usa_cleaned$Taxon, which = c("both"))  #remove trailing white space on species names

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_usda_usa<-merge(master_list,usda_usa_cleaned, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference usda_usa list with Master list
#getting a list of all CWR plants in usda_usa

write.csv(CWR_of_usda_usa, "CWR_of_usda_usa_nov17.csv")  
rm(CWR_of_usda_usa, usda_usa, usda_usa_cleaned, split_var)


############ USDA NPGS FULL COLLECTIONS #################

#Read data for whichever garden you want to compare
NPGS <- read.csv("Garden_PGRC_Data/GRIN/GRIN_full_clean.csv") #Note that there are no repeat names, instead they have a count column!

# format for join
NPGS <- NPGS %>% 
  mutate(Taxon = str_replace(Taxon, "×", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!
NPGS$Taxon <- trimws(NPGS$Taxon, which = c("both"))

# split ORIGIN and COORDINATES
NPGS_split <- NPGS %>%
  extract(ORIGIN, c("PROVINCE", "COUNTRY"), "([^,]+), ([^)]+)") %>%
  extract(COORDINATES, c("LATITUDE", "LONGITUDE"), "([^,]+), ([^)]+)")
  
# load inventory
inventory <- read.csv("Input_Data_and_Files/inventory.csv") #Note that there are no repeat names, instead they have a count column!
inventory <- inventory %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

CWR_of_NPGS_filtered <- merge(inventory, NPGS_split, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference usda_usa list with Master list
#getting a list of all CWR plants in usda_usa

write.csv(CWR_of_NPGS_filtered, "Garden_PGRC_Data/filtered_data/CWR_NPGS.csv")  

############ PGRC CANADA FULL #################

#Read data for whichever garden you want to compare
PGRC <- read.csv("Garden_PGRC_Data/GRIN_PGRC/PGRC_full_cleaned.csv") #Note that there are no repeat names, instead they have a count column!
PGRC <- PGRC %>% 
  mutate(Taxon = str_replace(Taxon, "×", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

PGRC$Taxon <- trimws(PGRC$Taxon, which = c("both"))  #remove trailing white space on species names

# load inventory
inventory <- read.csv("Input_Data_and_Files/inventory.csv") #Note that there are no repeat names, instead they have a count column!
inventory <- inventory %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) #removing 'x' and symbol from hybrids for filtering simplicity - will be added back later!

#Now that the garden dataset is cleaned in the same way as the master list, we can filter them against eachother
CWR_of_PGRC <- merge(inventory, PGRC, by.x = "TAXON", by.y = "Taxon") #Finally, cross-reference pgrc_cad list with Master list
#getting a list of all CWR plants in pgrc_cad

write.csv(CWR_of_PGRC, "Garden_PGRC_Data/filtered_data/CWR_PGRC.csv")  

