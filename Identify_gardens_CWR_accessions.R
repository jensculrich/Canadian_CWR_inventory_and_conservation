##GOAL: Compare Botanical Garden accession list with our Master Canadian CWR list
##END-RESULT: A list of all CWR accessions from Botanical Garden

setwd("~/Desktop/Grad courses/RES500/CWR project/CWR")
master_list <- read.csv("CWR_Master_list.csv")
library(stringr)
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

write.csv(CWR_of_RBG, "CWR_of_UBC.csv")  

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

#write.csv(total, "Cleaned_PGRC_US.csv")  


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

write.csv(total, "Cleaned_USDA_CAD.csv")  
