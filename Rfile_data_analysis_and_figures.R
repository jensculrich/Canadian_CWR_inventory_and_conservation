# INTRO 
# Use the species distribution data and the garden/genebank accession data
# to explore CWR and WUS diversity
# and more particularly
# the strengths and gaps in ex situ conservation 

# load required libraries
library(tidyverse)
library(ggplot2)
library(sf) # the base package manipulating shapes
#library(geojsonio) # geo json input and output
library(tmap) # libary for drawing spatial figures - i.e. fig 2
library(tigris) # spatial joins between sf's and df
library(gridExtra) # panelling figures
#library(ggnewscale) # for mixing continuous and discrete fill scales on a map

############
# CONTENTS #
############

# 1 - Load inventory and spatial data; species distribution and garden accessions 
# 2 - Compute some basic summary stats about the inventory   
# 3 - Determine patterns of species distributions  
# 4 - Load garden data format for gap analyses
# Figure 1: Ex situ conservation of each crop group, and in gardens v genebanks
# Figure 2: National geographic gaps of wild-orgin accessions; A = CWR, B = WUS
            # (wild origin accessions per region versus species distr density per region)
# Supplementary Figure: Figure 2 but by province (or ecoregion, whichever is not used)
# Figure 3: Geographic gap analysis: proportion of range conserved in different groups
            # A: CWR Tier 1
            # B: CWR Tier 2
            # C: WUS
# Supplementary Figure: Figure 3 but by province (or ecoregion, whichever is not used) 
# Figure 4: Case Study
# Figure 5: ? Or maybe we add a table of some sort?

################################################################################
# # 1 - Load inventory and spatial data     
################################################################################

# Load required data and shapefiles for plotting occurrence maps and data tables
inventory <- read.csv("./Input_Data_and_Files/inventory.csv") %>% 
  mutate(TAXON = str_replace(TAXON, "×", "")) %>% 
  mutate(SPECIES = str_replace(SPECIES, "×", "")) 

inventory_finest_taxon_resolution <- inventory %>%
  filter(FINEST_TAXON_RESOLUTION == "Y")

# aggregate inventory at the species level 
inventory_sp <- inventory %>%
  distinct(SPECIES, .keep_all = TRUE)

# filter inventory to Tier 1 species
inventory_sp_T1 <- inventory_sp %>%
  filter(TIER == 1)


canada_ecoregions_geojson <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE)
# rename PROVINCE in shapefile (needs to match the species distribution table)
canada_provinces_geojson <- canada_provinces_geojson %>%
  rename("PROVINCE" = "name")

# read in gap tables
province_gap_table <- read.csv("Garden_PGRC_Data/province_gap_table_species.csv")
ecoregion_gap_table <- read.csv("Garden_PGRC_Data/ecoregion_gap_table_species.csv")

# read in accessions summary
num_accessions <- read.csv("Garden_PGRC_Data/summary_accessions_all_species_2.csv")

##################################################################
# # FIGURE 1 # # # # # # # # # # # # # # #
##################################################################

num_accessions_cwr <- num_accessions %>%
  distinct(SPECIES, .keep_all=TRUE) %>%
  filter(TIER == 1) %>%
  mutate(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = as.factor(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) %>%
  group_by(PRIMARY_ASSOCIATED_CROP_COMMON_NAME) %>%
  mutate(mean = mean(total_accessions_sp)) %>%
  mutate(median = median(total_accessions_sp)) %>%
  mutate(binary = ifelse(total_accessions_sp > 0, 1, 0)) %>%
  group_by(PRIMARY_ASSOCIATED_CROP_COMMON_NAME) %>%
  mutate(total_CWR_taxa = n(), 
         total_in_ex_situ = sum(binary),
         proportion_in_ex_situ = total_in_ex_situ / total_CWR_taxa) %>%
  # label name = paste PRIMARY_ASSOCIATED_CROP_COMMON_NAME +
  # " - " + *GENUS* + " (" + total_in_ex_situ + 
  # "/" + "total_CWR_taxa"
  mutate(label = paste(PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                       " - ", GENUS, " (", total_in_ex_situ,
                       "/", total_CWR_taxa, ")"))

# write.csv(num_accessions_cwr, "accessions_tier1_CWR.csv")

lims <- num_accessions_cwr %>%
  distinct(PRIMARY_ASSOCIATED_CROP_COMMON_NAME, .keep_all = TRUE) %>%
  select(label) 
  
category_names <- c("Sugars", "Vegetables", 
                    "Cereals and pseudocereals", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

num_accessions_cwr <- num_accessions_cwr %>%
  mutate(across(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, factor, 
                levels=category_names)) %>%
  arrange(., PRIMARY_ASSOCIATED_CROP_COMMON_NAME)

num_accessions_cwr_outliers <- num_accessions_cwr
  
num_accessions_cwr_outliers$total_accessions_sp[which(
  num_accessions_cwr_outliers$total_accessions_sp > 1000)] = 1000

FIGURE_1A <- ggplot(num_accessions_cwr_outliers, 
             aes(x = PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                 y = total_accessions_sp,
                 color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_jitter(shape=16, position=position_jitter(0.3), size = 3, alpha = 0.5) +
  facet_grid(cols = vars(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1), scales = "free_x", space = "free_x") +
  stat_summary(fun=median, geom="point", shape='-', size= 8, color="black", fill="black") +
  # stat_summary(fun=mean, geom="point", shape='+', size= 8, color="black", fill="black") +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.spacing.x = unit(.1, "cm"),
        strip.text.x = element_blank(),
        axis.text.y  = element_text(angle=90, vjust = 1, hjust=0.5, size = 12), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  ylab("Accessions per CWR") +
  # really struggled to automate this (change just genus name to italic after plotting is done) 
  # so just manually wrote these expressions from the df lims created above
  scale_x_discrete(labels=c("Sugar Maple" = expression(paste("Sugar Maple - ", italic('Acer'), " (9/9)")),
                            "Onions, Garlic, Leeks" = expression(paste("Onion, etc. - ", italic('Allium'), " (11/11)")),
                            "Amaranth" = expression(paste("Amaranth - ", italic('Amaranthus'), " (5/5)")),
                            "Saskatoon" = expression(paste("Saskatoon - ", italic('Amelanchier'), " (10/11)")),
                            "Spinach" = expression(paste("Spinach - ", italic('Blitum'), " (2/2)")),
                            "Pecan, Hickory" = expression(paste("Pecan, Hickory - ", italic('Carya'), " (5/5)")),
                            "Chestnut" = expression(paste("Chestnut - ", italic('Castanea'), " (1/1)")),
                            "Quinoa" = expression(paste("Quinoa - ", italic('Chenopodium'), " (8/8)")),
                            "Proso-millet" = expression(paste("Proso-millet - ", italic('Panicum'), " (8/14)")),
                            "Strawberry" = expression(paste("Strawberry - ", italic('Fragaria, etc.'), " (7/7)")),
                            "Hazelnut" = expression(paste("Hazelnut - ", italic('Corylus'), " (3/3)")),
                            "Carrot" = expression(paste("Carrot - ", italic('Daucus'), " (1/1)")),
                            "Yam" = expression(paste("Yam - ", italic('Dioscorea'), " (1/1)")),
                            "Wheat" = expression(paste("Wheat - ", italic('Elymus, Leymus'), " (21/22)")),
                            "Sunflower" = expression(paste("Sunflower - ", italic('Helianthus'), " (12/12)")),
                            "Barley" = expression(paste("Barley - ", italic('Hordeum'), " (3/4)")),
                            "Hops" = expression(paste("Hops - ", italic('Humulus'), " (1/1)")),
                            "Walnut" = expression(paste("Walnut - ", italic('Juglans'), " (2/2)")),
                            "Lettuce" = expression(paste("Lettuce - ", italic('Lactuca'), " (1/1)")),
                            "Flax" = expression(paste("Flax - ", italic('Linum'), " (5/9)")),
                            "Lupin" = expression(paste("Lupin - ", italic('Lupinus'), " (16/19)")),
                            "Apple" = expression(paste("Apple - ", italic('Malus'), " (2/2)")),
                            "Mint" = expression(paste("Mint - ", italic('Mentha'), " (1/1)")),
                            "Tobacco" = expression(paste("Tobacco - ", italic('Nicotiana'), " (1/1)")),
                            "Tomatillo" = expression(paste("Tomatillo - ", italic('Physalis'), " (2/3)")),
                            "Apricot, Cherry, Peach, Plum" = expression(paste("Apricot, Cherry, etc. - ", italic('Prunus'), " (7/7)")),
                            "Currant, Gooseberry" = expression(paste("Currant, Gooseberry - ", italic('Ribes'), " (16/16)")),
                            "Blackberry, Raspberry" = expression(paste("Black-, Raspberry - ", italic('Rubus'), " (17/19)")),
                            "Rosinweed" = expression(paste("Rosinweed - ", italic('Silphium'), " (2/2)")),
                            "Blueberry, Cranberry" = expression(paste("Blue-, Cranberry - ", italic('Vaccinium'), " (18/20)")),
                            "Grape" = expression(paste("Grape - ", italic('Vitis'), " (2/2)")),
                            "Wild-rice" = expression(paste("Wild-rice - ", italic('Zizania'), " (2/2)"))
                            )
  )
FIGURE_1A


###############################
# Figure 2 ####################
###############################

num_accessions_cwr_long <- gather(num_accessions_cwr, INSTITUTION_TYPE, accessions,
                                  garden_accessions_sp, genebank_accessions_sp, 
                                  factor_key=TRUE) %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
    PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
    c("Cereals and pseudocereals"="Cereals, pseudo-"))) %>%
  mutate(institution_binary = ifelse(accessions > 0, 1, 0))

FIGURE_2B <- ggplot(num_accessions_cwr_long, aes(x = INSTITUTION_TYPE, 
                                           y = log(accessions), 
                                           fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  facet_grid(. ~ PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        strip.text.x = element_text(margin = margin(.4, 0, .1, 0, "cm")),
        strip.text = element_text(size = 12),
        axis.text.y  = element_text(size = 12), 
        axis.text.x = element_text(size = 12)) +
  scale_x_discrete(labels = c('BG','G')) +
  ylab("log(Accessions per CWR)")
FIGURE_2B

num_accessions_cwr_long_gardens <- num_accessions_cwr_long %>%
  filter(INSTITUTION_TYPE == "garden_accessions") %>%
  select(institution_binary) %>%
  mutate(sum = sum(institution_binary)) 
  

num_accessions_cwr_long_genebank <- num_accessions_cwr_long %>%
  filter(INSTITUTION_TYPE == "genebank_accessions") %>%
  select(institution_binary) %>%
  mutate(sum = sum(institution_binary))

######## 
# now look at proportions

# num_accessions_cwr_long_test <- num_accessions_cwr_long %>%
#  group_by(INSTITUTION_TYPE) %>%
#  with(., reorder(
#      SPECIES, institution_binary))


# num_accessions_cwr_long$SPECIES = 
#  with(num_accessions_cwr_long, reorder(
#    SPECIES, as.factor(INSTITUTION_TYPE)))

#arrange(num_accessions_cwr_long, INSTITUTION_TYPE, institution_binary)

(FIGURE_2A <- ggplot(num_accessions_cwr_long, 
       aes(x = INSTITUTION_TYPE, 
           fill = as.factor(institution_binary))) + 
  geom_bar(position = "stack") +
  facet_grid(. ~ PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)+
   geom_text(aes(y=1, label=as.factor(institution_binary)), vjust=1.6, 
             color="white", size=3.5) +
   # scale_fill_brewer(palette="Paired") +
   theme_bw() +
   theme(legend.position = "bottom",
         #axis.ticks.y = element_blank(),
         axis.title.x = element_blank(),
         strip.text.x = element_text(margin = margin(.4, 0, .1, 0, "cm")),
         strip.text = element_text(size = 12),
         #axis.text.y  = element_blank(), 
         axis.text.x = element_text(size = 12),
         legend.text=element_text(size = 12)) +
   scale_x_discrete(labels = c('BG','G')) +
   scale_fill_manual(labels = c("absent from ex situ collections", "in ex situ collections"), 
                     values = c("slategray2", "slategray4")) +
   ylab("CWR Species") + 
   guides(fill=guide_legend(title=""))
)



####################
# statistical difference between bg and g for each group?

# to log transform?
p1 <- ggplot(num_accessions_cwr_long, aes(accessions)) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ INSTITUTION_TYPE)

p2 <- ggplot(num_accessions_cwr_long, aes(log(accessions))) +
  geom_histogram(fill = "white", color = "grey30") +
  facet_wrap(~ INSTITUTION_TYPE) +
  scale_x_log10()

grid.arrange(p1, p2, nrow = 2)

# I would say it is better to compare the log-transformed data,
# not do a t-test at all, given the pretty enormous right-skew of the data distributions
# non-parametric test?


category_names <- c("Sugars", "Vegetables", 
                    "Cereals, pseudo-", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

ttest_accessions_by_category <- function(category){
  num_accessions_cwr_long_filtered <- num_accessions_cwr_long %>%
    filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == category) %>%
    mutate(accessions = as.numeric(accessions))
  
  res <- t.test(num_accessions_cwr_long_filtered$accessions ~
                num_accessions_cwr_long_filtered$INSTITUTION_TYPE)
  
  return(res)
}

ttest_accessions = list()
q = 1 # specify a list element position
for(i in 1:8) {
  
  selected_category <- category_names[i] # make a vector of cat names
  ttest_accessions[[q]] <- ttest_accessions_by_category(selected_category)
  
  q = q+1
  
} 

print(ttest_accessions)

# wilcoxon ranked sum test
wtest_accessions_by_category <- function(category){
  num_accessions_cwr_long_filtered <- num_accessions_cwr_long %>%
    filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == category) %>%
    mutate(accessions = as.numeric(accessions))
  
  res <- wilcox.test(num_accessions_cwr_long_filtered$accessions ~
                  num_accessions_cwr_long_filtered$INSTITUTION_TYPE,
                  exact = FALSE)
  
  return(res)
}

wtest_accessions = list()
q = 1 # specify a list element position
for(i in 1:8) {
  
  selected_category <- category_names[i] # make a vector of cat names
  wtest_accessions[[q]] <- wtest_accessions_by_category(selected_category)
  
  q = q+1
  
} 

print(wtest_accessions)

# print actual median values
medians <- num_accessions_cwr %>%
  filter(TIER == 1) %>%
  group_by(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) %>%
  mutate(median_BG = median(garden_accessions_sp),
         median_G = median(genebank_accessions_sp)) %>%
  distinct(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, .keep_all = TRUE) %>%
  select(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, median_BG, median_G) 

#### need to run proportion tests by group
category_names <- c("Sugars", "Vegetables", 
                    "Cereals, pseudo-", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

CWR_prop_test <- function(category){
  num_accessions_cwr_long_filtered <- num_accessions_cwr_long %>%
    filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 == category) %>%
    # group by institution type
    group_by(INSTITUTION_TYPE) %>%
    # calc number of species with 1
    # calc total number of species
    mutate(species_yes = sum(institution_binary),
           n = n()) %>%
    # just want one row from each group
    distinct(INSTITUTION_TYPE, .keep_all = TRUE)
  
  # x = successes (species yes), n = trials (total species)
  res <- prop.test(x = c(as.numeric(num_accessions_cwr_long_filtered[1, 45]),
                         as.numeric(num_accessions_cwr_long_filtered[2, 45])),
                   n = c(as.numeric(num_accessions_cwr_long_filtered[1, 46]),
                         as.numeric(num_accessions_cwr_long_filtered[2, 46]))
  )
  
  return(res)
  
}

ptest_accessions = list()
q = 1 # specify a list element position
for(i in 1:8) {
  
  selected_category <- category_names[i] # make a vector of cat names
  ptest_accessions[[q]] <- CWR_prop_test(selected_category)
  
  q = q+1
  
} 

print(ptest_accessions)

##############################
# # FIGURE 3 # # # # # # # ###
##############################

# Geographic gaps in accessions versus species distr density
# total accessions collected from each area, proportion of species in each region that are
# conserved ex situ via wild-origin accessions collected from that region

# global figure variables
province_names <- c("British Columbia", "Alberta", "Newfoundland and Labrador",
                    "Northwest Territories", "Nunavut", "Saskatchewan", 
                    "Manitoba", "Quebec", "Nova Scotia", "Ontario",
                    "New Brunswick", "Prince Edward Island", "Yukon")

# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# legend titles
legend_title_CWR = expression("CWR species richness")
legend_title_WUS = expression("WUS species richness")

# Figure 3A - TIER 1 CWR BY ECOREGION

# transform for plotting
ecoregion_species_gaps <- ecoregion_gap_table %>%
  filter(!is.na(ECO_NAME)) %>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  # now tally the number of CWR accessions from the province
  group_by(ECO_NAME) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  group_by(ECO_NAME, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_any = case_when(
    INSTITUTION == "G" | INSTITUTION == "BG" ~ 1)) %>%
  ungroup() %>%
  group_by(ECO_NAME) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_CWRs_in_ecoregion)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_CWRs_in_ecoregion)) %>%
  #mutate(proportion_in_both = 
   #        as.numeric((sum(!is.na(in_both)) / total_CWRs_in_ecoregion))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_CWRs_in_ecoregion)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  filter(ECO_NAME != "Canada") %>%
  select(ECO_NAME, latitude, longitude, total_CWRs_in_ecoregion,
         total_accessions, log_accessions,
         in_BG, in_G, in_any,
         proportion_in_BG, proportion_in_G,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")])

ecoregion_species_gaps_sf <- st_as_sf(ecoregion_species_gaps)

# plot
(FIGURE_3A <- tm_shape(ecoregion_species_gaps_sf,
                            projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_ecoregion",
                style = "cont",
                title = legend_title_CWR) + 
    tm_symbols(size = "proportion_in_any",
               title.size = "(%) CWR conserved ex situ",
               sizes.legend=c(0, 25, 50, 100),
               scale = 3,
               col = "grey",
               alpha = 0.7,
               legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1.5, 
              legend.text.size = 1)
)


# Supplementary Version - Tier 1 CWR by province
province_species_gaps <- province_gap_table %>%
  filter(!is.na(PROVINCE),
         PROVINCE %in% province_names) %>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(TIER == 1) %>%
  group_by(PROVINCE) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation
  ungroup() %>%
  group_by(PROVINCE) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_CWRs_in_province)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_CWRs_in_province)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_CWRs_in_province))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_CWRs_in_province)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(PROVINCE, .keep_all = TRUE) %>%
  filter(PROVINCE != "Canada") %>%
  select(PROVINCE, latitude, longitude, total_CWRs_in_province,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_provinces_geojson[ , c("PROVINCE", "geometry")],
            by = c("PROVINCE" = "PROVINCE"))

province_species_gaps_sf <- st_as_sf(province_species_gaps)

# plot
(FIGURE_3A_Supp <- tm_shape(province_species_gaps_sf,
                           projection = crs_string) +
    tm_polygons(col = "total_CWRs_in_province",
                style = "cont",
                title = legend_title_CWR) + 
    tm_symbols(size = "proportion_in_any",
      title.size = "(%) CWR conserved ex situ",
      sizes.legend=c(0, 25, 50, 100),
      scale = 3,
      col = "grey",
      #palette = rev(RColorBrewer::brewer.pal(5,"Greys")), 
      alpha = 0.7,
      legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1)
)


############################################
#  FIGURE 3B -  WUS                     ####
############################################

# ISSUE HERE THAT NEEDS TO BE FIXED - PROPORTION VALUES TOO BIG!

# Figure 3B - WUS By Ecoregion
ecoregion_species_gaps_WUS <- ecoregion_gap_table %>%
  filter(!is.na(ECO_NAME)) %>% # filter for if only want those collections with geo origin
  # filter for tier 1 CWR
  filter(WUS == "Y") %>%
  # now tally the number of CWR accessions from the province
  group_by(ECO_NAME) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  group_by(ECO_NAME, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation
  ungroup() %>%
  group_by(ECO_NAME) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_WUS_in_ecoregion)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_WUS_in_ecoregion)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_WUS_in_ecoregion))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_WUS_in_ecoregion)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  filter(ECO_NAME != "Canada") %>%
  select(ECO_NAME, latitude, longitude, total_WUS_in_ecoregion,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")])

ecoregion_species_gaps_WUS_sf <- st_as_sf(ecoregion_species_gaps_WUS)

# plot
(FIGURE_3B <- tm_shape(ecoregion_species_gaps_WUS_sf,
                                projection = crs_string) +
    tm_polygons(col = "total_WUS_in_ecoregion",
                style = "cont",
                title = legend_title_WUS,
                palette = "Blues") + 
    tm_symbols(size = "proportion_in_any",
      title.size = "(%) WUS conserved ex situ",
      sizes.legend=c(0, 25, 50, 100),
      scale = 4,
      col = "grey",
      alpha = 0.7,
      legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1.5, 
              legend.text.size = 1 )
)

# Supplementary Version - WUS by province
province_species_gaps_WUS <- province_gap_table %>%
  filter(!is.na(PROVINCE))%>% # filter for if only want those collections with geo origin
  # filter for WUS
  filter(WUS == "Y") %>%
  # now tally the number of CWR accessions from the province
  group_by(PROVINCE) %>%
  mutate(total_accessions = sum(!is.na(INSTITUTION))) %>%
  group_by(PROVINCE, SPECIES) %>%
  # binary 1 = species represented in the region 
  mutate(in_BG = case_when(
    INSTITUTION == "BG" ~ 1)) %>%
  mutate(in_G = case_when(
    INSTITUTION == "G" ~ 1)) %>%
  mutate(in_both = case_when(
    in_BG == 1 && in_G == 1 ~ 1)) %>% # no species are in both; so few G with geolocation  ungroup() %>%
  group_by(PROVINCE) %>%
  # distinct species # want one line per species
  # proportion of species in BG, G, both, or any
  # = sum of all rows (since values are binary) / nrow
  distinct(SPECIES, .keep_all = TRUE) %>%
  mutate(proportion_in_BG = 
           as.numeric(sum(!is.na(in_BG)) / total_WUS_in_province)) %>%
  mutate(proportion_in_G = 
           as.numeric(sum(!is.na(in_G)) / total_WUS_in_province)) %>%
  mutate(proportion_in_both = 
           as.numeric((sum(!is.na(in_both)) / total_WUS_in_province))) %>%
  mutate(proportion_in_any = 
           as.numeric(100 * (sum(!is.na(in_both)) + sum(!is.na(in_BG)) + sum(!is.na(in_G))) 
                      / total_WUS_in_province)) %>%
  mutate(proportion_in_neither = 
           as.numeric(100 - (proportion_in_any))) %>%
  mutate(log_accessions = as.numeric(log(total_accessions))) %>%
  distinct(PROVINCE, .keep_all = TRUE) %>%
  filter(PROVINCE %in% province_names) %>%
  select(PROVINCE, latitude, longitude, total_WUS_in_province,
         total_accessions, log_accessions,
         in_BG, in_G, in_both,
         proportion_in_BG, proportion_in_G, proportion_in_both,
         proportion_in_any, proportion_in_neither) %>%
  full_join(canada_provinces_geojson[ , c("PROVINCE", "geometry")],
            by = c("PROVINCE" = "PROVINCE"))

province_species_gaps_WUS_sf <- st_as_sf(province_species_gaps_WUS)

# plot
(FIGURE_3B_Supp <- tm_shape(province_species_gaps_WUS_sf,
                           projection = crs_string) +
    tm_polygons(col = "total_WUS_in_province",
                style = "cont",
                title = legend_title_WUS,
                palette = "Blues") + 
    tm_symbols(size = "proportion_in_any",
      title.size = "(%) WUS conserved ex situ",
      sizes.legend=c(0, 25, 50, 100),
      scale = 4,
      col = "grey",
      alpha = 0.7,
      legend.format = list(text.align="right", text.to.columns = TRUE)) +
    tm_layout(frame = FALSE,
              legend.outside = TRUE,
              legend.title.size = 1)
)

# the grobs don't fit well on a single page
grid.arrange(tmap_grob(FIGURE_3A), tmap_grob(FIGURE_3B), nrow = 2)

FIGURE_3A
FIGURE_3B
FIGURE_3A_Supp
FIGURE_3B_Supp

########################################################################
# Conduct Geographic Conservation Gap Analyses Figure 1B and Figure 4  #
########################################################################
# we determined the proportion of native regions (province or ecoregion) where 
# each CWR baturally occurs that were represented in garden collections

# Gap Analysis By Ecoregion

# filter to the garden occurrence points that were in Canada
ecoregion_gap_table_T1 <- ecoregion_gap_table %>%
  filter(TIER == 1)  # filter for tier 1 CWR

# define the gap analysis function
ecoregion_gap_analysis <- function(species) {
  
  ecoregionTableData <- ecoregion_gap_table_T1 %>%
    # filter the table to the selected CWR
    filter(ecoregion_gap_table_T1$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(ECO_NAME) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregion = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             (num_covered_ecoregion / num_native_ecoregion)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                  num_native_ecoregion, num_covered_ecoregion, perc_ecoregion_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_ecoregion = as.integer(num_covered_ecoregion)) %>%
    rename("native ecoregions" = num_native_ecoregion,
           "covered ecoregions" = num_covered_ecoregion,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_ecoregion <- data.frame("SPECIES" = character(),
                                           "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                           "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(), 
                                           "native ecoregions" = character(), 
                                           "covered ecoregions" = character(), 
                                           "accessions with geographic data" = character(),
                                           "accessions lacking geographic data" = character(),
                                           stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_T1)) {
  
  selected_taxon <- inventory_sp_T1[[i,2]] # column 1 is species ("sci_name")
  as.data.frame(temp <- ecoregion_gap_analysis(
    species = selected_taxon)) 
  gap_analysis_df_by_ecoregion <- rbind(
    gap_analysis_df_by_ecoregion, temp)
  
} 

# 
### now make the figure
#
category_names <- c("Sugars", "Vegetables", 
                    "Cereals and pseudocereals", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

gap_analysis_df_by_ecoregion_test <- gap_analysis_df_by_ecoregion %>%
  mutate(across(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, factor, 
                levels=category_names)) %>%
  arrange(., PRIMARY_ASSOCIATED_CROP_COMMON_NAME)


FIGURE_1B <- ggplot(gap_analysis_df_by_ecoregion_test, 
              aes(x = PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                  y = perc_ecoregion_range_covered,
                  color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_jitter(shape=16, position=position_jitter(0.3), size = 3, alpha = 0.5) +
  facet_grid(cols = vars(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1), scales = "free_x", space = "free_x") +
  stat_summary(fun=median, geom="point", shape='-', size= 8, color="black", fill="black") +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.spacing.x = unit(.1, "cm"),
        strip.text.x = element_blank(),
        axis.text.y  = element_text(angle=90, vjust = 1, hjust=0.5, size = 12), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  ylab("exGCS") +
  scale_y_continuous(labels = scales::percent) +
  # just keep these labels for now to match up and see potential issues
  scale_x_discrete(labels=c("Sugar Maple" = expression(paste(" ")),
                            "Onions, Garlic, Leeks" = expression(paste(" ")),
                            "Amaranth" = expression(paste(" ")),
                            "Saskatoon" = expression(paste(" ")),
                            "Spinach" = expression(paste(" ")),
                            "Pecan, Hickory" = expression(paste(" ")),
                            "Chestnut" = expression(paste(" ")),
                            "Quinoa" = expression(paste(" ")),
                            "Proso-millet" = expression(paste(" ")),
                            "Strawberry" = expression(paste(" ")),
                            "Hazelnut" = expression(paste(" ")),
                            "Carrot" = expression(paste(" ")),
                            "Yam" = expression(paste(" ")),
                            "Wheat" = expression(paste(" ")),
                            "Sunflower" = expression(paste(" ")),
                            "Barley" = expression(paste(" ")),
                            "Hops" = expression(paste(" ")),
                            "Walnut" = expression(paste(" ")),
                            "Lettuce" = expression(paste(" ")),
                            "Flax" = expression(paste(" ")),
                            "Lupin" = expression(paste(" ")),
                            "Apple" = expression(paste(" ")),
                            "Mint" = expression(paste(" ")),
                            "Tomatillo" = expression(paste(" ")),
                            "Tobacco" = expression(paste(" ")),
                            "Apricot, Cherry, Peach, Plum" = expression(paste(" ")),
                            "Currant, Gooseberry" = expression(paste(" ")),
                            "Blackberry, Raspberry" = expression(paste(" ")),
                            "Rosinweed" = expression(paste(" ")),
                            "Blueberry, Cranberry" = expression(paste(" ")),
                            "Grape" = expression(paste(" ")),
                            "Wild-rice" = expression(paste(" "))
  )
)
FIGURE_1B

grid.arrange(FIGURE_1B, FIGURE_1A, nrow = 1)

####################################
######## GAP ANALYSIS CASE STUDY   #
####################################

# potential figure 5
# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=10, base_family="") { # 3
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

##############
# By Ecoregion
# saskatoon_cwr_list <- inventory_sp %>%
#  filter(PRIMARY_ASSOCIATED_CROP_COMMON_NAME == "Saskatoon")

accessions_sf <- st_as_sf(ecoregion_gap_table, 
                          coords = c("longitude", "latitude"), 
                          crs = 4326, 
                          na.fail = FALSE)

accessions_sf_num_wild <- accessions_sf %>%
  filter(!is.na(GARDEN_CODE))

accessions_sf_num_wild_TIER1 <- accessions_sf_num_wild %>%
  filter(TIER == 1) %>%
  group_by(ECO_NAME) %>%
  count()
accessions_sf_num_wild_WUS <- accessions_sf_num_wild %>%
  filter(WUS == "Y")%>%
  group_by(ECO_NAME) %>%
  count()

accessions_sf_G <- accessions_sf %>%
  filter(INSTITUTION == "G")

accessions_sf_BG <- accessions_sf %>%
  filter(INSTITUTION == "BG")

# Function to get plot data by taxon
plotData_ecoregion <- function(species){
  # filter province_gap_table frame and calculate species specific stats
  ecoregionTableData <- ecoregion_gap_table %>%
    # filter the table to the selected CWR
    filter(ecoregion_gap_table$SPECIES == species) %>%
    
    # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
    group_by(ECO_NAME) %>%
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(total_accessions_for_species = sum(!is.na(GARDEN_CODE))) %>%
    mutate(accessions_no_geo_data = sum(is.na(latitude))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(latitude))) %>%
    
    # convert number of accessions to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregion = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             num_covered_ecoregion / num_native_ecoregion) 
  
  # join plot data with the spatial data frame necessary for projecting the plot  
  tigris::geo_join(canada_ecoregions_geojson, ecoregionTableData,  
                   by_sp = "ECO_NAME", by_df = "ECO_NAME")
  
} 


make_a_plot_ecoregion <- function(species) {
  accessions_sf_G_filtered <- accessions_sf_G %>%
    filter(SPECIES == species) 
  
  accessions_sf_BG_filtered <- accessions_sf_BG %>%
    filter(SPECIES == species) 
  
  df <- plotData_ecoregion(species)
  
  df$binary[is.na(df$binary)] <- -1
  
  plot <- ggplot(df) +
    geom_sf(fill = "white", color = "gray60", size = 0.1) +
    geom_sf(aes(fill = as.factor(binary))) +
    geom_sf(data = accessions_sf_G_filtered, color = 'mediumorchid1', alpha = 0.5, size = 5) +
    geom_sf(data = accessions_sf_BG_filtered, color = 'goldenrod1', alpha = 0.5, size = 5) +
    coord_sf(crs = crs_string) +
    scale_fill_manual(breaks = c("-1", "0", "1"),
                      values = c("white", "gray60", "gray10"), 
                      labels = c("Outside range",
                                 "Not in ex situ collections", 
                                 "In ex situ collections" 
                      )) +
    guides(fill = guide_legend(title = "", 
                               title.position = "top",
                               title.theme = element_text(size = 10, face = "bold")
    )) +
    theme_map() +
    ggtitle(species) +
    theme(panel.grid.major = element_line(color = "white"),
          plot.title = element_text(color="black",
                                    size=14, face="bold.italic", hjust = 0.5),
          plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
          legend.position = "bottom", legend.text = element_text(size=12))
  
  return(plot)
}

test1 <- make_a_plot_ecoregion("Amelanchier alnifolia")
test1

test2 <- make_a_plot_ecoregion("Helianthus nuttallii")
test2

test3 <- make_a_plot_ecoregion("Malus coronaria")
test3

test4 <- make_a_plot_ecoregion("Zizania palustris")
test4

test5 <- make_a_plot_ecoregion("Fragaria virginiana")
test5

test6 <- make_a_plot_ecoregion("Vaccinium myrtilloides")
test6

test3 <- make_a_plot_ecoregion("Malus fusca")
test3










#######################
# extra

# Gap Analysis By Province

# filter to the garden occurrence points that were in Canada
province_gap_table_T1 <- province_gap_table %>%
  filter(TIER == 1)  # filter for tier 1 CWR
# filter to the garden occurrence points that were in Canada

# define the gap analysis function
province_gap_analysis <- function(species) {
  
  provinceTableData <- province_gap_table_T1 %>%
    # filter the table to the selected CWR
    filter(province_gap_table_T1$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(PROVINCE) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(PROVINCE))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(PROVINCE) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(PROVINCE) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(PROVINCE)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(PROVINCE))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                  num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province <- data.frame("SPECIES" = character(),
                                          "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(),
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2" = character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3" = character(), 
                                          "native provinces" = character(), 
                                          "covered provinces" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_T1)) {
  
  selected_taxon <- inventory_sp_T1[[i,2]] # column 1 is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis(
    species = selected_taxon)) 
  gap_analysis_df_by_province <- rbind(
    gap_analysis_df_by_province, temp)
  
} 

# summary stats
gap_analysis_df_by_province <- gap_analysis_df_by_province %>%
  mutate(mean = mean(perc_province_range_covered)) %>%
  group_by(PRIMARY_ASSOCIATED_CROP_COMMON_NAME) %>%
  mutate(mean_crop = mean(perc_province_range_covered))

# Compute the analysis of variance
res.aov <- aov(perc_province_range_covered ~ 
                PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
             data = gap_analysis_df_by_province)
# Summary of the analysis
summary(res.aov)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
# not normally distributed!

res.kruskal <- kruskal.test(perc_province_range_covered ~ 
               PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
             data = gap_analysis_df_by_province)
(res.kruskal) # medians of one or more group-pairs are significantly different

DT = dunnTest(perc_province_range_covered ~ PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
              data=gap_analysis_df_by_province,
              method="bh")

DT

PT = DT$res
cldList(P.adj ~ Comparison,
        data = PT,
        threshold = 0.05)

# what are the actual medians?
(summary <- group_by(gap_analysis_df_by_province, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1) %>%
  summarise(
    count = n(),
    mean = mean(perc_province_range_covered, na.rm = TRUE),
    sd = sd(perc_province_range_covered, na.rm = TRUE),
    median = as.numeric(median(perc_province_range_covered, na.rm = FALSE)),
    IQR = IQR(perc_province_range_covered, na.rm = TRUE)
  )
)

# 
### now make the figure
#
category_names <- c("Sugars", "Vegetables", 
                    "Cereals and pseudocereals", "Fruits",
                    "Nuts", "Oils",
                    "Herbs and Spices", "Pulses")

gap_analysis_df_by_province_test <- gap_analysis_df_by_province %>%
  mutate(across(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, factor, 
                levels=category_names)) %>%
  arrange(., PRIMARY_ASSOCIATED_CROP_COMMON_NAME)
  

Fs1B <- ggplot(gap_analysis_df_by_province_test, 
             aes(x = PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                 y = perc_province_range_covered,
                 color = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1)) + 
  geom_jitter(shape=16, position=position_jitter(0.3), size = 3, alpha = 0.5) +
  facet_grid(cols = vars(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1), scales = "free_x", space = "free_x") +
  stat_summary(fun=mean, geom="point", shape='-', size= 8, color="black", fill="black") +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        panel.spacing.x = unit(.1, "cm"),
        strip.text.x = element_blank(),
        axis.text.y  = element_text(angle=90, vjust = 1, hjust=0.5, size = 12), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 10)) +
  ylab("Geographic distribution conserved") +
  scale_y_continuous(labels = scales::percent) +
  # just keep these labels for now to match up and see potential issues
  scale_x_discrete(labels=c("Sugar Maple" = expression(paste("Sugar Maple - ", italic('Acer'), " (10/10)")),
                            "Onions, Garlic, Leeks" = expression(paste("Onion, etc. - ", italic('Allium'), " (10/10)")),
                            "Amaranth" = expression(paste("Amaranth - ", italic('Amaranthus'), " (4/5)")),
                            "Saskatoon" = expression(paste("Saskatoon - ", italic('Amelanchier'), " (10/11)")),
                            "Spinach" = expression(paste("Spinach - ", italic('Blitum'), " (2/2)")),
                            "Pecan, Hickory" = expression(paste("Pecan, Hickory - ", italic('Carya'), " (5/5)")),
                            "Chestnut" = expression(paste("Chestnut - ", italic('Castanea'), " (1/1)")),
                            "Quinoa" = expression(paste("Quinoa - ", italic('Chenopodium'), " (4/8)")),
                            "Proso-millet" = expression(paste("Proso-millet - ", italic('Panicum'), " (1/16)")),
                            "Strawberry" = expression(paste("Strawberry - ", italic('Fragaria, etc.'), " (7/7)")),
                            "Filbert" = expression(paste("Filbert - ", italic('Corylus'), " (2/2)")),
                            "Carrot" = expression(paste("Carrot - ", italic('Daucus'), " (1/1)")),
                            "Yam" = expression(paste("Yam - ", italic('Dioscorea'), " (0/1)")),
                            "Wheat" = expression(paste("Wheat - ", italic('Elymus, Leymus'), " (15/23)")),
                            "Sunflower" = expression(paste("Sunflower - ", italic('Helianthus'), " (12/12)")),
                            "Barley" = expression(paste("Barley - ", italic('Hordeum'), " (3/6)")),
                            "Hop" = expression(paste("Hop - ", italic('Humulus'), " (1/1)")),
                            "Walnut" = expression(paste("Walnut - ", italic('Juglans'), " (2/2)")),
                            "Lettuce" = expression(paste("Lettuce - ", italic('Lactuca'), " (0/1)")),
                            "Flax" = expression(paste("Flax - ", italic('Linum'), " (3/6)")),
                            "Lupin" = expression(paste("Lupin - ", italic('Lupinus'), " (3/4)")),
                            "Apple" = expression(paste("Apple - ", italic('Malus'), " (2/2)")),
                            "Mint" = expression(paste("Mint - ", italic('Mentha'), " (1/1)")),
                            "Tobacco" = expression(paste("Tobacco - ", italic('Nicotiana'), " (1/1)")),
                            "Tomatillo" = expression(paste("Tomatillo - ", italic('Physalis'), " (1/2)")),
                            "Apricot, Cherry, Peach, Plum" = expression(paste("Apricot, Cherry, etc. - ", italic('Prunus'), " (7/9)")),
                            "Currant, Gooseberry" = expression(paste("Currant, Gooseberry - ", italic('Ribes'), " (16/16)")),
                            "Blackberry, Raspberry" = expression(paste("Black-, Raspberry - ", italic('Rubus'), " (14/20)")),
                            "Rosinweed" = expression(paste("Rosinweed - ", italic('Silphium'), " (2/2)")),
                            "Blueberry, Cranberry" = expression(paste("Blue-, Cranberry - ", italic('Vaccinium'), " (17/20)")),
                            "Grape" = expression(paste("Grape - ", italic('Vitis'), " (2/2)")),
                            "Wild-rice" = expression(paste("Wild-rice - ", italic('Zizania'), " (2/2)"))
  )
  )
Fs1B

gap_analysis_df_by_province_f4 <- gap_analysis_df_by_province %>%
  transform(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 = plyr::revalue(
  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
  c("Cereals and pseudocereals"="Cereals, pseudo-")))

(fig4a <- ggplot(gap_analysis_df_by_province_f4, 
               aes(x = perc_province_range_covered, 
                   y = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                   fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                   height = ..density..)) +
            geom_density_ridges(alpha = 0.6, trim = TRUE,
                                scale = 1.5, size = 0.5,
                                jittered_points = TRUE,
                                position = position_points_jitter(width = 0.01, height = 0),
                                point_shape = '|', point_size = 3, point_alpha = 1) +
            theme_ridges() +
            scale_fill_manual(values = c("grey87", "grey38",
                                         "grey87", "grey38",
                                         "grey87", "grey38",
                                         "grey87", "grey38")) +
            xlab(expression(paste(
              ""))) +
            theme(legend.position = "none",
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 14, 
                                              margin = margin(t = 20, r = 50))) +
            scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                               limits = c(0,1))
)  

######
# test new for T2
T2_levels = c("Forage and Feed", "Forest Resources", "Ornamentals")
inventory_sp_T2 <- inventory_sp %>%
  filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 %in% T2_levels | 
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2 %in% T2_levels |
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3 %in% T2_levels)

province_gap_table_T2 <- province_gap_table_species %>%
  filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 %in% T2_levels | 
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2 %in% T2_levels |
           PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3 %in% T2_levels)

# define the gap analysis function
province_gap_analysis_T2 <- function(species) {
  
  provinceTableData <- province_gap_table_T2 %>%
    # filter the table to the selected CWR
    filter(province_gap_table_T2$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(province) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                  num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}
 
# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province_T2 <- data.frame("SPECIES" = character(),
                                          "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(),
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2" = character(), 
                                          "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3" = character(), 
                                          "native provinces" = character(), 
                                          "covered provinces" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_T2)) {
  
  selected_taxon <- inventory_sp_T2[[i,2]] # column 1 is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis_T2(
    species = selected_taxon)) 
  gap_analysis_df_by_province_T2 <- rbind(
    gap_analysis_df_by_province_T2, temp)
  
} 

# 
### now make the figure
#

gap_analysis_df_by_province_T2_f4 <- gap_analysis_df_by_province_T2 %>%
 filter(PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1 %in% c(
   "Forage and Feed", "Ornamentals", "Forest Resources"))
# need to figure out how to include groups from multipple species
# because species have multiple levels in different columns 
# make the data long???
(fig4b <- ggplot(gap_analysis_df_by_province_T2_f4, 
                aes(x = perc_province_range_covered, 
                    y = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, 
                    fill = PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1,
                    height = ..density..)) +
    geom_density_ridges(alpha = 0.6, trim = TRUE,
                        scale = 1.5, size = 0.5,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.01, height = 0),
                        point_shape = '|', point_size = 3, point_alpha = 1) +
    theme_ridges() + 
    scale_fill_manual(values = c("grey38", "grey87", "grey38")) +
    xlab(expression(paste(
      ""))) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 14, 
                                      margin = margin(t = 20, r = 50))) +
    scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(0,1))
)  

######
# test new for WUS
inventory_sp_WUS <- inventory_sp %>%
  filter(WUS == "Y")

province_gap_table_WUS <- province_gap_table_species %>%
  filter(WUS == "Y")

# define the gap analysis function
province_gap_analysis_WUS <- function(species) {
  
  provinceTableData <- province_gap_table_WUS %>%
    # filter the table to the selected CWR
    filter(province_gap_table_WUS$SPECIES == species) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    # IMPORTANT: This step drop species without range data
    group_by(province) %>% 
    add_tally(!is.na(GARDEN_CODE)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(GARDEN_CODE)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(SPECIES, PRIMARY_ASSOCIATED_CROP_COMMON_NAME, 
                  PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1, PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1, PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2,
                  PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3,
                  num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province_WUS <- data.frame("SPECIES" = character(),
                                             "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1"= character(),
                                             "PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_2"= character(),
                                             "PRIMARY_ASSOCIATED_CROP_COMMON_NAME"= character(), 
                                             "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_1" = character(),
                                             "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_2" = character(), 
                                             "PRIMARY_CROP_OR_WUS_USE_SPECIFIC_3" = character(), 
                                             "native provinces" = character(), 
                                             "covered provinces" = character(), 
                                             "accessions with geographic data" = character(),
                                             "accessions lacking geographic data" = character(),
                                             stringsAsFactors=FALSE)

# for each species
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
# add that species as a row to the gap analysis df
for(i in 1:nrow(inventory_sp_WUS)) {
  
  selected_taxon <- inventory_sp_WUS[[i,2]] # column 1 is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis_WUS(
    species = selected_taxon)) 
  gap_analysis_df_by_province_WUS <- rbind(
    gap_analysis_df_by_province_WUS, temp)
  
} 

# 
### now make the figure
#

gap_analysis_df_by_province_WUS_f4 <- gap_analysis_df_by_province_WUS %>%
 filter(PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1 %in%
          c("Food", "Medicinals"))

(fig4 <- ggplot(gap_analysis_df_by_province_WUS_f4, 
                aes(x = perc_province_range_covered, 
                    y = PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1, 
                    fill = PRIMARY_ASSOCIATED_CROP_TYPE_GENERAL_1,
                    height = ..density..)) +
    geom_density_ridges(alpha = 0.6, trim = TRUE,
                        scale = 1.5, size = 0.5,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.01, height = 0),
                        point_shape = '|', point_size = 3, point_alpha = 1) +
    theme_ridges() + 
    scale_fill_manual(values = c("grey38", "grey87")) +
    xlab(expression(paste(
      "Proportion of range in ex situ collections"))) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 14, 
                                      margin = margin(t = 20, r = 50),
                                      hjust = 0.5)) +
    scale_x_continuous(labels = function(x) paste0(x*100, "%"),
                       limits = c(0,1))  +
    scale_y_discrete(labels=c("Medicinals" = "WUS - Medicinal", 
                              "Food" = "WUS - Food"))
)


