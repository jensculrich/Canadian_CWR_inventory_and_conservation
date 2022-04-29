## Prepare new supplementary figure
# graph number of accessions versus cwr in ecoregion
library(tidyverse)
library(sf)

df <- read.csv("./Garden_PGRC_Data/ecoregion_gap_table_by_species.csv")
canada_ecoregions_geojson <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson", quiet = TRUE)


# TIER 1 CWR BY ECOREGION
ecoregion_counts <- df %>%
  distinct(ECO_NAME, .keep_all = TRUE) %>%
  select(ECO_NAME, total_CWRs_in_ecoregion, total_WUS_in_ecoregion)
# transform for plotting
ecoregion_species_gaps <- df %>%
  filter(!is.na(INSTITUTION)) %>% # filter for if only want those collections with geo origin
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
  full_join(canada_ecoregions_geojson[ , c("ECO_NAME", "geometry")]) %>%
  select(-total_CWRs_in_ecoregion) %>%
  left_join(ecoregion_counts[ , c("ECO_NAME", "total_CWRs_in_ecoregion")])

ecoregion_species_gaps$log_accessions[is.na(ecoregion_species_gaps$log_accessions)] <- 0

plot(ecoregion_species_gaps$total_CWRs_in_ecoregion, ecoregion_species_gaps$log_accessions)
fit1 <- lm(log_accessions ~ total_CWRs_in_ecoregion, 
   data = ecoregion_species_gaps)
summary(fit1)
coef(lm(log_accessions ~ total_CWRs_in_ecoregion, 
        data = ecoregion_species_gaps))

p <- ggplot(ecoregion_species_gaps, aes(
  x = total_CWRs_in_ecoregion, 
  y = log_accessions)) +
  geom_point() +
  geom_abline(intercept = .025, slope = 0.047) +
  ylab("log(Total Accessions from Ecoregion)") +
  xlab("CWR Species Richness in Ecoregion") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))
p

