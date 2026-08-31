#################################################################
######## Addidional cleaning steps ###########################
#################################################################

#further cleanins steps after exploring the data

#20260805_combined_data.csv

setwd("/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/tpf-biodiversity/data")

#load packages
library(dplyr)
library(stringr)
library(readxl)

#load data
data <- read.csv("20260805_combined_data.csv")

## delete species with less than 2 records
data <- data %>%
  filter(!is.na(species), str_count(species, "\\S+") > 1) %>%
  group_by(species) %>%
  filter(n() > 2) %>%
  ungroup()

## delete species with less than 2 records
data <- data %>%
  filter(!is.na(species), str_count(species, "\\S+") > 1) %>%
  group_by(species) %>%
  filter(n() > 2) %>%
  ungroup()

# I noticed there were some marine mammals and birds that should be deleted
# use AVONET and PHYLACINE to extract this information and then filter

# BIRDS - AVONET
avonet <- read_xlsx("AVONET/AVONET Supplementary dataset 1.xlsx", sheet = "AVONET1_BirdLife")
names(avonet)

#extract genera of species in marine habitat
marine_bird_genera <- avonet %>%
  filter(Habitat == "Marine") %>%
  pull(Species1) %>%
  str_extract("^\\S+") %>%
  unique()

# MAMMALS - PHYLACINE
phylacine <- read.csv("PHYLACINE/Data/Traits/Trait_data.csv")
names(phylacine)

#extract genera of species in marine habitat
marine_mammal_genera <- phylacine %>%
  filter(Marine == 1) %>%
  mutate(species_clean = str_replace(Binomial.1.2, "_", " ")) %>%
  pull(species_clean) %>%
  str_extract("^\\S+") %>%
  unique()

# create column is_marine, to check and then delete
data_no_marine_genus <- data_no_sing2 %>%
  mutate(
    is_marine_genus = case_when(
      group == "birds"   & genus %in% marine_bird_genera   ~ TRUE,
      group == "mammals" & genus %in% marine_mammal_genera ~ TRUE,
      TRUE ~ FALSE
    )
  )

# check how many records before removing
data_no_marine_genus %>%
  filter(is_marine_genus) %>%
  distinct(genus, species, group) %>%
  arrange(group, genus)

# remove
data_no_marine_genus <- data_no_marine_genus %>%
  filter(!is_marine_genus)

#export
#write.csv(data_no_marine_genus, "20260813_combined_data.csv")


