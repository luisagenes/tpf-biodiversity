#################################################################
######## Standardize plant taxonomy #############################
#################################################################

# Luisa Genes 
# genes.luisa@gmail.com
# 2025 November

library(dplyr)
library(readxl)
library(stringr)
library(readr)

setwd("/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/data/")

# load data (do not use ç, and other special characters in file names. also, avoid having spreadsheets with multiple tabs)
data <- read_xlsx("planilha_unificada_coletas_macico_Tijuca_editada.xlsx",
                  sheet = 2)

####standardize plant spp names####
library(flora)

# create spp vector
species_subs <- data$scientificname

# keep only unique, non-empty and non-NA  to run faster
uniq_names <- unique(species_subs[!is.na(species_subs) & species_subs != ""])

#filter - and other characters
uniq_names <- uniq_names[!uniq_names %in% c("", "-", "_")]

#check if any NAs remaining
any(is.na(uniq_names))

# get accepted names from Flora e Funga do Brasil
brflora <- get.taxa(uniq_names, 
                    domain=TRUE)  

#original.search shows what we searched for
#scientific.name shows the accepted name according to Flora e Funga do Brasil.

# clean and tidy brflora to create a species.correct column 
brflora2 <- brflora

brflora2 <- brflora2 %>%
  mutate(
    species.correct = case_when(
      taxon.rank == "genus" ~
        str_extract(scientific.name, "^[A-Z][a-z-]+"),
      
      taxon.rank == "species" ~
        str_extract(scientific.name, "^[A-Z][a-z-]+\\s+[a-z-]+"),
      
      # catch subspecies/varieties/forms/etc.
      str_detect(taxon.rank, regex("sub|infra|var|forma|form|subsp", ignore_case = TRUE)) ~
        str_extract(scientific.name, "^[A-Z][a-z-]+\\s+[a-z-]+\\s+[a-z-]+"),
      
      TRUE ~ scientific.name
    )
  )

#make a genus and a subsp column and keep only family, genus and species correct
#also further tidy subspecies names to remove things like "subsp."
brflora2 <- brflora2 %>%
  mutate(
    family.correct = family,
    genus.correct  = word(scientific.name, 1),
    
    # remove from subspecies things like subsp., var, and capture third epithet after the wrong word
    sci_clean_for_subsp = str_replace(
      scientific.name,
      pattern = "^(\\p{Lu}[\\p{Ll}-]+\\s+[\\p{Ll}-]+)\\s+(?:subsp|ssp|subspecies|var|var\\.|forma|form|f)\\.?\\s+([\\p{Ll}-]+).*",
      replacement = "\\1 \\2"
    ),
    
    species.subspecies.correct = case_when(
      taxon.rank == "subspecies" &
        str_detect(sci_clean_for_subsp,
                   "^[\\p{Lu}][\\p{Ll}-]+\\s+[\\p{Ll}-]+\\s+[\\p{Ll}-]+\\b") ~
        str_extract(sci_clean_for_subsp,
                    "^[\\p{Lu}][\\p{Ll}-]+\\s+[\\p{Ll}-]+\\s+[\\p{Ll}-]+"),
      TRUE ~ NA_character_
    )
  ) %>%
  select(original.search, family.correct, genus.correct, species.correct, species.subspecies.correct)

#when there is a genus name in species.correct, replace it by NA (and keep information in corrected genus and family columns)
brflora2 <- brflora2 %>%
  mutate(
    species.correct = if_else(
      str_count(species.correct, "\\S+") == 1,  # count number of words
      NA_character_,                            # replace to NA if only one
      species.correct                          
    )
  )


#add corrected names back to original dataset
data_std <- data %>%
  left_join(brflora2, by = c("scientificname" = "original.search"))

#save file
#write.csv(data_std, "2025-11-07-corrected_planilha_unificada_coletas_macico_Tijuca_editada.xlsx")
