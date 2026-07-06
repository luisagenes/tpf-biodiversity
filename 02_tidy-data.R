#################################################################
######## Cleaning GBIF data #####################################
#################################################################

# Luisa Genes 
# genes.luisa@gmail.com
# 2025 October

setwd("/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/tpf-biodiversity/data")

#load packages
library(dplyr)
library(stringr)

#load data
gbif <- read.csv2("gbif-2025-10.csv")

# filters to ensure geographical boundaries are correct (Rio city)


# filter locations within PNT
# keywords for PNT within the locality column
pnt_keywords <- c(
  "Floresta da Tijuca",
  "PN da Tijuca",
  "Tijuca National Park",
  "Parque Nacional da Tijuca",
  "Tijuca National",
  "P.N. Tijuca",
  "Parc national de Tijuca",
  "Parque Nacional de Tijuca"
)

# build a single regex pattern from the keywords, escaping periods etc.
pnt_pattern <- paste(str_replace_all(pnt_keywords, "\\.", "\\\\."), collapse = "|")

#create a new column indicating weather individual was registered inside or outside pnt
gbif <- gbif %>%
  mutate(
    location_status = if_else(
      str_detect(locality, regex(pnt_pattern, ignore_case = TRUE)),
      "inside_park",
      "outside_park"
    )
  )
  
# check summary
    table(gbif$location_status, useNA = "always")
    
     

