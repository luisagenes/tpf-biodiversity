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
library(readxl)

#load data
gbif <- read.csv2("gbif-2025-10.csv")


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

# Mendanha and Pedra Branca
mendanha_keywords <- c("Mendanha")
pedrabranca_keywords <- c("Pedra Branca")

# regex pattern from the keywords, escaping periods etc
build_pattern <- function(keywords) {
  paste(str_replace_all(keywords, "\\.", "\\\\."), collapse = "|")
}

pnt_pattern         <- build_pattern(pnt_keywords)
mendanha_pattern    <- build_pattern(mendanha_keywords)
pedrabranca_pattern <- build_pattern(pedrabranca_keywords)

#create a new column indicating wether individual was registered inside or outside the ucs
gbif <- gbif %>%
  mutate(
    location_status = case_when(
      str_detect(locality, regex(pnt_pattern, ignore_case = TRUE))         ~ "inside_pnt",
      str_detect(locality, regex(mendanha_pattern, ignore_case = TRUE))    ~ "inside_mendanha",
      str_detect(locality, regex(pedrabranca_pattern, ignore_case = TRUE)) ~ "inside_pedrabranca",
      TRUE ~ "outside_ucs"
    )
  )

  
# check summary
table(gbif$location_status, useNA = "always")

#export data
#write.csv(gbif, "260706_gbif-location.csv")


##### filters based on geographical location of PNT - will not work now that we included Mendanha and Pedra Branca - needs revision if we decide this step is important
library(sf)
library(dplyr)
    
    # load Tijuca boundary
    park_boundary <- st_read("/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/data/tijuca_boundary.geojson")
    
    # confirm CRS
    st_crs(park_boundary)  # should show EPSG:4326
    
    # convert gbif records to spatial points (keep row_id to avoid join issues)
    gbif <- gbif %>% mutate(row_id = row_number())
    
    gbif_sf <- gbif %>%
      filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE) %>%
      mutate(inside_park_geo = lengths(st_intersects(., park_boundary)) > 0) %>%
      st_drop_geometry() %>%
      select(row_id, inside_park_geo)
    
    gbif <- gbif %>% left_join(gbif_sf, by = "row_id")
    
    # create table to show if it's inside or outside based on locality and coordinates
    gbif <- gbif %>%
      mutate(
        location_status_final = case_when(
          inside_park_geo == TRUE ~ "inside_park",
          inside_park_geo == FALSE ~ "outside_park",
          is.na(inside_park_geo) & location_status == "inside_park" ~ "inside_park_text_only",
          TRUE ~ "outside_park"
        )
      )
#only 4000 observations were written as outside the park but were actually inside based on coordinates    

    
    table(gbif$location_status, gbif$inside_park_geo, useNA = "always")
    
    
#### Load additional bird, mammal and plant data ####

#load data
#gbif <- read.csv("260706_gbif-location.csv")
#birds <- read_xlsx("meridian_biodiversidade_aves.xlsx")
#mammals <- read_xlsx("meridian_biodiversidade_mamiferos.xlsx")
#plants <- read_xlsx("meridian_biodiversidade_plantas.xlsx", sheet=2)

#tidy data before joining
#delete columns that are unnecessary now

#gbif
gbif <- gbif %>%
  select(-c(
    X.1,
    datasetKey,
    verbatimScientificName,
    verbatimScientificNameAuthorship,
    scientificName,
    countryCode,
    stateProvince,
    occurrenceStatus,
    publishingOrgKey,
    coordinatePrecision,
    coordinateUncertaintyInMeters,
    elevation,
    elevationAccuracy,
    depth,
    depthAccuracy,
    day,
    month,
    mediaType,
    license,
    issue,
    row_id,
    taxonKey,
    speciesKey,
    basisOfRecord,
    institutionCode,
    collectionCode,
    catalogNumber,
    recordNumber,
    identifiedBy,
    dateIdentified,
    rightsHolder,
    recordedBy,
    typeStatus,
    establishmentMeans,
    lastInterpreted
  ))

### Standardize column names
library(dplyr)

rename_map <- c(
  locality  = "Local",
  species   = "Espécie",
  latitude  = "Latitude",
  longitude = "Longitude",
  year      = "Ano",
  Source    = "Fonte",
  n_records = "N Registros"
)

birds <- birds %>% rename(any_of(rename_map))
mammals <- mammals %>% rename(any_of(rename_map))


rename_map2 <- c(
  latitude  = "decimalLatitude",
  longitude = "decimalLongitude"
)
gbif <- gbif %>% rename(any_of(rename_map2))


#convert lat long to decimal - remove degree symbol, and replace S by negative and N by positive
library(dplyr)
library(stringr)

convert_coord <- function(x) {
  value <- as.numeric(str_extract(x, "[0-9.]+"))
  hemisphere <- str_extract(x, "[NSEW]$")
  
  # apply sign: S and W are negative, N and E are positive
  sign <- if_else(hemisphere %in% c("S", "W"), -1, 1)
  
  value * sign
}

birds <- birds %>%
  mutate(
    latitude  = convert_coord(latitude),
    longitude = convert_coord(longitude)
  )

mammals <- mammals %>%
  mutate(
    latitude  = convert_coord(latitude),
    longitude = convert_coord(longitude)
  )

### Birds and Mammals 
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

# Mendanha and Pedra Branca
mendanha_keywords <- c("Mendanha")
pedrabranca_keywords <- c("Pedra Branca")

# regex pattern from the keywords, escaping periods etc
build_pattern <- function(keywords) {
  paste(str_replace_all(keywords, "\\.", "\\\\."), collapse = "|")
}

pnt_pattern         <- build_pattern(pnt_keywords)
mendanha_pattern    <- build_pattern(mendanha_keywords)
pedrabranca_pattern <- build_pattern(pedrabranca_keywords)

#create a new column indicating wether individual was registered inside or outside the ucs
mammals <- mammals %>%
  mutate(
    location_status = case_when(
      str_detect(locality, regex(pnt_pattern, ignore_case = TRUE))         ~ "inside_pnt",
      str_detect(locality, regex(mendanha_pattern, ignore_case = TRUE))    ~ "inside_mendanha",
      str_detect(locality, regex(pedrabranca_pattern, ignore_case = TRUE)) ~ "inside_pedrabranca",
      TRUE ~ "outside_ucs"
    )
  )

birds <- birds %>%
  mutate(
    location_status = case_when(
      str_detect(locality, regex(pnt_pattern, ignore_case = TRUE))         ~ "inside_pnt",
      str_detect(locality, regex(mendanha_pattern, ignore_case = TRUE))    ~ "inside_mendanha",
      str_detect(locality, regex(pedrabranca_pattern, ignore_case = TRUE)) ~ "inside_pedrabranca",
      TRUE ~ "outside_ucs"
    )
  )

# check summary
table(birds$location_status, useNA = "always")
table(mammals$location_status, useNA = "always")

#### build unique animal dataset 
combined_animals <- bind_rows(gbif, birds, mammals)

names(combined_animals)

#save
#write.csv2(combined_animals, "20260721_combined_animals.csv")





#delete repeated mammal observations (from inaturalist)

    
