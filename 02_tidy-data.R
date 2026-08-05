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
  "Parque Nacional de Tijuca",
  "Morro do Inglês",
  "Corcovado",
  "Paineiras",
  "Três Rios",
  "mata da tijuca",
  "cristo redentor",
  "vista chinesa",
  "mesa do imperador",
  "estrada das paineiras",
  "cachoeira das almas",
  "pico da tijuca"
)

# Mendanha and Pedra Branca
mendanha_keywords <- c("Mendanha")
pedrabranca_keywords <- c("Pedra Branca",
                          "Camorim")

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
#write.csv(gbif, "260804_gbif-location.csv")

    
#### Load additional bird, mammal and plant data ####

#load data
#gbif <- read.csv("260804_gbif-location.csv")
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
  "Parque Nacional de Tijuca",
  "Morro do Inglês",
  "Corcovado",
  "Paineiras",
  "Três Rios",
  "mata da tijuca",
  "cristo redentor",
  "vista chinesa",
  "mesa do imperador",
  "estrada das paineiras",
  "cachoeira das almas",
  "pico da tijuca"
)

# Mendanha and Pedra Branca
mendanha_keywords <- c("Mendanha")
pedrabranca_keywords <- c("Pedra Branca",
                          "Camorim")

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

#include group column
mammals <- mammals %>%
  mutate(group = "mammals")

birds <- birds %>%
  mutate(group = "birds")

gbif <- gbif %>%
  mutate(
    group = case_when(
      class == "Aves"     ~ "birds",
      class == "Mammalia" ~ "mammals",
      TRUE ~ NA_character_
    ))

#### build unique animal dataset 
combined_animals <- bind_rows(gbif, birds, mammals)

names(combined_animals)

#save
#write.csv2(combined_animals, "20260804_combined_animals.csv")

### Tidy plant data

#select columns to keep for now

plants <- plants %>%
  select(
    maciço,
    family,
    genus,
    species = species.correct,
    year = yearcollected,
    locality,
    longitude,
    latitude
  )


### filter locations within PNT (I should have included that in the previous step with mammals and birds)

# keywords for PNT within the locality column
pnt_keywords <- c(
  "Floresta da Tijuca",
  "PN da Tijuca",
  "Tijuca National Park",
  "Parque Nacional da Tijuca",
  "Tijuca National",
  "P.N. Tijuca",
  "Parc national de Tijuca",
  "Parque Nacional de Tijuca",
  "Morro do Inglês",
  "Corcovado",
  "Paineiras",
  "Três Rios",
  "mata da tijuca",
  "cristo redentor",
  "vista chinesa",
  "mesa do imperador",
  "estrada das paineiras",
  "cachoeira das almas",
  "pico da tijuca"
)

# Mendanha and Pedra Branca
mendanha_keywords <- c("Mendanha")
pedrabranca_keywords <- c("Pedra Branca",
                          "Camorim")

# regex pattern from the keywords, escaping periods etc
build_pattern <- function(keywords) {
  paste(str_replace_all(keywords, "\\.", "\\\\."), collapse = "|")
}

pnt_pattern         <- build_pattern(pnt_keywords)
mendanha_pattern    <- build_pattern(mendanha_keywords)
pedrabranca_pattern <- build_pattern(pedrabranca_keywords)

#create a new column indicating wether individual was registered inside or outside the ucs
plants <- plants %>%
  mutate(
    location_status = case_when(
      str_detect(locality, regex(pnt_pattern, ignore_case = TRUE))         ~ "inside_pnt",
      str_detect(locality, regex(mendanha_pattern, ignore_case = TRUE))    ~ "inside_mendanha",
      str_detect(locality, regex(pedrabranca_pattern, ignore_case = TRUE)) ~ "inside_pedrabranca",
      TRUE ~ "outside_ucs"
    )
  )

# check summary
table(plants$location_status, useNA = "always")


#fix potential errors in latlong
plants <- plants %>%
  mutate(
    latitude  = as.numeric(str_replace(latitude, ",", ".")),
    longitude = as.numeric(str_replace(longitude, ",", "."))
  )

plants$year <- as.numeric(plants$year)

#include group column
plants <- plants %>%
  mutate(group = "plants")


#### build unique dataset 
combined_data <- bind_rows(combined_animals, plants)

names(combined_data)

#remove unnecessary columns
combined_data <- combined_data %>%
  select(-inside_park_geo, -location_status_final)

#create summarized column with location inside or outside protected area
combined_data <- combined_data %>%
  mutate(
    within_forest = case_when(
      location_status %in% c("inside_pnt", "inside_mendanha", "inside_pedrabranca") ~ "inside_forest",
      location_status == "outside_ucs" ~ "outside_forest",
      TRUE ~ NA_character_
    )
  )

#save
#write.csv(combined_data, "20260723_combined_data.csv")


##### Filters based on geographical location of protected areas ######
# Filter locations for animals - edited by Matheus
library(sf)
library(dplyr)

#shapefiles directory
shapefiles_dir <- "/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/tpf-biodiversity/data/Shapefiles" #replace by the path in each computer

# --- load protected area shapefiles ---
pnt          <- st_read(file.path(shapefiles_dir, "limite_PARNA_TIJUCA/limite_PARNA_TIJUCA.shp"), quiet = TRUE)
pepb           <- st_read(file.path(shapefiles_dir, "Parque Estadual da Pedra Branca/Parque Estadual da Pedra Branca.shp"), quiet = TRUE)
ucs_municipais <- st_read(file.path(shapefiles_dir, "gpl_ucs_municipais_2023_icms_2024_me/gpl_ucs_municipais_2023_icms_2024_me.shp"), quiet = TRUE)

# --- fix any invalid geometries ---
pnt          <- st_make_valid(pnt)
pepb           <- st_make_valid(pepb)
ucs_municipais <- st_make_valid(ucs_municipais)

# --- standardize each layer into a common uc / categoria_uc / esfera_uc structure --- from matheus
pnt_uc <- pnt %>%
  summarise() %>%   # dissolves the 4 park sectors into one PNT polygon
  mutate(
    uc = "PARQUE NACIONAL DA TIJUCA",
    categoria_uc = "PARQUE NACIONAL",
    esfera_uc = "FEDERAL"
  )

pepb_uc <- pepb %>%
  transmute(
    uc = toupper(nome),
    categoria_uc = "PARQUE ESTADUAL",
    esfera_uc = "ESTADUAL"
  )

ucs_municipais_uc <- ucs_municipais %>%
  transmute(
    uc = toupper(nome),
    categoria_uc = toupper(categoria),
    esfera_uc = "MUNICIPAL"
  )

# prepare points
combined_animals <- combined_animals %>%
  mutate(
    row_id     = row_number(), #create a row_id column so that data can be later joined (spatial points vs. non spatial, based on keywords)
    has_coords = !is.na(latitude) & !is.na(longitude) #flag which ones dont have coordinates, so we can later use just keywords
  )


#make sure all data is within Rio
combined_animals %>%
  filter(group %in% c("birds", "mammals"), latitude > 0 | longitude > 0) %>%
  nrow()

combined_animals <- combined_animals %>%
  mutate(
    latitude  = if_else(latitude  > 0, -latitude,  latitude),
    longitude = if_else(longitude > 0, -longitude, longitude)
  )

#then convert
animals_sf <- combined_animals %>%
  filter(has_coords) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) #convert coordinates into spatial object

# --- reproject all UC layers to match the points CRS 
crs_ref <- st_crs(animals_sf)

pnt_uc          <- st_transform(pnt_uc, crs_ref)
pepb_uc           <- st_transform(pepb_uc, crs_ref)
ucs_municipais_uc <- st_transform(ucs_municipais_uc, crs_ref)

ucs <- bind_rows(pnt_uc, pepb_uc, ucs_municipais_uc)


#### make spatial filter (with one UC where they overlap -- APA da Pedra Branca and PE da Pedra Branca)

animals_sf <- st_join(animals_sf, ucs, join = st_intersects, left = TRUE)

animals_matches <- animals_sf %>%
  st_drop_geometry() %>%
  select(row_id, uc)

# --- classify each point using a priority order:
#     PNT > Mendanha > Pedra Branca (state park OR municipal APA, either counts)
#     > any other UC > outside_ucs ---
animals_classified <- animals_matches %>%
  group_by(row_id) %>%
  summarise(
    uc_final = case_when(
      any(uc == "PARQUE NACIONAL DA TIJUCA", na.rm = TRUE) ~
        "PARQUE NACIONAL DA TIJUCA",
      any(uc == "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA", na.rm = TRUE) ~
        "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
      any(uc %in% c("PARQUE ESTADUAL DA PEDRA BRANCA",
                    "AREA DE PROTECAO AMBIENTAL DA PEDRA BRANCA"), na.rm = TRUE) ~
        "PARQUE ESTADUAL DA PEDRA BRANCA",
      any(!is.na(uc)) ~ first(na.omit(uc)),  # some other UC - keep its own name
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )


### join
combined_animals <- combined_animals %>%
  left_join(animals_classified, by = "row_id")

# --- recode the earlier text/keyword-based location_status to the same
#     full UC names, so the whole column uses one consistent naming scheme ---
combined_animals <- combined_animals %>%
  mutate(
    location_status = case_when(
      has_coords & !is.na(uc_final) ~ uc_final,
      has_coords & is.na(uc_final)  ~ "outside_ucs",
      location_status == "inside_pnt"         ~ "PARQUE NACIONAL DA TIJUCA",
      location_status == "inside_mendanha"    ~ "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
      location_status == "inside_pedrabranca" ~ "PARQUE ESTADUAL DA PEDRA BRANCA",
      TRUE ~ location_status
    )
  ) %>%
  select(-row_id, -has_coords, -uc_final)

table(combined_animals$location_status, useNA = "always")


#### I noticed there were repeated rows from gbif and inaturalist. keep gbif

library(stringr)

combined_animals <- combined_animals %>%
  mutate(
    inat_id = coalesce(
      str_extract(occurrenceID, "(?<=observations/)\\d+"),
      str_extract(Source, "(?<=observations/)\\d+")
    )
  )

# how many observations appear more than once across sources?
combined_animals %>%
  filter(!is.na(inat_id)) %>%
  count(inat_id) %>%
  filter(n > 1) %>%
  nrow()

# split into rows that have iNaturalist ID and rows that dont
# rows without an inat_id can't be verified as duplicates, leave them as they are
with_id    <- combined_animals %>% filter(!is.na(inat_id))
without_id <- combined_animals %>% filter(is.na(inat_id))

# for rows sharing the same inat_id, prioritize the gbif-sourced version (gbif rows are the ones with a non-missing gbifID)
with_id_dedup <- with_id %>%
  mutate(has_gbif_id = !is.na(gbifID)) %>%
  arrange(inat_id, desc(has_gbif_id)) %>%   # TRUE (gbif) sorts before FALSE within each inat_id
  distinct(inat_id, .keep_all = TRUE) %>%
  select(-has_gbif_id)

# recombine
combined_animals <- bind_rows(with_id_dedup, without_id)


# 1. Update location_status_final for combined_animals
combined_animals <- combined_animals %>%
  mutate(
    location_status_final = if_else(
      location_status == "PARQUE NACIONAL DA TIJUCA",
      "inside_park",
      "outside_park"
    )
  )

combined_animals <- combined_animals %>%
  select(-inside_park_geo)

# 2. Combine animals with plants
combined_data <- bind_rows(combined_animals, plants)

# 3. Recompute location_status for plants ONLY, directly from locality,
#    using the pre-established keyword patterns. Animals keep their existing geometry-based location_status untouched.
combined_data <- combined_data %>%
  mutate(
    location_status = case_when(
      group == "plants" & str_detect(locality, regex(pnt_pattern, ignore_case = TRUE))         ~ "PARQUE NACIONAL DA TIJUCA",
      group == "plants" & str_detect(locality, regex(mendanha_pattern, ignore_case = TRUE))    ~ "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
      group == "plants" & str_detect(locality, regex(pedrabranca_pattern, ignore_case = TRUE)) ~ "PARQUE ESTADUAL DA PEDRA BRANCA",
      group == "plants"                                                                        ~ "outside_ucs",
      TRUE ~ location_status
    )
  )

# 4. Build within_forest across the full combined dataset
combined_data <- combined_data %>%
  mutate(
    within_forest = case_when(
      location_status %in% c(
        "PARQUE NACIONAL DA TIJUCA",
        "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
        "PARQUE ESTADUAL DA PEDRA BRANCA"
      ) ~ "inside_forest",
      location_status == "outside_ucs" ~ "outside_forest",
      TRUE ~ "inside_other_uc"
    )
  )

# check everything landed as expected
table(combined_data$location_status, combined_data$group, useNA = "always")
table(combined_data$within_forest, useNA = "always")

#export new data
#write.csv(combined_data, "20260805_combined_data.csv")




    
