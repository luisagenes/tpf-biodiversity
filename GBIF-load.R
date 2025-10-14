#################################################################
######## Gathering Biodiversity Data from GBIF ##################
#################################################################

# Luisa Genes 
# genes.luisa@gmail.com
# 2025 October

#install.packages("rgbif")

#load packages
library(rgbif)
library(geobr)
library(sf)
library(dplyr)
library(lubridate)

# 1 - Get the City of Rio de Janeiro boundary (IBGE code 3304557) 
# Using a simplified geometry so GBIF accepts it
rio <- geobr::read_municipality(code_muni = 3304557, year = 2020, simplified = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

wkt_rio <- st_as_text(st_geometry(rio))

# 2 - Build list of GBIF download predicates: birds (Aves=212) + mammals (Mammalia=359)
pred_list <- list(
  pred("geometry", wkt_rio),
  pred_or(pred("classKey", 212), pred("classKey", 359)),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE)
)

# 3- Submit the download 
options(gbif_user="luisagenes", gbif_pwd="gitsen24!", gbif_email="genes.luisa@gmail.com")
key <- do.call(rgbif::occ_download, c(pred_list, list(format = "SIMPLE_CSV")))
key 
#### steps for donwloading through R did not work, so I downloaded it directly from gbif portal with this link 
# https://www.gbif.org/occurrence/download/0001597-251009101135966
# the link refers to the key that was created based on the predicates

# Use this citation in publications
# GBIF.org (09 October 2025) GBIF Occurrence Download https://doi.org/10.15468/dl.k2hdpa 

# load

library(readr)
library(dplyr)
library(lubridate)

zipfile <- "/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/data/0001597-251009101135966.zip"

# find the data file inside the ZIP
contents <- unzip(zipfile, list = TRUE)
csv_name <- contents$Name[grepl("\\.csv$", contents$Name)][1]  # GBIF uses .csv but it's TSV

# Read as TAB-delimited (TSV)
data <- readr::read_tsv(
  unz(zipfile, csv_name),
  guess_max = 200000,
  show_col_types = FALSE
)

#save csv file
#write.csv2(data, "gbif-2025-10.csv")

# visualize data before cleaning
gbif_data <- read.csv2("gbif-2025-10.csv")



# make filters to make sure location is within Rio city



         