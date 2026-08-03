# Rodar este script sempre que abrir o projeto!
# Ele garante que os pacotes instalados sejam os mesmos pra todo mundo.

# Atualizando os pacotes:
renv::restore()

# Carregando os pacotes:
# (P.S. Adicionar à essa lista novos pacotes quando instalados)

pkgs <- c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "lubridate",
  "readr",
  "readxl",
  "vegan",
  "BIOMASS",
  "sf",
  "geobr",
  "rgbif",
  "flora",
  "purrr",
  "bipartite",
  "igraph",
  "tibble",
  "ggraph"
)

lapply(pkgs, library, character.only = TRUE)

# Para adicionar novos pacotes:

renv::install("ggraph")
renv::snapshot() # garante que o novo pacote fique guardado na lockfile e
# seja instalado sempre que alguem rodar renv::restore


