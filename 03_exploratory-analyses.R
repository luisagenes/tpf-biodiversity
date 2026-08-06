#################################################################
######## GBIF- exploratory analyses #####################################
#################################################################

# Luisa Genes 
# genes.luisa@gmail.com
# 2026 July

#20260721_combined_data.csv

setwd("/Users/luisagenes/Dropbox/_Research/1. PhD/Funding/2025 Meridian/Pesquisa/Meridian-Pesquisa/tpf-biodiversity/data")

#load packages
library(dplyr)
library(stringr)
library(readxl)


#load data
data <- read.csv("20260805_combined_data.csv")

#### Number of species inside vs. outside the park ####

# first make sure there are no repeated spp within those categories
library(dplyr)
data_unique <- data %>%
  distinct(species, within_forest, .keep_all = TRUE)

## set colors for plots
library(viridis)
group_colors <- c(
  "birds"   = "blue3",
  "mammals" = "orange2",
  "plants"  = "green4"
)

#barplot
library(ggplot2)

ggplot(data_unique, aes(x = within_forest, fill = group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = group_colors) +
  labs(
    x = "Location",
    y = "Number of species",
    fill = "Group"
  ) +
  theme_minimal()


