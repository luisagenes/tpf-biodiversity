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

# set order for plots
data_unique <- data_unique %>%
  mutate(group = factor(group, levels = c("plants", "birds", "mammals")))

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
  theme_bw() +
  theme(panel.grid = element_blank())


#### Create new column within massifs (Tijuca, Mendanha, Pedra Branca)
data_unique <- data_unique %>%
  mutate(
    within_massif = case_when(
      within_forest == "inside_forest" ~ "inside_massif",
      within_forest %in% c("inside_other_uc", "outside_forest") ~ "outside_massif",
      TRUE ~ NA_character_
    )
  ) %>%
  distinct(species, within_massif, group, .keep_all = TRUE)

#barplot
ggplot(data_unique, aes(x = within_massif, fill = group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = group_colors) +
  labs(
    x = "Location",
    y = "Number of species",
    fill = "Group"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

#exploring - visualize mammals
library(tibble)
data_unique %>%
  filter(group == "mammals") %>%
  distinct(species, within_massif) %>%
  arrange(within_massif, species) %>%
  as_tibble() %>%
  print(n = 200)


#### Exploratory analyses with animal data ####

#filter fauna
data_fauna <- data %>%
  filter(group %in% c("birds", "mammals"))

data_fauna_unique <- data_fauna %>%
  distinct(species, within_forest, .keep_all = TRUE) %>%
  mutate(is_genus_only = str_count(species, "\\S+") == 1) #true if the genus is also found in species within the same within_forest level

# for each genus + within_forest combination, check whether a full species-level record for that genus already exists
genus_has_species <- data_fauna_unique %>%
  filter(!is_genus_only) %>%
  distinct(genus, within_forest) %>%
  mutate(has_species_level = TRUE)

data_fauna_unique <- data_fauna_unique %>%
  left_join(genus_has_species, by = c("genus", "within_forest")) %>%
  filter(!(is_genus_only & !is.na(has_species_level))) %>%
  select(-is_genus_only, -has_species_level)

## set colors for plots
library(viridis)
group_colors <- c(
  "birds"   = "blue3",
  "mammals" = "orange2"
)

# set order for plots
data_fauna_unique <- data_fauna_unique %>%
  mutate(group = factor(group, levels = c("birds", "mammals")))

#barplot
library(ggplot2)

ggplot(data_fauna_unique, aes(x = within_forest, fill = group)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = group_colors) +
  labs(
    x = "Location",
    y = "Number of species",
    fill = "Group"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

## See which species are only found in inside or outside forest

species_by_category <- data_fauna_unique %>%
  distinct(species, within_forest) %>%
  group_by(species) %>%
  mutate(n_categories = n_distinct(within_forest)) %>%
  ungroup()

# species found in only ONE category
unique_species <- species_by_category %>%
  filter(n_categories == 1) %>%
  select(species, within_forest) %>%
  arrange(within_forest, species)

# view them
options(na.print = NULL)
unique_species %>% print(n = 300)

# or split into three separate lists, one per category
unique_species %>% filter(within_forest == "inside_forest")
unique_species %>% filter(within_forest == "inside_other_uc")
unique_species %>% filter(within_forest == "outside_forest")

## Venn diagram
library(ggVennDiagram)
library(ggplot2)
library(dplyr)
library(patchwork)

# build species sets per within_forest category, separately for birds and mammals
build_species_sets <- function(df) {
  list(
    inside_forest   = df %>% filter(within_forest == "inside_forest")   %>% pull(species),
    inside_other_uc = df %>% filter(within_forest == "inside_other_uc") %>% pull(species),
    outside_forest  = df %>% filter(within_forest == "outside_forest")  %>% pull(species)
  )
}

birds_sets   <- data_fauna_unique %>% filter(group == "birds")   %>% build_species_sets()
mammals_sets <- data_fauna_unique %>% filter(group == "mammals") %>% build_species_sets()

# rename the set names for display
rename_sets <- c(
  inside_forest   = "forest",
  inside_other_uc = "other_uc",
  outside_forest  = "outside"
)

names(birds_sets)   <- rename_sets[names(birds_sets)]
names(mammals_sets) <- rename_sets[names(mammals_sets)]


# plot
venn_birds <- ggVennDiagram(birds_sets, set_size = 3) +
  scale_fill_gradient(low = "white", high = "blue3") +
  labs(title = "Birds") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12))

venn_mammals <- ggVennDiagram(mammals_sets, set_size = 3) +
  scale_fill_gradient(low = "white", high = "orange2") +
  labs(title = "Mammals") +
  theme(legend.position = "none",
        plot.title = element_text(size = 12))

venn_birds + venn_mammals

###### Last year seen ####
#load data
data <- read.csv("20260805_combined_data.csv")

library(dplyr)
library(stringr)
library(ggplot2)

#drop genus-only entries and missing years
data_species <- data %>%
  filter(!is.na(species), str_count(species, "\\S+") > 1) %>%
  filter(!is.na(year), year !=3000) #delete year 3000, mistake

# get each species' most recent record
last_record <- data_species %>%
  group_by(species, group) %>%
  summarise(last_year = max(year, na.rm = TRUE), .groups = "drop")

# count how many species had their last record in each year
species_by_lastyear <- last_record %>%
  count(last_year, group, name = "n_species")

#colors
group_colors <- c(
  "birds"   = "blue3",
  "mammals" = "orange2",
  "plants"  = "green4"
)

# scatterplot
ggplot(species_by_lastyear, aes(x = last_year, y = n_species, color = group)) +
  geom_point(size = 2, alpha=0.7) +
  scale_color_manual(values = group_colors) +
  labs(
    x = "Year of last record",
    y = "Number of species",
    color = "Group"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

#should we define a baseline?

#which birds and mammals were last seen before 1950? - maybe check if they changed naems
last_record %>%
  filter(group %in% c("birds", "mammals"), last_year < 1950) %>%
  arrange(group, last_year) %>%
  print(n = 200)

#maybe many were mis-identified?

#exclude singletons 
data_no_sing <- data %>%
  filter(!is.na(species), str_count(species, "\\S+") > 1) %>%
  group_by(species) %>% 
  filter(n() > 1) %>%   # count total occurrences per species, then drop singletons
  ungroup()

# check how many species/records were removed
n_distinct(data$species) - n_distinct(data_no_sing$species) #species removed
nrow(data) - nrow(data_no_sing) # records removed


#plot again
# get each species' most recent record
last_record <- data_no_sing %>%
  filter(!is.na(year), year != 3000) %>%
  group_by(species, group) %>%
  summarise(last_year = max(year, na.rm = TRUE), .groups = "drop")

# count how many species had their last record in each year
species_by_lastyear <- last_record %>%
  count(last_year, group, name = "n_species")

#colors
group_colors <- c(
  "birds"   = "blue3",
  "mammals" = "orange2",
  "plants"  = "green4"
)

# scatterplot
ggplot(species_by_lastyear, aes(x = last_year, y = n_species, color = group)) +
  geom_point(size = 2, alpha=0.7) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "no singletons",
    x = "Year of last record",
    y = "Number of species",
    color = "Group"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

### delete spp with less than 2 records
#  drop species with less than 2 records
data_no_sing2 <- data %>%
  filter(!is.na(species), str_count(species, "\\S+") > 1) %>%
  group_by(species) %>%
  filter(n() > 2) %>%
  ungroup()

# check how many species/records were removed compared to the original
n_distinct(data$species) - n_distinct(data_no_sing2$species)
nrow(data) - nrow(data_no_sing2)

#plot again
last_record <- data_no_sing2 %>%
  filter(!is.na(year), year != 3000) %>%
  group_by(species, group) %>%
  summarise(last_year = max(year, na.rm = TRUE), .groups = "drop")

species_by_lastyear <- last_record %>%
  count(last_year, group, name = "n_species")

group_colors <- c(
  "birds"   = "blue3",
  "mammals" = "orange2",
  "plants"  = "green4"
)

ggplot(species_by_lastyear, aes(x = last_year, y = n_species, color = group)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = group_colors) +
  labs(
    title = "more than 2 records",
    x = "Year of last record",
    y = "Number of species",
    color = "Group"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

#repeat last seen list
#which birds and mammals were last seen before 1950? - maybe check if they changed naems
last_record %>%
  filter(group %in% c("birds", "mammals"), last_year < 1950) %>%
  arrange(group, last_year) %>%
  print(n = 200)

last_record %>%
  filter(group %in% "plants", last_year < 1950) %>%
  arrange(group, last_year) %>%
  print(n = 200)

#last record 30 years ago
last_record %>%
  filter(group %in% c("birds", "mammals"), last_year < 1995) %>%
  arrange(group, last_year) %>%
  print(n = 200)

last_record %>%
  filter(group %in% "plants", last_year < 1995) %>%
  arrange(group, last_year) %>%
  print(n = 400)

