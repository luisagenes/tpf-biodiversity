comunidades <- read.csv("comunidades_clean.csv")

top_por_site_aves <- comunidades %>%
  filter(group == "birds", !is.na(species_padronizado),
         location_status %in% c("outside_ucs",
                                "PARQUE NACIONAL DA TIJUCA",
                                "PARQUE ESTADUAL DA PEDRA BRANCA",
                                "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA")) %>%
  count(species_padronizado, location_status, sort = TRUE) %>%
  group_by(location_status) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  # truque manual pra permitir ordenação independente dentro de cada facet
  mutate(species_site = paste(species_padronizado, location_status, sep = "__"),
         species_site = reorder(species_site, n))

ggplot(top_por_site_aves, aes(x = species_site, y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~location_status, scales = "free") +
  scale_x_discrete(labels = function(x) str_remove(x, "__.*")) +
  labs(x = NULL, y = "Número de registros", title = "20 aves mais registradas por local") +
  theme_minimal()

top_por_site_mamiferos <- comunidades %>%
  filter(group == "mammals", !is.na(species_padronizado),
         location_status %in% c("outside_ucs",
                                "PARQUE NACIONAL DA TIJUCA",
                                "PARQUE ESTADUAL DA PEDRA BRANCA",
                                "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA")) %>%
  count(species_padronizado, location_status, sort = TRUE) %>%
  group_by(location_status) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  # truque manual pra permitir ordenação independente dentro de cada facet
  mutate(species_site = paste(species_padronizado, location_status, sep = "__"),
         species_site = reorder(species_site, n))

ggplot(top_por_site_mamiferos, aes(x = species_site, y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~location_status, scales = "free") +
  scale_x_discrete(labels = function(x) str_remove(x, "__.*")) +
  labs(x = NULL, y = "Número de registros", title = "20 mamíferos mais registrados por local") +
  theme_minimal()

top_por_site_plantas <- comunidades %>%
  filter(group == "plants", !is.na(species_padronizado),
         location_status %in% c("outside_ucs",
                                "PARQUE NACIONAL DA TIJUCA",
                                "PARQUE ESTADUAL DA PEDRA BRANCA",
                                "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA")) %>%
  count(species_padronizado, location_status, sort = TRUE) %>%
  group_by(location_status) %>%
  slice_max(n, n = 20) %>%
  ungroup() %>%
  # truque manual pra permitir ordenação independente dentro de cada facet
  mutate(species_site = paste(species_padronizado, location_status, sep = "__"),
         species_site = reorder(species_site, n))

ggplot(top_por_site_plantas, aes(x = species_site, y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~location_status, scales = "free") +
  scale_x_discrete(labels = function(x) str_remove(x, "__.*")) +
  labs(x = NULL, y = "Número de registros", title = "20 plantas mais registradas por local") +
  theme_minimal()

# ============================================================
# DIVERSIDADE -- iNEXT (Chao et al. 2016), por grupo taxonômico e site
# ============================================================


## --- Montar dados de "abundância" (nº de registros por espécie) por
##     grupo e site -- usar species_padronizado, já filtrando NA ---
construir_abundancia_inext <- function(grupo_alvo, locs) {
  comunidades %>%
    filter(group == grupo_alvo, location_status %in% locs, !is.na(species_padronizado)) %>%
    count(species_padronizado, name = "n") %>%
    arrange(desc(n)) %>%
    pull(n)
}

grupos <- c("birds", "mammals", "plants")

dados_inext <- list()
for (g in grupos) {
  for (nome_site in names(sites)) {
    chave <- paste(g, nome_site, sep = "_")
    dados_inext[[chave]] <- construir_abundancia_inext(g, sites[[nome_site]])
  }
}

## --- Conferir tamanho de amostra antes de rodar -- iNEXT precisa de
##     pelo menos alguns indivíduos/registros por site pra funcionar bem ---
map_dbl(dados_inext, sum)

filtrar_dados_validos <- function(prefixo) {
  d <- dados_inext[str_starts(names(dados_inext), prefixo)]
  d[sapply(d, function(x) length(x) >= 2 && sum(x) > 0)]
} # ^criado para generalizar no futuro. no nosso caso, apenas o mendanha eh excluido por n ter mamiferos

resultado_birds   <- iNEXT(filtrar_dados_validos("birds_"), q = c(0, 1, 2), datatype = "abundance")
resultado_mammals <- iNEXT(filtrar_dados_validos("mammals_"), q = c(0, 1, 2), datatype = "abundance")
resultado_plants  <- iNEXT(filtrar_dados_validos("plants_"), q = c(0, 1, 2), datatype = "abundance")

dados_parques_birds <- dados_inext[c("birds_inside_mendanha", "birds_inside_pedrabranca", "birds_inside_pnt")]
resultado_parques_birds <- iNEXT(dados_parques_birds, q = c(0, 1, 2), datatype = "abundance")
dados_parques_mammals <- dados_inext[c("mammals_inside_pedrabranca", "mammals_inside_pnt")]
resultado_parques_mammals <- iNEXT(dados_parques_mammals, q = c(0, 1, 2), datatype = "abundance")
dados_parques_plants <- dados_inext[c("plants_inside_mendanha", "plants_inside_pedrabranca", "plants_inside_pnt")]
resultado_parques_plants <- iNEXT(dados_parques_plants, q = c(0, 1, 2), datatype = "abundance")

dados_birds <- dados_inext[c("birds_inside_ucs", "birds_outside_ucs")]
resultado_birds <- iNEXT(dados_birds, q = c(0, 1, 2), datatype = "abundance")
dados_plants <- dados_inext[c("plants_inside_ucs", "plants_outside_ucs")]
resultado_plants <- iNEXT(dados_plants, q = c(0, 1, 2), datatype = "abundance")
dados_mammals <- dados_inext[c("mammals_inside_ucs", "mammals_outside_ucs")]
resultado_mammals <- iNEXT(dados_mammals, q = c(0, 1, 2), datatype = "abundance")

## --- Gráficos AVES ---

ggiNEXT(resultado_birds, type = 1, facet.var = "Order.q") +
  labs(title = "Aves: diversidade por tamanho de amostra")

ggiNEXT(resultado_parques_birds, type = 1, facet.var = "Order.q") +
  labs(title = "Aves: diversidade por tamanho de amostra (parques)")

ggiNEXT(resultado_birds, type = 2, facet.var = "Order.q") +
  labs(title = "Aves: cobertura por tamanho de amostra")

ggiNEXT(resultado_parques_birds, type = 2, facet.var = "Order.q") +
  labs(title = "Aves: cobertura por tamanho de amostra (parques)")

## --- Plot tipo 3: baseado em COBERTURA de amostra -- este é o que
##     compara diversidade no mesmo nível de cobertura, não no mesmo N bruto) ---
ggiNEXT(resultado_birds, type = 3, facet.var = "Order.q") +
  labs(title = "Aves: diversidade por cobertura de amostra")

ggiNEXT(resultado_parques_birds, type = 3, facet.var = "Order.q") +
  labs(title = "Aves: diversidade por cobertura de amostra (parques)")

resultado_birds$AsyEst
estimateD(
  filtrar_dados_validos("birds_"),  
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = 0.80
)

## --- Gráficos MAMIFEROS ---

ggiNEXT(resultado_mammals, type = 1, facet.var = "Order.q") +
  labs(title = "Mamíferos: diversidade por tamanho de amostra")

ggiNEXT(resultado_parques_mammals, type = 1, facet.var = "Order.q") +
  labs(title = "Mamíferos: diversidade por tamanho de amostra (parques)")

ggiNEXT(resultado_mammals, type = 2, facet.var = "Order.q") +
  labs(title = "Mamíferos: cobertura por tamanho de amostra")

ggiNEXT(resultado_parques_mammals, type = 2, facet.var = "Order.q") +
  labs(title = "Mamíferos: cobertura por tamanho de amostra (parques)")

## --- Plot tipo 3: baseado em COBERTURA de amostra -- este é o que
##     compara diversidade no mesmo nível de cobertura, não no mesmo N bruto) ---
ggiNEXT(resultado_mammals, type = 3, facet.var = "Order.q") +
  labs(title = "Mamíferos: diversidade por cobertura de amostra")

ggiNEXT(resultado_parques_mammals, type = 3, facet.var = "Order.q") +
  labs(title = "Mamíferos: diversidade por cobertura de amostra (parques)")

resultado_mammals$AsyEst
estimateD(
  filtrar_dados_validos("mammals_"),  
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = 0.80
)

## --- Gráficos PLANTAS ---

ggiNEXT(resultado_plants, type = 1, facet.var = "Order.q") +
  labs(title = "Plantas: diversidade por tamanho de amostra")

ggiNEXT(resultado_parques_plants, type = 1, facet.var = "Order.q") +
  labs(title = "Plantas: diversidade por tamanho de amostra (parques)")

ggiNEXT(resultado_plants, type = 2, facet.var = "Order.q") +
  labs(title = "Plantas: cobertura por tamanho de amostra")

ggiNEXT(resultado_parques_plants, type = 2, facet.var = "Order.q") +
  labs(title = "Plantas: cobertura por tamanho de amostra (parques)")

## --- Plot tipo 3: baseado em COBERTURA de amostra -- este é o que
##     compara diversidade no mesmo nível de cobertura, não no mesmo N bruto) ---
ggiNEXT(resultado_plants, type = 3, facet.var = "Order.q") +
  labs(title = "Plantas: diversidade por cobertura de amostra")

ggiNEXT(resultado_parques_plants, type = 3, facet.var = "Order.q") +
  labs(title = "Plantas: diversidade por cobertura de amostra (parques)")

resultado_plants$AsyEst
estimateD(
  filtrar_dados_validos("plants_"),  
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = 0.80
)