#---------- Rede de frugivoria --------------

# ============================================================
# DADOS
# ============================================================

comunidades <- read.csv2("data/20260723_combined_data.csv")
str(comunidades)

at_frug <- read.csv2("data/ATLANTIC_frugivory.csv", fileEncoding = "Latin1",
                     stringsAsFactors = FALSE)
str(at_frug)

avonet1 <- read.csv("data/AVONET/ELEData/ELEData/TraitData/AVONET1_BirdLife.csv")
avonet3 <- read.csv2("data/AVONET/ELEData/ELEData/TraitData/AVONET3_BirdTree.csv")
head(avonet1)
head(avonet3)

elton <- read.csv("data/Elton Traits/MamFuncDat.csv")
head(elton)

# ============================================================
# LIMPANDO
# ============================================================

## --- Comunidades: padronizar nomes de espécie ---

especies_animais_com <- comunidades %>%
  filter(group %in% c("birds", "mammals"), taxonRank %in% c("SPECIES", "SUBSPECIES")) %>%
  distinct(species) %>%
  pull(species)
# ^ só espécie/subespécie identificada -- nome de gênero solto não tem o que padronizar

backbone_animais_com <- name_backbone_checklist(
  especies_animais_com,
  bucket_size = 50,   
  sleep = 3            
)
# ^ consulta em lote no GBIF Backbone; devolve o nome aceito atual pra cada nome de entrada

de_para_animais_com <- backbone_animais_com %>%
  select(species_original = verbatim_name, species_padronizado = species) %>%
  distinct(species_original, .keep_all = TRUE)
# ^ tabela de-para, casada pelo nome 

especies_plantas_com <- comunidades %>%
  filter(
    group == "plants",
    !is.na(species), species != "",
    str_detect(species, "^[A-Z][a-z]+ [a-z-]+$")  # exige "Gênero espécie", exclui só-gênero, "sp.", etc.
  ) %>% # isso pq plantas nao tem informacao em taxonRank
  distinct(species) %>%
  pull(species)

flora_com <- get.taxa(especies_plantas_com, domain = TRUE)
# ^ mesma ideia, mas com outra base de dados

de_para_plantas_com <- flora_com %>%
  select(species_original = original.search, species_padronizado = scientific.name) %>%
  distinct(species_original, .keep_all = TRUE)

de_para_com <- bind_rows(de_para_animais_com, de_para_plantas_com)
comunidades <- comunidades %>%
  left_join(de_para_com, by = c("species" = "species_original")) %>%
  mutate(genero_padronizado = word(species_padronizado, 1))
# ^ gênero padronizado = primeira palavra do nome de espécie já padronizado

### Atlantic frugivory:

at_frug %>%
  mutate(row_id = row_number()) %>%
  filter(Frugivore_Species %in% c("", "Animal-oriented", "Plant-oriented", "Network study")) %>%
  select(row_id, ID, Frugivore_Species, Study_Method)

at_frug <- at_frug %>%
  filter(Frugivore_Species != "", Plant_Species != "") %>% # remove linhas problematicas
  mutate(across(c(Frugivore_Species, Plant_Species, Frug_Genus, Plant_genus),
                str_squish)) # limpa espacoes duplos/soltos

especies_frug_atfrug <- at_frug %>% distinct(Frugivore_Species) %>% pull()
backbone_frug_atfrug <- name_backbone_checklist(
  especies_frug_atfrug,
  bucket_size = 50,   
  sleep = 3)

especies_plant_atfrug <- at_frug %>% distinct(Plant_Species) %>% pull()
flora_atfrug <- get.taxa(especies_plant_atfrug, domain = TRUE)

de_para_plant_atfrug <- flora_atfrug %>%
  select(Plant_Species = original.search, Plant_Species_padronizado = scientific.name) %>%
  distinct(Plant_Species, .keep_all = TRUE)  

de_para_frug_atfrug <- backbone_frug_atfrug %>%
  select(Frugivore_Species = verbatim_name, Frugivore_Species_padronizado = species) %>%
  distinct(Frugivore_Species, .keep_all = TRUE)

at_frug <- at_frug %>%
  left_join(de_para_plant_atfrug, by = "Plant_Species") %>%
  left_join(de_para_frug_atfrug, by = "Frugivore_Species") %>%
  mutate(
    Frug_Genus_padronizado  = word(Frugivore_Species_padronizado, 1),
    Plant_genus_padronizado = word(Plant_Species_padronizado, 1)
  )

table(backbone_frug_atfrug$matchType, useNA = "ifany")
# ^ conferência: quase tudo deve ser EXACT; poucos HIGHERRANK/VARIANT são esperados

especies_elton <- elton %>% distinct(Scientific) %>% pull()

lote_tamanho <- 50
blocos <- split(especies_elton, ceiling(seq_along(especies_elton) / lote_tamanho))

resultados_elton <- vector("list", length(blocos))
for (i in seq_along(blocos)) {
  message("Processando bloco ", i, " de ", length(blocos))
  resultados_elton[[i]] <- name_backbone_checklist(blocos[[i]], bucket_size = 50, sleep = 3)
  Sys.sleep(2)
}

backbone_elton <- bind_rows(resultados_elton)

de_para_elton <- backbone_elton %>%
  select(Scientific = verbatim_name, Scientific_padronizado = species) %>%
  distinct(Scientific, .keep_all = TRUE)

elton <- elton %>%
  left_join(de_para_elton, by = "Scientific")

# ============================================================
# MATCH -- teste real de quanto a limpeza/padronização ajudou
# ============================================================

## --- Espécie ---

frug_sp_comunidades <- comunidades %>%
  filter(group %in% c("birds", "mammals"), !is.na(species_padronizado)) %>%
  distinct(species_padronizado) %>%
  pull(species_padronizado)

plant_sp_comunidades <- comunidades %>%
  filter(group == "plants", !is.na(species_padronizado)) %>%
  distinct(species_padronizado) %>%
  pull(species_padronizado)

frug_sp_atfrug  <- unique(at_frug$Frugivore_Species_padronizado) %>% na.omit()
plant_sp_atfrug <- unique(at_frug$Plant_Species_padronizado) %>% na.omit()

n_match_frug  <- length(intersect(frug_sp_comunidades, frug_sp_atfrug)) # 223
n_match_plant <- length(intersect(plant_sp_comunidades, plant_sp_atfrug)) # 377

n_match_frug  / length(frug_sp_comunidades) # 0.31 (era 0.28 antes de padronizar)
n_match_plant / length(plant_sp_comunidades) # 0.12 (era 0.08 antes de padronizar)
# ^taxa de match baixa (31% frug-especie, 12% planta-especie)  
# Diferenca depois de padronizar mostra que a maior parte da lacuna é cobertura dos dados mesmo

# Match a nivel de genero:

genus_frug_comunidades  <- comunidades %>%
  filter(group %in% c("birds", "mammals"), !is.na(genero_padronizado)) %>%
  distinct(genero_padronizado) %>% pull()

genus_plant_comunidades <- comunidades %>%
  filter(group == "plants", !is.na(genero_padronizado)) %>%
  distinct(genero_padronizado) %>% pull()

genus_frug_atfrug  <- unique(at_frug$Frug_Genus_padronizado) %>% na.omit()
genus_plant_atfrug <- unique(at_frug$Plant_genus_padronizado) %>% na.omit()

n_match_genus_frug  <- length(intersect(genus_frug_comunidades, genus_frug_atfrug)) # 160
n_match_genus_plant <- length(intersect(genus_plant_comunidades, genus_plant_atfrug)) # 235

n_match_genus_frug  / length(genus_frug_comunidades) # 0.34
n_match_genus_plant / length(genus_plant_comunidades)# 0.21

animais_so_dentro <- setdiff(animais_dentro, animais_fora)
animais_so_fora   <- setdiff(animais_fora, animais_dentro)

plantas_so_dentro <- setdiff(plantas_dentro, plantas_fora)
plantas_so_fora   <- setdiff(plantas_fora, plantas_dentro)

tibble(
  categoria = c("animais_so_dentro", "animais_so_fora", "plantas_so_dentro", "plantas_so_fora"),
  n = c(length(animais_so_dentro), length(animais_so_fora),
        length(plantas_so_dentro), length(plantas_so_fora))
)

sort(animais_so_dentro)
sort(animais_so_fora)
sort(plantas_so_dentro)
sort(plantas_so_fora)

# ============================================================
# ATRIBUTOS -- massa corporal (AVONET + EltonTraits + at_frug)
# ============================================================

## --- AVONET (aves) ---
avonet_ref <- at_frug %>%
  distinct(Frugivore_Species_padronizado) %>%
  filter(!is.na(Frugivore_Species_padronizado)) %>%
  left_join(avonet1 %>% select(Species1, Beak.Width, Mass),
            by = c("Frugivore_Species_padronizado" = "Species1")) %>%
  left_join(avonet3 %>% select(Species3, Beak.Width3 = Beak.Width, Mass3 = Mass),
            by = c("Frugivore_Species_padronizado" = "Species3")) %>%
  mutate(Beak.Width = coalesce(Beak.Width, Beak.Width3),
         Mass = coalesce(Mass, Mass3)) %>%
  select(-Beak.Width3, -Mass3)

sinonimos_avonet <- tribble(
  ~Frugivore_Species_padronizado, ~nome_avonet,
  "Stilpnia cayana",               "Tangara cayana",
  "Stilpnia preciosa",              "Tangara preciosa",
  "Stilpnia peruviana",             "Tangara peruviana",
  "Ramphocelus bresilia",           "Ramphocelus bresilius",
  "Loriotus cristatus",             "Tachyphonus cristatus",
  "Rauenia bonariensis",            "Thraupis bonariensis"
)

avonet_extra <- sinonimos_avonet %>%
  left_join(avonet1 %>% select(Species1, Beak.Width, Mass), by = c("nome_avonet" = "Species1")) %>%
  left_join(avonet3 %>% select(Species3, Beak.Width3 = Beak.Width, Mass3 = Mass),
            by = c("nome_avonet" = "Species3")) %>%
  mutate(Beak.Width = coalesce(Beak.Width, Beak.Width3),
         Mass = coalesce(Mass, Mass3)) %>%
  select(Frugivore_Species_padronizado, Beak.Width, Mass)

avonet_ref <- avonet_ref %>%
  rows_update(avonet_extra, by = "Frugivore_Species_padronizado")

## --- EltonTraits (mamíferos) ---
elton_ref <- elton %>%
  filter(!is.na(Scientific_padronizado)) %>%
  distinct(Scientific_padronizado, .keep_all = TRUE) %>%
  select(Scientific_padronizado, BodyMass.Value, Diet.Fruit)

## --- Checagem pendente: Frug_Body_Mass de mamífero tem o mesmo
##     bug de ponto-milhar que já vimos em ave? ---
n_pontos <- function(x) lengths(regmatches(x, gregexpr("\\.", x)))

at_frug %>%
  filter(Frug_Class == "Mammalia", !is.na(Frug_Body_Mass)) %>%
  distinct(Frugivore_Species_padronizado, Frug_Body_Mass) %>%
  filter(n_pontos(as.character(Frug_Body_Mass)) > 1)
# ^ se vier vazio, pode seguir. Se vier linha, reconstrua ANTES do próximo bloco.

## --- Combinar as 3 fontes ---
frug_body_mass_atfrug <- at_frug %>%
  mutate(Frug_Body_Mass = suppressWarnings(as.numeric(Frug_Body_Mass))) %>%
  filter(!is.na(Frugivore_Species_padronizado), !is.na(Frug_Body_Mass)) %>%
  group_by(Frugivore_Species_padronizado) %>%
  summarise(Mass_atfrug = mean(Frug_Body_Mass, na.rm = TRUE), .groups = "drop")

massa_ref <- avonet_ref %>%
  select(species_padronizado = Frugivore_Species_padronizado, Mass_avonet = Mass) %>%
  full_join(elton_ref %>% select(species_padronizado = Scientific_padronizado, Mass_elton = BodyMass.Value),
            by = "species_padronizado") %>%
  full_join(frug_body_mass_atfrug %>% rename(species_padronizado = Frugivore_Species_padronizado),
            by = "species_padronizado") %>%
  mutate(
    Mass = coalesce(Mass_avonet, Mass_elton, Mass_atfrug),
    discrepancia_grande = pmap_lgl(list(Mass_avonet, Mass_elton, Mass_atfrug), function(a, e, f) {
      valores <- na.omit(c(a, e, f))
      if (length(valores) < 2) return(FALSE)
      (max(valores) / min(valores)) > 2
    })
  )

massa_ref %>% filter(discrepancia_grande)

## --- GRÁFICO ---

massa_por_site <- map_dfr(sites, function(locs) {
  comunidades %>%
    filter(location_status %in% locs, group %in% c("birds", "mammals"),
           !is.na(species_padronizado)) %>%
    distinct(species_padronizado, class) %>%
    left_join(massa_ref, by = "species_padronizado")
}, .id = "site")

ggplot(massa_por_site, aes(x = site, y = Mass)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  scale_y_log10() +
  labs(x = NULL, y = "Massa corporal (g, escala log)",
       title = "Distribuição de massa corporal dos frugívoros por comunidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

## --- Conferir se a conversão pra número perde algum valor esquisito,
##     antes de confiar -- mesmo cuidado que tivemos com Frug_Body_Mass ---
at_frug %>%
  select(fruit_diameter, fruit_length, seed_diameter, seed_length) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(convertido = suppressWarnings(as.numeric(valor))) %>%
  filter(is.na(convertido)) %>%
  distinct(variavel, valor)
# ^ se vier vazio, segue tranquila pro bloco de baixo

traits_fruto_semente <- at_frug %>%
  filter(!is.na(Plant_genus_padronizado)) %>%
  mutate(across(c(fruit_diameter, fruit_length, seed_diameter, seed_length),
                ~ suppressWarnings(as.numeric(.x)))) %>%
  group_by(Plant_genus_padronizado) %>%
  summarise(
    fruit_diameter_medio = mean(fruit_diameter, na.rm = TRUE),
    fruit_length_medio   = mean(fruit_length, na.rm = TRUE),
    seed_diameter_medio  = mean(seed_diameter, na.rm = TRUE),
    seed_length_medio    = mean(seed_length, na.rm = TRUE),
    .groups = "drop"
  )

## --- Juntar com presença por comunidade ---
traits_por_site <- map_dfr(sites, function(locs) {
  comunidades %>%
    filter(location_status %in% locs, group == "plants", !is.na(genero_padronizado)) %>%
    distinct(genero_padronizado) %>%
    left_join(traits_fruto_semente, by = c("genero_padronizado" = "Plant_genus_padronizado"))
}, .id = "site")

## --- Plot: fruto e semente lado a lado, cada um com diâmetro e comprimento ---
traits_por_site %>%
  select(site, fruit_diameter_medio, fruit_length_medio,
         seed_diameter_medio, seed_length_medio) %>%
  pivot_longer(-site, names_to = "medida", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(
    parte = if_else(str_starts(medida, "fruit"), "Fruto", "Semente"),
    dimensao = if_else(str_detect(medida, "diameter"), "Diâmetro (mm)", "Comprimento (mm)")
  ) %>%
  ggplot(aes(x = site, y = valor)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  facet_grid(dimensao ~ parte, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Tamanho de fruto e semente por comunidade") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

## --- Agregar por espécie (em vez de gênero) ---
traits_fruto_semente_especie <- at_frug %>%
  filter(!is.na(Plant_Species_padronizado)) %>%
  mutate(across(c(fruit_diameter, fruit_length, seed_diameter, seed_length),
                ~ suppressWarnings(as.numeric(.x)))) %>%
  group_by(Plant_Species_padronizado) %>%
  summarise(
    fruit_diameter_medio = mean(fruit_diameter, na.rm = TRUE),
    fruit_length_medio   = mean(fruit_length, na.rm = TRUE),
    seed_diameter_medio  = mean(seed_diameter, na.rm = TRUE),
    seed_length_medio    = mean(seed_length, na.rm = TRUE),
    .groups = "drop"
  )

## --- Juntar com presença por comunidade (species_padronizado, não genero) ---
traits_por_site_especie <- map_dfr(sites, function(locs) {
  comunidades %>%
    filter(location_status %in% locs, group == "plants", !is.na(species_padronizado)) %>%
    distinct(species_padronizado) %>%
    left_join(traits_fruto_semente_especie, by = c("species_padronizado" = "Plant_Species_padronizado"))
}, .id = "site")

## --- Conferir N antes de plotar -- essencial dado o match baixo esperado ---
traits_por_site_especie %>%
  group_by(site) %>%
  summarise(
    n_especies = n(),
    n_com_fruto = sum(!is.na(fruit_diameter_medio)),
    n_com_semente = sum(!is.na(seed_diameter_medio))
  )

## --- Plot ---
traits_por_site_especie %>%
  select(site, fruit_diameter_medio, fruit_length_medio,
         seed_diameter_medio, seed_length_medio) %>%
  pivot_longer(-site, names_to = "medida", values_to = "valor") %>%
  filter(!is.na(valor)) %>%
  mutate(
    parte = if_else(str_starts(medida, "fruit"), "Fruto", "Semente"),
    dimensao = if_else(str_detect(medida, "diameter"), "Diâmetro (mm)", "Comprimento (mm)")
  ) %>%
  ggplot(aes(x = site, y = valor)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1) +
  facet_grid(dimensao ~ parte, scales = "free_y") +
  labs(x = NULL, y = NULL,
       title = "Tamanho de fruto e semente por comunidade (nível espécie)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

# ============================================================
# REDES
# ============================================================

## --- Data frames ---

### Genero-genero
interacoes_genero <- at_frug %>%
  filter(!is.na(Frug_Genus_padronizado), !is.na(Plant_genus_padronizado)) %>%
  distinct(Frug_Genus_padronizado, Plant_genus_padronizado)

### Genero planta - especie frugivoro
interacoes_mista <- at_frug %>% 
  filter(!is.na(Frugivore_Species_padronizado), !is.na(Plant_genus_padronizado)) %>%
  distinct(Frugivore_Species_padronizado, Plant_genus_padronizado)

sites <- list(
  outside_ucs        = "outside_ucs",
  inside_ucs         = c("inside_mendanha", "inside_pedrabranca", "inside_pnt"),
  inside_mendanha    = "inside_mendanha",
  inside_pedrabranca = "inside_pedrabranca",
  inside_pnt         = "inside_pnt"
)


### Funcao para pegar quem está presente no site e filtrar a 
#     listra mestra por co-ocorrencia
construir_rede_genero <- function(locs) {
  frug_genero_site <- comunidades %>%
    filter(location_status %in% locs, group %in% c("birds", "mammals"),
           !is.na(genero_padronizado)) %>%
    distinct(genero_padronizado) %>% pull()
  
  plant_genero_site <- comunidades %>%
    filter(location_status %in% locs, group == "plants",
           !is.na(genero_padronizado)) %>%
    distinct(genero_padronizado) %>% pull()
  
  interacoes_genero %>%
    filter(Frug_Genus_padronizado %in% frug_genero_site,
           Plant_genus_padronizado %in% plant_genero_site)
}

construir_rede_mista <- function(locs) {
  frug_especie_site <- comunidades %>%
    filter(location_status %in% locs, group %in% c("birds", "mammals"),
           !is.na(species_padronizado)) %>%
    distinct(species_padronizado) %>% pull()
  
  plant_genero_site <- comunidades %>%
    filter(location_status %in% locs, group == "plants",
           !is.na(genero_padronizado)) %>%
    distinct(genero_padronizado) %>% pull()
  
  interacoes_mista %>%
    filter(Frugivore_Species_padronizado %in% frug_especie_site,
           Plant_genus_padronizado %in% plant_genero_site)
}

redes_genero <- map(sites, construir_rede_genero)
redes_mista  <- map(sites, construir_rede_mista)

names(redes_genero) <- paste0("genero_", names(redes_genero))
names(redes_mista)  <- paste0("mista_", names(redes_mista))

redes <- c(redes_genero, redes_mista)

map_dfr(redes, ~tibble(n_interacoes = nrow(.x)), .id = "rede")

## --- Analises ---

### Funcao pra converter as listas em matrizes
construir_matriz <- function(rede, col_frug, col_plant) {
  rede %>%
    mutate(interacao = 1) %>%
    pivot_wider(names_from = {{col_plant}}, values_from = interacao, values_fill = 0) %>%
    column_to_rownames(var = as_label(enquo(col_frug))) %>%
    as.matrix()
}

matrizes_genero <- map(redes_genero, ~construir_matriz(.x, Frug_Genus_padronizado, Plant_genus_padronizado))
matrizes_mista  <- map(redes_mista,  ~construir_matriz(.x, Frugivore_Species_padronizado, Plant_genus_padronizado))
matrizes <- c(matrizes_genero, matrizes_mista)

### Metricas de rede:

#   Conectância e aninhamento (NODF) são conhecidamente sensíveis ao
#   tamanho da matriz: redes com poucas espécies tendem a parecer mais
#   conectadas/aninhadas só por serem pequenas, mesmo sem nenhuma
#   diferença ecológica real de estrutura em relação a uma rede maior.
#   Como as nossas 10 redes têm tamanhos bem diferentes (de ~680 a
#   ~2440 interações, com Mendanha/Pedra Branca muito menores que
#   outside_ucs).

#   Pra separar as duas coisas, cada métrica é comparada contra uma
#   distribuição nula: embaralha-se a matriz observada centenas de
#   vezes (mantendo os totais de linha/coluna, i.e., cada espécie
#   continua com o mesmo número de interações que já tinha, só
#   redistribuídas ao acaso) e recalcula-se a métrica em cada versão
#   embaralhada. O z-score resultante ((observado - média dos nulos) /
#   desvio-padrão dos nulos) diz quantos desvios-padrão o valor real
#   está distante do que se esperaria só pelo tamanho/grau da rede —
#   esse número é o que de fato pode ser comparado entre redes de
#   tamanhos diferentes, não o valor bruto.

calcular_zscore <- function(m, index, n_sim = 100) {
  obs <- networklevel(m, index = index)
  nulos <- nullmodel(m, N = n_sim, method = "r2dtable")  # mantem totais de linha/coluna
  sim <- sapply(nulos, function(x) networklevel(x, index = index))
  (obs - mean(sim)) / sd(sim)
}

metricas <- map_dfr(matrizes, function(m) {
  tibble(
    n_frug        = nrow(m),
    n_plant       = ncol(m),
    n_interacoes  = sum(m),
    conectancia         = networklevel(m, index = "connectance"),
    conectancia_zscore  = calcular_zscore(m, "connectance"),
    nestedness          = networklevel(m, index = "NODF"),
    nestedness_zscore   = calcular_zscore(m, "NODF"),
    modularidade  = computeModules(m)@likelihood
    # H2 removido -- algoritmo de busca do H2max falha em matrizes esparsas
  )
}, .id = "rede")

robustez <- map_dfr(matrizes, function(m) {
  tibble(
    robustez_frug  = robustness(second.extinct(m, participant = "higher", method = "degree")),
    robustez_plant = robustness(second.extinct(m, participant = "lower", method = "degree"))
  )
}, .id = "rede")

metricas <- metricas %>% left_join(robustez, by = "rede")

centralidade <- map(matrizes, ~specieslevel(.x, index = c("normalised degree", "species strength")))

## --- Gráficos ---

metricas_plot <- metricas %>%
  mutate(
    resolucao = if_else(str_starts(rede, "genero_"), "gênero-gênero", "mista"),
    site = str_remove(rede, "^(genero_|mista_)"),
    site = factor(site, levels = c("outside_ucs", "inside_ucs",
                                   "inside_mendanha", "inside_pedrabranca", "inside_pnt"))
  )

### Conectancia e aninhamento (nestedness):
#     Conectancia: proporção de interações realizadas sobre o total possível.
#     Aninhamento: pra saber se a rede tem uma estrutura onde 
#                   generalistas interagem com a maioria e especialistas 
#                   interagindo só com o que os generalistas também usam. 
#                   Padrão gerlamente encontrado em redes mutualísticas.
metricas_plot %>%
  select(site, resolucao, conectancia_zscore, nestedness_zscore) %>%
  pivot_longer(c(conectancia_zscore, nestedness_zscore), names_to = "metrica", values_to = "zscore") %>%
  ggplot(aes(x = site, y = zscore, fill = resolucao)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  facet_wrap(~metrica, scales = "free_y") +
  labs(x = NULL, y = "z-score (desvio do modelo nulo)",
       title = "Estrutura das redes: quanto cada uma se desvia do esperado ao acaso") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

### Modularidade e especialização (H2) -- já normalizadas, não precisam de z-score
#   Modularidade: oposto de aninhamento. Bom para ver se os modulos dentro de UCs
#                 sao diferentes dos foras das UCs
#   Especializacao: quao especializadas sao as interacoes, de maneira geral, nas redes 
metricas_plot %>%
  ggplot(aes(x = site, y = modularidade, fill = resolucao)) +
  geom_col(position = "dodge") +
  labs(x = NULL, y = NULL, title = "Modularidade por rede") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

### Robustez
metricas_plot %>%
  select(site, resolucao, robustez_frug, robustez_plant) %>%
  pivot_longer(c(robustez_frug, robustez_plant), names_to = "grupo", values_to = "robustez") %>%
  ggplot(aes(x = site, y = robustez, fill = resolucao)) +
  geom_col(position = "dodge") +
  facet_wrap(~grupo) +
  labs(x = NULL, y = "Robustez (área sob a curva de extinção)",
       title = "Robustez à perda de espécies mais conectadas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

curvas_extincao <- imap_dfr(matrizes, function(m, nome) {
  ext <- second.extinct(m, participant = "higher", method = "degree")
  n_plantas_total <- nrow(m)  # total de espécies de planta ANTES de qualquer extinção
  tibble(
    rede = nome,
    prop_frug_removidos = ext[, "no"] / max(ext[, "no"]),
    prop_plant_sobrevivendo = 1 - cumsum(ext[, "ext.lower"]) / n_plantas_total
  )
})

curvas_extincao %>%
  mutate(
    resolucao = if_else(str_starts(rede, "genero_"), "gênero-gênero", "mista"),
    site = str_remove(rede, "^(genero_|mista_)")
  ) %>%
  ggplot(aes(x = prop_frug_removidos, y = prop_plant_sobrevivendo, color = site)) +
  geom_line(linewidth = 1) +
  facet_wrap(~resolucao) +
  labs(x = "Proporção de frugívoros removidos (do mais pro menos conectado)",
       y = "Proporção de plantas ainda com pelo menos 1 interação",
       title = "Curvas de extinção secundária") +
  theme_minimal()

### Redes em si:

rede_completa <- bind_rows(
  redes_mista$mista_inside_ucs,
  redes_mista$mista_outside_ucs
) %>%
  distinct(Frugivore_Species_padronizado, Plant_genus_padronizado)

rede_escolhida <- redes_mista$mista_inside_pnt

animais_dentro <- unique(redes_mista$mista_inside_ucs$Frugivore_Species_padronizado)
animais_fora   <- unique(redes_mista$mista_outside_ucs$Frugivore_Species_padronizado)

plantas_dentro <- unique(redes_mista$mista_inside_ucs$Plant_genus_padronizado)
plantas_fora   <- unique(redes_mista$mista_outside_ucs$Plant_genus_padronizado)

classificar <- function(nome, so_dentro, so_fora) {
  case_when(
    nome %in% so_dentro & nome %in% so_fora ~ "comum",
    nome %in% so_dentro                      ~ "so_dentro",
    nome %in% so_fora                        ~ "so_fora",
    TRUE ~ NA_character_
  )
}

status_animais <- tibble(nome = union(animais_dentro, animais_fora)) %>%
  mutate(status = classificar(nome, animais_dentro, animais_fora), tipo = "animal")

status_plantas <- tibble(nome = union(plantas_dentro, plantas_fora)) %>%
  mutate(status = classificar(nome, plantas_dentro, plantas_fora), tipo = "planta")

status_nos <- bind_rows(status_animais, status_plantas)

grafo_completo <- graph_from_data_frame(rede_completa, directed = FALSE)

V(grafo_completo)$status <- status_nos$status[match(V(grafo_completo)$name, status_nos$nome)]
V(grafo_completo)$tipo   <- status_nos$tipo[match(V(grafo_completo)$name, status_nos$nome)]

ggraph(grafo_completo, layout = "fr") +
  geom_edge_link(alpha = 0.15, color = "grey60") +
  geom_node_point(aes(color = status, shape = tipo), size = 3) +
  scale_color_manual(values = c(
    "comum"     = "grey30",
    "so_dentro" = "#1b9e77",
    "so_fora"   = "#d95f02"
  )) +
  scale_shape_manual(values = c("animal" = 16, "planta" = 15)) +
  theme_void() +
  labs(title = "Rede combinada: espécies comuns vs. exclusivas de dentro/fora de UC",
       color = "Status", shape = "Tipo")

especies_so_dentro <- setdiff(
  unique(rede_escolhida$Frugivore_Species_padronizado),
  unique(redes_mista$mista_outside_ucs$Frugivore_Species_padronizado)
)

grafo <- graph_from_data_frame(
  rede_escolhida %>% select(Frugivore_Species_padronizado, Plant_genus_padronizado),
  directed = FALSE
)
V(grafo)$tipo <- case_when(
  V(grafo)$name %in% rede_escolhida$Plant_genus_padronizado ~ "planta",
  V(grafo)$name %in% especies_so_dentro ~ "exclusivo_dentro",
  TRUE ~ "frugivoro_comum"
)
E(grafo)$destaque <- apply(as_edgelist(grafo), 1, function(par) {
  any(par %in% especies_so_dentro)
})
ggraph(grafo, layout = "fr") +  # fr = Fruchterman-Reingold, mesmo estilo do artigo
  geom_edge_link(aes(color = destaque, alpha = destaque), show.legend = FALSE) +
  scale_edge_color_manual(values = c("TRUE" = "#e63946", "FALSE" = "grey80")) +
  scale_edge_alpha_manual(values = c("TRUE" = 0.6, "FALSE" = 0.25)) +
  geom_node_point(aes(color = tipo, size = tipo)) +
  scale_color_manual(values = c(
    "planta" = "#2c7a4b",
    "frugivoro_comum" = "grey40",
    "exclusivo_dentro" = "#e63946"
  )) +
  scale_size_manual(values = c(
    "planta" = 2, "frugivoro_comum" = 2, "exclusivo_dentro" = 4
  )) +
  theme_void() +
  labs(title = "Espécies exclusivas de dentro de UC e suas conexões (inside_pnt)")
