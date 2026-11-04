#################################################################
######## 06 — Listas de espécies por site + frugívoros ##########
#################################################################
# Objetivo: (1) tabela espécie x site; (2) classificar quem é
# frugívoro; (3) refazer a taxa de match com o ATLANTIC-frugivory
# usando o denominador certo (só frugívoros), pra saber se a lacuna
# de dados é real ou é efeito de animais não-frugívoros no denominador.
#
# Saídas: outputs/presenca_site.rds, outputs/frugivoros.rds,
#         outputs/interacoes_mistas.rds, outputs/match_report.csv

source("R/funcoes_meridian.R")

com <-  read_csv("data/comunidades_clean.csv", show_col_types = FALSE,
                 col_select = c(group, class, order, family, species, taxonRank,
                                location_status, within_forest,
                                species_padronizado, genero_padronizado)) %>%
  filter(!is.na(species_padronizado)) %>%
  mutate(nome   = binomio(species_padronizado),
         genero = coalesce(genero_padronizado, genero(nome))) %>%
  filter(str_detect(nome, "^[A-Z][a-z]+ [a-z-]+$"))

# Sinônimos manuais entre o backbone do GBIF (comunidades) e a
# taxonomia usada no ATLANTIC-frugivory. Lista curta, só os que
# importam pras espécies-candidatas e frugívoros grandes. 
sinonimos_af <- tribble(
  ~nome_af,                  ~nome,
  "Aburria jacutinga",       "Pipile jacutinga",
  "Alouatta clamitans",      "Alouatta guariba",
  "Cebus nigritus",          "Sapajus nigritus"
)
af  <- read_delim("data/ATLANTIC_frugivory.csv", delim = ";", locale = locale(encoding = "latin1"),
             col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    filter(!is.na(Frugivore_Species), !is.na(Plant_Species),
           !Frugivore_Species %in% c("", "Animal-oriented", "Plant-oriented", "Network study")) %>%
    mutate(across(c(Frugivore_Species, Plant_Species, Plant_genus), str_squish),
           across(c(fruit_diameter, fruit_length, seed_diameter, seed_length,
                    Frug_Body_Mass, Frug_Mean_Gape_Size), num_seguro)) %>%
    left_join(sinonimos_af, by = c("Frugivore_Species" = "nome_af")) %>%
    mutate(frug   = coalesce(nome, Frugivore_Species),
           planta_genero = genero(Plant_Species)) %>%
    select(-nome)

int <- af %>%
  group_by(frug, planta_genero) %>%
  summarise(n_registros = n(),
            n_estudos   = n_distinct(`Study reference`),
            .groups = "drop")

# ------------------------------------------------------------
# 1. Presença por site
# ------------------------------------------------------------
presenca_site <- imap_dfr(sites, function(locs, nome_site) {
  com %>%
    filter(location_status %in% locs) %>%
    distinct(grupo = group, class, nome, genero) %>%
    mutate(site = nome_site)
})
# saveRDS(presenca_site, "outputs/presenca_site.rds")

# ------------------------------------------------------------
# 2. Quem é frugívoro?
# ------------------------------------------------------------
# Três fontes:
#  - EltonTraits Diet.Fruit (% da dieta) — tem erros conhecidos
#    (ex.: Tapirus terrestris e Cerdocyon thous aparecem com 0% de fruto)
#  - AVONET Trophic.Niche == "Frugivore" (nicho principal; perde
#    onívoros que comem muito fruto)
#  - presença no ATLANTIC-frugivory (evidência direta de interação)
# Critério: frugívoro se qualquer uma das três indicar.

elton <- bind_rows(
  read_tsv("data/Elton Traits/BirdFuncDat.txt", locale = locale(encoding = "latin1"),
           show_col_types = FALSE, col_select = c(Scientific, `Diet-Fruit`, `BodyMass-Value`)),
  read_tsv("data/Elton Traits/MamFuncDat.txt", locale = locale(encoding = "latin1"),
           show_col_types = FALSE, col_select = c(Scientific, `Diet-Fruit`, `BodyMass-Value`))
) %>%
  transmute(nome = str_squish(Scientific), dieta_fruto = `Diet-Fruit`, massa_elton = `BodyMass-Value`) %>%
  distinct(nome, .keep_all = TRUE)

avonet <- read_csv("data/AVONET/ELEData/ELEData/TraitData/AVONET1_BirdLife.csv",
                   show_col_types = FALSE,
                   col_select = c(Species1, Beak.Width, Mass, `Hand-Wing.Index`,
                                  Habitat, Habitat.Density, Trophic.Niche)) %>%
  rename(nome = Species1)

animais <- presenca_site %>%
  filter(grupo %in% c("birds", "mammals")) %>%
  distinct(grupo, class, nome, genero) %>%
  distinct(nome, .keep_all = TRUE)

frugivoros <- animais %>%
  left_join(elton, by = "nome") %>%
  left_join(avonet %>% select(nome, Trophic.Niche, Beak.Width, Mass, `Hand-Wing.Index`,
                              Habitat, Habitat.Density), by = "nome") %>%
  mutate(no_atfrug = nome %in% int$frug,
         frugivoro = (dieta_fruto >= 10) %in% TRUE |
                     (Trophic.Niche == "Frugivore") %in% TRUE |
                     no_atfrug,
         massa = coalesce(Mass, massa_elton))
# saveRDS(frugivoros, "outputs/frugivoros.rds")
# saveRDS(int, "outputs/interacoes_mistas.rds")

# ------------------------------------------------------------
# 3. Taxa de match com denominador correto
# ------------------------------------------------------------
# Calculo feito para entender qual a proporcao de frugivoros nos
# nossos dados que tambem esta presente no Atlantic Frugivory.
# Porem, como a lista dos frugivoros nos nossos dados tambem usa 
# presenca no AtFrug para fazer essa determinacao, fiz um outro 
# calculo retirando essa informacao
match_report <- bind_rows(
  frugivoros %>% summarise(conjunto = "todos os animais",
                           n = n(), no_atfrug = sum(no_atfrug)),
  frugivoros %>% filter((dieta_fruto >= 10) %in% TRUE) %>%
    summarise(conjunto = "Elton Diet.Fruit >= 10%", n = n(), no_atfrug = sum(no_atfrug)),
  frugivoros %>% filter((dieta_fruto >= 50) %in% TRUE) %>%
    summarise(conjunto = "Elton Diet.Fruit >= 50%", n = n(), no_atfrug = sum(no_atfrug)),
  frugivoros %>% filter((dieta_fruto == 0) %in% TRUE) %>%
    summarise(conjunto = "Elton Diet.Fruit = 0 (não-frugívoros)", n = n(), no_atfrug = sum(no_atfrug))
) %>%
  mutate(prop = round(no_atfrug / n, 2))
#^mostra que, das especies que parecem com >50% de frutos na dieta no Elton,
# e que estao nos nossos dados, 72% estao no Frugivory. 

# Plantas: sem síndrome de dispersão não dá pra separar o denominador
# (i.e., plants que sao zoocoricas mas nao estao no AtFrug).
# Taxa por site e a nível de gênero, pra referência.
plantas_match <- presenca_site %>%
  filter(grupo == "plants") %>%
  group_by(site) %>%
  summarise(n_generos = n_distinct(genero),
            generos_no_atfrug = n_distinct(genero[genero %in% int$planta_genero]),
            .groups = "drop") %>%
  mutate(prop = round(generos_no_atfrug / n_generos, 2))

print(match_report)
print(plantas_match)
write_csv(match_report, "outputs/match_report_animais.csv")
write_csv(plantas_match, "outputs/match_report_plantas.csv")

# Frugívoros que NÃO estão no ATLANTIC-frugivory, ordenados por % de
# fruto na dieta — esses são os candidatos à imputação por atributos
# (script 10) ou a busca manual de interações na literatura.
frugivoros %>%
  filter(frugivoro, !no_atfrug) %>%
  arrange(desc(dieta_fruto)) %>%
  select(class, nome, dieta_fruto, Trophic.Niche, massa)

# Plantas que nao estao no Atlantic Frugivory

fam <- com %>% filter(group == "plants") %>%
  distinct(nome, family) %>% group_by(nome) %>% slice(1) %>% ungroup()
plantas <- presenca_site %>% filter(grupo == "plants") %>%
  group_by(nome, genero) %>%
  summarise(n_sites = n_distinct(site),
            em_ucs = any(site %in% c("inside_pnt","inside_mendanha","inside_pedrabranca","inside_ucs")),
            .groups = "drop") %>%
  left_join(fam, by = "nome")

# o que o ATLANTIC-frugivory tem, em cada nível
af_sp  <- af %>% mutate(nome = binomio(Plant_Species)) %>% count(nome, name = "n_reg_especie")
af_gen <- af %>% count(planta_genero, name = "n_reg_genero")
af_fam <- af %>% count(Plant_family, name = "n_reg_familia")

plantas <- plantas %>%
  left_join(af_sp,  by = "nome") %>%
  left_join(af_gen, by = c("genero" = "planta_genero")) %>%
  left_join(af_fam, by = c("family" = "Plant_family")) %>%
  mutate(across(starts_with("n_reg"), ~ replace_na(.x, 0L)),
         no_af_especie = n_reg_especie > 0,
         no_af_genero  = n_reg_genero  > 0,
         no_af_familia = n_reg_familia > 0)

# ---- Relatório de match (como o dos animais) ----
match_plantas <- bind_rows(
  plantas %>% summarise(nivel = "espécie", n = n(), no_atfrug = sum(no_af_especie)),
  plantas %>% summarise(nivel = "gênero",  n = n(), no_atfrug = sum(no_af_genero)),
  plantas %>% summarise(nivel = "família", n = n(), no_atfrug = sum(no_af_familia))) %>%
  mutate(prop = round(no_atfrug / n, 2))
print(match_plantas)

# ---- ESPÉCIES sem registro no ATLANTIC-frugivory ----
# ordem: gênero com registro (dá pra inferir) > família bem representada
# (proxy de zoocoria) > ocorre em mais sites
sem_especie <- plantas %>% filter(!no_af_especie) %>%
  arrange(desc(no_af_genero), desc(n_reg_familia), desc(n_sites)) %>%
  select(family, genero, nome, n_sites, em_ucs, n_reg_genero, n_reg_familia)
# write_csv(sem_especie, "outputs/plantas_sem_atfrug_especie.csv")

# ---- GÊNEROS sem registro, por família ----
sem_genero <- plantas %>% filter(!no_af_genero) %>%
  group_by(family, genero) %>%
  summarise(n_especies = n(), n_sites = max(n_sites), em_ucs = any(em_ucs),
            n_reg_familia = max(n_reg_familia), .groups = "drop") %>%
  arrange(desc(n_reg_familia), desc(n_especies))
