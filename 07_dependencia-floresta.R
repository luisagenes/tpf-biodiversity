#################################################################
######## 07 — Dependência de floresta (IUCN + AVONET) ###########
#################################################################
# Pergunta: quais espécies registradas no Rio dependem de floresta
# (e portanto, plausivelmente, das UCs) e não persistiriam num
# sistema só urbano?
#
# Fonte principal: habitats da IUCN Red List (API v4). Precisa de um
# token gratuito: https://api.iucnredlist.org/users/sign_up
# Guarde no .Renviron como IUCN_REDLIST_KEY=xxxx (não commitar!).
#
# !!! NÃO TESTADO contra a API (não tenho token aqui). A estrutura do
# JSON de habitats foi escrita de forma defensiva; rode primeiro com
# 2-3 espécies (TESTE <- TRUE) e confira o objeto `hab_teste`.
#
# Esquema de habitats da IUCN (v3.1), os códigos que importam:
#   1.x   Forest (1.5 dry, 1.6 moist lowland, 1.9 moist montane...)
#   14.x  Artificial/Terrestrial: 14.1 arable, 14.2 pasture,
#         14.3 plantations, 14.4 rural gardens, 14.5 URBAN AREAS,
#         14.6 heavily degraded former forest
# Não existe "floresta secundária" como classe; o mais próximo é 14.6.
#
# Classificação proposta (discutir com o grupo):
#   "dependente_floresta" = todos os habitats "Suitable" são 1.x
#   "tolerante_antropico" = algum habitat "Suitable" em 14.4/14.5
#   "intermediaria"       = o resto (usa campo, arbusto, 14.6 etc.)
#
# Saída: outputs/dependencia_floresta.csv

source("R/funcoes_meridian.R")
suppressPackageStartupMessages({ library(httr2); library(jsonlite) })

TESTE <- TRUE   # mude pra FALSE pra rodar todas as espécies
token <- Sys.getenv("IUCN_REDLIST_KEY")
base  <- "https://api.iucnredlist.org/api/v4"
cache <- "outputs/iucn_cache"; dir.create(cache, showWarnings = FALSE, recursive = TRUE)

iucn_get <- function(caminho, query = list()) {
  request(paste0(base, caminho)) |>
    req_headers(Authorization = token) |>
    req_url_query(!!!query) |>
    req_retry(max_tries = 3, backoff = ~ 5) |>
    req_throttle(rate = 1) |>              # 1 requisição/seg, pra não ser bloqueada
    req_error(is_error = function(resp) FALSE) |>
    req_perform()
}

# Uma espécie -> tabela de habitats da avaliação global mais recente.
# Cacheia o JSON em disco (rodar de novo não refaz a consulta).
habitats_iucn <- function(nome) {
  arq <- file.path(cache, paste0(str_replace_all(nome, " ", "_"), ".json"))
  if (!file.exists(arq)) {
    p <- str_split_1(nome, " ")
    r <- iucn_get("/taxa/scientific_name", list(genus_name = p[1], species_name = p[2]))
    if (resp_status(r) != 200) { write_json(list(erro = resp_status(r)), arq); return(NULL) }
    tx <- resp_body_json(r, simplifyVector = TRUE)
    av <- tx$assessments
    if (is.null(av) || NROW(av) == 0) { write_json(list(erro = "sem_avaliacao"), arq); return(NULL) }
    # avaliação mais recente; prefere escopo global (code "1") quando houver
    av <- as_tibble(av)
    if ("latest" %in% names(av)) av <- filter(av, latest %in% TRUE)
    if ("scopes" %in% names(av)) {
      glob <- map_lgl(av$scopes, ~ any(unlist(.x) %in% c("1", "Global")))
      if (any(glob)) av <- av[glob, ]
    }
    id <- av$assessment_id[1]
    r2 <- iucn_get(paste0("/assessment/", id))
    writeLines(resp_body_string(r2), arq)
  }
  js <- fromJSON(arq, simplifyVector = TRUE)
  if (!is.null(js$erro) || is.null(js$habitats) || NROW(js$habitats) == 0) return(NULL)
  h <- as_tibble(js$habitats)
  # nomes de campo podem variar; procuro por padrão
  tibble(
    codigo       = as.character(h[[grep("^code$", names(h))[1]]]),
    adequacao    = if (any(grepl("suitab", names(h)))) as.character(h[[grep("suitab", names(h))[1]]]) else NA_character_,
    importancia  = if (any(grepl("major", names(h), ignore.case = TRUE))) as.character(h[[grep("major", names(h), ignore.case = TRUE)[1]]]) else NA_character_
  ) %>% mutate(nome = nome, .before = 1)
}

classificar_habitats <- function(h) {
  if (is.null(h) || nrow(h) == 0) return(tibble(classe_iucn = NA_character_))
  # trata "Suitable" e vazio como adequado; descarta "Marginal"/"Unknown"
  s <- h %>% filter(is.na(adequacao) | str_detect(adequacao, regex("^suitable", ignore_case = TRUE)))
  if (nrow(s) == 0) s <- h
  floresta  <- str_detect(s$codigo, "^1(_|\\.|$)")
  urbano    <- str_detect(s$codigo, "^14(_|\\.)(4|5)")
  tibble(
    n_habitats   = nrow(s),
    so_floresta  = all(floresta),
    usa_urbano   = any(urbano),
    usa_degradado = any(str_detect(s$codigo, "^14(_|\\.)6")),
    classe_iucn  = case_when(all(floresta) ~ "dependente_floresta",
                             any(urbano)   ~ "tolerante_antropico",
                             TRUE          ~ "intermediaria")
  )
}

# ------------------------------------------------------------
# Espécies a consultar: frugívoros (prioridade) + plantas dos gêneros
# que estão na rede. Plantas têm cobertura baixa na IUCN — ver abaixo.
# ------------------------------------------------------------
frugivoros <- readRDS("outputs/frugivoros.rds")
alvo <- frugivoros %>% filter(frugivoro) %>% pull(nome)
if (TESTE) alvo <- c("Pipile jacutinga", "Turdus rufiventris", "Brachyteles arachnoides")

if (token == "") {
  message("Sem IUCN_REDLIST_KEY no ambiente — pulando a API e usando só o proxy AVONET.")
  iucn <- tibble(nome = alvo, classe_iucn = NA_character_)
} else {
  hab <- map(set_names(alvo), possibly(habitats_iucn, NULL), .progress = TRUE)
  if (TESTE) { hab_teste <- hab; print(hab_teste) }
  iucn <- imap_dfr(hab, ~ classificar_habitats(.x) %>% mutate(nome = .y, .before = 1))
}

# ------------------------------------------------------------
# Proxy independente pra aves: AVONET Habitat == "Forest" e
# Habitat.Density == 1 (vegetação densa). Serve pra checar a IUCN e
# pra preencher espécies sem avaliação.
# Mamíferos: não há proxy equivalente nas bases que temos — só IUCN.
# ------------------------------------------------------------
dependencia <- frugivoros %>%
  filter(frugivoro) %>%
  select(grupo, class, nome, Habitat, Habitat.Density) %>%
  left_join(iucn, by = "nome") %>%
  mutate(
    proxy_avonet = case_when(
      grupo != "birds" ~ NA_character_,
      Habitat == "Forest" & Habitat.Density == 1 ~ "dependente_floresta",
      Habitat == "Human Modified" | Habitat.Density == 3 ~ "tolerante_antropico",
      !is.na(Habitat) ~ "intermediaria"),
    classe_final = coalesce(classe_iucn, proxy_avonet),
    fonte = case_when(!is.na(classe_iucn) ~ "IUCN",
                      !is.na(proxy_avonet) ~ "AVONET (proxy)",
                      TRUE ~ NA_character_)
  )

if (!all(is.na(dependencia$classe_iucn)))
  print(with(dependencia, table(IUCN = classe_iucn, AVONET = proxy_avonet, useNA = "ifany")))
# ^ concordância entre as duas fontes; discordâncias valem uma olhada manual

write_csv(dependencia, "outputs/dependencia_floresta.csv")
dependencia %>% count(grupo, classe_final, fonte) %>% print()

# ------------------------------------------------------------
# PLANTAS — a IUCN cobre mal a flora da Mata Atlântica (muitas não
# avaliadas globalmente). Alternativa: Flora e Funga do Brasil via
# pacote `flora`, que devolve tipo de vegetação e estabelecimento
# (nativa / cultivada / naturalizada). Isso é útil por si só: separa
# ornamentais urbanas (cultivadas) das nativas de floresta.
# Conferir os nomes dos argumentos com ?flora::get.taxa — escrevi de
# memória e não consegui testar aqui.
# ------------------------------------------------------------
# plantas <- readRDS("outputs/presenca_site.rds") %>% filter(grupo == "plants") %>% distinct(nome) %>% pull()
# flora_veg <- flora::get.taxa(plantas, vegetation.type = TRUE, establishment = TRUE,
#                              habitat = TRUE, life.form = TRUE)
# write_csv(flora_veg, "outputs/plantas_flora_vegetacao.csv")
