#################################################################
######## Funções compartilhadas — scripts 06, 07 e 08 ###########
#################################################################
# Carregado via source("R/funcoes_redes.R"), a partir da raiz do projeto.
# Não altera nenhum script da Luísa; só lê os arquivos em data/.
#
# Convenções (as mesmas do 04_networks.R):
#  - rede "mista": frugívoro ESPÉCIE x planta GÊNERO, binária;
#  - interação = co-ocorrência estrita: o par existe no ATLANTIC-frugivory
#    E os dois táxons foram registrados no site (comunidades);
#  - nomes padronizados por NOME (join, nunca por posição): GBIF backbone
#    pra animais (rgbif) e Flora do Brasil pra plantas (flora).
#
# A padronização do ATLANTIC-frugivory é lenta (API do GBIF), então o
# resultado fica em cache em outputs/cache_tax_*.rds. Apague esses
# arquivos pra refazer do zero.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  library(ggplot2)
  library(igraph)
})

dir.create("outputs", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

binomio <- function(x) word(str_squish(x), 1, 2)   # tira autor (flora::get.taxa devolve com autor)
genero  <- function(x) word(str_squish(x), 1)

# ------------------------------------------------------------
# Sites (mesma definição do 04_networks.R) + Rio inteiro
# ------------------------------------------------------------
sites <- list(
  rio_total          = "todos",   # todos os registros, qualquer location_status
  outside_ucs        = "outside_ucs",
  inside_ucs         = c("PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
                         "PARQUE ESTADUAL DA PEDRA BRANCA",
                         "PARQUE NACIONAL DA TIJUCA"),
  inside_mendanha    = "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
  inside_pedrabranca = "PARQUE ESTADUAL DA PEDRA BRANCA",
  inside_pnt         = "PARQUE NACIONAL DA TIJUCA"
)
# Obs.: "outside_ucs" é só o location_status "outside_ucs" (como no 04);
# APAs e parques municipais entram em rio_total, mas não em outside_ucs.

# ------------------------------------------------------------
# Padronização taxonômica com cache
# ------------------------------------------------------------
# Sinônimos aplicados ANTES de consultar o GBIF (casos conhecidos em
# que a grafia da base de interações não casa com a das comunidades).
SINONIMOS_ANIMAIS <- c(
  "Chelonoidis denticulata" = "Chelonoidis denticulatus",   # grafia do NeoFrugivory
  "Alouatta clamitans"      = "Alouatta guariba",
  "Cebus nigritus"          = "Sapajus nigritus"
)

ler_cache <- function(cache) {
  if (file.exists(cache)) readRDS(cache) else tibble(original = character(), padronizado = character())
}

# Devolve o nome padronizado de cada elemento de x (mesmo comprimento de x).
padronizar_animais <- function(x, cache = "outputs/cache_tax_animais.rds") {
  x0 <- str_squish(x)
  x1 <- coalesce(unname(SINONIMOS_ANIMAIS[x0]), x0)
  tab <- ler_cache(cache)
  faltam <- setdiff(unique(na.omit(x1)), tab$original)
  if (length(faltam)) {
    message("GBIF backbone: consultando ", length(faltam), " nomes de animais (salvo em ", cache, ")")
    novo <- tryCatch({
      bb <- rgbif::name_backbone_checklist(faltam, bucket_size = 50, sleep = 3)
      if (!"species" %in% names(bb)) bb$species <- NA_character_
      tibble(original = bb$verbatim_name, padronizado = bb$species) %>% distinct(original, .keep_all = TRUE)
    }, error = function(e) NULL)
    if (is.null(novo)) {
      warning("Sem acesso ao GBIF: ", length(faltam), " nomes de animais ficam SEM padronizar ",
              "(não vão pro cache). Rode de novo com internet.", call. = FALSE)
      tab <- bind_rows(tab, tibble(original = faltam, padronizado = faltam))
    } else {
      tab <- bind_rows(tab, novo); saveRDS(tab, cache)
    }
  }
  tab$padronizado[match(x1, tab$original)]
}

padronizar_plantas <- function(x, cache = "outputs/cache_tax_plantas.rds") {
  x0 <- str_squish(x)
  tab <- ler_cache(cache)
  faltam <- setdiff(unique(na.omit(x0)), tab$original)
  if (length(faltam)) {
    message("Flora do Brasil: consultando ", length(faltam), " nomes de plantas (salvo em ", cache, ")")
    novo <- tryCatch({
      fl <- flora::get.taxa(faltam, domain = TRUE)
      tibble(original = fl$original.search, padronizado = binomio(fl$scientific.name)) %>%
        distinct(original, .keep_all = TRUE)
    }, error = function(e) NULL)
    if (is.null(novo)) {
      warning("Pacote flora indisponível: ", length(faltam), " nomes de plantas ficam SEM padronizar ",
              "(não vão pro cache).", call. = FALSE)
      tab <- bind_rows(tab, tibble(original = faltam, padronizado = faltam))
    } else {
      tab <- bind_rows(tab, novo); saveRDS(tab, cache)
    }
  }
  tab$padronizado[match(x0, tab$original)]
}

# ------------------------------------------------------------
# Leitura dos dados
# ------------------------------------------------------------
ler_comunidades <- function(arq = "data/comunidades_clean.csv") {
  read_csv(arq, show_col_types = FALSE,
           col_select = c(group, order, family, species, location_status,
                          species_padronizado, genero_padronizado)) %>%
    filter(group %in% c("birds", "mammals", "plants"), !is.na(species_padronizado),
           # mesmos filtros de táxons marinhos do 04_networks.R
           !order %in% c("Cetacea", "Sphenisciformes", "Procellariiformes"),
           !family %in% c("Otariidae", "Phocidae", "Delphinidae", "Fregatidae",
                          "Sulidae", "Laridae", "Stercorariidae"),
           !species %in% c("Fregata magnificens", "Larus dominicanus")) %>%
    mutate(nome   = binomio(species_padronizado),
           genero = coalesce(genero_padronizado, genero(nome)))
}

# ATLANTIC-frugivory -> pares (frugívoro espécie, planta gênero) padronizados
ler_atfrug <- function(arq = "data/ATLANTIC_frugivory.csv") {
  af <- read_delim(arq, delim = ";", locale = locale(encoding = "latin1"),
                   col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(c(Frugivore_Species, Plant_Species), str_squish)) %>%
    filter(!is.na(Frugivore_Species), !is.na(Plant_Species),
           Frugivore_Species != "", Plant_Species != "",
           !Frugivore_Species %in% c("Animal-oriented", "Plant-oriented", "Network study"))
  af %>%
    mutate(frug = padronizar_animais(Frugivore_Species),
           planta_genero = genero(padronizar_plantas(Plant_Species))) %>%
    filter(!is.na(frug), !is.na(planta_genero)) %>%
    group_by(frug) %>% mutate(n_estudos_frug = n_distinct(`Study reference`)) %>%
    group_by(frug, planta_genero) %>%
    summarise(n_registros = n(), n_estudos = n_distinct(`Study reference`),
              n_estudos_frug = first(n_estudos_frug), .groups = "drop") %>%
    mutate(fonte = "ATLANTIC-frugivory")
}

# NeoFrugivory (Fuzessy & Pizo 2025) — região Neotropical inteira.
# Usado SÓ para as espécies pedidas em `especies` (ex.: o jabuti, que não
# tem nenhum registro no ATLANTIC-frugivory), para não mudar a definição
# das redes do 04.
ler_neofrugivory <- function(especies, dir = "data/NeoFrugivory") {
  ler <- function(f) read_tsv(file.path(dir, f), col_types = cols(.default = "c"), show_col_types = FALSE)
  vert <- ler("Vertebrates.txt") %>% distinct(vertebrate_sp_paper, .keep_all = TRUE)
  pl   <- ler("Plants.txt") %>% distinct(Plant_sp_paper, .keep_all = TRUE)
  ler("Interactions.txt") %>%
    left_join(vert %>% select(Animal_sp_paper = vertebrate_sp_paper, sp = vertebrate_sp_current),
              by = "Animal_sp_paper") %>%
    left_join(pl %>% select(Plant_sp_paper, planta_genero = Plant_genus_current), by = "Plant_sp_paper") %>%
    mutate(sp = str_replace_all(coalesce(sp, Animal_sp_paper), "_", " "),
           frug = coalesce(unname(SINONIMOS_ANIMAIS[sp]), sp)) %>%
    filter(frug %in% especies, !is.na(planta_genero), planta_genero != "NA") %>%
    mutate(frug = padronizar_animais(frug)) %>%
    group_by(frug) %>% mutate(n_estudos_frug = n_distinct(Study)) %>%
    group_by(frug, planta_genero) %>%
    summarise(n_registros = n(), n_estudos = n_distinct(Study),
              n_estudos_frug = first(n_estudos_frug), .groups = "drop") %>%
    mutate(fonte = "NeoFrugivory")
}

# Interações: ATLANTIC-frugivory + NeoFrugivory só pra quem não tem
# nenhum registro no ATLANTIC-frugivory e foi pedido em `completar_com_neo`.
ler_interacoes <- function(completar_com_neo = character(0)) {
  int <- ler_atfrug()
  completar <- setdiff(completar_com_neo, int$frug)
  if (length(completar)) {
    neo <- ler_neofrugivory(completar)
    sem <- setdiff(completar, neo$frug)
    if (length(sem)) message("Sem interações nem no NeoFrugivory: ", paste(sem, collapse = ", "))
    int <- bind_rows(int, neo)
  }
  int
}

# ------------------------------------------------------------
# Presença por site e redes
# ------------------------------------------------------------
filtrar_site <- function(com, locs) {
  if (identical(locs, "todos")) com else filter(com, location_status %in% locs)
}
frugivoros_site <- function(com, locs) {
  filtrar_site(com, locs) %>% filter(group %in% c("birds", "mammals")) %>% pull(nome) %>% unique()
}
plantas_site <- function(com, locs) {
  filtrar_site(com, locs) %>% filter(group == "plants") %>% pull(genero) %>% unique()
}

# `forcar`: frugívoros tratados como presentes mesmo sem registro em
# comunidades (ex.: o jabuti — comunidades não tem répteis).
rede_site <- function(int, com, locs, forcar = character(0)) {
  int %>%
    filter(frug %in% union(frugivoros_site(com, locs), forcar),
           planta_genero %in% plantas_site(com, locs)) %>%
    distinct(frug, planta_genero)
}

para_matriz <- function(el) {
  el <- distinct(el, frug, planta_genero)
  m <- unclass(table(factor(el$frug), factor(el$planta_genero)))
  m[m > 0] <- 1
  storage.mode(m) <- "numeric"
  m
}

# ------------------------------------------------------------
# Métricas e comparação "com vs sem"
# ------------------------------------------------------------
# Linhas = frugívoros, colunas = plantas (como no 04). Em bipartite,
# linhas são o nível "lower": participant = "lower" remove frugívoros e
# conta plantas que ficam sem nenhum parceiro (ext.higher).
# Ordem de remoção aleatória (nrep réplicas): por grau, a espécie
# retirada (em geral generalista) sairia primeiro e mascararia o efeito.
metricas_rede <- function(B, nrep_rob = 50) {
  B <- B[rowSums(B) > 0, colSums(B) > 0, drop = FALSE]
  if (min(dim(B)) < 2) return(NULL)
  c(n_frugivoros = nrow(B),
    plantas_com_dispersor = ncol(B),
    n_links = sum(B),
    conectancia = sum(B) / prod(dim(B)),
    NODF = unname(bipartite::networklevel(B, index = "NODF")),
    robustez_plantas = bipartite::robustness(
      bipartite::second.extinct(B, participant = "lower", method = "random", nrep = nrep_rob)))
}
# Atenção à leitura da robustez: ela é calculada sobre as plantas que
# estão na rede. Tirar uma espécie que é a única dispersora de algumas
# plantas tira essas plantas da rede, o que pode até AUMENTAR a robustez
# média. Por isso a perda de plantas aparece à parte
# (plantas_com_dispersor e a lista de plantas órfãs).

# Compara a rede B com ela mesma sem `remover`. Nulo (como em Mittelman
# et al. 2022): remove k frugívoros sorteados de `pool`, com k = nº de
# espécies de `remover` que estão em B.
#   delta = métrica(com) - métrica(sem)
#   z     = (delta_obs - média(delta_nulo)) / dp(delta_nulo)
#   p_nulo_maior_igual = proporção dos nulos com delta >= observado
comparar_remocao <- function(B, remover, pool = setdiff(rownames(B), remover),
                             n_nulos = 100, nrep_rob = 50, rotulo = "") {
  remover <- intersect(rownames(B), remover)
  k <- length(remover)
  if (k == 0) { message(rotulo, ": nenhuma espécie a remover está na rede"); return(NULL) }
  m_com <- metricas_rede(B, nrep_rob)
  m_sem <- metricas_rede(B[!rownames(B) %in% remover, , drop = FALSE], nrep_rob)
  obs   <- m_com - m_sem
  pool  <- intersect(pool, rownames(B))
  if (n_nulos > 0 && length(pool) >= k) {
    nul <- t(replicate(n_nulos, {
      x <- sample(pool, k)
      m_com - metricas_rede(B[!rownames(B) %in% x, , drop = FALSE], nrep_rob)
    }))
    media <- colMeans(nul); dp <- apply(nul, 2, sd)
    p <- colMeans(sweep(nul, 2, obs, ">="))
  } else {
    if (n_nulos > 0) message(rotulo, ": pool do nulo (", length(pool), ") menor que k (", k, ") — sem nulo")
    media <- dp <- p <- rep(NA_real_, length(obs))
  }
  orfas <- setdiff(colnames(B)[colSums(B) > 0],
                   colnames(B)[colSums(B[!rownames(B) %in% remover, , drop = FALSE]) > 0])
  tibble(cenario = rotulo, k_removidas = k, metrica = names(obs),
         com = m_com, sem = m_sem, delta = obs,
         delta_nulo_media = media, delta_nulo_dp = dp,
         z = ifelse(dp > 0, (obs - media) / dp, NA_real_), p_nulo_maior_igual = p,
         plantas_orfas = paste(sort(orfas), collapse = "; "))
}

# ------------------------------------------------------------
# Figura: rede "com" e "sem" lado a lado, mesma posição dos nós
# ------------------------------------------------------------
# Layout Fruchterman-Reingold (como no 04) calculado na rede "com"; no
# painel "sem" os nós ficam no mesmo lugar, as espécies destacadas e seus
# links somem, e as plantas que ficam sem nenhum dispersor aparecem como
# círculos vazios na cor do destaque.
plot_com_sem <- function(el, destaque, titulo, rotulo_com, rotulo_sem,
                         rotulo_destaque, cor_destaque = "#e63946",
                         rotular = TRUE, seed = 1) {
  el <- distinct(el, frug, planta_genero)
  g <- graph_from_data_frame(el, directed = FALSE)
  set.seed(seed)
  lay <- layout_with_fr(g)
  nos <- tibble(nome = V(g)$name, x = lay[, 1], y = lay[, 2]) %>%
    mutate(tipo = case_when(nome %in% destaque          ~ rotulo_destaque,
                            nome %in% el$planta_genero  ~ "planta (gênero)",
                            TRUE                        ~ "frugívoro"))
  arestas <- el %>%
    left_join(select(nos, frug = nome, x, y), by = "frug") %>%
    left_join(select(nos, planta_genero = nome, xend = x, yend = y), by = "planta_genero") %>%
    mutate(destacada = frug %in% destaque)

  sobram <- el %>% filter(!frug %in% destaque)
  orfas  <- setdiff(el$planta_genero, sobram$planta_genero)
  lv <- c(rotulo_com, rotulo_sem)

  a <- bind_rows(mutate(arestas, painel = rotulo_com),
                 mutate(filter(arestas, !destacada), painel = rotulo_sem)) %>%
    mutate(painel = factor(painel, lv)) %>% arrange(destacada)
  n <- bind_rows(mutate(nos, painel = rotulo_com),
                 nos %>% filter(nome %in% c(sobram$frug, sobram$planta_genero)) %>% mutate(painel = rotulo_sem),
                 nos %>% filter(nome %in% orfas) %>%
                   mutate(painel = rotulo_sem, tipo = "planta sem dispersor")) %>%
    mutate(painel = factor(painel, lv)) %>%
    arrange(tipo %in% c(rotulo_destaque, "planta sem dispersor"))   # destaque desenhado por cima

  cores  <- setNames(c("grey45", "#2c7a4b", cor_destaque, cor_destaque),
                     c("frugívoro", "planta (gênero)", rotulo_destaque, "planta sem dispersor"))
  formas <- setNames(c(16, 15, 16, 1), names(cores))
  tam    <- setNames(c(1.6, 1.4, 3.5, 2.2), names(cores))

  p <- ggplot() +
    geom_segment(data = a, aes(x, y, xend = xend, yend = yend, alpha = destacada),
                 colour = ifelse(a$destacada, cor_destaque, "grey70"),
                 linewidth = 0.25, show.legend = FALSE) +
    scale_alpha_manual(values = c(`FALSE` = 0.25, `TRUE` = 0.55)) +
    geom_point(data = n, aes(x, y, colour = tipo, shape = tipo, size = tipo), stroke = 0.8) +
    scale_colour_manual(values = cores, name = NULL) +
    scale_shape_manual(values = formas, name = NULL) +
    scale_size_manual(values = tam, name = NULL) +
    facet_wrap(~ painel) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "bottom", strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(hjust = 0.5)) +
    labs(title = titulo)
  if (rotular) {
    rot <- filter(n, tipo == rotulo_destaque, painel == rotulo_com)
    p <- p + if (requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_label_repel(data = rot, aes(x, y, label = nome), size = 2.8, fontface = "italic",
                                colour = cor_destaque, label.size = 0, fill = alpha("white", 0.85),
                                min.segment.length = 0, box.padding = 0.6, max.overlaps = Inf, seed = seed)
    } else {
      geom_label(data = rot, aes(x, y, label = nome), size = 2.6, fontface = "italic",
                 colour = cor_destaque, label.size = 0, alpha = 0.8, vjust = -0.6)
    }
  }
  p
}
