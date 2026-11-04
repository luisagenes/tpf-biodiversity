#################################################################
######## Funções compartilhadas — redes, reintroduções, UCs #####
#################################################################
# Carregado por 06_ a 10_ via source("R/funcoes_meridian.R").
# Não altera nenhum script da Luísa; só lê os arquivos em data/.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
})

dir.create("outputs", showWarnings = FALSE)

# ------------------------------------------------------------
# Números: a corrupção de separador de milhar ("1.243.866.667")
# não tem como ser decodificada com segurança (a posição da vírgula
# decimal original se perdeu), então esses valores viram NA e são
# preenchidos por outra fonte (AVONET / EltonTraits) quando possível.
# ------------------------------------------------------------
num_seguro <- function(x) {
  x <- str_trim(as.character(x))
  corrompido <- str_count(x, fixed(".")) > 1
  x <- str_replace(x, ",", ".")
  out <- suppressWarnings(as.numeric(x))
  out[corrompido %in% TRUE] <- NA_real_
  out
}

binomio <- function(x) word(str_squish(x), 1, 2)   # tira autor (flora::get.taxa devolve com autor)
genero  <- function(x) word(str_squish(x), 1)

# ------------------------------------------------------------
# Leitura dos dados
# ------------------------------------------------------------

# Sinônimos manuais entre o backbone do GBIF (comunidades) e a
# taxonomia usada no ATLANTIC-frugivory. Lista curta, só os que
# importam pras espécies-candidatas e frugívoros grandes. Se você
# rodar a padronização completa do 04_networks.R (name_backbone_checklist
# no at_frug), troque isto pelo de_para_frug_atfrug.
sinonimos_af <- tribble(
  ~nome_af,                  ~nome,
  "Aburria jacutinga",       "Pipile jacutinga",
  "Alouatta clamitans",      "Alouatta guariba",
  "Cebus nigritus",          "Sapajus nigritus"
  # "Cebus apella" no ATLANTIC-frugivory pode ser S. nigritus, S. robustus
  # ou S. xanthosternos dependendo da região do estudo — não mapeei.
)


# ------------------------------------------------------------
# Sites (mesma lógica do 04_networks.R)
# ------------------------------------------------------------
sites <- list(
  outside_ucs        = "outside_ucs",
  inside_ucs         = c("PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
                         "PARQUE ESTADUAL DA PEDRA BRANCA",
                         "PARQUE NACIONAL DA TIJUCA"),
  inside_mendanha    = "PARQUE NATURAL MUNICIPAL DA SERRA DO MENDANHA",
  inside_pedrabranca = "PARQUE ESTADUAL DA PEDRA BRANCA",
  inside_pnt         = "PARQUE NACIONAL DA TIJUCA"
)

# ------------------------------------------------------------
# Redes
# ------------------------------------------------------------
# edgelist (frug, planta_genero) -> matriz binária frug x planta
para_matriz <- function(el) {
  el <- distinct(el, frug, planta_genero)
  if (nrow(el) == 0) return(matrix(0, 0, 0))
  m <- table(factor(el$frug), factor(el$planta_genero))
  m <- unclass(m); m[m > 0] <- 1
  storage.mode(m) <- "numeric"
  m
}

rede_do_site <- function(int, frug_site, plantas_genero_site) {
  int %>% filter(frug %in% frug_site, planta_genero %in% plantas_genero_site)
}

# ------------------------------------------------------------
# Caminhos indiretos — como em Mittelman et al. 2022 (Ecography):
# matriz de adjacência quadrada A (animais + plantas); o nº de caminhos
# de comprimento l é a soma de TODOS os elementos de A^l (Guimarães et
# al. 2017). Eles usaram l = 2 a 5 (5 = diâmetro da rede do PNT).
# Calculado por produto matriz-vetor (1' A^l 1), sem montar A^l.
# Obs.: a versão anterior deste arquivo usava o modelo de coevolução de
# Guimarães et al. 2017 para "efeitos indiretos" — NÃO é o que o
# Mittelman fez; foi substituída depois de ler o artigo.
# ------------------------------------------------------------
n_caminhos <- function(B, L = 2:5) {
  B <- B[rowSums(B) > 0, colSums(B) > 0, drop = FALSE]
  na <- nrow(B); np <- ncol(B); n <- na + np
  A <- matrix(0, n, n)
  A[1:na, (na + 1):n] <- B
  A[(na + 1):n, 1:na] <- t(B)
  v <- rep(1, n); out <- numeric(max(L))
  for (l in seq_len(max(L))) { v <- A %*% v; out[l] <- sum(v) }
  setNames(out[L], paste0("caminhos_L", L))
}

# Robustez rápida (remoção aleatória de frugívoros = linhas).
# Uma planta perde a dispersão quando o ÚLTIMO dos seus parceiros sai,
# então o "tempo de extinção" dela é o maior rank (na ordem de remoção)
# entre os parceiros. Área sob a curva por trapézio, como em
# bipartite::robustness (Burgos et al. 2007), mas ~30x mais rápido —
# importa porque os nulos chamam isto milhares de vezes.
# Não é idêntico ao bipartite em redes pequenas: o bipartite começa a
# curva na 1ª remoção, normaliza pelo total de extinções e integra um
# spline. Em redes do tamanho das nossas (~170 x 130) a diferença é
# < 0.01. Pra comparar cenários (deltas e nulos) isso não importa,
# desde que tudo use a mesma função.
robustez_rapida <- function(B, nrep = 100, n_universo = NULL) {
  # n_universo: nº total de plantas que "deveriam" ter dispersor (ex.:
  # todos os gêneros zoocóricos do site). Plantas fora de B contam como
  # já sem dispersor no início. Sem isso, adicionar uma espécie que
  # "resgata" plantas órfãs DIMINUI a robustez média (entram plantas com
  # 1 parceiro só), o que é o oposto do que interessa pra restauração.
  B <- B[rowSums(B) > 0, colSums(B) > 0, drop = FALSE]
  n <- nrow(B); x <- (0:n) / n
  if (is.null(n_universo)) n_universo <- ncol(B)
  mean(replicate(nrep, {
    ordem <- sample.int(n)                      # posição em que cada linha sai
    t_ext <- apply(B * ordem, 2, max)           # plantas morrem no passo t_ext
    surv  <- c(ncol(B), ncol(B) - cumsum(tabulate(t_ext, n))) / n_universo
    sum(diff(x) * (head(surv, -1) + tail(surv, -1)) / 2)
  }))
}

# Métricas por rede. Modularidade é cara em matrizes grandes, por isso
# é opcional (com_modularidade = FALSE por padrão).
metricas_rede <- function(B, n_nulos = 0, com_modularidade = FALSE) {
  B <- B[rowSums(B) > 0, colSums(B) > 0, drop = FALSE]
  if (min(dim(B)) < 2) return(tibble())
  out <- tibble(
    n_frug       = nrow(B),
    n_plantas    = ncol(B),
    n_links      = sum(B),
    conectancia  = bipartite::networklevel(B, index = "connectance"),
    NODF         = bipartite::networklevel(B, index = "NODF"),
    robustez_plantas = robustez_rapida(B, nrep = 100),
  ) %>% bind_cols(as_tibble_row(n_caminhos(B)))
  if (com_modularidade) out$modularidade <- bipartite::computeModules(B)@likelihood
  if (n_nulos > 0) {
    nulos <- bipartite::nullmodel(B, N = n_nulos, method = "r2dtable")
    sim <- sapply(nulos, function(x) bipartite::networklevel(x, index = "NODF"))
    out$NODF_z <- (out$NODF - mean(sim)) / sd(sim)
  }
  out
}
# Nota sobre robustez: second.extinct(participant = "lower") remove
# frugívoros (linhas) e conta plantas que ficam sem nenhum parceiro.
# Aqui uso ordem aleatória (nrep = 50) porque, pra comparar cenários
# de adição de espécies, a ordem por grau faz a espécie adicionada
# (geralmente de grau alto) sair primeiro, o que mascara o efeito.
