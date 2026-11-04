#################################################################
######## 09 — Cenários de rede: + candidatas / − dependentes ####
#################################################################
# Duas perguntas, mesma maquinaria:
#  A) Reintrodução (estilo Mittelman et al. 2022, Ecography,
#     doi:10.1111/ecog.05838): o que acontece com a rede do PNT se
#     adicionarmos cada candidata, as melhores combinações, ou todas?
#  B) Contribuição das UCs: o que a rede do Rio perde se tirarmos os
#     frugívoros dependentes de floresta (script 07)?
#
# Pra cada cenário compara com um nulo, porque "adicionar espécie"
# sempre muda as métricas só por aumentar a rede:
#  A) dois nulos (escolha em NULO):
#     "mittelman" (padrão) = como no artigo: sorteia k frugívoros que JÁ
#        estão na rede e mede o quanto a rede muda entre "sem eles" e
#        "com eles" (reintrodução aleatória de k espécies);
#     "virtual" = espécie virtual com o MESMO grau da candidata, ligada a
#        plantas do PNT sorteadas com prob. proporcional ao grau delas —
#        isola o efeito da IDENTIDADE dos parceiros (mesmo nº de links).
#  B) nulo = remover o mesmo nº de frugívoros sorteados ao acaso
# z = (delta observado − média delta nulo) / dp delta nulo
# z > 0 em A = a candidata contribui MAIS do que uma espécie qualquer
# com o mesmo nº de interações (a identidade dos parceiros importa).
#
# Saídas: outputs/cenarios_reintroducao.csv, outputs/cenarios_ucs.csv

source("R/funcoes_meridian.R")
library(bipartite)
set.seed(42)

N_NULOS <- as.integer(Sys.getenv("N_NULOS", 99))   # o artigo usou 100
NULO    <- Sys.getenv("NULO", "mittelman")          # ou "virtual"

presenca   <- readRDS("outputs/presenca_site.rds")
frugivoros <- readRDS("outputs/frugivoros.rds")
int        <- readRDS("outputs/interacoes_mistas.rds")
credito    <- read_csv("outputs/credito_candidatas.csv", show_col_types = FALSE)
portfolio  <- read_csv("outputs/portfolio_reintroducao.csv", show_col_types = FALSE)

frug_site    <- function(s) intersect(presenca$nome[presenca$site == s & presenca$grupo != "plants"],
                                      frugivoros$nome[frugivoros$frugivoro])
plantas_site <- function(s) unique(presenca$genero[presenca$site == s & presenca$grupo == "plants"])

# Métricas leves pra usar dentro dos nulos (sem modularidade)
# Caminhos em log10: os números passam de 10^15 em L = 5, e o artigo
# reporta aumento relativo (%). delta de log10 -> % = 100 * (10^delta - 1).
metricas_leves <- function(B, n_universo = NULL) {
  B <- B[rowSums(B) > 0, colSums(B) > 0, drop = FALSE]
  c(n_links = sum(B),
    plantas_com_dispersor = ncol(B),
    conectancia = sum(B) / prod(dim(B)),
    NODF = unname(networklevel(B, index = "NODF")),
    robustez_plantas = robustez_rapida(B, nrep = 50, n_universo = n_universo),
    log10(n_caminhos(B, 2:5)))
}

# ============================================================
# A) REINTRODUÇÃO NO PNT
# ============================================================
plantas_pnt <- plantas_site("inside_pnt")
rede_pnt    <- rede_do_site(int, frug_site("inside_pnt"), plantas_pnt)
B0          <- para_matriz(rede_pnt)
# universo fixo de plantas = gêneros do PNT com registro de zoocoria
N_UNIV      <- length(intersect(plantas_pnt, int$planta_genero))
base0       <- metricas_leves(B0, N_UNIV)

# grau de cada gênero no ATLANTIC-frugivory (pra sortear parceiros no nulo)
peso_planta <- int %>% filter(planta_genero %in% plantas_pnt) %>% count(planta_genero, name = "grau")

adicionar <- function(B, el_novos) {
  el <- bind_rows(
    as_tibble(as.table(B), .name_repair = ~ c("frug", "planta_genero", "v")) %>% filter(v > 0) %>% select(-v),
    el_novos %>% select(frug, planta_genero))
  para_matriz(el)
}

cenario_adicao <- function(especies, rotulo) {
  novos <- int %>% filter(frug %in% especies, planta_genero %in% plantas_pnt)
  if (nrow(novos) == 0) return(NULL)
  obs <- metricas_leves(adicionar(B0, novos), N_UNIV) - base0
  k <- n_distinct(novos$frug)
  nul <- t(replicate(N_NULOS, {
    if (NULO == "mittelman") {
      # k frugívoros atuais sorteados: rede sem eles -> rede com eles
      x <- sample(rownames(B0), k)
      sem <- B0[!rownames(B0) %in% x, , drop = FALSE]
      metricas_leves(B0, N_UNIV) - metricas_leves(sem, N_UNIV)
    } else {
      graus <- count(novos, frug)$n
      el <- map2_dfr(graus, seq_along(graus), ~ tibble(
        frug = paste0("VIRTUAL", .y),
        planta_genero = sample(peso_planta$planta_genero, .x, prob = peso_planta$grau)))
      metricas_leves(adicionar(B0, el), N_UNIV) - base0
    }
  }))
  tibble(cenario = rotulo, metrica = names(obs), delta = obs,
         delta_nulo_media = colMeans(nul),
         z = (obs - colMeans(nul)) / apply(nul, 2, sd))
}

ausentes <- credito %>% filter(!registrada_pnt, credito_links >= 3) %>% pull(nome)
# ^ candidatas com < 3 links no PNT não têm como mexer na rede; ficam de fora

cen_individuais <- map_dfr(ausentes, ~ cenario_adicao(.x, paste0("+ ", .x)), .progress = TRUE)
cen_portfolio   <- map_dfr(2:min(5, nrow(portfolio)), function(k)
  cenario_adicao(portfolio$especie[1:k], paste0("portfólio top ", k)))
cen_todas       <- cenario_adicao(ausentes, "todas as ausentes")

cenarios_A <- bind_rows(cen_individuais, cen_portfolio, cen_todas) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(cenarios_A %>% filter(metrica %in% c("conectancia", "NODF", "robustez_plantas", "caminhos_L5")) %>%
        select(cenario, metrica, delta, z) %>%
        pivot_wider(names_from = metrica, values_from = c(delta, z)), n = Inf, width = Inf)
write_csv(cenarios_A, "outputs/cenarios_reintroducao.csv")

# Sinergia (Mittelman et al., Fig. 5): os caminhos adicionados pelas
# 3 primeiras do portfólio JUNTAS são mais que a soma das 3 SEPARADAS?
# (em contagem bruta, não em log)
caminhos_brutos <- function(B) n_caminhos(B, 2:5)
c0 <- caminhos_brutos(B0)
add_sp <- function(sp) adicionar(B0, int %>% filter(frug %in% sp, planta_genero %in% plantas_pnt))
top3 <- portfolio$especie[1:3]
soma_sep <- Reduce(`+`, map(top3, ~ caminhos_brutos(add_sp(.x)) - c0))
juntas   <- caminhos_brutos(add_sp(top3)) - c0
sinergia <- tibble(comprimento = names(c0),
                   aumento_pct_juntas = round(100 * juntas / c0, 1),
                   aumento_pct_soma_separadas = round(100 * soma_sep / c0, 1),
                   razao_juntas_sobre_soma = round(juntas / soma_sep, 2))
print(sinergia)
write_csv(sinergia, "outputs/sinergia_caminhos.csv")

# ============================================================
# B) CONTRIBUIÇÃO DAS UCs: remover dependentes de floresta
# ============================================================
dep <- read_csv("outputs/dependencia_floresta.csv", show_col_types = FALSE)
dependentes <- dep %>% filter(classe_final == "dependente_floresta") %>% pull(nome)
cat("Frugívoros dependentes de floresta (fonte mista IUCN/AVONET):", length(dependentes), "\n")
# !!! Enquanto não rodar a IUCN, isto é só aves (proxy AVONET).

cenario_remocao <- function(s) {
  fs <- frug_site(s); ps <- plantas_site(s)
  B  <- para_matriz(rede_do_site(int, fs, ps))
  nu <- ncol(B)   # universo = plantas da rede original
  b  <- metricas_leves(B, nu)
  sai <- intersect(rownames(B), dependentes)
  if (length(sai) == 0) return(NULL)
  Br <- B[!rownames(B) %in% sai, , drop = FALSE]
  obs <- metricas_leves(Br, nu) - b
  nul <- t(replicate(N_NULOS, {
    x <- sample(rownames(B), length(sai))
    metricas_leves(B[!rownames(B) %in% x, , drop = FALSE], nu) - b
  }))
  # plantas que ficam sem nenhum dispersor = plantas cuja dispersão
  # depende exclusivamente de frugívoros de floresta
  orfas <- setdiff(colnames(B)[colSums(B) > 0], colnames(Br)[colSums(Br) > 0])
  tibble(site = s, n_frug = nrow(B), n_removidos = length(sai),
         metrica = names(obs), valor_original = b, delta = obs,
         delta_nulo_media = colMeans(nul),
         z = (obs - colMeans(nul)) / apply(nul, 2, sd),
         plantas_orfas = paste(sort(orfas), collapse = "; "))
}

cenarios_B <- map_dfr(c("inside_pnt", "inside_ucs", "outside_ucs"), cenario_remocao) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(cenarios_B %>% select(-plantas_orfas), n = Inf, width = Inf)
write_csv(cenarios_B, "outputs/cenarios_ucs.csv")
# Leitura: z << 0 = perder os dependentes de floresta desestrutura a
# rede MAIS do que perder o mesmo nº de frugívoros quaisquer —
# i.e., as espécies que as UCs sustentam ocupam papéis estruturais.
