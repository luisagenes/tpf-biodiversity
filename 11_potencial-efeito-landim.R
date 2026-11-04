#################################################################
######## 11 — Potencial efeito das candidatas (Landim et al. 2024)
#################################################################
# Aplica o framework de Landim et al. 2024 (Restoration Ecology,
# doi:10.1111/rec.14151) às candidatas do workshop.
#
# Dados do PNT vindos do repositório do artigo
# (github.com/annarlandim/Species-potential-effect-according-to-Landim-et-al.-2022),
# copiados para data/landim2024/:
#   PlantTraitsPNT.csv — comunidade de recursos (plantas do PNT + atributos)
#   ConsumersPNT.csv   — rede do PNT no "modelo filogenético" do Mittelman
#                        et al. 2022: cada linha = consumidor x espécie de
#                        planta do PNT cujo GÊNERO aparece na dieta do
#                        consumidor no ATLANTIC-frugivory, com os atributos
#                        da planta. (Conferido: Alouatta tem 205 linhas e
#                        há 206 spp. do PNT nos gêneros da sua dieta.)
#
# As candidatas entram exatamente do mesmo jeito: candidata x espécies
# do PNT nos gêneros da sua dieta no ATLANTIC-frugivory.
#
# Equações (numeração do artigo):
#   C  = PR niche da comunidade de consumidores (soma das TPDs dos grupos,
#        pesadas pelo nº de espécies — como no Example_case_study.R)
#   T_i = C + S_i                               (Eq. 2, sem dividir por N+1)
#   O_i = S_i / T_i                             (Eq. 3)
#   R  = nicho funcional da comunidade de plantas (Eq. 4)
#   Y_i(θ) = ( ∫ O_i(x)^θ R(x) dx )^(1/θ)       (Eq. 5)
#   A_i = vol{S_i > 0 e R > 0} / vol{R > 0}     (Eq. 6, ver nota em amplitude())
# Atributos: comprimento e diâmetro do fruto e diâmetro da semente
# (comprimento da semente fora, r = 0.9 — Supplement S5 do artigo).
#
# Diferenças em relação ao código do repositório (que tem alguns bugs:
# `pe()` é chamada mas a função definida é `pe.v`; `comprimento` não
# existe; `Kdes` da 1ª seção usa `kde.G1` em vez de `kde.Sp`):
#   - grupos e pesos calculados a partir dos dados, não digitados;
#   - Alouatta e Dasyprocta entram na comunidade ATUAL (já reintroduzidos);
#   - amplitude reportada de dois jeitos (ver nota no fim).
#
# Validação: a seção 1 reproduz o caso de estudo do artigo (cutia vs.
# bugio contra a comunidade sem os dois).
#
# Saídas: outputs/landim_Y_theta.csv, outputs/landim_resumo.csv,
#         outputs/landim_sequencia.csv

source("R/funcoes_meridian.R")
suppressPackageStartupMessages(library(ks))

TRAITS <- c("Fruit_length", "Fruit_diameter", "Seed_diameter")
GRID   <- c(61, 87, 84)          # mesmos valores do artigo
THETAS <- 1:20
MIN_PONTOS <- 10                 # mínimo de linhas completas pra estimar uma TPD 3D

plantas <- read_csv("data/landim2024/PlantTraitsPNT.csv", show_col_types = FALSE) %>%
  mutate(genero = genero(Esp))
cons <- read_csv("data/landim2024/ConsumersPNT.csv", show_col_types = FALSE,
                 col_types = cols(.default = "c")) %>%
  mutate(across(all_of(TRAITS), as.numeric)) %>%
  filter(!is.na(CONSUMER), !is.na(TYPE))

pl <- plantas %>% select(all_of(TRAITS)) %>% na.omit() %>% as.data.frame()
XMAX <- apply(pl, 2, max)
VOL  <- prod(XMAX / GRID)        # volume de uma célula, como no artigo

# TPD de um conjunto de pontos (linhas = interações, colunas = TRAITS),
# no grid da comunidade de recursos, normalizada para integral = 1.
tpd <- function(x) {
  x <- as.data.frame(na.omit(x[, TRAITS]))
  if (nrow(x) < MIN_PONTOS) return(NULL)
  k <- kde(x = x, H = Hpi(x), gridsize = GRID, xmin = c(0, 0, 0), xmax = XMAX, binned = FALSE)
  k$estimate / (sum(k$estimate) * VOL)
}

R_tpd <- tpd(pl)

Y_theta <- function(S, Cc, thetas = THETAS) {
  Tt <- Cc + S
  ok <- Tt > 0
  O  <- S[ok] / Tt[ok]
  map_dbl(thetas, ~ (sum(O^.x * R_tpd[ok]) * VOL)^(1 / .x))
}

amplitude <- function(S, massa = NULL) {
  # Eq. 6 com a interseção com R no numerador: lida ao pé da letra
  # (vol{S>0}/vol{R>0}) passa de 1 quando a TPD da espécie tem banda
  # larga (poucos pontos) e a de R tem zeros numéricos nas bordas — a
  # cutia dava 7.8. Com a interseção a cutia dá ~0.99, como no artigo.
  if (is.null(massa)) return(sum(S > 0 & R_tpd > 0) / sum(R_tpd > 0))
  # Com kernel gaussiano, S > 0 em quase todo o grid (caudas minúsculas),
  # então a versão literal tende a ~1 pra quem tem poucos pontos.
  # Alternativa: região de maior densidade (HDR) que contém `massa`
  # da probabilidade, também intersectada com a HDR de R.
  hdr <- function(f) {
    o <- sort(as.vector(f), decreasing = TRUE)
    corte <- o[which(cumsum(o) * VOL >= massa)[1]]
    f >= corte
  }
  hR <- hdr(R_tpd)
  sum(hdr(S) & hR) / sum(hR)
}

# TPDs dos grupos do PNT + peso = nº de espécies no grupo
grupos <- cons %>% group_by(TYPE) %>% summarise(n_spp = n_distinct(CONSUMER), .groups = "drop")
tpd_grupos <- map(set_names(grupos$TYPE), ~ tpd(filter(cons, TYPE == .x)))
grupos$tem_tpd <- !map_lgl(tpd_grupos, is.null)
print(grupos)

soma_comunidade <- function(excluir = character(0)) {
  g <- grupos %>% filter(tem_tpd, !TYPE %in% excluir)
  Reduce(`+`, map2(g$TYPE, g$n_spp, ~ .y * tpd_grupos[[.x]]))
}

# ============================================================
# 1. VALIDAÇÃO: reproduzir o caso de estudo (Fig. 2 do artigo)
# ============================================================
C_pre <- soma_comunidade(excluir = c("Alouatta guariba", "Dasyprocta leporina"))
valid <- tibble(
  especie = c("Alouatta guariba", "Dasyprocta leporina"),
  Y = list(Y_theta(tpd_grupos[["Alouatta guariba"]], C_pre),
           Y_theta(tpd_grupos[["Dasyprocta leporina"]], C_pre)),
  A_literal = c(amplitude(tpd_grupos[["Alouatta guariba"]]),
                amplitude(tpd_grupos[["Dasyprocta leporina"]]))
)
cat("\nValidação — artigo: cutia > bugio em todo θ, Y parecidos em θ = 1;",
    "A_cutia = 0.99, A_bugio = 0.44\n")
print(valid %>% mutate(Y1 = map_dbl(Y, 1), Y5 = map_dbl(Y, 5), Y20 = map_dbl(Y, 20)) %>% select(-Y))

# ============================================================
# 2. CANDIDATAS contra a comunidade ATUAL (com bugio e cutia)
# ============================================================
af <- ler_atlantic_frugivory()
candidatas <- read_csv("outputs/candidatas_limpas.csv", show_col_types = FALSE)

linhas_candidata <- function(sp) {
  gen <- unique(af$planta_genero[af$frug == sp])
  plantas %>% filter(genero %in% gen) %>% select(all_of(TRAITS))
}

C_atual <- soma_comunidade()
alvo <- candidatas %>% filter(n_registros_atfrug > 0, !registrada_pnt) %>% pull(nome)

tpd_cand <- map(set_names(alvo), ~ tpd(linhas_candidata(.x)))
n_pontos <- map_int(alvo, ~ nrow(na.omit(linhas_candidata(.x))))
sem_dados <- alvo[map_lgl(tpd_cand, is.null)]
if (length(sem_dados)) message("Poucas plantas do PNT com atributos (< ", MIN_PONTOS, ") — fora: ",
                               paste(sem_dados, collapse = ", "))
tpd_cand <- compact(tpd_cand)

Y_tab <- imap_dfr(tpd_cand, ~ tibble(especie = .y, theta = THETAS, Y = Y_theta(.x, C_atual)))
resumo <- imap_dfr(tpd_cand, ~ tibble(
  especie = .y,
  n_plantas_pnt_com_atributos = n_pontos[match(.y, alvo)],
  Y_theta1  = Y_theta(.x, C_atual, 1),
  Y_theta10 = Y_theta(.x, C_atual, 10),
  Y_theta20 = Y_theta(.x, C_atual, 20),
  A_literal = amplitude(.x),
  A_hdr95   = amplitude(.x, massa = 0.95))) %>%
  arrange(desc(Y_theta1))
print(resumo, width = Inf)
write_csv(Y_tab, "outputs/landim_Y_theta.csv")
write_csv(resumo, "outputs/landim_resumo.csv")

# ============================================================
# 3. SEQUÊNCIA de reintroduções (o artigo sugere esse uso)
# ============================================================
# A cada passo, escolhe a candidata de maior Y_i(θ) e a incorpora à
# comunidade antes do próximo passo. Assim a 2ª escolha já desconta a
# sobreposição com a 1ª. Feito para θ = 1 (ênfase em recursos frequentes)
# e θ = 10 (ênfase em originalidade).
sequencia <- function(theta) {
  Cc <- C_atual; pool <- names(tpd_cand); out <- list()
  while (length(pool)) {
    y <- map_dbl(pool, ~ Y_theta(tpd_cand[[.x]], Cc, theta))
    best <- pool[which.max(y)]
    out[[length(out) + 1]] <- tibble(theta = theta, passo = length(out) + 1, especie = best, Y = max(y))
    Cc <- Cc + tpd_cand[[best]]
    pool <- setdiff(pool, best)
  }
  bind_rows(out)
}
seq_tab <- bind_rows(sequencia(1), sequencia(10))
print(seq_tab, n = Inf)
write_csv(seq_tab, "outputs/landim_sequencia.csv")

# Notas de interpretação
# - Y(θ=1): média da originalidade ponderada pela disponibilidade de
#   frutos — alto = interage muito com atributos comuns no PNT.
# - Inclinação de Y com θ: quão "única" a espécie é em algum ponto do
#   espaço de atributos (complementaridade).
# - O nicho da candidata é construído só com plantas QUE EXISTEM no PNT,
#   então mede o papel potencial NAQUELE lugar, não o nicho global da espécie.
# - Os grupos da comunidade atual são táxons altos (Passeriformes = 86 spp.),
#   como no artigo; a candidata entra como 1 espécie.
