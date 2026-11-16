#################################################################
######## 08 — Candidatas à reintrodução no PNT ##################
#################################################################
# Para cada espécie da lista de candidatas:
#  a) interações potenciais: nº de gêneros de planta registrados no PNT
#     que a espécie consome (ATLANTIC-frugivory, qualquer local da Mata
#     Atlântica; NeoFrugivory só se ela não tiver nenhum registro no
#     ATLANTIC-frugivory — ver coluna `fonte_interacoes`);
#  b) interações únicas: desses gêneros, quantos NÃO têm nenhum
#     dispersor na rede atual do PNT (rede com as reintroduzidas, sem a
#     própria candidata) — i.e., plantas que só ela passaria a dispersar;
#  c) contribuição funcional: framework de Landim et al. 2024
#     (Restoration Ecology, doi:10.1111/rec.14151) — Y_i(θ) e amplitude.
#
# A lista é lida de ARQ_CANDIDATAS; pra uma lista nova é só trocar o
# arquivo (colunas: nome científico na 1ª, nome comum na 2ª).
#
# Saídas: outputs/08_candidatas_limpas.csv
#         outputs/08_contribuicao_candidatas.csv   (a, b, c por espécie)
#         outputs/08_landim_Y_theta.csv            (Y para θ = 1..20)
#         outputs/08_landim_validacao.csv
#         figures/08_landim_Y_theta.png

source("R/funcoes_redes.R")
suppressPackageStartupMessages(library(ks))

ARQ_CANDIDATAS <- "data/candidatas_reintroducao_workshop.csv"

# ============================================================
# 0. Lista de candidatas
# ============================================================
# Correções da lista do workshop: grafia, duplicatas (Bradypus torquatus
# e cateto aparecem duas vezes) e a linha da cutia, que veio com as
# colunas trocadas. As linhas depois da 46ª são anotações, não espécies.
correcoes <- c(
  "Amazona hodochoryta"      = "Amazona rhodocorytha",
  "Brachyteles arachinoides" = "Brachyteles arachnoides",
  "Pecari tacaju"            = "Pecari tajacu",
  "Pyrrhyra cruentata"       = "Pyrrhura cruentata",
  "Brdypus variegatus"       = "Bradypus variegatus",
  "Dasypus septensinctus"    = "Dasypus septemcinctus",
  "Leopardus wieddi"         = "Leopardus wiedii",
  "Procyon cancrivorous"     = "Procyon cancrivorus",
  "Mazama amaricana"         = "Mazama americana",
  "Puma yagouaroundi"        = "Herpailurus yagouaroundi",
  "Cutia"                    = "Dasyprocta leporina"
)
cand_raw <- read_csv(ARQ_CANDIDATAS, show_col_types = FALSE, col_types = cols(.default = "c"))
names(cand_raw)[1:2] <- c("nome_original", "nome_comum")
if (str_detect(ARQ_CANDIDATAS, "workshop")) cand_raw <- cand_raw[1:46, ]

candidatas <- cand_raw %>%
  filter(!is.na(nome_original), nome_original != "") %>%
  mutate(nome_original = str_squish(nome_original),
         nome_comum = if_else(nome_original == "Cutia", "cutia", nome_comum),
         nome_lista = coalesce(unname(correcoes[nome_original]), nome_original)) %>%
  filter(!str_detect(nome_lista, " sp\\.?$")) %>%      # Conepatus sp.: sem espécie, sem como cruzar
  mutate(nome = padronizar_animais(nome_lista)) %>%
  distinct(nome, .keep_all = TRUE) %>%
  select(nome, nome_lista, nome_comum, any_of(c("Ordem de Prioridade", "Observações")))
write_csv(candidatas, "outputs/08_candidatas_limpas.csv")

# Já reintroduzidas no PNT (mesma lista do script 06)
reintroduzidas <- padronizar_animais(c("Ramphastos vitellinus", "Dasyprocta leporina",
                                       "Alouatta guariba", "Chelonoidis denticulatus"))

com <- ler_comunidades()
int <- ler_interacoes(completar_com_neo = c(candidatas$nome, reintroduzidas))
pnt <- sites$inside_pnt
plantas_pnt <- plantas_site(com, pnt)
frug_pnt    <- frugivoros_site(com, pnt)

# Rede atual do PNT (com as reintroduzidas, inclusive o jabuti)
rede_atual <- rede_site(int, com, pnt, forcar = reintroduzidas)

# ============================================================
# 1. (a) interações potenciais e (b) interações únicas
# ============================================================
ab <- candidatas %>%
  mutate(tab = map(nome, function(sp) {
    dieta    <- int %>% filter(frug == sp)
    pot      <- dieta %>% filter(planta_genero %in% plantas_pnt)
    # rede de referência SEM a própria candidata: pra quem já está no
    # PNT, (b) vira "plantas que hoje só ela dispersa"
    outros   <- rede_atual %>% filter(frug != sp)
    unicas   <- setdiff(pot$planta_genero, outros$planta_genero)
    tibble(fonte_interacoes  = paste(unique(dieta$fonte), collapse = "+"),
           n_generos_dieta_total = n_distinct(dieta$planta_genero),
           a_interacoes_potenciais = n_distinct(pot$planta_genero),
           b_interacoes_unicas     = length(unicas),
           b_prop_unicas = ifelse(nrow(pot) > 0, length(unicas) / n_distinct(pot$planta_genero), NA_real_),
           generos_unicos = paste(sort(unicas), collapse = "; "),
           # esforço amostral: espécies muito estudadas acumulam mais
           # interações registradas só por isso — ler (a) à luz disto
           n_estudos = if (nrow(dieta)) max(dieta$n_estudos_frug) else 0L)
  })) %>%
  unnest(tab) %>%
  mutate(registrada_pnt = nome %in% frug_pnt,
         ja_reintroduzida = nome %in% reintroduzidas,
         fonte_interacoes = na_if(fonte_interacoes, ""))

cat("\nSem nenhuma interação registrada (fora de a, b e c):\n")
print(ab %>% filter(a_interacoes_potenciais == 0) %>% select(nome, nome_comum, n_generos_dieta_total), n = Inf)

# ============================================================
# 2. (c) Contribuição funcional — Landim et al. 2024
# ============================================================
# Dados do PNT do repositório do artigo, em data/landim2024/:
#   PlantTraitsPNT.csv — comunidade de recursos (plantas do PNT + atributos)
#   ConsumersPNT.csv   — consumidor x espécie de planta do PNT cujo GÊNERO
#                        está na dieta do consumidor no ATLANTIC-frugivory
#                        (modelo "filogenético" de Mittelman et al. 2022),
#                        com os grupos (TYPE) usados no artigo.
# A candidata entra do mesmo jeito: candidata x espécies do PNT nos
# gêneros da sua dieta.
#
# Equações (numeração do artigo):
#   C   = nicho da comunidade de consumidores: soma das TPDs dos grupos,
#         pesadas pelo nº de espécies do grupo
#   T_i = C + S_i                                (Eq. 2)
#   O_i = S_i / T_i                              (Eq. 3)
#   R   = nicho funcional da comunidade de plantas (Eq. 4)
#   Y_i(θ) = ( ∫ O_i(x)^θ R(x) dx )^(1/θ)        (Eq. 5)
#   A_i = vol{S_i > 0 e R > 0} / vol{R > 0}      (Eq. 6, ver amplitude())
# Atributos: comprimento e diâmetro do fruto e diâmetro da semente
# (comprimento da semente fica fora, r = 0.9 — Supplement S5).
#
# Diferenças em relação ao código do repositório do artigo, que tem
# bugs (`pe()` é chamada mas a função definida é `pe.v`; `comprimento`
# não existe; os Kdes da 1ª seção usam `kde.G1` em vez de `kde.Sp`):
#   - grupos e pesos calculados dos dados, não digitados;
#   - bugio e cutia entram na comunidade ATUAL (já reintroduzidos);
#   - amplitude reportada de dois jeitos (ver amplitude()).

TRAITS <- c("Fruit_length", "Fruit_diameter", "Seed_diameter")
GRID   <- c(61, 87, 84)          # mesmos valores do artigo
THETAS <- 1:20
MIN_PONTOS <- 10                 # mínimo de plantas com atributos pra estimar uma TPD 3D

plantas_tr <- read_csv("data/landim2024/PlantTraitsPNT.csv", show_col_types = FALSE) %>%
  mutate(genero = genero(Esp))
cons <- read_csv("data/landim2024/ConsumersPNT.csv", show_col_types = FALSE,
                 col_types = cols(.default = "c")) %>%
  mutate(across(all_of(TRAITS), as.numeric)) %>%
  filter(!is.na(CONSUMER), !is.na(TYPE))

pl   <- plantas_tr %>% select(all_of(TRAITS)) %>% na.omit() %>% as.data.frame()
XMAX <- apply(pl, 2, max)
VOL  <- prod(XMAX / GRID)        # volume de uma célula, como no artigo

# TPD no grid da comunidade de recursos, normalizada pra integral = 1
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
  # Eq. 6 com a interseção com R no numerador: ao pé da letra
  # (vol{S>0}/vol{R>0}) passa de 1 quando a TPD da espécie tem banda larga
  # e a de R tem zeros numéricos nas bordas (a cutia dava 7.8).
  if (is.null(massa)) return(sum(S > 0 & R_tpd > 0) / sum(R_tpd > 0))
  # Com kernel gaussiano S > 0 em quase todo o grid (caudas minúsculas),
  # então a versão literal tende a ~1 pra quem tem poucos pontos.
  # Alternativa: região de maior densidade (HDR) que contém `massa` da
  # probabilidade, intersectada com a HDR de R.
  hdr <- function(f) {
    o <- sort(as.vector(f), decreasing = TRUE)
    f >= o[which(cumsum(o) * VOL >= massa)[1]]
  }
  hR <- hdr(R_tpd)
  sum(hdr(S) & hR) / sum(hR)
}

grupos <- cons %>% group_by(TYPE) %>% summarise(n_spp = n_distinct(CONSUMER), .groups = "drop")
tpd_grupos <- map(set_names(grupos$TYPE), ~ tpd(filter(cons, TYPE == .x)))
grupos$tem_tpd <- !map_lgl(tpd_grupos, is.null)
print(grupos)

soma_comunidade <- function(excluir = character(0)) {
  g <- grupos %>% filter(tem_tpd, !TYPE %in% excluir)
  Reduce(`+`, map2(g$TYPE, g$n_spp, ~ .y * tpd_grupos[[.x]]))
}

# ---- 2.1 Validação: reproduzir o caso de estudo do artigo ----
# Artigo: cutia > bugio em todo θ; A_cutia = 0.99, A_bugio = 0.44
C_pre <- soma_comunidade(excluir = c("Alouatta guariba", "Dasyprocta leporina"))
validacao <- map_dfr(c("Alouatta guariba", "Dasyprocta leporina"), function(sp) {
  y <- Y_theta(tpd_grupos[[sp]], C_pre)
  tibble(especie = sp, Y_theta1 = y[1], Y_theta5 = y[5], Y_theta20 = y[20],
         A_literal = amplitude(tpd_grupos[[sp]]))
})
cat("\nValidação (artigo: cutia > bugio em todo θ; A_cutia = 0.99, A_bugio = 0.44):\n")
print(validacao)
write_csv(validacao, "outputs/08_landim_validacao.csv")

# ---- 2.2 Candidatas contra a comunidade atual ----
linhas_candidata <- function(sp) {
  gen <- unique(int$planta_genero[int$frug == sp])
  plantas_tr %>% filter(genero %in% gen) %>% select(all_of(TRAITS))
}
alvo <- ab %>% filter(a_interacoes_potenciais > 0) %>% pull(nome)
c_res <- map_dfr(alvo, function(sp) {
  x <- linhas_candidata(sp)
  S <- tpd(x)
  base <- tibble(nome = sp, n_plantas_pnt_com_atributos = nrow(na.omit(x)))
  if (is.null(S)) return(base)
  # se a candidata já é um grupo da comunidade (bugio, cutia), ela sai
  # de C pra não competir com ela mesma
  Cc <- soma_comunidade(excluir = sp)
  y  <- Y_theta(S, Cc)
  base %>% mutate(c_Y_theta1 = y[1], c_Y_theta10 = y[10], c_Y_theta20 = y[20],
                  c_inclinacao_Y = y[20] - y[1],
                  c_amplitude_literal = amplitude(S),
                  c_amplitude_hdr95 = amplitude(S, massa = 0.95),
                  Y = list(tibble(theta = THETAS, Y = y)))
})
sem_tpd <- c_res$nome[is.na(c_res$c_Y_theta1)]
if (length(sem_tpd)) message("Menos de ", MIN_PONTOS, " plantas do PNT com atributos — sem (c): ",
                             paste(sem_tpd, collapse = ", "))

Y_tab <- c_res %>% filter(!is.na(c_Y_theta1)) %>% select(nome, Y) %>% unnest(Y)
write_csv(Y_tab, "outputs/08_landim_Y_theta.csv")

# ============================================================
# 3. Tabela final
# ============================================================
contribuicao <- ab %>%
  left_join(select(c_res, -any_of("Y")), by = "nome") %>%
  mutate(nota = case_when(
    ja_reintroduzida ~ "já reintroduzida no PNT: (b) = plantas que hoje só ela dispersa",
    registrada_pnt   ~ "já registrada no PNT: (b) = plantas que hoje só ela dispersa",
    nome == "Callithrix jacchus" ~ "exótica invasora no RJ — conferir por que está na lista",
    str_detect(nome, "^(Ara|Amazona|Pyrrhura) ") ~ "psitacídeo: muitas vezes predador de semente, não dispersor",
    fonte_interacoes == "NeoFrugivory" ~ "interações de fora da Mata Atlântica (NeoFrugivory)",
    TRUE ~ NA_character_)) %>%
  arrange(desc(b_interacoes_unicas), desc(a_interacoes_potenciais))
# Nota: pra quem já é parte de um grupo agregado do ConsumersPNT (ex.:
# macaco-prego em "Primates"), não dá pra tirar a espécie de C; o (c)
# dela subestima a contribuição.

print(contribuicao %>% select(nome, registrada_pnt, a_interacoes_potenciais, b_interacoes_unicas,
                              c_Y_theta1, c_Y_theta20, c_amplitude_hdr95, fonte_interacoes),
      n = Inf, width = Inf)
write_csv(contribuicao, "outputs/08_contribuicao_candidatas.csv")

# Figura: Y(θ) por candidata (Fig. 2 do artigo)
if (nrow(Y_tab)) {
  p <- ggplot(Y_tab, aes(theta, Y, group = nome)) +
    geom_line(colour = "grey60") +
    geom_text(data = filter(Y_tab, theta == max(THETAS)), aes(label = nome),
              hjust = -0.05, size = 2.6, fontface = "italic") +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.45))) +
    labs(x = expression(theta), y = expression(Y[i](theta)),
         title = "Contribuição funcional potencial das candidatas no PNT (Landim et al. 2024)") +
    theme_minimal()
  ggsave("figures/08_landim_Y_theta.png", p, width = 9, height = 6, dpi = 300, bg = "white")
}
