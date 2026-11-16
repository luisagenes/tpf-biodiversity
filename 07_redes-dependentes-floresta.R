#################################################################
######## 07 — Redes com e sem as espécies dependentes de floresta
#################################################################
# Pergunta: o que a rede de frugivoria de cada área (e do Rio inteiro)
# perde sem os frugívoros que dependem de floresta — i.e., as espécies
# que, plausivelmente, só persistem por causa das UCs?
#
# Dados de dependência (IUCN, compilados pela aluna), duas listas:
#   data/especies_exclusivas_floresta.csv      — TODOS os habitats da
#       espécie na IUCN são florestais (tem_outro_habitat = FALSE)
#   data/especies_floresta_maior_importancia.csv — floresta marcada como
#       habitat de "maior importância" na IUCN
# As listas não são aninhadas: 14 espécies só-floresta (ex.: paca,
# veado-mateiro, Sturnira lilium) não aparecem na lista de "maior
# importância". Por isso:
#   estrito = só-floresta
#   amplo   = só-floresta  OU  floresta de maior importância (união)
# As listas só trazem as espécies dependentes. Quem não está na lista
# conta como NÃO dependente — inclusive espécies sem avaliação da IUCN.
#
# Nulo: remover o mesmo nº de frugívoros NÃO dependentes, sorteados ao
# acaso. z < 0 em plantas_com_dispersor / NODF / robustez = perder as
# dependentes de floresta desestrutura a rede mais do que perder
# espécies quaisquer.
#
# Saídas: outputs/07_cobertura_dependencia.csv
#         outputs/07_metricas_com_sem_dependentes.csv
#         figures/07_rede_<site>_<criterio>.png / .pdf

source("R/funcoes_redes.R")
set.seed(42)
N_NULOS <- 100

# ============================================================
# CONFIGURAÇÃO
# ============================================================
ARQ_EXCLUSIVAS <- "data/especies_exclusivas_floresta.csv"
ARQ_MAIOR_IMP  <- "data/especies_floresta_maior_importancia.csv"

SITES_ANALISE <- c("rio_total", "outside_ucs", "inside_ucs",
                   "inside_pnt", "inside_pedrabranca", "inside_mendanha")
SITES_FIGURA  <- c("rio_total", "inside_ucs", "outside_ucs")

# ============================================================
# 1. Listas de dependentes
# ============================================================
# Lê só a 1ª coluna (nome da espécie). O cabeçalho do arquivo de
# exclusivas veio com as aspas quebradas (a linha inteira entre aspas),
# então não dá pra confiar no read_csv aqui.
ler_lista <- function(arq) {
  if (!file.exists(arq)) stop("Não achei ", arq, call. = FALSE)
  x <- read_lines(arq, skip = 1)
  x <- str_squish(str_remove_all(word(x, 1, sep = fixed(",")), '"'))
  unique(x[x != ""])
}
exclusivas <- ler_lista(ARQ_EXCLUSIVAS)
maior_imp  <- ler_lista(ARQ_MAIOR_IMP)

criterios <- list(
  estrito = exclusivas,
  amplo   = union(exclusivas, maior_imp)
) %>% map(~ unique(na.omit(padronizar_animais(binomio(.x)))))
cat("Espécies dependentes de floresta — estrito:", length(criterios$estrito),
    "| amplo:", length(criterios$amplo), "\n")

com <- ler_comunidades()
int <- ler_interacoes()

# ============================================================
# 2. Quantas dependentes estão de fato nas redes?
# ============================================================
# Muitas espécies das listas não são frugívoras (não estão no
# ATLANTIC-frugivory) e por isso nunca entram na rede.
redes <- map(set_names(SITES_ANALISE), ~ rede_site(int, com, sites[[.x]]))
cobertura <- imap_dfr(redes, function(el, s) {
  f <- unique(el$frug)
  imap_dfr(criterios, ~ tibble(
    site = s, criterio = .y, n_frugivoros_rede = length(f),
    n_dependentes_na_rede = sum(f %in% .x),
    dependentes_na_rede = paste(sort(intersect(f, .x)), collapse = "; ")))
})
print(cobertura %>% select(-dependentes_na_rede))
write_csv(cobertura, "outputs/07_cobertura_dependencia.csv")

# ============================================================
# 3. Com vs sem dependentes de floresta, por site e critério
# ============================================================
metricas <- imap_dfr(criterios, function(dep, crit) {
  imap_dfr(redes, function(el, s) {
    B <- para_matriz(el)
    comparar_remocao(B, remover = dep, pool = setdiff(rownames(B), dep),
                     n_nulos = N_NULOS, rotulo = s) %>%
      mutate(criterio = crit, .before = 1)
  }, .progress = paste("critério", crit))
}) %>%
  rename(site = cenario)
print(metricas %>% select(criterio, site, metrica, com, sem, delta, z, p_nulo_maior_igual),
      n = Inf, width = Inf)
write_csv(metricas, "outputs/07_metricas_com_sem_dependentes.csv")

# ============================================================
# 4. Figuras
# ============================================================
for (crit in names(criterios)) for (s in SITES_FIGURA) {
  el  <- redes[[s]]
  dep <- criterios[[crit]]
  if (!any(el$frug %in% dep)) next
  p <- plot_com_sem(el, destaque = dep,
                    titulo = paste0("Rede de frugivoria — ", s, " (critério ", crit, ")"),
                    rotulo_com = "Com as dependentes de floresta",
                    rotulo_sem = "Sem as dependentes de floresta",
                    rotulo_destaque = "dependente de floresta",
                    cor_destaque = "#d95f02",
                    rotular = sum(unique(el$frug) %in% dep) <= 25)
  arq <- paste0("figures/07_rede_", s, "_", crit)
  ggsave(paste0(arq, ".png"), p, width = 14, height = 7.5, dpi = 300, bg = "white")
  ggsave(paste0(arq, ".pdf"), p, width = 14, height = 7.5)
}
