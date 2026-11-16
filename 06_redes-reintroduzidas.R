#################################################################
######## 06 — Rede do PNT com e sem as espécies reintroduzidas ##
#################################################################
# Rede de frugivoria do Parque Nacional da Tijuca (frugívoro espécie x
# planta gênero, como a rede "mista" do 04) COM as espécies
# reintroduzidas (destacadas na figura) e SEM elas.
#
# Reintroduzidas = as quatro analisadas em Mittelman et al. 2022
# (Ecography, doi:10.1111/ecog.05838). Se houver outra reintrodução que
# deva entrar, é só acrescentar uma linha em `reintroduzidas`.
#
# Interações: ATLANTIC-frugivory. O jabuti não tem nenhum registro lá
# (Mittelman et al. fizeram revisão de literatura pra ele); aqui uso o
# NeoFrugivory (Fuzessy & Pizo 2025), que cobre a região Neotropical
# inteira, não só a Mata Atlântica. Comunidades não tem répteis, então o
# jabuti é forçado como presente no PNT.
#
# Nulo (como no artigo): remover k frugívoros NÃO reintroduzidos
# sorteados ao acaso, com k = nº de reintroduzidas removidas. z > 0 =
# as reintroduzidas mudam a rede mais do que k espécies quaisquer.
#
# Saídas: outputs/06_metricas_com_sem_reintroduzidas.csv
#         outputs/06_contribuicao_por_reintroduzida.csv
#         outputs/06_rede_pnt_com_reintroduzidas.csv (lista de links)
#         figures/06_rede_pnt_reintroduzidas.png / .pdf

source("R/funcoes_redes.R")
set.seed(42)
N_NULOS <- 100   # o artigo usou 100

reintroduzidas <- tribble(
  ~nome,                      ~nome_comum,
  "Ramphastos vitellinus",    "tucano-de-bico-preto (R. v. ariel)",
  "Dasyprocta leporina",      "cutia",
  "Alouatta guariba",         "bugio-ruivo",
  "Chelonoidis denticulatus", "jabuti-tinga"
) %>%
  mutate(nome = padronizar_animais(nome))

com <- ler_comunidades()
int <- ler_interacoes(completar_com_neo = reintroduzidas$nome)

# ------------------------------------------------------------
# 1. Rede COM reintroduzidas
# ------------------------------------------------------------
pnt   <- sites$inside_pnt
el    <- rede_site(int, com, pnt, forcar = reintroduzidas$nome)
B     <- para_matriz(el)

reintroduzidas <- reintroduzidas %>%
  mutate(registrada_pnt = nome %in% frugivoros_site(com, pnt),
         fonte_interacoes = map_chr(nome, ~ paste(unique(int$fonte[int$frug == .x]), collapse = "+")),
         na_rede = nome %in% rownames(B))
print(reintroduzidas)
if (any(!reintroduzidas$na_rede))
  warning("Fora da rede (sem interação com plantas do PNT): ",
          paste(reintroduzidas$nome[!reintroduzidas$na_rede], collapse = ", "), call. = FALSE)

write_csv(el %>% mutate(reintroduzida = frug %in% reintroduzidas$nome),
          "outputs/06_rede_pnt_com_reintroduzidas.csv")

# ------------------------------------------------------------
# 2. Métricas com vs sem (todas juntas e cada uma sozinha)
# ------------------------------------------------------------
outras <- setdiff(rownames(B), reintroduzidas$nome)
cenarios <- c(list(todas = reintroduzidas$nome),
              set_names(as.list(reintroduzidas$nome), reintroduzidas$nome))

metricas <- imap_dfr(cenarios, ~ comparar_remocao(B, remover = .x, pool = outras,
                                                  n_nulos = N_NULOS, rotulo = .y),
                     .progress = TRUE)
print(metricas %>% select(cenario, metrica, com, sem, delta, z, p_nulo_maior_igual),
      n = Inf, width = Inf)
write_csv(metricas, "outputs/06_metricas_com_sem_reintroduzidas.csv")

# ------------------------------------------------------------
# 3. Contribuição de cada reintroduzida
# ------------------------------------------------------------
# links: gêneros do PNT com que a espécie interage
# exclusivas_sozinha: plantas dispersas SÓ por essa espécie na rede
# exclusivas_reintroduzidas: plantas dispersas só por reintroduzidas
#   (por qualquer uma delas, mas por nenhum frugívoro "original")
disp_por_planta <- el %>% group_by(planta_genero) %>%
  summarise(n_disp = n(), so_reintro = all(frug %in% reintroduzidas$nome), .groups = "drop")

contribuicao <- reintroduzidas %>%
  mutate(tab = map(nome, function(sp) {
    p <- el %>% filter(frug == sp) %>% left_join(disp_por_planta, by = "planta_genero")
    tibble(n_links = nrow(p),
           exclusivas_sozinha = sum(p$n_disp == 1),
           exclusivas_reintroduzidas = sum(p$so_reintro),
           plantas_exclusivas = paste(sort(p$planta_genero[p$n_disp == 1]), collapse = "; "))
  })) %>%
  unnest(tab)
print(contribuicao, width = Inf)
write_csv(contribuicao, "outputs/06_contribuicao_por_reintroduzida.csv")

# ------------------------------------------------------------
# 4. Figura
# ------------------------------------------------------------
p <- plot_com_sem(el, destaque = reintroduzidas$nome,
                  titulo = "Rede de frugivoria do Parque Nacional da Tijuca",
                  rotulo_com = "Com as reintroduzidas", rotulo_sem = "Sem as reintroduzidas",
                  rotulo_destaque = "reintroduzida")
ggsave("figures/06_rede_pnt_reintroduzidas.png", p, width = 14, height = 7.5, dpi = 300, bg = "white")
ggsave("figures/06_rede_pnt_reintroduzidas.pdf", p, width = 14, height = 7.5)
