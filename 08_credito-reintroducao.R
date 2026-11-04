#################################################################
######## 08 — Crédito de interações das candidatas (PNT) ########
#################################################################
# Aplica a ideia de "credit of ecological interactions" (Genes et al.
# 2017, Ecol Evol, doi:10.1002/ece3.2746) à lista de candidatas do
# workshop: quantas interações cada espécie poderia restaurar com as
# plantas que JÁ existem no PNT, e quais delas são complementares às
# dos frugívoros atuais.
#
# Crédito aqui = interações potenciais (ATLANTIC-frugivory, qualquer
# local da Mata Atlântica) com gêneros de planta registrados no PNT.
# Isso é crédito POTENCIAL: o realizado depende da abundância da
# reintroduzida e do "rewiring time" (Genes et al. 2017). Pra
# aproximar do realizado com dados de ocorrência + atributos, ver
# Marjakangas et al. 2018 (Phil Trans B), citado na planilha.
#
# Saídas: outputs/candidatas_limpas.csv, outputs/credito_candidatas.csv,
#         outputs/portfolio_reintroducao.csv

source("R/funcoes_meridian.R")

presenca  <- readRDS("outputs/presenca_site.rds")
frugivoros <- readRDS("outputs/frugivoros.rds")
int       <- readRDS("outputs/interacoes_mistas.rds")
af        <- ler_atlantic_frugivory()

# ------------------------------------------------------------
# 1. Limpar a lista do workshop
# ------------------------------------------------------------
# Correções de grafia + duplicatas (Bradypus torquatus e cateto
# aparecem duas vezes; a linha da cutia veio com colunas trocadas).
cand_raw <- read_csv("data/candidatas_reintroducao_workshop.csv", show_col_types = FALSE, n_max = 46,
                     col_types = cols(.default = "c"))
names(cand_raw)[c(1, 2, 11, 12)] <- c("nome_original", "nome_comum", "prioridade", "obs")  # evita acentos nos nomes de coluna

correcoes <- c(
  "Amazona hodochoryta"       = "Amazona rhodocorytha",
  "Brachyteles arachinoides"  = "Brachyteles arachnoides",
  "Pecari tacaju"             = "Pecari tajacu",
  "Pyrrhyra cruentata"        = "Pyrrhura cruentata",
  "Brdypus variegatus"        = "Bradypus variegatus",
  "Dasypus septensinctus"     = "Dasypus septemcinctus",
  "Leopardus wieddi"          = "Leopardus wiedii",
  "Procyon cancrivorous"      = "Procyon cancrivorus",
  "Mazama amaricana"          = "Mazama americana",
  "Puma yagouaroundi"         = "Herpailurus yagouaroundi",
  "Cutia"                     = "Dasyprocta leporina"
)

candidatas <- cand_raw %>%
  filter(!is.na(nome_original), nome_original != "") %>%
  mutate(nome_original = str_squish(nome_original),
         nome = coalesce(unname(correcoes[nome_original]), nome_original),
         nome_comum = if_else(nome_original == "Cutia", "cutia", nome_comum),
         prioridade_workshop = prioridade,
         obs_workshop = obs) %>%
  filter(!str_detect(nome, " sp\\.?$")) %>%           # Conepatus sp. — sem espécie, sem como cruzar
  distinct(nome, .keep_all = TRUE) %>%
  select(nome, nome_comum, prioridade_workshop, obs_workshop)

frug_pnt   <- presenca %>% filter(site == "inside_pnt", grupo %in% c("birds", "mammals")) %>% pull(nome) %>% unique()
plantas_pnt <- presenca %>% filter(site == "inside_pnt", grupo == "plants") %>% pull(genero) %>% unique()

# dieta direto do EltonTraits (várias candidatas não têm registro no
# Rio e por isso não estão em `frugivoros`)
elton_dieta <- bind_rows(
  read_tsv("data/Elton Traits/BirdFuncDat.txt", locale = locale(encoding = "latin1"),
           show_col_types = FALSE, col_select = c(Scientific, `Diet-Fruit`)),
  read_tsv("data/Elton Traits/MamFuncDat.txt", locale = locale(encoding = "latin1"),
           show_col_types = FALSE, col_select = c(Scientific, `Diet-Fruit`))) %>%
  transmute(nome = str_squish(Scientific), dieta_fruto = `Diet-Fruit`) %>%
  distinct(nome, .keep_all = TRUE) %>%
  bind_rows(tibble(nome = c("Herpailurus yagouaroundi", "Sapajus nigritus", "Alouatta guariba"),
                   dieta_fruto = NA_real_)) %>%      # preenchidas abaixo via sinônimo
  distinct(nome, .keep_all = TRUE)
sin_elton <- c("Herpailurus yagouaroundi" = "Puma yagouaroundi", "Sapajus nigritus" = "Cebus nigritus",
               "Alouatta guariba" = "Alouatta guariba", "Leopardus guttulus" = "Leopardus tigrinus")
elton_dieta <- elton_dieta %>%
  mutate(dieta_fruto = coalesce(dieta_fruto,
                                elton_dieta$dieta_fruto[match(sin_elton[nome], elton_dieta$nome)]))

# EltonTraits dá 0% de fruto pra espécies sabidamente frugívoras
# (erro conhecido da base); não invento um valor, só marco.
frugivoros_elton_errado <- c("Mazama americana", "Tapirus terrestris")

candidatas <- candidatas %>%
  left_join(elton_dieta, by = "nome") %>%
  mutate(
    registrada_pnt = nome %in% frug_pnt,
    n_registros_atfrug = map_int(nome, ~ sum(int$n_registros[int$frug == .x])),
    status_rede = case_when(
      n_registros_atfrug > 0 ~ "com interações no ATLANTIC-frugivory",
      (dieta_fruto >= 10) %in% TRUE | nome %in% frugivoros_elton_errado ~ "frugívora sem interação registrada -> script 10",
      TRUE ~ "não-frugívora (fora da análise de rede)"),
    nota = case_when(
      nome == "Callithrix jacchus" ~ "exótica invasora no RJ — conferir por que está na lista",
      nome %in% frugivoros_elton_errado ~ "EltonTraits diz 0% de fruto (erro da base) — tratada como frugívora",
      nome == "Leopardus tigrinus" ~ "no RJ provavelmente é L. guttulus (split de 2013); conferir",
      str_detect(nome, "^(Ara|Amazona|Pyrrhura) ") ~ "psitacídeo: frequentemente predador de semente, não dispersor",
      registrada_pnt ~ "já tem registro no PNT (reforço populacional, não reintrodução?)",
      TRUE ~ NA_character_)
  )
write_csv(candidatas, "outputs/candidatas_limpas.csv")
print(count(candidatas, status_rede))

# ------------------------------------------------------------
# 2. Rede atual do PNT e plantas "órfãs"
# ------------------------------------------------------------
frug_atuais <- frugivoros %>% filter(frugivoro, nome %in% frug_pnt) %>% pull(nome)
rede_atual  <- rede_do_site(int, frug_atuais, plantas_pnt)

# gêneros do PNT com algum registro de dispersão por animal no
# ATLANTIC-frugivory = proxy de zoocoria (enquanto não temos síndrome)
plantas_zoo_pnt <- intersect(plantas_pnt, int$planta_genero)

dispersores_atuais <- tibble(planta_genero = plantas_zoo_pnt) %>%
  left_join(count(rede_atual, planta_genero, name = "n_disp"), by = "planta_genero") %>%
  mutate(n_disp = coalesce(n_disp, 0L))

semente <- af %>%
  group_by(planta_genero) %>%
  summarise(semente_mm = mean(seed_diameter, na.rm = TRUE),
            fruto_mm   = mean(fruit_diameter, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(c(semente_mm, fruto_mm), ~ if_else(is.nan(.x), NA_real_, .x)))

dispersores_atuais <- dispersores_atuais %>% left_join(semente, by = "planta_genero")
cat("Gêneros zoocóricos no PNT:", nrow(dispersores_atuais),
    "| sem nenhum dispersor registrado no PNT:", sum(dispersores_atuais$n_disp == 0), "\n")

# ------------------------------------------------------------
# 3. Crédito por candidata
# ------------------------------------------------------------
LIMIAR_SEMENTE_GRANDE <- 12   # mm; limiar usado em Galetti et al. 2013 (Science) — confirmar

credito <- candidatas %>%
  filter(n_registros_atfrug > 0) %>%
  mutate(tab = map(nome, function(sp) {
    parc <- int %>% filter(frug == sp, planta_genero %in% plantas_pnt) %>%
      left_join(dispersores_atuais, by = "planta_genero")
    tibble(
      credito_links       = nrow(parc),
      # se a candidata já está no PNT esses links já existem; por isso
      # o crédito "novo" é só o que não está na rede atual
      credito_links_novos = sum(!paste(sp, parc$planta_genero) %in% paste(rede_atual$frug, rede_atual$planta_genero)),
      plantas_orfas       = sum(parc$n_disp == 0, na.rm = TRUE),
      plantas_baixa_redund = sum(parc$n_disp <= 2, na.rm = TRUE),
      # crédito ponderado por complementaridade: um link vale mais
      # quanto menos dispersores a planta já tem
      credito_ponderado   = sum(1 / (parc$n_disp + 1), na.rm = TRUE),
      sementes_grandes    = sum(parc$semente_mm >= LIMIAR_SEMENTE_GRANDE, na.rm = TRUE),
      prop_links_2estudos = mean(parc$n_estudos >= 2),
      # esforço amostral: espécies muito estudadas (muriqui!) acumulam
      # mais interações registradas só por isso. Olhe o crédito à luz disto.
      n_estudos_total     = n_distinct(af$`Study reference`[af$frug == sp])
    )
  })) %>%
  unnest(tab) %>%
  arrange(desc(credito_ponderado))

print(credito %>% select(nome, registrada_pnt, credito_links, credito_links_novos,
                         plantas_orfas, credito_ponderado, sementes_grandes, prop_links_2estudos),
      n = Inf)
write_csv(credito, "outputs/credito_candidatas.csv")

# ------------------------------------------------------------
# 4. Portfólio: qual COMBINAÇÃO de espécies restaura mais?
# ------------------------------------------------------------
# Mittelman et al. 2022 mostram que reintroduções múltiplas somam mais
# que as individuais (efeitos indiretos). Aqui a pergunta é mais
# simples: dado que as candidatas se sobrepõem nas plantas que comem,
# qual sequência maximiza a cobertura da comunidade de plantas?
# Objetivo com retorno decrescente: sum_p log(1 + n_dispersores_p).
# Ganha mais quem resgata órfãs / plantas com poucos dispersores.
# Só entram candidatas ainda ausentes do PNT.

objetivo <- function(n_disp) sum(log1p(n_disp))

pool <- credito %>% filter(!registrada_pnt) %>% pull(nome)
estado <- dispersores_atuais %>% select(planta_genero, n_disp)
escolhidas <- character(0)
passos <- list()
while (length(pool) > 0) {
  ganhos <- map_dbl(pool, function(sp) {
    novo <- estado %>%
      mutate(n_disp = n_disp + planta_genero %in% int$planta_genero[int$frug == sp])
    objetivo(novo$n_disp) - objetivo(estado$n_disp)
  })
  melhor <- pool[which.max(ganhos)]
  estado <- estado %>% mutate(n_disp = n_disp + planta_genero %in% int$planta_genero[int$frug == melhor])
  escolhidas <- c(escolhidas, melhor)
  passos[[length(passos) + 1]] <- tibble(
    passo = length(escolhidas), especie = melhor, ganho = max(ganhos),
    orfas_restantes = sum(estado$n_disp == 0))
  pool <- setdiff(pool, melhor)
}
portfolio <- bind_rows(passos)
print(portfolio)
write_csv(portfolio, "outputs/portfolio_reintroducao.csv")
# Interpretação: se o ganho cai rápido depois das 2-3 primeiras,
# as demais candidatas são redundantes com elas (pelo menos em termos
# de quais plantas comem). Isso não captura quantidade/qualidade de
# dispersão — um muriqui e um sagui "comendo" o mesmo gênero não são
# equivalentes. Dá pra ponderar por massa corporal ou tamanho de semente.
