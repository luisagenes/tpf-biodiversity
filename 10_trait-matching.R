#################################################################
######## 10 — Imputação de interações por atributos #############
#################################################################
# Problema: várias candidatas (e ~40% dos frugívoros do Rio) não têm
# nenhuma interação no ATLANTIC-frugivory. Em vez de construir a rede
# INTEIRA por atributos (o que gera redes muito densas e circulares),
# a proposta é híbrida:
#   - interação observada quando existe (ATLANTIC-frugivory);
#   - probabilidade prevista por atributos SÓ para as espécies sem dado.
#
# Modelo: GLM binomial treinado no próprio ATLANTIC-frugivory.
#   presenças  = pares frugívoro x gênero registrados;
#   "ausências" = pares que aparecem juntos em pelo menos um estudo
#                 (os dois foram observados) mas nunca interagindo.
#   Isso NÃO é ausência verdadeira — é o melhor que dá com dados de
#   literatura. Estudos "animal-oriented" quase não geram ausências.
# Atributos:
#   aves: largura do bico (AVONET Beak.Width, proxy de gape), massa,
#         diâmetro do fruto, e o "excesso" log(fruto/bico) quando o
#         fruto é maior que o bico (barreira morfológica);
#   mamíferos: massa x diâmetro do fruto (não há gape nas bases).
#   planta: forma de vida (árvore/arbusto/palmeira/liana...).
#   + % de fruto na dieta (EltonTraits) nos dois — sem isso o modelo
#     vira "bicho grande come tudo" e prevê dezenas de links pra onça.
# Validação: AUC com validação cruzada por ESPÉCIE de frugívoro
#   (a pergunta é prever espécies novas, então não pode vazar).
#
# Próximo passo natural: versão brms com efeito aleatório de família
# do frugívoro e da planta (filogenia ajuda muito aqui).
#
# Saídas: outputs/trait_model_auc.csv, outputs/links_imputados.csv

source("R/funcoes_meridian.R")
set.seed(42)

af  <- ler_atlantic_frugivory()
int <- readRDS("outputs/interacoes_mistas.rds")
presenca   <- readRDS("outputs/presenca_site.rds")
candidatas <- read_csv("outputs/candidatas_limpas.csv", show_col_types = FALSE)

# ------------------------------------------------------------
# Atributos
# ------------------------------------------------------------
elton <- bind_rows(
  read_tsv("data/Elton Traits/BirdFuncDat.txt", locale = locale(encoding = "latin1"), show_col_types = FALSE,
           col_select = c(Scientific, `Diet-Fruit`, `BodyMass-Value`)),
  read_tsv("data/Elton Traits/MamFuncDat.txt", locale = locale(encoding = "latin1"), show_col_types = FALSE,
           col_select = c(Scientific, `Diet-Fruit`, `BodyMass-Value`))) %>%
  transmute(frug = str_squish(Scientific), massa_elton = `BodyMass-Value`, dieta_elton = `Diet-Fruit`) %>%
  distinct(frug, .keep_all = TRUE)
avonet <- read_csv("data/AVONET/ELEData/ELEData/TraitData/AVONET1_BirdLife.csv", show_col_types = FALSE,
                   col_select = c(Species1, Beak.Width, Mass)) %>%
  rename(frug = Species1)

# sinônimos pra achar atributos das candidatas com nome novo
nomes_atrib <- c("Pipile jacutinga" = "Aburria jacutinga", "Herpailurus yagouaroundi" = "Puma yagouaroundi",
                 "Sapajus nigritus" = "Cebus nigritus", "Alouatta guariba" = "Alouatta guariba")

frug_traits <- af %>%
  group_by(frug) %>%
  summarise(classe = first(Frug_Class), massa_af = median(Frug_Body_Mass, na.rm = TRUE), .groups = "drop") %>%
  full_join(tibble(frug = candidatas$nome), by = "frug") %>%
  mutate(nome_busca = coalesce(unname(nomes_atrib[frug]), frug)) %>%
  left_join(avonet, by = c("frug" = "frug")) %>%
  left_join(avonet %>% rename(bw2 = Beak.Width, m2 = Mass), by = c("nome_busca" = "frug")) %>%
  left_join(elton, by = "frug") %>%
  left_join(elton %>% rename(me2 = massa_elton, de2 = dieta_elton), by = c("nome_busca" = "frug")) %>%
  mutate(bico  = coalesce(Beak.Width, bw2),
         massa = coalesce(Mass, m2, massa_elton, me2, if_else(is.nan(massa_af), NA_real_, massa_af)),
         dieta = coalesce(dieta_elton, de2) / 100,
         classe = case_when(!is.na(classe) ~ classe,
                            !is.na(bico) ~ "Aves",
                            TRUE ~ "Mammalia")) %>%
  select(frug, classe, massa, bico, dieta)

plant_traits <- af %>%
  group_by(planta_genero) %>%
  summarise(fruto  = mean(fruit_diameter, na.rm = TRUE),
            semente = mean(seed_diameter, na.rm = TRUE),
            forma  = names(sort(table(Plant_Form), decreasing = TRUE))[1] %||% NA_character_,
            .groups = "drop") %>%
  mutate(across(c(fruto, semente), ~ if_else(is.nan(.x), NA_real_, .x)),
         forma = case_when(forma %in% c("tree", "scrub", "palm", "liana") ~ forma,
                           TRUE ~ "outra"))

# ------------------------------------------------------------
# Pares de treino
# ------------------------------------------------------------
pares <- af %>%
  distinct(estudo = `Study reference`, frug, planta_genero) %>%
  group_by(estudo) %>%
  reframe(expand_grid(frug = unique(frug), planta_genero = unique(planta_genero))) %>%
  distinct(frug, planta_genero) %>%
  mutate(y = as.integer(paste(frug, planta_genero) %in% paste(int$frug, int$planta_genero))) %>%
  left_join(frug_traits, by = "frug") %>%
  left_join(plant_traits, by = "planta_genero") %>%
  filter(classe %in% c("Aves", "Mammalia"), !is.na(massa), !is.na(fruto), !is.na(dieta)) %>%
  mutate(l_massa = log(massa), l_fruto = log(fruto),
         l_bico = log(bico),
         excesso = pmax(0, l_fruto - l_bico))
cat("Pares de treino:", nrow(pares), "| prevalência:", round(mean(pares$y), 3), "\n")
print(count(pares, classe, y))

formulas <- list(
  Aves     = y ~ dieta + l_bico + l_massa + l_fruto + excesso + forma,
  Mammalia = y ~ dieta + l_massa * l_fruto + forma
)

auc <- function(y, p) {               # AUC = estatística U de Mann-Whitney
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

ajustar <- function(cl, k = 5) {
  d <- pares %>% filter(classe == cl)
  if (cl == "Aves") d <- filter(d, !is.na(bico))
  esp <- unique(d$frug); dobra <- sample(rep_len(1:k, length(esp))); names(dobra) <- esp
  d$dobra <- dobra[d$frug]
  pred <- numeric(nrow(d))
  for (i in 1:k) {
    fit <- glm(formulas[[cl]], family = binomial, data = d[d$dobra != i, ])
    pred[d$dobra == i] <- predict(fit, d[d$dobra == i, ], type = "response")
  }
  fit_final <- glm(formulas[[cl]], family = binomial, data = d)
  # limiar que maximiza TSS (sensibilidade + especificidade − 1)
  lim <- seq(0.01, 0.99, by = 0.01)
  tss <- map_dbl(lim, ~ mean(pred[d$y == 1] >= .x) + mean(pred[d$y == 0] < .x) - 1)
  list(fit = fit_final,
       resumo = tibble(classe = cl, n_pares = nrow(d), n_especies = length(esp),
                       auc_cv_por_especie = auc(d$y, pred),
                       limiar_tss = lim[which.max(tss)], tss = max(tss)))
}

modelos <- map(set_names(c("Aves", "Mammalia")), ajustar)
resumo  <- map_dfr(modelos, "resumo")
print(resumo)
print(map(modelos, ~ summary(.x$fit)$coefficients %>% round(3)))
write_csv(resumo, "outputs/trait_model_auc.csv")
# AUC ~0.5 = atributos não preveem nada; > 0.7 já é útil pra triagem.

# ------------------------------------------------------------
# Previsão: candidatas frugívoras SEM interações x plantas do PNT
# ------------------------------------------------------------
plantas_pnt <- presenca %>% filter(site == "inside_pnt", grupo == "plants") %>% pull(genero) %>% unique()

alvo <- candidatas %>%
  filter(str_detect(status_rede, "script 10") |
           (n_registros_atfrug > 0 & n_registros_atfrug < 5 & (dieta_fruto >= 10) %in% TRUE)) %>%
  pull(nome)

novos <- expand_grid(frug = alvo, planta_genero = intersect(plantas_pnt, plant_traits$planta_genero)) %>%
  left_join(frug_traits, by = "frug") %>%
  left_join(plant_traits, by = "planta_genero") %>%
  filter(!is.na(fruto), !is.na(massa), !is.na(dieta)) %>%
  mutate(l_massa = log(massa), l_fruto = log(fruto), l_bico = log(bico),
         excesso = pmax(0, l_fruto - l_bico))

sem_atributo <- setdiff(alvo, unique(novos$frug))
if (length(sem_atributo)) message("Sem atributos suficientes pra prever: ", paste(sem_atributo, collapse = ", "))

links_imp <- novos %>%
  group_split(classe) %>%
  map_dfr(function(d) {
    cl <- d$classe[1]
    if (cl == "Aves") d <- filter(d, !is.na(bico))
    d %>% mutate(p = predict(modelos[[cl]]$fit, d, type = "response"),
                 previsto = p >= modelos[[cl]]$resumo$limiar_tss)
  }) %>%
  mutate(observado = paste(frug, planta_genero) %in% paste(int$frug, int$planta_genero))

resumo_imp <- links_imp %>%
  group_by(frug, classe) %>%
  summarise(grau_esperado = round(sum(p), 1),     # soma das probabilidades
            links_acima_limiar = sum(previsto),
            links_observados = sum(observado),
            .groups = "drop") %>%
  arrange(desc(grau_esperado))
print(resumo_imp)
write_csv(links_imp, "outputs/links_imputados.csv")
write_csv(resumo_imp, "outputs/links_imputados_resumo.csv")
# Pra usar no 09: bind_rows(int, links_imp %>% filter(previsto) %>% select(frug, planta_genero))
# e rodar os cenários de novo como análise de sensibilidade ("observado"
# vs "observado + imputado"). Os psitacídeos aqui são suspeitos: o
# modelo prevê CONSUMO de fruto, e eles muitas vezes predam a semente.
