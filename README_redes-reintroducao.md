# Redes: contribuição das UCs e reintroduções (scripts 06–10)

Scripts novos, independentes do `04_networks.R` (nenhum script da Luísa foi alterado).
Rodar na ordem, a partir da raiz do projeto. Tudo sai em `outputs/`.

| Script | O que faz | Precisa de |
|---|---|---|
| `R/funcoes_meridian.R` | leitura dos dados, redes, robustez rápida, caminhos indiretos | — |
| `06_listas-especies.R` | espécie × site, quem é frugívoro, taxa de match com o denominador certo | dados em `data/` |
| `07_dependencia-floresta.R` | habitats IUCN → dependente de floresta / tolerante; proxy AVONET pra aves | token IUCN (`IUCN_REDLIST_KEY` no `.Renviron`) |
| `08_credito-reintroducao.R` | lista do workshop limpa; crédito de interações (Genes et al. 2017); portfólio de reintroduções | `data/candidatas_reintroducao_workshop.csv` |
| `09_cenarios-rede.R` | PNT + candidatas (estilo Mittelman et al. 2022) e Rio − dependentes de floresta, com modelos nulos | 06–08 |
| `10_trait-matching.R` | modelo de atributos pra imputar interações das candidatas sem dados | 06, 08 |
| `11_potencial-efeito-landim.R` | framework de Landim et al. 2024 (originalidade × disponibilidade de recurso) para as candidatas + sequência de reintroduções | 08, `data/landim2024/` (copiado do repositório do artigo) |

Pacotes: dplyr, tidyr, purrr, stringr, readr, tibble, bipartite, ks, httr2, jsonlite.

## Avisos importantes
- **07 não foi testado contra a API da IUCN** (sem token). Rode com `TESTE <- TRUE` e confira `hab_teste` antes.
  Sem token, a classificação é só o proxy AVONET (aves; mamíferos ficam NA).
- **Caminhos indiretos** (`n_caminhos()`): como no Mittelman et al. 2022 — soma de todos os elementos de A^l,
  l = 2..5. O nulo padrão do 09 também segue o artigo (k frugívoros atuais sorteados, "sem" -> "com").
- **11 (Landim et al. 2024)**: a validação reproduz o caso de estudo (cutia > bugio em todo θ;
  A_bugio = 0.43 vs 0.44 no artigo; A_cutia = 0.94 vs 0.99). A Eq. 6 foi implementada com S∩R no
  numerador, senão passa de 1. O código do repositório tem bugs (`pe()` x `pe.v()`, `comprimento`, `kde.G1`).
- **Robustez**: `robustez_rapida()` usa remoção aleatória e um universo fixo de plantas (gêneros
  zoocóricos do site). Não é numericamente idêntica ao `bipartite::robustness()`.
- **Zoocoria** ainda é aproximada por "o gênero aparece no ATLANTIC-frugivory". Falta síndrome de dispersão.
- **10**: o modelo de atributos tem AUC ~0.63–0.67 (validação por espécie) e põe quase todas as plantas
  acima do limiar. Serve no máximo como análise de sensibilidade, não pra construir rede.
- Nº de nulos no 09: `N_NULOS` (padrão 99; usar 999 na versão final).
