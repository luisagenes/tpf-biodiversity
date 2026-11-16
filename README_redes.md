# Redes: reintroduzidas, dependentes de floresta e candidatas (scripts 06–08)

Substituem os antigos 06–11 (`06_listas-especies.R` … `11_potencial-efeito-landim.R`,
`R/funcoes_meridian.R`), removidos em 12/11/2026. Eles continuam no histórico do git
(commit `d3e160a`) se precisar recuperar algo.

Scripts novos, independentes do `04_networks.R` (nenhum script da Luísa foi alterado).
Rodar a partir da raiz do projeto. Tabelas saem em `outputs/`, figuras em `figures/`.

| Script | O que faz | Precisa de |
|---|---|---|
| `R/funcoes_redes.R` | leitura e padronização (com cache), redes por site, métricas, comparação com/sem + nulo, figura com/sem | — |
| `06_redes-reintroduzidas.R` | rede do PNT **com** (reintroduzidas destacadas) e **sem** as 4 reintroduzidas de Mittelman et al. 2022 | `data/` |
| `07_redes-dependentes-floresta.R` | redes com e sem os frugívoros dependentes de floresta, por site e Rio inteiro | `data/especies_exclusivas_floresta.csv`, `data/especies_floresta_maior_importancia.csv` (IUCN, da aluna) |
| `08_candidatas-reintroducao.R` | para cada candidata: (a) interações potenciais, (b) interações únicas, (c) contribuição funcional (Landim et al. 2024) | `data/candidatas_reintroducao_workshop.csv`, `data/landim2024/` |

Pacotes: dplyr, tidyr, purrr, stringr, readr, tibble, ggplot2, igraph, bipartite, ks, rgbif, flora
(readxl se a planilha do 07 for .xlsx; ggrepel opcional, melhora os rótulos das figuras).

## Definições
- Rede = frugívoro **espécie** x planta **gênero**, binária, co-ocorrência estrita (como a rede "mista" do 04).
- Padronização: GBIF backbone (animais) e Flora do Brasil (plantas), por nome. A primeira rodada consulta as APIs
  e salva `outputs/cache_tax_animais.rds` e `outputs/cache_tax_plantas.rds`; as seguintes usam o cache.
  **Sem internet, os nomes ficam sem padronizar (com aviso) — não interpretar resultados assim.**
- Nulo (06 e 07): remover k frugívoros sorteados entre os "outros" (não reintroduzidos / não dependentes),
  k = nº de espécies removidas. `z > 0` = o grupo removido muda a rede mais que k espécies quaisquer.
- (b) no 08: gêneros do PNT na dieta da candidata que não têm **nenhum** dispersor na rede atual do PNT
  (com as reintroduzidas, sem a própria candidata).

## Avisos
- **Jabuti**: sem registros no ATLANTIC-frugivory; as interações vêm do NeoFrugivory (Neotropical inteiro) e
  ele é forçado como presente no PNT (comunidades não tem répteis). Isso infla o nº de plantas "exclusivas"
  dele em relação às outras reintroduzidas, que só têm dados da Mata Atlântica. O mesmo vale para
  candidatas com `fonte_interacoes = "NeoFrugivory"` no 08.
- **(a) e esforço amostral**: espécies muito estudadas (muriqui, 167 estudos) acumulam mais interações só por isso.
  Ver a coluna `n_estudos`.
- **(c) Landim et al. 2024**: a validação reproduz o caso de estudo (cutia > bugio em todo θ; A_bugio = 0.43 vs
  0.44 no artigo; A_cutia = 0.94 vs 0.99). A Eq. 6 usa S∩R no numerador, senão passa de 1. Candidatas com
  < 10 plantas do PNT com atributos ficam sem (c). Para espécies dentro de um grupo agregado do
  `ConsumersPNT` (ex.: macaco-prego em "Primates") não dá pra tirá-las de C, e o (c) subestima a contribuição.
- **Robustez**: calculada sobre as plantas que estão na rede; perder a única dispersora de uma planta tira a planta
  da rede. Ler junto com `plantas_com_dispersor` e `plantas_orfas`.
- **07**: dois critérios. `estrito` = só habitats florestais na IUCN; `amplo` = estrito + floresta de "maior importância"
  (união, porque as listas não são aninhadas). No amplo ~60% dos frugívoros viram "dependentes" e não sobram
  não-dependentes suficientes para o nulo — tratar o amplo como limite superior, não como resultado principal.
  Quem não está nas listas conta como não dependente, inclusive espécies sem avaliação da IUCN.
