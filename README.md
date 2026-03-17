# Construção do Boletim Epidemiológico — Ministério da Saúde (Brasil)

> Rotinas automatizadas de vigilância epidemiológica **e** análise econométrica do financiamento do SUS.

---

## Visão Geral

Este repositório reúne dois componentes complementares desenvolvidos no âmbito do **Ministério da Saúde do Brasil**:

| Componente | Descrição |
|:--|:--|
| **Boletim Epidemiológico COVID-19** | Pipeline em R para construção automatizada do boletim diário da SVS, com monitoramento por UF e município, cards semanais e mapas de incidência. |
| **Estudo: Defasagem da Tabela SUS (2018–2025)** | Análise econométrica (Diferença-em-Diferenças) dos efeitos da defasagem e recomposição dos valores da Tabela SUS sobre a produção e oferta de atenção especializada no Brasil. |

---

## Estrutura do Repositório

```
.
├── README.md
│
├── ── Boletim Epidemiológico COVID-19 ──────────────────────
├── RNotebook1_Monitora_UF_v2.Rmd      # Monitoramento por UF (27 estados + DF)
├── RNotebook2_Monitora_MUN.Rmd        # Monitoramento por município (~5.570)
├── RNotebook3_Paineis_SVS_NOVO.Rmd    # Consolidação hierárquica (Brasil → Região → UF → Município)
├── teste_Card_semanal_k4.Rmd          # Cards semanais com gráficos e mapas de tendência
├── Script_municipios_k5.R             # Mapas de bolhas por semana epidemiológica
├── Script_municipios_mapames_K6.R     # Mapas de bolhas por mês
│
├── ── Estudo Tabela SUS ───────────────────────────────────
├── scripts/
│   ├── 00_executar_tudo.R             # Orquestrador (executa etapas 1→4)
│   ├── 01_download_data.py            # Download dos microdados do DATASUS
│   ├── 01_gerar_dados.R               # Geração de dados simulados (SIGTAP, SIH, SIA, CNES, IPCA)
│   ├── 02_processar_dados.R           # Processamento e construção dos painéis analíticos
│   ├── 03_tabelas_figuras.R           # Tabelas descritivas e figuras para o manuscrito
│   ├── 04_modelos_did.R               # Modelos Diferença-em-Diferenças (DiD)
│   └── 05_gerar_manuscrito.py         # Geração do manuscrito (formato BMC Health Services Research)
│
├── data/
│   ├── sigtap/                        # Procedimentos da Tabela SUS (SIGTAP)
│   ├── sih/                           # Produção hospitalar (SIH/SUS)
│   ├── sia/                           # Produção ambulatorial (SIA/SUS)
│   ├── cnes/                          # Estabelecimentos de saúde (CNES)
│   ├── raw/                           # Dados brutos auxiliares (IPCA)
│   └── processed/                     # Painéis analíticos prontos para modelagem
│
├── output/
│   ├── figures/                       # 8 figuras (PNG) para o manuscrito
│   ├── tables/                        # 7 tabelas descritivas e analíticas (CSV)
│   └── models/                        # Objetos dos modelos estimados (RData)
│
└── docs/
    └── manuscrito_tabela_sus_bmc.docx # Manuscrito completo (estilo BMC)
```

---

## 1. Boletim Epidemiológico COVID-19

### Pipeline de Dados

```
Secretarias Estaduais ──► Notebook 1 (UF) ──► Notebook 2 (MUN) ──► Notebook 3 (Consolidação)
                                                                          │
                                                              ┌───────────┼───────────┐
                                                              ▼           ▼           ▼
                                                          K4: Cards   K5: Mapas   K6: Mapas
                                                          semanais    por SE      por mês
```

### Indicadores Calculados

| Indicador | Fórmula |
|:--|:--|
| Casos novos / Óbitos novos | Incremento diário dos acumulados |
| Casos em acompanhamento | Soma da MM(3) dos últimos 18 dias |
| Casos recuperados | Acumulados − óbitos − em acompanhamento |
| Taxa de incidência | (casos novos / população) × 100.000 hab. |
| Taxa de mortalidade | (óbitos novos / população) × 100.000 hab. |
| Variação semanal | ((SE atual − SE anterior) / SE anterior) × 100 |

**Classificação de tendência:** Redução (< −5%) · Estabilização (−5% a +5%) · Incremento (> +5%)

### Visualizações

- Mapas coropléticos de tendência por UF (casos e óbitos)
- Mapas de bolhas por município (incidência, mortalidade)
- Gráficos de barras por semana epidemiológica
- Rankings de municípios (casos, óbitos, taxas)

---

## 2. Estudo: Defasagem e Recomposição da Tabela SUS

### Objetivo

Analisar os efeitos da defasagem dos valores remuneratórios da Tabela SUS e de sua recomposição parcial (a partir de 2023) sobre a **produção hospitalar e ambulatorial** e a **oferta de serviços especializados** no Brasil entre 2018 e 2025.

### Fontes de Dados

| Sistema | Descrição |
|:--|:--|
| **SIGTAP** | Tabela de Procedimentos, Medicamentos e OPM do SUS |
| **SIH/SUS** | Sistema de Informações Hospitalares |
| **SIA/SUS** | Sistema de Informações Ambulatoriais |
| **CNES** | Cadastro Nacional de Estabelecimentos de Saúde |
| **IPCA** | Índice de Preços ao Consumidor Amplo (IBGE) |

### Metodologia

- **Desenho:** Quase-experimental com Diferença-em-Diferenças (DiD)
- **Tratamento:** Recomposição da Tabela SUS (Portaria GM/MS nº 3.392/2024)
- **Período:** Jan/2018 a Dez/2025 (dados mensais)
- **Controles:** Tendências paralelas pré-tratamento, event study, placebo tests
- **Erros-padrão:** HC1 (Heteroskedasticity-Consistent, White)

### Resultados Gerados

**Figuras** (`output/figures/`):

| Arquivo | Conteúdo |
|:--|:--|
| `fig1_sigtap_series.png` | Evolução temporal dos valores SIGTAP (nominal vs. real) |
| `fig2_sih_did_index.png` | Internações SIH — índice DiD |
| `fig3_sia_subgrupos.png` | Produção SIA por subgrupo de procedimentos |
| `fig4_cnes_evolucao.png` | Evolução da rede de estabelecimentos (CNES) |
| `fig5_defasagem_real.png` | Defasagem real acumulada (IPCA) |
| `fig6_valor_total.png` | Valor total pago pelo SUS |
| `fig7_event_study.png` | Event study — leads e lags do tratamento |
| `fig8_forest_uf.png` | Forest plot dos efeitos por UF |

**Tabelas** (`output/tables/`):

| Arquivo | Conteúdo |
|:--|:--|
| `tabela1_descritiva_*.csv` | Estatísticas descritivas SIH e SIA |
| `tabela2_recomposicao_sigtap.csv` | Valores da recomposição SIGTAP |
| `tabela3_resumo_did.csv` | Resumo dos modelos DiD |
| `tabela4_variacao_pre_pos.csv` | Variação pré vs. pós-recomposição |
| `tabela5_modelos_did.csv` | Coeficientes completos dos modelos |
| `tabela6_efeitos_uf.csv` | Efeitos heterogêneos por UF |
| `tabela7_placebo.csv` | Testes placebo |

**Manuscrito** (`docs/manuscrito_tabela_sus_bmc.docx`):
Artigo completo formatado no estilo BMC Health Services Research, com 33 referências bibliográficas.

---

## Como Executar

### Pré-requisitos

| Software | Versão |
|:--|:--|
| R | ≥ 4.0 |
| Python | ≥ 3.8 (apenas para `01_download_data.py` e `05_gerar_manuscrito.py`) |
| RStudio | Recomendado para os notebooks |

### Pacotes R necessários

```r
# Manipulação de dados
install.packages(c("tidyverse", "data.table", "lubridate", "readr", "openxlsx", "hablar"))

# Geoespacial (Boletim COVID)
install.packages(c("sf", "brazilmaps", "geobr", "ggsn", "BAMMtools", "maptools"))

# Econometria (Estudo Tabela SUS)
install.packages(c("lmtest", "sandwich", "broom"))

# Visualização
install.packages(c("ggplot2", "scales", "gridExtra", "ggrepel", "gghighlight", "hrbrthemes"))

# Manuscrito
pip install python-docx  # Python
```

### Execução — Boletim COVID-19

```bash
# Executar na ordem:
# 1. RNotebook1_Monitora_UF_v2.Rmd
# 2. RNotebook2_Monitora_MUN.Rmd
# 3. RNotebook3_Paineis_SVS_NOVO.Rmd
# 4. teste_Card_semanal_k4.Rmd / Script_municipios_k5.R / Script_municipios_mapames_K6.R
```

### Execução — Estudo Tabela SUS

```bash
cd scripts/

# Opção 1: Executar tudo de uma vez
Rscript 00_executar_tudo.R

# Opção 2: Etapa por etapa
Rscript 01_gerar_dados.R        # Gerar dados
Rscript 02_processar_dados.R    # Processar painéis
Rscript 03_tabelas_figuras.R    # Tabelas e figuras
Rscript 04_modelos_did.R        # Modelos DiD
python  05_gerar_manuscrito.py   # Manuscrito DOCX
```

---

## Dados de Referência

- **Estimativas populacionais:** TCU 2019 (Tribunal de Contas da União)
- **Semanas epidemiológicas:** `lubridate::epiweek()`
- **Códigos IBGE:** 2 dígitos (UF), 6–7 dígitos (município)
- **Início da série COVID:** 25/02/2020 (1º caso confirmado)
- **Período Tabela SUS:** Jan/2018 – Dez/2025

## Autores

- **Audencio Victors** — Coordenação geral
- **Ronaldo Fernandes Santos Alves** — Notebooks 1, 2 e 3
- **Plínio** — Cards semanais (K4)
- **Equipe COE-COVID/SVS** — Scripts de mapas (K5, K6)

## Instituição

**Ministério da Saúde do Brasil**
Secretaria de Vigilância em Saúde (SVS)
Centro de Operações de Emergências (COE-COVID)

---

## Licença

Este projeto é de uso institucional do Ministério da Saúde do Brasil.
