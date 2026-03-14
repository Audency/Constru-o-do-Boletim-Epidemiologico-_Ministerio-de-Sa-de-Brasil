# Construção do Boletim Epidemiológico - Ministério da Saúde (Brasil)

## Sobre o Projeto

Sistema de rotinas em R desenvolvido para a **Secretaria de Vigilância em Saúde (SVS)** do **Ministério da Saúde do Brasil**, responsável pela construção automatizada do **Boletim Epidemiológico da COVID-19**.

O projeto processa dados diários das Secretarias Estaduais de Saúde, realiza cálculos epidemiológicos e gera visualizações cartográficas e gráficas para publicação nos painéis oficiais do governo brasileiro.

## Pipeline de Dados

O fluxo de trabalho segue uma ordem sequencial de execução:

```
┌─────────────────────────────────────────────────────────────────┐
│                    FONTES DE DADOS                              │
│  Secretarias Estaduais de Saúde → Planilhas Excel/CSV           │
└──────────────────────┬──────────────────────────────────────────┘
                       │
          ┌────────────▼────────────┐
          │  Notebook 1 (K1)        │
          │  Monitora_UF_v2.Rmd     │
          │  Dados por Estado (UF)  │
          └────────────┬────────────┘
                       │
          ┌────────────▼────────────┐
          │  Notebook 2 (K2)        │
          │  Monitora_MUN.Rmd       │
          │  Dados por Município    │
          └────────────┬────────────┘
                       │
          ┌────────────▼────────────┐
          │  Notebook 3 (K3)        │
          │  Paineis_SVS_NOVO.Rmd   │
          │  Consolidação Geral     │
          │  (DT_Monitora_COVID)    │
          └────────────┬────────────┘
                       │
          ┌────────┬───┴───┬────────┐
          ▼        ▼       ▼        ▼
       ┌─────┐ ┌─────┐ ┌─────┐
       │ K4  │ │ K5  │ │ K6  │
       │Cards│ │Mapas│ │Mapas│
       │Seman│ │ SE  │ │ Mês │
       └─────┘ └─────┘ └─────┘
```

## Estrutura dos Arquivos

| Arquivo | Descrição | Tipo |
|---------|-----------|------|
| `RNotebook1_Monitora_UF_v2.Rmd` | Monitoramento diário por **Unidade da Federação** (27 estados + DF). Importa dados brutos, calcula casos/óbitos novos, média móvel, casos em acompanhamento e recuperados. | R Notebook |
| `RNotebook2_Monitora_MUN.Rmd` | Monitoramento diário por **Município** (~5.570 municípios). Padroniza nomes, valida consistência temporal entre dias e gera relatório de inconsistências. | R Notebook |
| `RNotebook3_Paineis_SVS_NOVO.Rmd` | **Consolidação hierárquica** dos dados em 4 níveis: Brasil (1), Regiões (2), UFs (3), Municípios (7). Gera o dataset principal `DT_Monitora_COVID` usado por todos os scripts de visualização. | R Notebook |
| `teste_Card_semanal_k4.Rmd` | Gera **cards semanais** com gráficos de barras (casos/óbitos por SE) e mapas de tendência por UF (Redução/Estabilização/Incremento). | R Notebook |
| `Script_municipios_k5.R` | Gera **mapas de bolhas por semana epidemiológica**: casos novos, óbitos novos, taxa de incidência e taxa de mortalidade por município. | Script R |
| `Script_municipios_mapames_K6.R` | Gera **mapas de bolhas por mês**: mesmas visualizações do K5, mas com agregação mensal. | Script R |

## Indicadores Calculados

- **Casos novos / Óbitos novos**: incremento diário a partir dos dados acumulados
- **Casos em acompanhamento**: soma da média móvel de 3 dias dos últimos 18 dias (estimativa de casos ativos)
- **Casos recuperados**: casos acumulados - óbitos - em acompanhamento
- **Taxa de incidência**: (casos novos / população) × 100.000 habitantes
- **Taxa de mortalidade**: (óbitos novos / população) × 100.000 habitantes
- **Variação semanal**: ((SE atual - SE anterior) / SE anterior) × 100, classificada em:
  - **Redução**: < -5%
  - **Estabilização**: entre -5% e +5%
  - **Incremento**: > +5%

## Visualizações Geradas

### Mapas
- Mapa coroplético de tendência por UF (casos e óbitos)
- Mapa de bolhas de casos novos por município
- Mapa de bolhas de óbitos novos por município
- Mapa de incidência por município (/100 mil hab.)
- Mapa de mortalidade por município (/100 mil hab.)

### Gráficos
- Gráfico de barras de casos novos por semana epidemiológica
- Gráfico de barras de óbitos novos por semana epidemiológica

### Tabelas
- Ranking de municípios com maior número de casos/óbitos
- Ranking de municípios com maior incidência/mortalidade
- Tabela comparativa semanal por UF

## Dependências (Pacotes R)

### Manipulação de Dados
- `tidyverse` - Conjunto de pacotes para ciência de dados
- `data.table` - Manipulação eficiente de grandes volumes
- `lubridate` - Manipulação de datas e semanas epidemiológicas
- `hablar` - Funções auxiliares (sum_, min_, max_ com tratamento de NA)
- `openxlsx` - Leitura/escrita de Excel
- `abjutils` - Remoção de acentos (rm_accent)
- `sjmisc` - Conversão de valores (set_na)

### Geoespacial
- `sf` - Simple Features para dados geoespaciais
- `brazilmaps` - Shapefiles do Brasil (estados, municípios, regiões)
- `geobr` - Dados geográficos do IBGE
- `ggsn` - Barra de escala e norte para mapas
- `BAMMtools` - Classificação por Jenks natural breaks
- `maptools` - Ferramentas para mapas

### Visualização
- `ggplot2` - Gráficos (incluso no tidyverse)
- `gghighlight` - Destaque condicional em gráficos
- `ggrepel` - Rótulos sem sobreposição
- `ggflags` - Bandeiras como elementos gráficos
- `gridExtra` - Arranjo de tabelas e gráficos
- `hrbrthemes` - Temas elegantes para ggplot2
- `extrafont` - Fontes tipográficas adicionais

## Dados de Referência

- **Estimativas populacionais**: TCU 2019 (Tribunal de Contas da União)
- **Semanas epidemiológicas**: Calculadas com `lubridate::epiweek()`
- **Códigos IBGE**: Códigos de 2 dígitos (UF) e 6-7 dígitos (municípios)
- **Início da série**: 25/02/2020 (1º caso confirmado no Brasil)

## Como Usar

### Pré-requisitos
1. R (versão 4.0+)
2. RStudio (recomendado)
3. Pacotes listados na seção de dependências

### Execução

1. **Atualizar caminhos**: Ajustar a variável `path` em cada arquivo para o diretório do seu usuário
2. **Executar na ordem**:
   - Primeiro: `RNotebook1_Monitora_UF_v2.Rmd`
   - Segundo: `RNotebook2_Monitora_MUN.Rmd`
   - Terceiro: `RNotebook3_Paineis_SVS_NOVO.Rmd`
   - Depois (independentes): `teste_Card_semanal_k4.Rmd`, `Script_municipios_k5.R`, `Script_municipios_mapames_K6.R`
3. **Atualizar variáveis de controle** (marcadas com `### ATUALIZAR !!!`):
   - Semana epidemiológica (`corteSemana`, `SE`)
   - Datas dos arquivos de entrada
   - Filtros de período

## Autores
- **Audencio Victors** - All
- **Ronaldo Fernandes Santos Alves** - Notebooks 1, 2 e 3
- **Plínio** - Cards semanais (K4)
- **Equipe COE-COVID/SVS** - Scripts de mapas (K5, K6)

## Instituição

**Ministério da Saúde do Brasil**
Secretaria de Vigilância em Saúde (SVS)
Centro de Operações de Emergências (COE-COVID)
