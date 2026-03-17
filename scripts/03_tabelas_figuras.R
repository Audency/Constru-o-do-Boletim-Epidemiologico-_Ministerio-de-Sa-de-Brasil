#!/usr/bin/env Rscript
# ===================================================================
# 03_tabelas_figuras.R
# Gera tabelas descritivas e figuras para o artigo
# ===================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(lubridate)

BASE_DIR <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE),
  error = function(e) normalizePath("..", mustWork = FALSE)
)

fig_dir <- file.path(BASE_DIR, "output", "figures")
tab_dir <- file.path(BASE_DIR, "output", "tables")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

# ── Carregar dados processados ────────────────────────────────────
sigtap   <- read_csv(file.path(BASE_DIR, "data", "processed", "sigtap_processado.csv"),
                     show_col_types = FALSE)
sih_br   <- read_csv(file.path(BASE_DIR, "data", "processed", "sih_br.csv"),
                     show_col_types = FALSE)
sia_br   <- read_csv(file.path(BASE_DIR, "data", "processed", "sia_br.csv"),
                     show_col_types = FALSE)
cnes_tot <- read_csv(file.path(BASE_DIR, "data", "processed", "cnes_total.csv"),
                     show_col_types = FALSE)
cnes_nat <- read_csv(file.path(BASE_DIR, "data", "processed", "cnes_natureza.csv"),
                     show_col_types = FALSE)
ipca     <- read_csv(file.path(BASE_DIR, "data", "processed", "ipca_processado.csv"),
                     show_col_types = FALSE)

# Tema padrão
tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
  )

# Cores para grupos DiD
cores_did <- c("tratamento_2024" = "#D62728", "tratamento_2023" = "#FF7F0E",
               "controle" = "#1F77B4", "outros" = "#7F7F7F")

# ══════════════════════════════════════════════════════════════════
# TABELA 1: Estatísticas descritivas
# ══════════════════════════════════════════════════════════════════
cat("Gerando Tabela 1: Estatísticas descritivas...\n")

tab1_sih <- sih_br %>%
  mutate(periodo = case_when(
    competencia < "2020-03" ~ "Pré-COVID (2018–2020/02)",
    competencia < "2021-07" ~ "COVID (2020/03–2021/06)",
    competencia < "2023-01" ~ "Recuperação (2021/07–2022/12)",
    competencia < "2024-03" ~ "Pós-recomp. 2023 (2023/01–2024/02)",
    TRUE ~ "Pós-recomp. 2024+ (2024/03–2025/09)"
  )) %>%
  group_by(cod_subgrupo, nome_subgrupo, grupo_did, periodo) %>%
  summarise(
    qtd_media_mensal = round(mean(qtd_aprovada)),
    valor_medio_real_R = round(mean(valor_medio_real, na.rm = TRUE), 2),
    valor_total_real_M = round(sum(valor_total_real) / 1e6, 1),
    n_meses = n(),
    .groups = "drop"
  ) %>%
  arrange(grupo_did, cod_subgrupo, periodo)

write_csv(tab1_sih, file.path(tab_dir, "tabela1_descritiva_sih.csv"))

tab1_sia <- sia_br %>%
  mutate(periodo = case_when(
    competencia < "2020-03" ~ "Pré-COVID",
    competencia < "2021-07" ~ "COVID",
    competencia < "2023-01" ~ "Recuperação",
    competencia < "2024-03" ~ "Pós-recomp. 2023",
    TRUE ~ "Pós-recomp. 2024+"
  )) %>%
  group_by(cod_subgrupo, nome_subgrupo, grupo_did, periodo) %>%
  summarise(
    qtd_media_mensal = round(mean(qtd_aprovada)),
    valor_total_real_M = round(sum(valor_total_real) / 1e6, 1),
    .groups = "drop"
  ) %>%
  arrange(grupo_did, cod_subgrupo, periodo)

write_csv(tab1_sia, file.path(tab_dir, "tabela1_descritiva_sia.csv"))

# ══════════════════════════════════════════════════════════════════
# TABELA 2: Recomposição acumulada por subgrupo (SIGTAP)
# ══════════════════════════════════════════════════════════════════
cat("Gerando Tabela 2: Recomposição acumulada...\n")

tab2 <- sigtap %>%
  filter(competencia %in% c("2018-01", "2022-12", "2024-02", "2025-09")) %>%
  select(cod_subgrupo, nome_subgrupo, competencia, valor_referencia, valor_real) %>%
  pivot_wider(names_from = competencia,
              values_from = c(valor_referencia, valor_real)) %>%
  mutate(
    var_nominal_pct = round((`valor_referencia_2025-09` /
                             `valor_referencia_2018-01` - 1) * 100, 1),
    var_real_pct = round((`valor_real_2025-09` /
                          `valor_real_2018-01` - 1) * 100, 1)
  ) %>%
  arrange(desc(var_real_pct))

write_csv(tab2, file.path(tab_dir, "tabela2_recomposicao_sigtap.csv"))

# ══════════════════════════════════════════════════════════════════
# FIGURA 1: Séries temporais – Valores SIGTAP (nominal vs real)
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 1: Séries SIGTAP...\n")

# Selecionar subgrupos representativos
subgrupos_fig <- c("0206","0207","0304","0404","0505","0301")

df_fig1 <- sigtap %>%
  filter(cod_subgrupo %in% subgrupos_fig) %>%
  mutate(data = as.Date(paste0(competencia, "-01"))) %>%
  select(data, cod_subgrupo, nome_subgrupo, valor_referencia, valor_real) %>%
  pivot_longer(cols = c(valor_referencia, valor_real),
               names_to = "tipo", values_to = "valor") %>%
  mutate(tipo = ifelse(tipo == "valor_referencia", "Nominal", "Real (dez/2024)"))

p1 <- ggplot(df_fig1, aes(x = data, y = valor, color = tipo, linetype = tipo)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2023-01-01"), linetype = "dashed",
             color = "grey50", alpha = 0.7) +
  geom_vline(xintercept = as.Date("2024-03-01"), linetype = "dashed",
             color = "grey50", alpha = 0.7) +
  annotate("text", x = as.Date("2023-01-01"), y = Inf, label = "Recomp.\n2023",
           vjust = 1.5, hjust = 0.5, size = 2.5, color = "grey40") +
  annotate("text", x = as.Date("2024-03-01"), y = Inf, label = "Recomp.\n2024",
           vjust = 1.5, hjust = 0.5, size = 2.5, color = "grey40") +
  facet_wrap(~nome_subgrupo, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Nominal" = "#2171B5", "Real (dez/2024)" = "#CB181D")) +
  labs(
    title = "Figura 1. Evolução dos valores SIGTAP – Nominal e Real (R$ dez/2024)",
    subtitle = "Subgrupos selecionados de atenção especializada, Brasil, 2018–2025",
    x = "", y = "Valor (R$)", color = "", linetype = ""
  ) +
  tema

ggsave(file.path(fig_dir, "fig1_sigtap_series.png"), p1,
       width = 12, height = 10, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# FIGURA 2: Produção hospitalar – Tratamento vs Controle
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 2: Produção SIH por grupo DiD...\n")

df_fig2 <- sih_br %>%
  filter(grupo_did %in% c("tratamento_2024", "controle")) %>%
  mutate(data = as.Date(paste0(competencia, "-01"))) %>%
  group_by(data, grupo_did) %>%
  summarise(
    qtd_total = sum(qtd_aprovada),
    .groups = "drop"
  ) %>%
  # Indexar: jan/2018 = 100
  group_by(grupo_did) %>%
  mutate(
    base = first(qtd_total),
    indice = qtd_total / base * 100
  ) %>%
  ungroup()

p2 <- ggplot(df_fig2, aes(x = data, y = indice, color = grupo_did)) +
  geom_line(linewidth = 0.9) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.5, alpha = 0.5,
              linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted",
             color = "grey60") +
  geom_vline(xintercept = as.Date("2024-03-01"), linetype = "dashed",
             color = "red", alpha = 0.5) +
  geom_hline(yintercept = 100, linetype = "solid", color = "grey80") +
  annotate("text", x = as.Date("2020-03-01"), y = 60,
           label = "COVID-19", size = 3, color = "grey40") +
  annotate("text", x = as.Date("2024-03-01"), y = 60,
           label = "Recomposição\n2024", size = 3, color = "red") +
  scale_color_manual(
    values = c("tratamento_2024" = "#D62728", "controle" = "#1F77B4"),
    labels = c("tratamento_2024" = "Tratamento (recomp. 2024)",
               "controle" = "Controle")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Figura 2. Produção hospitalar indexada (jan/2018 = 100)",
    subtitle = "Grupos tratamento (recomposição 2024) vs controle, Brasil",
    x = "", y = "Índice (jan/2018 = 100)", color = ""
  ) +
  tema

ggsave(file.path(fig_dir, "fig2_sih_did_index.png"), p2,
       width = 12, height = 7, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# FIGURA 3: Produção ambulatorial – Séries por subgrupo
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 3: Produção SIA por subgrupo...\n")

df_fig3 <- sia_br %>%
  mutate(data = as.Date(paste0(competencia, "-01"))) %>%
  group_by(data, cod_subgrupo, nome_subgrupo, grupo_did) %>%
  summarise(qtd = sum(qtd_aprovada), .groups = "drop")

p3 <- ggplot(df_fig3, aes(x = data, y = qtd / 1e6, color = grupo_did)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = as.Date("2024-03-01"), linetype = "dashed",
             color = "grey50", alpha = 0.7) +
  facet_wrap(~nome_subgrupo, scales = "free_y", ncol = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_color_manual(values = cores_did) +
  labs(
    title = "Figura 3. Produção ambulatorial mensal por subgrupo (SIA/SUS)",
    subtitle = "Brasil, 2018–2025 (milhões de procedimentos)",
    x = "", y = "Quantidade (milhões)", color = "Grupo DiD"
  ) +
  tema +
  theme(strip.text = element_text(size = 8))

ggsave(file.path(fig_dir, "fig3_sia_subgrupos.png"), p3,
       width = 14, height = 10, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# FIGURA 4: CNES – Evolução da rede prestadora
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 4: Evolução CNES...\n")

df_fig4a <- cnes_tot %>%
  mutate(data = as.Date(data))

p4a <- ggplot(df_fig4a, aes(x = data, y = total_estab, fill = tipo_estabelecimento)) +
  geom_col(position = "stack") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Figura 4a. Estabelecimentos de saúde ativos (CNES)",
    subtitle = "Brasil, 2018–2025 (trimestral)",
    x = "", y = "Nº estabelecimentos", fill = ""
  ) +
  tema

df_fig4b <- cnes_nat %>%
  mutate(data = as.Date(data))

p4b <- ggplot(df_fig4b, aes(x = data, y = total_estab, color = natureza_juridica)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("Público" = "#1F77B4",
                                "Filantrópico" = "#2CA02C",
                                "Privado" = "#D62728")) +
  labs(
    title = "Figura 4b. Estabelecimentos por natureza jurídica (CNES)",
    subtitle = "Brasil, 2018–2025 (trimestral)",
    x = "", y = "Nº estabelecimentos", color = ""
  ) +
  tema

p4 <- grid.arrange(p4a, p4b, ncol = 1)
ggsave(file.path(fig_dir, "fig4_cnes_evolucao.png"), p4,
       width = 12, height = 12, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# FIGURA 5: Defasagem real – Perda acumulada de valor
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 5: Defasagem real acumulada...\n")

df_fig5 <- sigtap %>%
  mutate(data = as.Date(paste0(competencia, "-01"))) %>%
  group_by(cod_subgrupo, nome_subgrupo, grupo_did) %>%
  mutate(
    valor_real_base = first(valor_real),
    defasagem_pct = (valor_real / valor_real_base - 1) * 100
  ) %>%
  ungroup() %>%
  filter(cod_subgrupo %in% c("0206","0207","0304","0404","0301","0302"))

p5 <- ggplot(df_fig5, aes(x = data, y = defasagem_pct, color = nome_subgrupo)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey60") +
  geom_vline(xintercept = as.Date("2024-03-01"), linetype = "dashed",
             color = "grey50") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Figura 5. Variação real acumulada dos valores SIGTAP (base jan/2018)",
    subtitle = "Subgrupos selecionados – Valores deflacionados pelo IPCA",
    x = "", y = "Variação real (%)", color = ""
  ) +
  tema

ggsave(file.path(fig_dir, "fig5_defasagem_real.png"), p5,
       width = 12, height = 7, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# FIGURA 6: Valor total da produção – SIH vs SIA
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 6: Valor total da produção...\n")

prod_total <- read_csv(file.path(BASE_DIR, "data", "processed", "producao_total.csv"),
                       show_col_types = FALSE)

df_fig6 <- prod_total %>%
  mutate(data = as.Date(paste0(competencia, "-01"))) %>%
  group_by(data, sistema) %>%
  summarise(
    valor_real_bi = sum(valor_total_real, na.rm = TRUE) / 1e9,
    .groups = "drop"
  )

p6 <- ggplot(df_fig6, aes(x = data, y = valor_real_bi, fill = sistema)) +
  geom_area(alpha = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_manual(values = c("SIH" = "#2171B5", "SIA" = "#6BAED6")) +
  labs(
    title = "Figura 6. Valor total da produção aprovada (R$ bilhões, real dez/2024)",
    subtitle = "SIH/SUS e SIA/SUS, Brasil, 2018–2025",
    x = "", y = "R$ bilhões (real)", fill = ""
  ) +
  tema

ggsave(file.path(fig_dir, "fig6_valor_total.png"), p6,
       width = 12, height = 6, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# TABELA 3: Resumo por período e grupo DiD
# ══════════════════════════════════════════════════════════════════
cat("Gerando Tabela 3: Resumo por período e grupo...\n")

tab3 <- sih_br %>%
  mutate(periodo = case_when(
    competencia < "2020-03" ~ "1_Pre_COVID",
    competencia < "2021-07" ~ "2_COVID",
    competencia < "2023-01" ~ "3_Recuperacao",
    competencia < "2024-03" ~ "4_Pos_Recomp_2023",
    TRUE ~ "5_Pos_Recomp_2024"
  )) %>%
  group_by(grupo_did, periodo) %>%
  summarise(
    n_meses = n_distinct(competencia),
    qtd_media = round(mean(qtd_aprovada)),
    qtd_mediana = round(median(qtd_aprovada)),
    valor_real_medio = round(mean(valor_medio_real, na.rm = TRUE), 2),
    valor_total_real_M = round(sum(valor_total_real) / 1e6, 1),
    .groups = "drop"
  ) %>%
  arrange(grupo_did, periodo)

write_csv(tab3, file.path(tab_dir, "tabela3_resumo_did.csv"))

# ══════════════════════════════════════════════════════════════════
# TABELA 4: Variação percentual pré vs pós recomposição
# ══════════════════════════════════════════════════════════════════
cat("Gerando Tabela 4: Variação pré/pós...\n")

tab4 <- sih_br %>%
  mutate(
    fase = case_when(
      competencia >= "2018-01" & competencia < "2020-03" ~ "pre_covid",
      competencia >= "2024-03" ~ "pos_recomp",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(fase)) %>%
  group_by(cod_subgrupo, nome_subgrupo, grupo_did, fase) %>%
  summarise(qtd_media = mean(qtd_aprovada), .groups = "drop") %>%
  pivot_wider(names_from = fase, values_from = qtd_media) %>%
  mutate(
    variacao_pct = round((pos_recomp / pre_covid - 1) * 100, 1)
  ) %>%
  arrange(desc(variacao_pct))

write_csv(tab4, file.path(tab_dir, "tabela4_variacao_pre_pos.csv"))

cat("\n══════════════════════════════════════\n")
cat("  TABELAS E FIGURAS GERADAS\n")
cat("  Figuras:", fig_dir, "\n")
cat("  Tabelas:", tab_dir, "\n")
cat("══════════════════════════════════════\n")
