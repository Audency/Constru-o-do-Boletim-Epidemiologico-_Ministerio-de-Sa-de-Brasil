#!/usr/bin/env Rscript
# ===================================================================
# 04_modelos_did.R
# Modelos de Diferença-em-Diferenças (DiD) para estimar efeito
# da recomposição da Tabela SUS sobre produção e oferta
# ===================================================================

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(lmtest)
library(sandwich)
library(broom)
library(gridExtra)

BASE_DIR <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE),
  error = function(e) normalizePath("..", mustWork = FALSE)
)

mod_dir <- file.path(BASE_DIR, "output", "models")
fig_dir <- file.path(BASE_DIR, "output", "figures")
tab_dir <- file.path(BASE_DIR, "output", "tables")
dir.create(mod_dir, recursive = TRUE, showWarnings = FALSE)

# ── Carregar dados ────────────────────────────────────────────────
painel_sih <- read_csv(file.path(BASE_DIR, "data", "processed", "painel_sih.csv"),
                       show_col_types = FALSE)
painel_sia <- read_csv(file.path(BASE_DIR, "data", "processed", "painel_sia.csv"),
                       show_col_types = FALSE)
sih_uf     <- read_csv(file.path(BASE_DIR, "data", "processed", "sih_uf.csv"),
                       show_col_types = FALSE)

# ══════════════════════════════════════════════════════════════════
# MODELO 1: DiD básico – Recomposição 2024 sobre SIH
# Y_it = α + β₁·Tratado_i + β₂·Pós_t + β₃·(Tratado×Pós)_it + ε_it
# ══════════════════════════════════════════════════════════════════
cat("\n═══ MODELO 1: DiD SIH – Recomposição 2024 ═══\n")

# Filtrar apenas tratamento_2024 e controle
df_m1 <- painel_sih %>%
  filter(grupo_did %in% c("tratamento_2024", "controle")) %>%
  mutate(
    tratado = as.integer(grupo_did == "tratamento_2024"),
    pos = pos_2024
  )

# Modelo 1a: DiD simples
m1a <- lm(log_qtd ~ tratado + pos + tratado:pos, data = df_m1)
m1a_robust <- coeftest(m1a, vcov = vcovHC(m1a, type = "HC1"))

cat("\nModelo 1a: DiD simples (log quantidade)\n")
print(m1a_robust)

# Modelo 1b: Com efeitos fixos de procedimento e tendência
m1b <- lm(log_qtd ~ tratado:pos + factor(cod_subgrupo) + tempo,
           data = df_m1)
m1b_robust <- coeftest(m1b, vcov = vcovHC(m1b, type = "HC1"))

cat("\nModelo 1b: DiD com EF procedimento + tendência\n")
print(m1b_robust)

# Modelo 1c: Com efeitos fixos de procedimento e tempo (mês-ano)
m1c <- lm(log_qtd ~ tratado:pos + factor(cod_subgrupo) + factor(competencia),
           data = df_m1)
m1c_robust <- coeftest(m1c, vcov = vcovHC(m1c, type = "HC1"))

# Extrair coeficiente DiD
did_coef <- tidy(m1c) %>% filter(grepl("tratado:pos", term))
cat("\nCoeficiente DiD (Modelo 1c):\n")
print(did_coef)

# ══════════════════════════════════════════════════════════════════
# MODELO 2: DiD – Recomposição 2023 sobre SIH
# ══════════════════════════════════════════════════════════════════
cat("\n═══ MODELO 2: DiD SIH – Recomposição 2023 ═══\n")

df_m2 <- painel_sih %>%
  filter(grupo_did %in% c("tratamento_2023", "controle")) %>%
  mutate(
    tratado = as.integer(grupo_did == "tratamento_2023"),
    pos = pos_2023
  )

m2 <- lm(log_qtd ~ tratado:pos + factor(cod_subgrupo) + factor(competencia),
          data = df_m2)
m2_robust <- coeftest(m2, vcov = vcovHC(m2, type = "HC1"))

did_coef_2023 <- tidy(m2) %>% filter(grepl("tratado:pos", term))
cat("Coeficiente DiD 2023:\n")
print(did_coef_2023)

# ══════════════════════════════════════════════════════════════════
# MODELO 3: DiD SIA – Produção ambulatorial
# ══════════════════════════════════════════════════════════════════
cat("\n═══ MODELO 3: DiD SIA – Recomposição 2024 ═══\n")

df_m3 <- painel_sia %>%
  filter(grupo_did %in% c("tratamento_2024", "controle")) %>%
  mutate(
    tratado = as.integer(grupo_did == "tratamento_2024"),
    pos = pos_2024
  )

m3 <- lm(log_qtd ~ tratado:pos + factor(cod_subgrupo) + factor(competencia),
          data = df_m3)
m3_robust <- coeftest(m3, vcov = vcovHC(m3, type = "HC1"))

did_coef_sia <- tidy(m3) %>% filter(grepl("tratado:pos", term))
cat("Coeficiente DiD SIA:\n")
print(did_coef_sia)

# ══════════════════════════════════════════════════════════════════
# MODELO 4: DiD com heterogeneidade regional (UF)
# ══════════════════════════════════════════════════════════════════
cat("\n═══ MODELO 4: DiD Regional (SIH por UF) ═══\n")

df_m4 <- sih_uf %>%
  filter(grupo_did %in% c("tratamento_2024", "controle")) %>%
  mutate(
    tratado = as.integer(grupo_did == "tratamento_2024"),
    pos = pos_2024,
    log_qtd = log(qtd_aprovada + 1)
  )

m4 <- lm(log_qtd ~ tratado:pos + factor(cod_subgrupo) + factor(competencia) +
           factor(uf) + tratado:pos:factor(uf),
         data = df_m4)
m4_robust <- coeftest(m4, vcov = vcovHC(m4, type = "HC1"))

# Extrair efeitos por UF
m4_tidy <- tidy(m4) %>%
  filter(grepl("tratado:pos", term)) %>%
  mutate(
    uf = ifelse(grepl("factor\\(uf\\)", term),
                gsub(".*factor\\(uf\\)", "", term), "Base (AC)"),
    significativo = p.value < 0.05
  )

cat("Efeitos DiD por UF (primeiras linhas):\n")
print(head(m4_tidy, 10))

# ══════════════════════════════════════════════════════════════════
# MODELO 5: Event study – Tendências paralelas
# ══════════════════════════════════════════════════════════════════
cat("\n═══ MODELO 5: Event Study ═══\n")

# Criar dummies de período relativo ao tratamento (2024-03)
df_m5 <- painel_sih %>%
  filter(grupo_did %in% c("tratamento_2024", "controle")) %>%
  mutate(
    tratado = as.integer(grupo_did == "tratamento_2024"),
    # Semestres relativos
    semestre_rel = case_when(
      competencia < "2019-01" ~ -6,
      competencia < "2019-07" ~ -5,
      competencia < "2020-01" ~ -4,
      competencia < "2020-07" ~ -3,
      competencia < "2021-01" ~ -2,
      competencia < "2021-07" ~ -1,
      competencia < "2022-01" ~ 0,  # normalização
      competencia < "2022-07" ~ 1,
      competencia < "2023-01" ~ 2,
      competencia < "2023-07" ~ 3,
      competencia < "2024-01" ~ 4,
      competencia < "2024-07" ~ 5,
      competencia < "2025-01" ~ 6,
      TRUE ~ 7
    )
  )

# Omitir período -1 como referência
df_m5$sem_fac <- relevel(factor(df_m5$semestre_rel), ref = "-1")

m5 <- lm(log_qtd ~ tratado:sem_fac + factor(cod_subgrupo), data = df_m5)
m5_robust <- coeftest(m5, vcov = vcovHC(m5, type = "HC1"))

# Extrair coeficientes do event study
es_coefs <- tidy(m5) %>%
  filter(grepl("tratado:sem_fac", term)) %>%
  mutate(
    semestre = as.numeric(gsub("tratado:sem_fac", "", term)),
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  ) %>%
  arrange(semestre)

# ══════════════════════════════════════════════════════════════════
# FIGURA 7: Event Study Plot
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 7: Event Study...\n")

# Adicionar período de referência
es_plot <- bind_rows(
  es_coefs %>% select(semestre, estimate, ci_low, ci_high),
  data.frame(semestre = -1, estimate = 0, ci_low = 0, ci_high = 0)
) %>%
  arrange(semestre)

p7 <- ggplot(es_plot, aes(x = semestre, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "grey60") +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "red", alpha = 0.6) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "#2171B5") +
  geom_line(color = "#2171B5", linewidth = 0.8) +
  geom_point(color = "#2171B5", size = 2.5) +
  annotate("text", x = 4.5, y = max(es_plot$ci_high, na.rm = TRUE) * 0.9,
           label = "Recomposição\n2024", size = 3, color = "red") +
  scale_x_continuous(breaks = -6:7,
                     labels = c("2018-S1","2018-S2","2019-S1","2019-S2",
                               "2020-S1","Ref","2021-S2","2022-S1",
                               "2022-S2","2023-S1","2023-S2","2024-S1",
                               "2024-S2","2025-S1")) +
  labs(
    title = "Figura 7. Event Study: Efeito da recomposição sobre produção hospitalar",
    subtitle = "Coeficientes semestrais relativos (referência: 2021-S1). IC 95%",
    x = "Semestre relativo", y = "Coeficiente (log quantidade)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "fig7_event_study.png"), p7,
       width = 12, height = 7, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# FIGURA 8: Efeitos regionais (forest plot)
# ══════════════════════════════════════════════════════════════════
cat("Gerando Figura 8: Forest plot regional...\n")

# Estimar DiD por UF separadamente
ufs_analise <- c("SP","RJ","MG","BA","RS","PR","PE","CE","PA","GO",
                 "MA","SC","AM","DF","ES")

resultados_uf <- list()
for (uf_sel in ufs_analise) {
  df_uf <- sih_uf %>%
    filter(uf == uf_sel, grupo_did %in% c("tratamento_2024", "controle")) %>%
    mutate(
      tratado = as.integer(grupo_did == "tratamento_2024"),
      pos = pos_2024,
      log_qtd = log(qtd_aprovada + 1)
    )

  if (nrow(df_uf) < 20) next

  m_uf <- lm(log_qtd ~ tratado * pos + factor(cod_subgrupo), data = df_uf)
  m_uf_r <- coeftest(m_uf, vcov = vcovHC(m_uf, type = "HC1"))

  # Extrair interação
  idx <- which(grepl("tratado:pos", rownames(m_uf_r)))
  if (length(idx) > 0) {
    resultados_uf[[uf_sel]] <- data.frame(
      uf = uf_sel,
      estimate = m_uf_r[idx, 1],
      se = m_uf_r[idx, 2],
      pvalue = m_uf_r[idx, 4],
      ci_low = m_uf_r[idx, 1] - 1.96 * m_uf_r[idx, 2],
      ci_high = m_uf_r[idx, 1] + 1.96 * m_uf_r[idx, 2],
      stringsAsFactors = FALSE
    )
  }
}

df_forest <- bind_rows(resultados_uf) %>%
  mutate(
    sig = ifelse(pvalue < 0.05, "p < 0.05", "p ≥ 0.05"),
    uf = reorder(uf, estimate)
  )

p8 <- ggplot(df_forest, aes(x = estimate, y = uf, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.3) +
  geom_point(size = 3) +
  scale_color_manual(values = c("p < 0.05" = "#D62728", "p ≥ 0.05" = "#7F7F7F")) +
  labs(
    title = "Figura 8. Efeito DiD da recomposição 2024 por UF",
    subtitle = "Coeficiente da interação tratado×pós, erros robustos HC1, IC 95%",
    x = "Coeficiente DiD (log quantidade)", y = "", color = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(fig_dir, "fig8_forest_uf.png"), p8,
       width = 10, height = 8, dpi = 300, bg = "white")

# ══════════════════════════════════════════════════════════════════
# TABELA 5: Resultados dos modelos DiD
# ══════════════════════════════════════════════════════════════════
cat("Gerando Tabela 5: Resultados dos modelos...\n")

# Função para extrair resultados
extract_did <- function(model, name, vcov_type = "HC1") {
  robust <- coeftest(model, vcov = vcovHC(model, type = vcov_type))
  tidy_m <- tidy(model)
  did_row <- tidy_m %>% filter(grepl("tratado.*pos|did", term))

  if (nrow(did_row) == 0) {
    did_row <- tidy_m %>% filter(grepl("tratado", term) & grepl("pos", term))
  }

  # Get robust SE for DiD term
  idx <- which(grepl("tratado.*pos", rownames(robust)))
  if (length(idx) > 0) {
    data.frame(
      modelo = name,
      coef_did = round(robust[idx, 1], 4),
      se_robusto = round(robust[idx, 2], 4),
      t_stat = round(robust[idx, 3], 3),
      p_valor = round(robust[idx, 4], 4),
      r2 = round(summary(model)$r.squared, 4),
      r2_adj = round(summary(model)$adj.r.squared, 4),
      n_obs = nobs(model),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(modelo = name, coef_did = NA, se_robusto = NA,
               t_stat = NA, p_valor = NA, r2 = NA, r2_adj = NA,
               n_obs = NA, stringsAsFactors = FALSE)
  }
}

tab5 <- bind_rows(
  extract_did(m1a, "M1a: DiD simples SIH (2024)"),
  extract_did(m1b, "M1b: DiD + EF proc + tendência (2024)"),
  extract_did(m1c, "M1c: DiD + EF proc + EF tempo (2024)"),
  extract_did(m2,  "M2: DiD SIH – Recomp. 2023"),
  extract_did(m3,  "M3: DiD SIA – Recomp. 2024")
)

write_csv(tab5, file.path(tab_dir, "tabela5_modelos_did.csv"))
cat("\nTabela 5 – Resultados dos modelos DiD:\n")
print(tab5)

# ══════════════════════════════════════════════════════════════════
# TABELA 6: Efeitos regionais
# ══════════════════════════════════════════════════════════════════
write_csv(df_forest %>% mutate(uf = as.character(uf)) %>%
            select(uf, estimate, se, pvalue, ci_low, ci_high, sig),
          file.path(tab_dir, "tabela6_efeitos_uf.csv"))

# ══════════════════════════════════════════════════════════════════
# MODELO 6: Placebo test – falso tratamento em 2021
# ══════════════════════════════════════════════════════════════════
cat("\n═══ MODELO 6: Placebo Test ═══\n")

df_placebo <- painel_sih %>%
  filter(grupo_did %in% c("tratamento_2024", "controle"),
         competencia < "2024-03") %>%
  mutate(
    tratado = as.integer(grupo_did == "tratamento_2024"),
    pos_placebo = as.integer(competencia >= "2021-07")
  )

m_placebo <- lm(log_qtd ~ tratado * pos_placebo + factor(cod_subgrupo) +
                  factor(competencia), data = df_placebo)
placebo_robust <- coeftest(m_placebo, vcov = vcovHC(m_placebo, type = "HC1"))

cat("Placebo test (falso tratamento em 2021-07):\n")
idx_p <- which(grepl("tratado:pos_placebo", rownames(placebo_robust)))
if (length(idx_p) > 0) {
  cat(sprintf("  Coef: %.4f, SE: %.4f, p: %.4f\n",
              placebo_robust[idx_p, 1], placebo_robust[idx_p, 2],
              placebo_robust[idx_p, 4]))
  cat("  → Esperado: NÃO significativo (validação de tendências paralelas)\n")
}

# Salvar sumário do placebo
placebo_result <- data.frame(
  teste = "Placebo (falso tratamento 2021-07)",
  coef = round(placebo_robust[idx_p, 1], 4),
  se = round(placebo_robust[idx_p, 2], 4),
  p_valor = round(placebo_robust[idx_p, 4], 4),
  conclusao = ifelse(placebo_robust[idx_p, 4] > 0.05,
                     "Não rejeita H0 - Tendências paralelas validadas",
                     "Rejeita H0 - Tendências não paralelas"),
  stringsAsFactors = FALSE
)
write_csv(placebo_result, file.path(tab_dir, "tabela7_placebo.csv"))

# ══════════════════════════════════════════════════════════════════
# Salvar objetos dos modelos
# ══════════════════════════════════════════════════════════════════
save(m1a, m1b, m1c, m2, m3, m4, m5, m_placebo,
     file = file.path(mod_dir, "modelos_did.RData"))

cat("\n══════════════════════════════════════\n")
cat("  MODELOS DiD ESTIMADOS E SALVOS\n")
cat("══════════════════════════════════════\n")
