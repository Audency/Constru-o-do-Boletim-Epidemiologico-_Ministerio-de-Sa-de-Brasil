#!/usr/bin/env Rscript
# ===================================================================
# 02_processar_dados.R
# Processa e limpa os dados brutos, gera datasets analíticos
# ===================================================================

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

BASE_DIR <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE),
  error = function(e) normalizePath("..", mustWork = FALSE)
)

cat("Processando dados...\n")

# ── Carregar dados ────────────────────────────────────────────────
sigtap <- read_csv(file.path(BASE_DIR, "data", "sigtap", "sigtap_procedimentos.csv"),
                   show_col_types = FALSE)
sih    <- read_csv(file.path(BASE_DIR, "data", "sih", "sih_producao.csv"),
                   show_col_types = FALSE)
sia    <- read_csv(file.path(BASE_DIR, "data", "sia", "sia_producao.csv"),
                   show_col_types = FALSE)
cnes   <- read_csv(file.path(BASE_DIR, "data", "cnes", "cnes_estabelecimentos.csv"),
                   show_col_types = FALSE)
ipca   <- read_csv(file.path(BASE_DIR, "data", "raw", "ipca_mensal.csv"),
                   show_col_types = FALSE)

# ── 1. Deflacionar valores ───────────────────────────────────────
cat("  Deflacionando valores...\n")

# SIGTAP com valores reais
sigtap <- sigtap %>%
  left_join(ipca %>% select(competencia, deflator), by = "competencia") %>%
  mutate(valor_real = round(valor_referencia * deflator, 2))

# SIH deflacionado
sih <- sih %>%
  left_join(ipca %>% select(competencia, deflator), by = "competencia") %>%
  mutate(
    valor_total_real = round(valor_total * deflator, 2),
    valor_medio = ifelse(qtd_aprovada > 0, valor_total / qtd_aprovada, 0),
    valor_medio_real = ifelse(qtd_aprovada > 0, valor_total_real / qtd_aprovada, 0)
  )

# SIA deflacionado
sia <- sia %>%
  left_join(ipca %>% select(competencia, deflator), by = "competencia") %>%
  mutate(
    valor_total_real = round(valor_total * deflator, 2),
    valor_medio = ifelse(qtd_aprovada > 0, valor_total / qtd_aprovada, 0),
    valor_medio_real = ifelse(qtd_aprovada > 0, valor_total_real / qtd_aprovada, 0)
  )

# ── 2. Classificar grupos: tratamento vs controle (DiD) ──────────
cat("  Classificando grupos tratamento/controle...\n")

# Tratamento: procedimentos que receberam recomposição significativa
# em 2024 (Portaria 6.465/2024 e anteriores)
tratamento_2024 <- c("0206","0207","0208","0403","0404","0405",
                      "0407","0408","0505")
tratamento_2023 <- c("0304","0305","0416")

# Controle: procedimentos sem recomposição expressiva antes de 2025
controle <- c("0201","0202","0203","0204","0205","0301","0302","0303",
              "0401","0411","0412","0413","0414","0415")

# Marcar nos datasets
classificar_grupo <- function(df) {
  df %>%
    mutate(
      grupo_did = case_when(
        cod_subgrupo %in% tratamento_2024 ~ "tratamento_2024",
        cod_subgrupo %in% tratamento_2023 ~ "tratamento_2023",
        cod_subgrupo %in% controle ~ "controle",
        TRUE ~ "outros"
      ),
      tratado = as.integer(grupo_did %in% c("tratamento_2024", "tratamento_2023")),
      # Período pós-tratamento
      pos_2023 = as.integer(competencia >= "2023-01"),
      pos_2024 = as.integer(competencia >= "2024-03"),
      pos_2025 = as.integer(competencia >= "2025-01"),
      # Interações DiD
      did_2023 = as.integer(cod_subgrupo %in% tratamento_2023 & competencia >= "2023-01"),
      did_2024 = as.integer(cod_subgrupo %in% tratamento_2024 & competencia >= "2024-03"),
      # Tempo numérico
      ano = as.integer(substr(competencia, 1, 4)),
      mes = as.integer(substr(competencia, 6, 7)),
      tempo = (ano - 2018) * 12 + mes,
      data = as.Date(paste0(competencia, "-01"))
    )
}

sigtap <- classificar_grupo(sigtap)
sih    <- classificar_grupo(sih)
sia    <- classificar_grupo(sia)

# ── 3. Agregar dados nacionais para análise principal ─────────────
cat("  Agregando dados nacionais...\n")

sih_br <- sih %>% filter(uf == "BR")
sia_br <- sia %>% filter(uf == "BR")

# Combinar SIH + SIA para visão total da produção
producao_total <- bind_rows(
  sih_br %>% mutate(sistema = "SIH"),
  sia_br %>% mutate(sistema = "SIA")
)

# ── 4. Preparar painel para DiD ──────────────────────────────────
cat("  Preparando painel para DiD...\n")

# Painel: procedimento × tempo (nacional)
painel_sih <- sih_br %>%
  group_by(cod_subgrupo, nome_subgrupo, competencia, grupo_did, tratado,
           pos_2023, pos_2024, pos_2025, did_2023, did_2024, tempo, data) %>%
  summarise(
    qtd_aprovada = sum(qtd_aprovada),
    valor_total_real = sum(valor_total_real),
    .groups = "drop"
  ) %>%
  mutate(log_qtd = log(qtd_aprovada + 1))

painel_sia <- sia_br %>%
  group_by(cod_subgrupo, nome_subgrupo, competencia, grupo_did, tratado,
           pos_2023, pos_2024, pos_2025, did_2023, did_2024, tempo, data) %>%
  summarise(
    qtd_aprovada = sum(qtd_aprovada),
    valor_total_real = sum(valor_total_real),
    .groups = "drop"
  ) %>%
  mutate(log_qtd = log(qtd_aprovada + 1))

# ── 5. CNES: evolução da rede ────────────────────────────────────
cat("  Processando CNES...\n")

cnes_br <- cnes %>%
  filter(uf == "BR") %>%
  mutate(
    ano = as.integer(substr(competencia, 1, 4)),
    data = as.Date(paste0(competencia, "-01"))
  )

cnes_total <- cnes_br %>%
  group_by(competencia, data, tipo_estabelecimento) %>%
  summarise(total_estab = sum(qtd_estabelecimentos), .groups = "drop")

cnes_natureza <- cnes_br %>%
  group_by(competencia, data, natureza_juridica) %>%
  summarise(total_estab = sum(qtd_estabelecimentos), .groups = "drop")

# ── 6. Salvar datasets processados ───────────────────────────────
cat("  Salvando datasets processados...\n")

proc_dir <- file.path(BASE_DIR, "data", "processed")
write_csv(sigtap,        file.path(proc_dir, "sigtap_processado.csv"))
write_csv(sih_br,        file.path(proc_dir, "sih_br.csv"))
write_csv(sia_br,        file.path(proc_dir, "sia_br.csv"))
write_csv(producao_total,file.path(proc_dir, "producao_total.csv"))
write_csv(painel_sih,    file.path(proc_dir, "painel_sih.csv"))
write_csv(painel_sia,    file.path(proc_dir, "painel_sia.csv"))
write_csv(cnes_total,    file.path(proc_dir, "cnes_total.csv"))
write_csv(cnes_natureza, file.path(proc_dir, "cnes_natureza.csv"))
write_csv(ipca,          file.path(proc_dir, "ipca_processado.csv"))

# Salvar SIH por UF para análises regionais
sih_uf <- sih %>% filter(uf != "BR")
write_csv(sih_uf, file.path(proc_dir, "sih_uf.csv"))

cat("\n══════════════════════════════════════\n")
cat("  PROCESSAMENTO CONCLUÍDO\n")
cat("══════════════════════════════════════\n")
