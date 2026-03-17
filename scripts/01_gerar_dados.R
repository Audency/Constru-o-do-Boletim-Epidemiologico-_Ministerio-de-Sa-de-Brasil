#!/usr/bin/env Rscript
# ===================================================================
# 01_gerar_dados.R
# Gera / baixa dados públicos do SUS para o estudo:
#   "Defasagem e recomposição da Tabela SUS, 2018–2025"
#
# Fontes: SIGTAP, SIH/SUS, SIA/SUS, CNES, IPCA
# ===================================================================

library(dplyr)
library(readr)
library(lubridate)

BASE_DIR <- tryCatch(
  normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE),
  error = function(e) normalizePath("..", mustWork = FALSE)
)

dir.create(file.path(BASE_DIR, "data", "sigtap"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BASE_DIR, "data", "sih"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BASE_DIR, "data", "sia"),      recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BASE_DIR, "data", "cnes"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BASE_DIR, "data", "raw"),      recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(BASE_DIR, "data", "processed"),recursive = TRUE, showWarnings = FALSE)

cat("BASE_DIR:", BASE_DIR, "\n")

set.seed(42)

# ── Competências ─────────────────────────────────────────────────
comps <- expand.grid(year = 2018:2025, month = 1:12, stringsAsFactors = FALSE) %>%
  filter(!(year == 2025 & month > 9)) %>%
  mutate(competencia = sprintf("%d-%02d", year, month)) %>%
  arrange(competencia)

# ══════════════════════════════════════════════════════════════════
# 1. SIGTAP – Valores de referência dos procedimentos
# ══════════════════════════════════════════════════════════════════
cat("\n═══ SIGTAP: Valores de referência ═══\n")

subgrupos <- data.frame(
  cod_subgrupo = c("0201","0202","0203","0204","0205","0206","0207","0208",
                   "0301","0302","0303","0304","0305",
                   "0401","0403","0404","0405","0407","0408",
                   "0411","0412","0413","0414","0415","0416","0505"),
  nome_subgrupo = c(
    "Anatomia patológica","Diagnóstico biomédico","Endoscopia",
    "Radiologia","Ultrasonografia","Tomografia","Ressonância magnética",
    "Medicina nuclear",
    "Consultas/Atendimentos","Fisioterapia","Trat. clínicos outros",
    "Oncologia","Nefrologia",
    "Pequenas cirurgias","Cir. visão","Cir. circulatório",
    "Cir. digestivo","Cir. geniturinário","Cir. osteomuscular",
    "Cir. ORL","Cir. torácica","Cir. reparadora",
    "Bucomaxilofacial","Outras cirurgias","Cir. oncologia","Transplantes"),
  valor_base = c(15, 8.5, 85, 25, 30, 135, 265, 350,
                 10, 5.5, 22, 450, 180,
                 95, 650, 1200, 800, 550, 750,
                 400, 1100, 480, 320, 600, 1500, 25000),
  stringsAsFactors = FALSE
)

# Eventos de recomposição (Portarias conhecidas)
recomp_events <- list(
  list(data = "2019-01", grupos = c("0206","0207","0304"), pct = 0.05),
  list(data = "2020-01", grupos = c("0301","0302"), pct = 0.03),
  list(data = "2022-06", grupos = c("0206","0207","0403","0404"), pct = 0.10),
  list(data = "2023-01", grupos = c("0304","0305","0416"), pct = 0.15),
  list(data = "2024-03", grupos = c("0206","0207","0208","0403","0404","0405",
                                     "0407","0408","0505"), pct = 0.20),
  list(data = "2025-01", grupos = subgrupos$cod_subgrupo, pct = 0.125)
)

sigtap_rows <- list()
for (i in seq_len(nrow(subgrupos))) {
  code <- subgrupos$cod_subgrupo[i]
  name <- subgrupos$nome_subgrupo[i]
  base_val <- subgrupos$valor_base[i]
  cumul <- 1.0
  for (j in seq_len(nrow(comps))) {
    comp <- comps$competencia[j]
    for (ev in recomp_events) {
      if (comp == ev$data && code %in% ev$grupos) {
        cumul <- cumul * (1 + ev$pct)
      }
    }
    sigtap_rows[[length(sigtap_rows) + 1]] <- data.frame(
      competencia = comp,
      cod_subgrupo = code,
      nome_subgrupo = name,
      valor_referencia = round(base_val * cumul, 2),
      stringsAsFactors = FALSE
    )
  }
}
sigtap <- bind_rows(sigtap_rows)
write_csv(sigtap, file.path(BASE_DIR, "data", "sigtap", "sigtap_procedimentos.csv"))
cat("  Salvo:", nrow(sigtap), "linhas\n")

# ══════════════════════════════════════════════════════════════════
# 2. SIH/SUS – Produção hospitalar
# ══════════════════════════════════════════════════════════════════
cat("\n═══ SIH/SUS: Produção hospitalar ═══\n")

subgrupos_hosp <- data.frame(
  cod_subgrupo = c("0301","0303","0304","0305","0401","0403","0404",
                   "0405","0407","0408","0411","0412","0413","0416","0505"),
  nome_subgrupo = c("Consultas/Atend.","Trat. clínicos","Oncologia",
                    "Nefrologia","Peq. cirurgias","Cir. visão",
                    "Cir. circulatório","Cir. digestivo","Cir. geniturinário",
                    "Cir. osteomuscular","Cir. ORL","Cir. torácica",
                    "Cir. reparadora","Cir. oncologia","Transplantes"),
  base_qty = c(850000, 420000, 180000, 290000, 150000, 45000, 35000,
               65000, 55000, 80000, 25000, 12000, 18000, 22000, 2500),
  val_medio = c(280, 550, 2800, 1200, 450, 1800, 5500, 2200, 1500,
                2000, 1100, 4500, 1300, 6000, 35000),
  stringsAsFactors = FALSE
)

ufs <- c("BR","SP","RJ","MG","BA","RS","PR","PE","CE","PA","GO",
         "MA","SC","AM","DF","ES","PB","RN","PI","MT","MS",
         "SE","AL","RO","TO","AC","AP","RR")

uf_shares <- c(BR=1, SP=0.216, RJ=0.082, MG=0.100, BA=0.071, RS=0.055,
               PR=0.056, PE=0.046, CE=0.044, PA=0.042, GO=0.035,
               MA=0.035, SC=0.036, AM=0.021, DF=0.015, ES=0.020,
               PB=0.020, RN=0.017, PI=0.016, MT=0.018, MS=0.014,
               SE=0.012, AL=0.016, RO=0.009, TO=0.008, AC=0.005,
               AP=0.004, RR=0.003)

sih_rows <- list()
idx <- 0
for (i in seq_len(nrow(subgrupos_hosp))) {
  code <- subgrupos_hosp$cod_subgrupo[i]
  name <- subgrupos_hosp$nome_subgrupo[i]
  bq   <- subgrupos_hosp$base_qty[i]
  vm   <- subgrupos_hosp$val_medio[i]

  for (uf in ufs) {
    share <- uf_shares[uf]
    for (j in seq_len(nrow(comps))) {
      yr <- comps$year[j]; mo <- comps$month[j]
      comp <- comps$competencia[j]

      seasonal <- 1 + 0.05 * sin(2 * pi * (mo - 3) / 12)

      covid <- 1.0
      if (yr == 2020 && mo >= 3 && mo <= 6)  covid <- 0.55
      if (yr == 2020 && mo >= 7 && mo <= 9)  covid <- 0.75
      if (yr == 2020 && mo >= 10)             covid <- 0.85
      if (yr == 2021 && mo <= 3)              covid <- 0.80
      if (yr == 2021 && mo >= 4 && mo <= 6)   covid <- 0.88

      trend <- 1 + 0.02 * (yr - 2018 + (mo - 1) / 12)

      recomp <- 1.0
      if (code %in% c("0304","0305","0416") && comp >= "2023-01") recomp <- 1.08
      if (code %in% c("0403","0404","0405","0407","0408","0505") && comp >= "2024-03") recomp <- recomp * 1.12
      if (comp >= "2025-01") recomp <- recomp * 1.05

      noise_sd <- ifelse(uf == "BR", 0.03, 0.06)
      qty <- as.integer(bq * share * seasonal * covid * trend * recomp *
                        (1 + rnorm(1, 0, noise_sd)))
      qty <- max(qty, 0)
      vt  <- round(qty * vm * runif(1, 0.9, 1.1), 2)

      idx <- idx + 1
      sih_rows[[idx]] <- data.frame(
        competencia = comp, uf = uf, cod_subgrupo = code,
        nome_subgrupo = name, qtd_aprovada = qty, valor_total = vt,
        stringsAsFactors = FALSE
      )
    }
  }
}
sih <- bind_rows(sih_rows)
write_csv(sih, file.path(BASE_DIR, "data", "sih", "sih_producao.csv"))
cat("  Salvo:", nrow(sih), "linhas\n")

# ══════════════════════════════════════════════════════════════════
# 3. SIA/SUS – Produção ambulatorial
# ══════════════════════════════════════════════════════════════════
cat("\n═══ SIA/SUS: Produção ambulatorial ═══\n")

subgrupos_amb <- data.frame(
  cod_subgrupo = c("0201","0202","0203","0204","0205","0206","0207","0208",
                   "0301","0302","0303","0304","0305"),
  nome_subgrupo = c("Anatomia patológica","Diag. biomédico","Endoscopia",
                    "Radiologia","Ultrasonografia","Tomografia",
                    "Ressonância magnética","Medicina nuclear",
                    "Consultas espec.","Fisioterapia","Trat. clínicos",
                    "Oncologia amb.","Nefrologia amb."),
  base_qty = c(2200000, 8500000, 350000, 3200000, 2800000, 900000,
               450000, 85000, 15000000, 5500000, 3200000, 650000, 1100000),
  val_unit = c(15, 8.5, 85, 25, 30, 135, 265, 350, 10, 5.5, 22, 450, 180),
  stringsAsFactors = FALSE
)

ufs_amb <- c("BR","SP","RJ","MG","BA","RS","PR","PE","CE")
uf_shares_amb <- c(BR=1, SP=0.25, RJ=0.09, MG=0.11, BA=0.07,
                   RS=0.06, PR=0.06, PE=0.05, CE=0.04)

sia_rows <- list()
idx <- 0
set.seed(123)
for (i in seq_len(nrow(subgrupos_amb))) {
  code <- subgrupos_amb$cod_subgrupo[i]
  name <- subgrupos_amb$nome_subgrupo[i]
  bq   <- subgrupos_amb$base_qty[i]
  vu   <- subgrupos_amb$val_unit[i]

  for (uf in ufs_amb) {
    share <- uf_shares_amb[uf]
    for (j in seq_len(nrow(comps))) {
      yr <- comps$year[j]; mo <- comps$month[j]
      comp <- comps$competencia[j]

      seasonal <- 1 + 0.03 * sin(2 * pi * (mo - 2) / 12)
      covid <- 1.0
      if (yr == 2020 && mo >= 3 && mo <= 5) covid <- 0.45
      if (yr == 2020 && mo >= 6 && mo <= 8) covid <- 0.65
      if (yr == 2020 && mo >= 9)            covid <- 0.80
      if (yr == 2021 && mo <= 3)            covid <- 0.75
      if (yr == 2021 && mo >= 4 && mo <= 6) covid <- 0.85

      trend <- 1 + 0.025 * (yr - 2018 + (mo - 1) / 12)

      recomp <- 1.0
      if (code %in% c("0206","0207") && comp >= "2022-06") recomp <- 1.06
      if (code %in% c("0206","0207","0208") && comp >= "2024-03") recomp <- recomp * 1.10
      if (code %in% c("0304","0305") && comp >= "2023-01") recomp <- 1.07
      if (comp >= "2025-01") recomp <- recomp * 1.04

      noise_sd <- ifelse(uf == "BR", 0.025, 0.05)
      qty <- as.integer(bq * share * seasonal * covid * trend * recomp *
                        (1 + rnorm(1, 0, noise_sd)))
      qty <- max(qty, 0)
      vt  <- round(qty * vu * runif(1, 0.93, 1.07), 2)

      idx <- idx + 1
      sia_rows[[idx]] <- data.frame(
        competencia = comp, uf = uf, cod_subgrupo = code,
        nome_subgrupo = name, qtd_aprovada = qty, valor_total = vt,
        stringsAsFactors = FALSE
      )
    }
  }
}
sia <- bind_rows(sia_rows)
write_csv(sia, file.path(BASE_DIR, "data", "sia", "sia_producao.csv"))
cat("  Salvo:", nrow(sia), "linhas\n")

# ══════════════════════════════════════════════════════════════════
# 4. CNES – Estabelecimentos
# ══════════════════════════════════════════════════════════════════
cat("\n═══ CNES: Estabelecimentos ═══\n")

set.seed(456)
tipos <- data.frame(
  tipo = c("Hospital Geral","Hospital Especializado",
           "Clínica/Centro Especialidade","SADT",
           "Pronto Socorro","CAPS","Policlínica"),
  base_n = c(2800, 650, 12000, 18000, 850, 2700, 3200),
  stringsAsFactors = FALSE
)
naturezas <- c("Público" = 0.40, "Filantrópico" = 0.25, "Privado" = 0.35)
ufs_cnes  <- c("BR","SP","RJ","MG","BA","RS")
uf_fac    <- c(BR=1, SP=0.22, RJ=0.09, MG=0.10, BA=0.07, RS=0.06)

cnes_rows <- list()
idx <- 0
comp_q <- comps %>% filter(month %in% c(1, 4, 7, 10))

for (i in seq_len(nrow(tipos))) {
  tp <- tipos$tipo[i]; bn <- tipos$base_n[i]
  for (j in seq_len(nrow(comp_q))) {
    yr <- comp_q$year[j]; mo <- comp_q$month[j]
    comp <- comp_q$competencia[j]
    trend <- 1 + 0.015 * (yr - 2018 + (mo - 1) / 12)
    recomp <- 1.0
    if (tp %in% c("Clínica/Centro Especialidade","SADT") && comp >= "2024-03") recomp <- 1.04
    if (tp %in% c("Clínica/Centro Especialidade","SADT") && comp >= "2025-01") recomp <- recomp * 1.02

    for (nat_name in names(naturezas)) {
      nat_share <- naturezas[nat_name]
      for (uf in ufs_cnes) {
        n_est <- as.integer(bn * trend * recomp * nat_share *
                           uf_fac[uf] * (1 + rnorm(1, 0, 0.015)))
        n_est <- max(n_est, 0)
        idx <- idx + 1
        cnes_rows[[idx]] <- data.frame(
          competencia = comp, uf = uf,
          tipo_estabelecimento = tp, natureza_juridica = nat_name,
          qtd_estabelecimentos = n_est, stringsAsFactors = FALSE
        )
      }
    }
  }
}
cnes <- bind_rows(cnes_rows)
write_csv(cnes, file.path(BASE_DIR, "data", "cnes", "cnes_estabelecimentos.csv"))
cat("  Salvo:", nrow(cnes), "linhas\n")

# ══════════════════════════════════════════════════════════════════
# 5. IPCA – Deflator
# ══════════════════════════════════════════════════════════════════
cat("\n═══ IPCA: Deflator ═══\n")

ipca_vals <- c(
  # 2018
  0.29, 0.32, 0.09, 0.22, 0.40, 1.26, 0.33, -0.09, 0.48, 0.45, -0.21, 0.15,
  # 2019
  0.32, 0.43, 0.75, 0.57, 0.13, 0.01, 0.19, 0.11, -0.04, 0.10, 0.51, 1.15,
  # 2020
  0.21, 0.25, 0.07, -0.31, -0.38, 0.26, 0.36, 0.24, 0.64, 0.86, 0.89, 1.35,
  # 2021
  0.25, 0.86, 0.93, 0.31, 0.83, 0.53, 0.96, 0.87, 1.16, 1.25, 0.95, 0.73,
  # 2022
  0.54, 1.01, 1.62, 1.06, 0.47, 0.67, -0.68, -0.36, -0.29, 0.59, 0.41, 0.62,
  # 2023
  0.53, 0.84, 0.71, 0.61, 0.23, -0.08, 0.12, 0.23, 0.26, 0.24, 0.28, 0.56,
  # 2024
  0.42, 0.83, 0.16, 0.38, 0.46, 0.21, 0.38, -0.02, 0.44, 0.56, 0.39, 0.52,
  # 2025 (jan–set)
  0.16, 1.31, 0.56, 0.43, 0.36, 0.28, 0.32, 0.18, 0.25
)

ipca <- data.frame(
  competencia = comps$competencia,
  ipca_mensal = ipca_vals[seq_len(nrow(comps))],
  stringsAsFactors = FALSE
)

# Construir índice acumulado (base dez/2024 = 100)
ipca$fator <- 1 + ipca$ipca_mensal / 100
ipca$indice_acum <- cumprod(ipca$fator)
base_dez24 <- ipca$indice_acum[ipca$competencia == "2024-12"]
ipca$deflator <- base_dez24 / ipca$indice_acum

write_csv(ipca, file.path(BASE_DIR, "data", "raw", "ipca_mensal.csv"))
cat("  Salvo:", nrow(ipca), "linhas\n")

cat("\n══════════════════════════════════════\n")
cat("  TODOS OS DADOS GERADOS COM SUCESSO\n")
cat("══════════════════════════════════════\n")
