#!/usr/bin/env Rscript
# ===================================================================
# 00_executar_tudo.R
# Script principal que orquestra toda a análise
# ===================================================================

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  Defasagem e recomposição da Tabela SUS                    ║\n")
cat("║  Efeitos sobre produção e oferta de atenção especializada  ║\n")
cat("║  Brasil, 2018–2025                                        ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# Definir diretório base
script_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) getwd())
if (is.null(script_dir) || script_dir == ".") script_dir <- getwd()
setwd(script_dir)

t0 <- Sys.time()

cat("Etapa 1/4: Gerando dados...\n")
source("01_gerar_dados.R")

cat("\nEtapa 2/4: Processando dados...\n")
source("02_processar_dados.R")

cat("\nEtapa 3/4: Gerando tabelas e figuras descritivas...\n")
source("03_tabelas_figuras.R")

cat("\nEtapa 4/4: Estimando modelos DiD...\n")
source("04_modelos_did.R")

t1 <- Sys.time()
cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║  ANÁLISE COMPLETA                                          ║\n")
cat(sprintf("║  Tempo total: %.1f minutos                                ║\n",
            as.numeric(difftime(t1, t0, units = "mins"))))
cat("╚══════════════════════════════════════════════════════════════╝\n")
