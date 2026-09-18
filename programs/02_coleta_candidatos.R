# =============================================================================
# Candidatos do NOVO em PE, eleicoes gerais de 2018/2022 (electionsBR/TSE) ->
# data/raw/candidate_<ano>.rds (bruto, todos os partidos -- o filtro pro NOVO
# acontece em 05_consolidar.R).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(electionsBR)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

for (ano in ANOS_ELEICOES_GERAIS) {
  destino <- sprintf("data/raw/candidate_%d.rds", ano)
  if (file.exists(destino)) {
    cat(sprintf("Ja existe, pulando: %s\n", destino))
    next
  }
  cat(sprintf("Baixando candidatos %d/%s...\n", ano, UF_ALVO))
  dados <- baixar_com_retry(elections_tse, year = ano, type = "candidate", uf = UF_ALVO, temp = FALSE)
  saveRDS(dados, destino)
  cat(sprintf("  %s salvo: %d linhas\n", destino, nrow(dados)))
}
