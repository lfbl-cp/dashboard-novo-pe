# =============================================================================
# Votos por partido/municipio/zona, eleicoes gerais de 2018/2022 em PE
# (electionsBR/TSE, type "party_mun_zone") -> data/raw/party_mun_zone_<ano>.rds
# (bruto, todos os partidos). Traz QT_VOTOS_LEGENDA_VALIDOS e
# QT_VOTOS_NOMINAIS_VALIDOS por partido -- e a fonte tanto do card "Votos na
# legenda" quanto do denominador de "% de votos validos" usado no mapa (soma
# de TODOS os partidos no municipio/cargo/ano).
#
# Obs.: o type "legends" do electionsBR NAO tem contagem de votos (so
# metadados de coligacao/denominacao) -- por isso "party_mun_zone", nao
# "legends".
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(electionsBR)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

for (ano in ANOS_ELEICOES_GERAIS) {
  destino <- sprintf("data/raw/party_mun_zone_%d.rds", ano)
  if (file.exists(destino)) {
    cat(sprintf("Ja existe, pulando: %s\n", destino))
    next
  }
  cat(sprintf("Baixando votos por partido/municipio/zona %d/%s...\n", ano, UF_ALVO))
  dados <- baixar_com_retry(elections_tse, year = ano, type = "party_mun_zone", uf = UF_ALVO, temp = FALSE)
  saveRDS(dados, destino)
  cat(sprintf("  %s salvo: %d linhas\n", destino, nrow(dados)))
}
