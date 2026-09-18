# =============================================================================
# Votos por SECAO ELEITORAL, eleicoes GERAIS (2018/2022), PE inteiro -- mesmo
# papel de 08_coleta_votos_secao_municipal.R (que ja cobre 2020/2024), aqui
# pros anos de Deputado Federal. Nao existia antes porque o pipeline geral
# (05_consolidar.R) usa vote_mun_zone/party_mun_zone, ja agregado por
# municipio/zona -- granularidade suficiente pro mapa/ranking, mas nao pro
# indice de representacao demografica (16_consolidar_secao.R), que precisa do
# voto por SECAO pra cruzar com o perfil do eleitorado (15_coleta_perfil_eleitorado.R).
#
# type="vote_section" e a granularidade mais fina que o TSE disponibiliza --
# volume esperado: mesma ordem de grandeza do que ja foi medido pras
# municipais (~1.5M linhas / ~50MB por ano pra PE inteiro).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(electionsBR)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

for (ano in ANOS_ELEICOES_GERAIS) {
  destino <- sprintf("data/raw/vote_section_%d.rds", ano)
  if (file.exists(destino)) {
    cat(sprintf("Ja existe, pulando: %s\n", destino))
    next
  }
  cat(sprintf("Baixando votos por secao %d/%s (pode levar alguns minutos)...\n", ano, UF_ALVO))
  dados <- baixar_com_retry(elections_tse, year = ano, type = "vote_section", uf = UF_ALVO, temp = FALSE,
                             tentativas = 4, espera_seg = 20)
  saveRDS(dados, destino)
  cat(sprintf("  %s salvo: %d linhas\n", destino, nrow(dados)))
}
