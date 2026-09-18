# =============================================================================
# Votos por SECAO ELEITORAL, eleicoes municipais de 2020/2024, PE (bruto,
# todos os partidos/municipios do estado -- o TSE nao filtra por municipio no
# pull, so por UF; o filtro pro NOVO/municipios-alvo acontece em
# 11_consolidar_municipal.R) -> data/raw/vote_section_<ano>.rds.
#
# type="vote_section" e a granularidade MAIS FINA que o TSE disponibiliza pra
# votos (abaixo de secao so existiria voto individual, que nao e publico) --
# e o unico jeito de amarrar voto a um LOCAL DE VOTACAO fisico (e dai a um
# bairro, via geocodificacao do endereco em 09/10). Cada linha ja vem com
# NR_LOCAL_VOTACAO/NM_LOCAL_VOTACAO/DS_LOCAL_VOTACAO_ENDERECO -- NAO precisa
# baixar o arquivo separado "eleitorado_local_votacao" do TSE, o endereco ja
# esta aqui.
#
# Volume: ~1.5M linhas / ~50MB pra PE inteiro num ano (medido ao vivo,
# ~1-2min de download) -- nada problematico, so maior que os pulls do
# pipeline geral (vote_mun_zone, agregado por municipio) por ser desagregado
# por secao.
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(electionsBR)

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

for (ano in ANOS_ELEICOES_MUNICIPAIS) {
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
