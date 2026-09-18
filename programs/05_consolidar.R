# =============================================================================
# Consolida os brutos de 02-04 + a geometria de 01 nos .rds finais que o app
# le (data/processed/). Mesmo papel do 08_consolidar_indicadores.R da
# referencia: junta, valida, salva -- nao faz chamada de rede.
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(dplyr)
library(sf)

municipios <- readRDS("data/processed/municipios_pe.rds")
stopifnot("Rode antes 01_municipios_pe.R" = !is.null(municipios))

fontes_faltando <- c(
  sprintf("data/raw/candidate_%d.rds", ANOS_ELEICOES_GERAIS),
  sprintf("data/raw/vote_mun_zone_%d.rds", ANOS_ELEICOES_GERAIS),
  sprintf("data/raw/party_mun_zone_%d.rds", ANOS_ELEICOES_GERAIS)
)
fontes_faltando <- fontes_faltando[!file.exists(fontes_faltando)]
stopifnot("Rode antes os scripts 02-04 (arquivo(s) bruto(s) faltando)" =
            length(fontes_faltando) == 0)

# --- candidatos do NOVO (1 linha por candidato/ano/cargo) -------------------
preparar_candidatos <- function(cand_raw, ano) {
  cand_raw %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO)) %>%
    filter(SG_PARTIDO == PARTIDO_ALVO, DS_CARGO %in% CARGOS_ALVO) %>%
    transmute(
      ano = ano,
      cargo = DS_CARGO,
      sq_candidato = SQ_CANDIDATO,
      nr_candidato = NR_CANDIDATO,
      nm_candidato = NM_CANDIDATO,
      nm_urna = NM_URNA_CANDIDATO,
      situacao = classificar_situacao(DS_SIT_TOT_TURNO)
    ) %>%
    distinct()
}

# --- votos nominais do NOVO por candidato/municipio (soma zonas) -----------
preparar_votos_candidato_municipio <- function(vmz_raw, ano) {
  vmz_raw %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO)) %>%
    filter(SG_PARTIDO == PARTIDO_ALVO, DS_CARGO %in% CARGOS_ALVO) %>%
    group_by(
      ano = ano, cargo = DS_CARGO, cd_municipio = CD_MUNICIPIO,
      nm_municipio = NM_MUNICIPIO, sq_candidato = SQ_CANDIDATO, nm_urna = NM_URNA_CANDIDATO
    ) %>%
    summarise(votos_nominais = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE), .groups = "drop")
}

# --- votos por partido/municipio (todos os partidos, p/ %-valida) ----------
# QT_VOTOS_NOMINAIS_VALIDOS + QT_VOTOS_LEGENDA_VALIDOS = total de votos
# validos do partido naquele municipio/cargo (validado empiricamente: soma
# nominal do party_mun_zone bate exatamente com a soma do vote_mun_zone
# candidato-a-candidato).
preparar_votos_partido_municipio <- function(pmz_raw, ano) {
  pmz_raw %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO)) %>%
    filter(DS_CARGO %in% CARGOS_ALVO) %>%
    group_by(ano = ano, cargo = DS_CARGO, cd_municipio = CD_MUNICIPIO,
             nm_municipio = NM_MUNICIPIO, sg_partido = SG_PARTIDO) %>%
    summarise(
      votos_legenda = sum(QT_VOTOS_LEGENDA_VALIDOS, na.rm = TRUE),
      votos_nominais = sum(QT_VOTOS_NOMINAIS_VALIDOS, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(votos_validos_partido = votos_legenda + votos_nominais)
}

candidatos_lista <- list()
votos_cand_lista <- list()
votos_partido_lista <- list()

for (ano in ANOS_ELEICOES_GERAIS) {
  cand_raw <- readRDS(sprintf("data/raw/candidate_%d.rds", ano))
  vmz_raw  <- readRDS(sprintf("data/raw/vote_mun_zone_%d.rds", ano))
  pmz_raw  <- readRDS(sprintf("data/raw/party_mun_zone_%d.rds", ano))

  candidatos_lista[[as.character(ano)]]    <- preparar_candidatos(cand_raw, ano)
  votos_cand_lista[[as.character(ano)]]    <- preparar_votos_candidato_municipio(vmz_raw, ano)
  votos_partido_lista[[as.character(ano)]] <- preparar_votos_partido_municipio(pmz_raw, ano)
}

candidatos_novo           <- bind_rows(candidatos_lista)
votos_candidato_municipio <- bind_rows(votos_cand_lista)
votos_partido_municipio   <- bind_rows(votos_partido_lista)

total_validos_municipio <- votos_partido_municipio %>%
  group_by(ano, cargo, cd_municipio, nm_municipio) %>%
  summarise(total_validos = sum(votos_validos_partido, na.rm = TRUE), .groups = "drop")

# --- chave de join com a geometria (nome normalizado) -----------------------
votos_partido_municipio$chave_municipio   <- normalizar_texto(votos_partido_municipio$nm_municipio)
votos_candidato_municipio$chave_municipio <- normalizar_texto(votos_candidato_municipio$nm_municipio)
total_validos_municipio$chave_municipio   <- normalizar_texto(total_validos_municipio$nm_municipio)

# --- validacao ---------------------------------------------------------------

chaves_municipios <- unique(st_drop_geometry(municipios)$chave_municipio)
sem_match <- setdiff(unique(votos_partido_municipio$chave_municipio), chaves_municipios)
stopifnot("Municipio(s) da TSE sem correspondencia na geometria (municipios_pe.rds)" =
            length(sem_match) == 0)

stopifnot(
  "Cargo(s) inesperado(s) em candidatos_novo — confira 02_coleta_candidatos.R" =
    all(candidatos_novo$cargo %in% CARGOS_ALVO),
  "Candidato(s) do NOVO sem situacao classificada (DS_SIT_TOT_TURNO novo/desconhecido)" =
    !any(is.na(candidatos_novo$situacao))
)

saveRDS(candidatos_novo, "data/processed/candidatos_novo.rds")
saveRDS(votos_candidato_municipio, "data/processed/votos_candidato_municipio.rds")
saveRDS(votos_partido_municipio, "data/processed/votos_partido_municipio.rds")
saveRDS(total_validos_municipio, "data/processed/total_validos_municipio.rds")

cat("Consolidacao concluida.\n\n")
cat("Candidatos do NOVO por ano/cargo/situacao:\n")
print(candidatos_novo %>% count(ano, cargo, situacao))
cat("\nArquivos gravados em data/processed/: candidatos_novo.rds, ",
    "votos_candidato_municipio.rds, votos_partido_municipio.rds, ",
    "total_validos_municipio.rds\n", sep = "")
