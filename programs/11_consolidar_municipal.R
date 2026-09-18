# =============================================================================
# Consolida os votos municipais (Prefeito/Vereador, 2020/2024, 1o turno) no
# nivel de BAIRRO -- data/processed/votos_candidato_bairro.rds,
# votos_partido_bairro.rds, total_validos_bairro.rds,
# candidatos_novo_municipais.rds. Mesmo papel do 05_consolidar.R, mas a
# origem e vote_section (por secao) em vez de vote_mun_zone/party_mun_zone --
# nao existe um pull pre-agregado por partido nas eleicoes municipais, entao
# o voto de legenda e identificado direto no vote_section (SQ_CANDIDATO==-3,
# NR_VOTAVEL = numero do partido -- confirmado ao vivo). SQ_CANDIDATO==-1 e
# branco/nulo (excluido, nao e voto valido). Voto sem local resolvido
# (fora_do_bairro ou local sem chave_bairro) fica de fora da agregacao --
# mesmo padrao de exclusao ja usado p/ municipio sem geometria de bairro (ver
# 09_bairros_municipios_alvo.R e o cabecalho de 10_locais_votacao_geocodificados.R).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(dplyr)

locais <- readRDS("data/processed/locais_votacao_municipal.rds")
stopifnot("Rode antes 10_locais_votacao_geocodificados.R" = !is.null(locais))

bairros_pe <- readRDS("data/processed/bairros_pe.rds")
municipios_escopo <- sort(unique(bairros_pe$chave_municipio))

local_bairro <- locais %>%
  filter(!fora_do_bairro, !is.na(chave_bairro)) %>%
  distinct(chave_local, chave_bairro)

n_excluidos <- n_distinct(locais$chave_local) - nrow(local_bairro)
cat(sprintf("Locais de votacao no escopo: %d | com bairro resolvido: %d | excluidos (rural/fora de bairro): %d\n",
            n_distinct(locais$chave_local), nrow(local_bairro), n_excluidos))

# --- por ano: prepara vote_section (so cargos/municipios/turno do escopo) e
# atribui SG_PARTIDO a cada linha (candidato real via SQ_CANDIDATO; voto de
# legenda via NR_VOTAVEL == NR_PARTIDO) ---------------------------------------

preparar_ano <- function(ano) {
  vs <- readRDS(sprintf("data/raw/vote_section_%d.rds", ano))
  cand <- readRDS(sprintf("data/raw/candidate_%d.rds", ano)) %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO)) %>%
    filter(DS_CARGO %in% CARGOS_ALVO_MUNICIPAIS, NR_TURNO == 1) %>%
    distinct(SQ_CANDIDATO, DS_CARGO, SG_PARTIDO, NR_PARTIDO)

  crosswalk_partido <- cand %>% distinct(NR_PARTIDO, SG_PARTIDO)

  vs_escopo <- vs %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO), chave_municipio = normalizar_texto(NM_MUNICIPIO)) %>%
    filter(NR_TURNO == 1, DS_CARGO %in% CARGOS_ALVO_MUNICIPAIS, chave_municipio %in% municipios_escopo,
           SQ_CANDIDATO != -1) %>%
    mutate(chave_local = paste(chave_municipio, NR_ZONA, NR_LOCAL_VOTACAO, ANO_ELEICAO, sep = "|")) %>%
    inner_join(local_bairro, by = "chave_local")

  vs_cand <- vs_escopo %>%
    filter(SQ_CANDIDATO > 0) %>%
    inner_join(cand %>% select(SQ_CANDIDATO, SG_PARTIDO), by = "SQ_CANDIDATO")

  vs_legenda <- vs_escopo %>%
    filter(SQ_CANDIDATO == -3) %>%
    inner_join(crosswalk_partido, by = c("NR_VOTAVEL" = "NR_PARTIDO"))

  bind_rows(vs_cand, vs_legenda) %>%
    select(ano = ANO_ELEICAO, cargo = DS_CARGO, chave_local, chave_bairro, sg_partido = SG_PARTIDO,
           sq_candidato = SQ_CANDIDATO, qt_votos = QT_VOTOS)
}

votos_todos <- bind_rows(lapply(ANOS_ELEICOES_MUNICIPAIS, preparar_ano))

# --- votos do NOVO por candidato/bairro --------------------------------------

votos_candidato_bairro <- votos_todos %>%
  filter(sg_partido == PARTIDO_ALVO, sq_candidato > 0) %>%
  group_by(ano, cargo, chave_bairro, sq_candidato) %>%
  summarise(votos_nominais = sum(qt_votos, na.rm = TRUE), .groups = "drop")

# --- votos por partido/bairro (TODOS os partidos, p/ %-valida) --------------
# legenda so existe de fato p/ VEREADOR (proporcional) -- p/ PREFEITO
# (majoritario) o vote_section nunca gera linha SQ_CANDIDATO==-3, entao
# votos_legenda fica 0 organicamente, sem precisar de caso especial aqui.

votos_partido_bairro <- votos_todos %>%
  group_by(ano, cargo, chave_bairro, sg_partido) %>%
  summarise(
    votos_nominais = sum(qt_votos[sq_candidato > 0], na.rm = TRUE),
    votos_legenda  = sum(qt_votos[sq_candidato == -3], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(votos_validos_partido = votos_nominais + votos_legenda)

total_validos_bairro <- votos_partido_bairro %>%
  group_by(ano, cargo, chave_bairro) %>%
  summarise(total_validos = sum(votos_validos_partido, na.rm = TRUE), .groups = "drop")

# --- votos do NOVO por candidato/LOCAL DE VOTACAO (mapa de densidade) -------
# Mesma granularidade de chave_local que locais_votacao_municipal.rds
# (10_locais_votacao_geocodificados.R), pra juntar por essa chave na hora de
# montar o heatmap (R/dados.R::pontos_calor_bairro()). So o NOVO (nao todos os
# partidos, como votos_partido_bairro) -- o heatmap so precisa do peso do
# recorte do NOVO em cada ponto, nao de um denominador de %.

votos_candidato_local <- votos_todos %>%
  filter(sg_partido == PARTIDO_ALVO, sq_candidato > 0) %>%
  group_by(ano, cargo, chave_local, sq_candidato) %>%
  summarise(votos_nominais = sum(qt_votos, na.rm = TRUE), .groups = "drop")

votos_partido_local <- votos_todos %>%
  filter(sg_partido == PARTIDO_ALVO) %>%
  group_by(ano, cargo, chave_local) %>%
  summarise(
    votos_nominais = sum(qt_votos[sq_candidato > 0], na.rm = TRUE),
    votos_legenda  = sum(qt_votos[sq_candidato == -3], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(votos_validos_partido = votos_nominais + votos_legenda)

# --- candidatos do NOVO (1 linha por candidato/ano/cargo) --------------------

candidatos_novo_municipais <- bind_rows(lapply(ANOS_ELEICOES_MUNICIPAIS, function(ano) {
  cand_raw <- readRDS(sprintf("data/raw/candidate_%d.rds", ano))
  cand_raw %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO), chave_municipio = normalizar_texto(NM_UE)) %>%
    filter(SG_PARTIDO == PARTIDO_ALVO, DS_CARGO %in% CARGOS_ALVO_MUNICIPAIS, NR_TURNO == 1,
           chave_municipio %in% municipios_escopo) %>%
    transmute(
      ano = ano,
      cargo = DS_CARGO,
      chave_municipio = chave_municipio,
      sq_candidato = SQ_CANDIDATO,
      nr_candidato = NR_CANDIDATO,
      nm_candidato = NM_CANDIDATO,
      nm_urna = NM_URNA_CANDIDATO,
      situacao = classificar_situacao(DS_SIT_TOT_TURNO)
    ) %>%
    distinct()
}))

# --- validacao ----------------------------------------------------------------

stopifnot(
  "Cargo(s) inesperado(s) em candidatos_novo_municipais" =
    all(candidatos_novo_municipais$cargo %in% CARGOS_ALVO_MUNICIPAIS),
  "chave_bairro em votos_candidato_bairro sem correspondencia em bairros_pe" =
    length(setdiff(unique(votos_candidato_bairro$chave_bairro), unique(bairros_pe$chave_bairro))) == 0
)

# DS_SIT_TOT_TURNO == "#NULO" (placeholder de dado ausente do TSE, confirmado
# ao vivo: 4 candidatos a vereador em 2024) vira situacao NA -- caso real, nao
# um bug de parsing (a candidatura existe, so a situacao final nao veio
# preenchida na base). badge_situacao() (R/perfil.R) ja trata NA de forma
# graciosa ("Situacao nao informada"), entao aqui so loga, nao trava o pipeline.
n_sem_situacao <- sum(is.na(candidatos_novo_municipais$situacao))
if (n_sem_situacao > 0) {
  cat(sprintf("\nAviso: %d candidato(s) do NOVO sem situacao classificada (DS_SIT_TOT_TURNO ausente/#NULO na base do TSE):\n", n_sem_situacao))
  print(candidatos_novo_municipais %>% filter(is.na(situacao)) %>% select(ano, cargo, chave_municipio, nm_urna))
}

saveRDS(votos_candidato_bairro, "data/processed/votos_candidato_bairro.rds")
saveRDS(votos_partido_bairro, "data/processed/votos_partido_bairro.rds")
saveRDS(total_validos_bairro, "data/processed/total_validos_bairro.rds")
saveRDS(votos_candidato_local, "data/processed/votos_candidato_local.rds")
saveRDS(votos_partido_local, "data/processed/votos_partido_local.rds")
saveRDS(candidatos_novo_municipais, "data/processed/candidatos_novo_municipais.rds")

cat("\nConsolidacao municipal concluida.\n\n")
cat("Candidatos do NOVO por ano/cargo/situacao:\n")
print(candidatos_novo_municipais %>% count(ano, cargo, situacao))
cat("\nVotos nominais do NOVO por ano/cargo (soma bairros):\n")
print(votos_candidato_bairro %>% group_by(ano, cargo) %>% summarise(votos = sum(votos_nominais), .groups = "drop"))
cat("\nArquivos gravados em data/processed/: votos_candidato_bairro.rds, ",
    "votos_partido_bairro.rds, total_validos_bairro.rds, ",
    "votos_candidato_local.rds, votos_partido_local.rds, ",
    "candidatos_novo_municipais.rds\n", sep = "")
