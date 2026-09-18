# =============================================================================
# Voto do NOVO por SECAO ELEITORAL, todas as eleicoes do dashboard (Dep.
# Federal 2018/2022 + Vereador/Prefeito 2020/2024) -- fonte do indice de
# representacao demografica (R/dados.R::indice_representacao()), que cruza
# isso com data/intermediate/perfil_eleitorado_secao.rds
# (15_coleta_perfil_eleitorado.R) pela chave_secao. MESMA formula de chave
# (chave_municipio|zona|secao|ano) usada nos dois lados, pra bater o join.
#
# Reaproveita o raciocinio de preparar_ano() de 05_consolidar.R/
# 11_consolidar_municipal.R (candidato via SQ_CANDIDATO, legenda via
# NR_VOTAVEL==NR_PARTIDO), so que aqui agrupado por SECAO (nao municipio nem
# bairro) e cobrindo os DOIS tipos de eleicao numa tabela so -- o indice de
# representacao nao depende de mapa/geometria, entao funciona em qualquer
# municipio com candidato do NOVO, nao so nos 8 com bairro do IBGE (a UI da
# aba municipal continua so oferecendo esses 8 no seletor, entao na pratica o
# indice nunca e pedido fora deles, mas a tabela em si nao precisa da mesma
# restricao do mapa).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(dplyr)

preparar_ano_secao <- function(ano, cargos_alvo) {
  vs <- readRDS(sprintf("data/raw/vote_section_%d.rds", ano))
  cand <- readRDS(sprintf("data/raw/candidate_%d.rds", ano)) %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO)) %>%
    filter(DS_CARGO %in% cargos_alvo, NR_TURNO == 1) %>%
    distinct(SQ_CANDIDATO, DS_CARGO, SG_PARTIDO, NR_PARTIDO)

  crosswalk_partido <- cand %>% distinct(NR_PARTIDO, SG_PARTIDO)

  vs_escopo <- vs %>%
    mutate(DS_CARGO = normalizar_cargo(DS_CARGO), chave_municipio = normalizar_texto(NM_MUNICIPIO)) %>%
    filter(NR_TURNO == 1, DS_CARGO %in% cargos_alvo, SQ_CANDIDATO != -1) %>%
    mutate(chave_secao = paste(chave_municipio, NR_ZONA, NR_SECAO, ano, sep = "|"))

  vs_cand <- vs_escopo %>%
    filter(SQ_CANDIDATO > 0) %>%
    inner_join(cand %>% select(SQ_CANDIDATO, SG_PARTIDO), by = "SQ_CANDIDATO")

  vs_legenda <- vs_escopo %>%
    filter(SQ_CANDIDATO == -3) %>%
    inner_join(crosswalk_partido, by = c("NR_VOTAVEL" = "NR_PARTIDO"))

  bind_rows(vs_cand, vs_legenda) %>%
    select(ano = ANO_ELEICAO, cargo = DS_CARGO, chave_municipio, chave_secao, sg_partido = SG_PARTIDO,
           sq_candidato = SQ_CANDIDATO, qt_votos = QT_VOTOS)
}

votos_todos <- bind_rows(
  bind_rows(lapply(ANOS_ELEICOES_GERAIS, preparar_ano_secao, cargos_alvo = "DEPUTADO FEDERAL")),
  bind_rows(lapply(ANOS_ELEICOES_MUNICIPAIS, preparar_ano_secao, cargos_alvo = CARGOS_ALVO_MUNICIPAIS))
)

votos_candidato_secao <- votos_todos %>%
  filter(sg_partido == PARTIDO_ALVO, sq_candidato > 0) %>%
  group_by(ano, cargo, chave_municipio, chave_secao, sq_candidato) %>%
  summarise(votos_nominais = sum(qt_votos, na.rm = TRUE), .groups = "drop")

votos_partido_secao <- votos_todos %>%
  filter(sg_partido == PARTIDO_ALVO) %>%
  group_by(ano, cargo, chave_municipio, chave_secao) %>%
  summarise(
    votos_nominais = sum(qt_votos[sq_candidato > 0], na.rm = TRUE),
    votos_legenda  = sum(qt_votos[sq_candidato == -3], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(votos_validos_partido = votos_nominais + votos_legenda)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(votos_candidato_secao, "data/processed/votos_candidato_secao.rds")
saveRDS(votos_partido_secao, "data/processed/votos_partido_secao.rds")

cat("Consolidacao por secao concluida.\n\n")
cat("Votos nominais do NOVO por ano/cargo (soma secoes):\n")
print(votos_candidato_secao %>% group_by(ano, cargo) %>% summarise(votos = sum(votos_nominais), .groups = "drop"))
cat("\nArquivos gravados em data/processed/: votos_candidato_secao.rds, votos_partido_secao.rds\n")
