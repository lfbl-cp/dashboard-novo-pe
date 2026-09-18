# =============================================================================
# Perfil enriquecido de cada candidato do NOVO (demografia, gasto de campanha
# declarado, redes sociais, foto oficial) via API publica do TSE
# DivulgaCandContas (a mesma que alimenta divulgacandcontas.tse.jus.br, sem
# autenticacao). Roda depois de 05_consolidar.R -- precisa de
# candidatos_novo.rds pra saber quais sq_candidato/ano buscar.
#
# GET https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/buscar/{ano}/{UF}/{id_eleicao}/candidato/{sq_candidato}
#
# Endpoint e id_eleicao confirmados empiricamente (nao documentados de forma
# completa nem no swagger nao-oficial do projeto nem no electionsBR). Volume
# baixissimo (poucas dezenas de candidatos do NOVO em PE) -- cache por
# candidato em disco, nunca refeito a menos que o cache seja apagado.
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

candidatos_novo <- readRDS("data/processed/candidatos_novo.rds")
stopifnot("Rode antes 05_consolidar.R" = !is.null(candidatos_novo))

dir.create("data/raw/perfil", recursive = TRUE, showWarnings = FALSE)
dir.create("www/fotos", recursive = TRUE, showWarnings = FALSE)

buscar_perfil_tse <- function(ano, sq_candidato) {
  id_eleicao <- ID_ELEICAO_DIVULGACAND[[as.character(ano)]]
  stopifnot("Ano sem id_eleicao mapeado em ID_ELEICAO_DIVULGACAND (00_utils.R)" = !is.null(id_eleicao))
  url <- sprintf(
    "https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/buscar/%d/%s/%d/candidato/%s",
    ano, UF_ALVO, id_eleicao, sq_candidato
  )
  buscar_json_tse(url)
}

baixar_foto <- function(sq_candidato, foto_url) {
  destino <- sprintf("www/fotos/%s.jpg", sq_candidato)
  if (file.exists(destino)) return(TRUE)
  ok <- tryCatch({
    baixar_arquivo_tse(foto_url, destino)
    TRUE
  }, error = function(e) {
    cat(sprintf("  falha ao baixar foto de %s: %s\n", sq_candidato, conditionMessage(e)))
    FALSE
  })
  if (isTRUE(ok) && (!file.exists(destino) || file.info(destino)$size == 0)) {
    unlink(destino)
    ok <- FALSE
  }
  ok
}

perfis <- vector("list", nrow(candidatos_novo))

for (i in seq_len(nrow(candidatos_novo))) {
  ano <- candidatos_novo$ano[i]
  sq  <- as.character(candidatos_novo$sq_candidato[i])
  cache_path <- sprintf("data/raw/perfil/%s.rds", sq)

  if (file.exists(cache_path)) {
    perfil_raw <- readRDS(cache_path)
  } else {
    cat(sprintf("Buscando perfil de %s (%s, %d)...\n", candidatos_novo$nm_urna[i], sq, ano))
    perfil_raw <- tryCatch(buscar_perfil_tse(ano, sq), error = function(e) {
      cat(sprintf("  falha ao buscar %s: %s\n", sq, conditionMessage(e)))
      NULL
    })
    if (!is.null(perfil_raw)) saveRDS(perfil_raw, cache_path)
    Sys.sleep(0.4)
  }

  if (is.null(perfil_raw)) {
    perfis[[i]] <- tibble(
      ano = ano, sq_candidato = candidatos_novo$sq_candidato[i],
      idade = NA_integer_, genero = NA_character_, estado_civil = NA_character_,
      cor_raca = NA_character_, grau_instrucao = NA_character_, ocupacao = NA_character_,
      gasto_campanha = NA_real_, redes = list(character(0)),
      foto_path = NA_character_, foto_disponivel = FALSE
    )
    next
  }

  data_eleicao <- as.Date(sprintf("%d-10-02", ano))
  nascimento <- suppressWarnings(as.Date(perfil_raw$dataDeNascimento))
  idade <- if (length(nascimento) == 1 && !is.na(nascimento)) {
    as.integer(floor(as.numeric(difftime(data_eleicao, nascimento, units = "days")) / 365.25))
  } else NA_integer_

  gasto <- suppressWarnings(sum(
    as.numeric(perfil_raw$gastoCampanha1T %||% NA_real_),
    as.numeric(perfil_raw$gastoCampanha2T %||% NA_real_),
    na.rm = TRUE
  ))
  if (gasto == 0 && is.null(perfil_raw$gastoCampanha1T) && is.null(perfil_raw$gastoCampanha2T)) gasto <- NA_real_

  redes <- perfil_raw$sites
  if (is.null(redes)) redes <- character(0)

  foto_ok <- FALSE
  if (isTRUE(perfil_raw$fotoUrlPublicavel) && !is.null(perfil_raw$fotoUrl) && nzchar(perfil_raw$fotoUrl)) {
    foto_ok <- baixar_foto(sq, perfil_raw$fotoUrl)
  }

  perfis[[i]] <- tibble(
    ano = ano,
    sq_candidato = candidatos_novo$sq_candidato[i],
    idade = idade,
    genero = perfil_raw$descricaoSexo %||% NA_character_,
    estado_civil = perfil_raw$descricaoEstadoCivil %||% NA_character_,
    cor_raca = perfil_raw$descricaoCorRaca %||% NA_character_,
    grau_instrucao = perfil_raw$grauInstrucao %||% NA_character_,
    ocupacao = perfil_raw$ocupacao %||% NA_character_,
    gasto_campanha = gasto,
    redes = list(redes),
    foto_path = if (foto_ok) sprintf("fotos/%s.jpg", sq) else NA_character_,
    foto_disponivel = foto_ok
  )
}

perfil_candidatos <- bind_rows(perfis)
saveRDS(perfil_candidatos, "data/processed/perfil_candidatos.rds")

cat("\nPerfis coletados:", nrow(perfil_candidatos), "\n")
cat("Com foto:", sum(perfil_candidatos$foto_disponivel), "\n")
cat("Arquivo gravado em data/processed/perfil_candidatos.rds\n")
