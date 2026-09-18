# =============================================================================
# Perfil enriquecido de cada candidato municipal do NOVO (demografia, gasto de
# campanha declarado, redes sociais, foto oficial) via API publica do TSE
# DivulgaCandContas -- mesma API/endpoint de 06_perfil_candidatos.R, so troca
# o id_eleicao (ID_ELEICAO_DIVULGACAND_MUNICIPAL) e a fonte de candidatos
# (candidatos_novo_municipais.rds em vez de candidatos_novo.rds). Roda depois
# de 11_consolidar_municipal.R.
#
# Fotos compartilham a mesma pasta www/fotos/ e o mesmo cache
# data/raw/perfil/ que o pipeline geral, chaveados por sq_candidato -- SQ
# do TSE e unico em toda a base (nao colide entre eleicao geral e municipal,
# nem entre 2020/2024 e 2018/2022).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(dplyr)

`%||%` <- function(a, b) if (is.null(a)) b else a

candidatos_novo_municipais <- readRDS("data/processed/candidatos_novo_municipais.rds")
stopifnot("Rode antes 11_consolidar_municipal.R" = !is.null(candidatos_novo_municipais))

# A API DivulgaCandContas espera o codigo TSE do MUNICIPIO (SG_UE) no lugar
# de UF nesse segmento da URL p/ eleicoes municipais (tipoAbrangencia="M") --
# diferente das eleicoes gerais (tipoAbrangencia="F"), onde SG_UE=UF ja e a
# propria UF. Testado ao vivo: URL com "PE" devolve 200 com corpo VAZIO (nao
# 404/erro), por isso o pipeline geral nao detectava a diferenca sem
# depurar o corpo da resposta. sg_ue vem de candidate_<ano>.rds (raw, todos
# os partidos), unico por chave_municipio dentro de um ano.
sg_ue_por_municipio <- bind_rows(lapply(ANOS_ELEICOES_MUNICIPAIS, function(ano) {
  readRDS(sprintf("data/raw/candidate_%d.rds", ano)) %>%
    transmute(ano = ano, chave_municipio = normalizar_texto(NM_UE), sg_ue = as.character(SG_UE)) %>%
    distinct()
}))
candidatos_novo_municipais <- candidatos_novo_municipais %>%
  left_join(sg_ue_por_municipio, by = c("ano", "chave_municipio"))
stopifnot("Candidato(s) sem SG_UE (codigo TSE do municipio) resolvido" =
            !any(is.na(candidatos_novo_municipais$sg_ue)))

dir.create("data/raw/perfil", recursive = TRUE, showWarnings = FALSE)
dir.create("www/fotos", recursive = TRUE, showWarnings = FALSE)

buscar_perfil_tse_municipal <- function(ano, sg_ue, sq_candidato) {
  id_eleicao <- ID_ELEICAO_DIVULGACAND_MUNICIPAL[[as.character(ano)]]
  stopifnot("Ano sem id_eleicao mapeado em ID_ELEICAO_DIVULGACAND_MUNICIPAL (00_utils.R)" = !is.null(id_eleicao))
  url <- sprintf(
    "https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/buscar/%d/%s/%d/candidato/%s",
    ano, sg_ue, id_eleicao, sq_candidato
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

perfis <- vector("list", nrow(candidatos_novo_municipais))

for (i in seq_len(nrow(candidatos_novo_municipais))) {
  ano <- candidatos_novo_municipais$ano[i]
  sg_ue <- candidatos_novo_municipais$sg_ue[i]
  sq  <- as.character(candidatos_novo_municipais$sq_candidato[i])
  cache_path <- sprintf("data/raw/perfil/%s.rds", sq)

  if (file.exists(cache_path)) {
    perfil_raw <- readRDS(cache_path)
  } else {
    cat(sprintf("Buscando perfil de %s (%s, %d)...\n", candidatos_novo_municipais$nm_urna[i], sq, ano))
    perfil_raw <- tryCatch(buscar_perfil_tse_municipal(ano, sg_ue, sq), error = function(e) {
      cat(sprintf("  falha ao buscar %s: %s\n", sq, conditionMessage(e)))
      NULL
    })
    if (!is.null(perfil_raw)) saveRDS(perfil_raw, cache_path)
    Sys.sleep(0.4)
  }

  if (is.null(perfil_raw)) {
    perfis[[i]] <- tibble(
      ano = ano, sq_candidato = candidatos_novo_municipais$sq_candidato[i],
      idade = NA_integer_, genero = NA_character_, estado_civil = NA_character_,
      cor_raca = NA_character_, grau_instrucao = NA_character_, ocupacao = NA_character_,
      gasto_campanha = NA_real_, redes = list(character(0)),
      foto_path = NA_character_, foto_disponivel = FALSE
    )
    next
  }

  data_eleicao <- as.Date(sprintf("%d-11-15", ano))
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
    sq_candidato = candidatos_novo_municipais$sq_candidato[i],
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

perfil_candidatos_municipais <- bind_rows(perfis)
saveRDS(perfil_candidatos_municipais, "data/processed/perfil_candidatos_municipais.rds")

cat("\nPerfis coletados:", nrow(perfil_candidatos_municipais), "\n")
cat("Com foto:", sum(perfil_candidatos_municipais$foto_disponivel), "\n")
cat("Arquivo gravado em data/processed/perfil_candidatos_municipais.rds\n")
