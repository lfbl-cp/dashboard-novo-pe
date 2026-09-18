# =============================================================================
# Constantes e normalizacao compartilhadas pelos scripts de coleta/consolidacao
# (02 a 05). Fonte-ado por eles, nunca pelo app (R/ e so codigo de runtime,
# mesma separacao do projeto de referencia "Dashboard economia PE").
# =============================================================================

PARTIDO_ALVO <- "NOVO"
UF_ALVO <- "PE"
ANOS_ELEICOES_GERAIS <- c(2018, 2022)
CARGOS_ALVO <- c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "SENADOR")

# Eleicoes municipais (Prefeito/Vereador) -- NOVO so lancou candidato nesses
# 2 anos em PE (nao ha eleicao municipal antes de 2020 relevante aqui).
# VICE-PREFEITO fica de fora: e um cargo registrado (tem candidato/perfil
# proprio na base do TSE), mas nao recebe voto proprio -- o eleitor vota no
# numero do Prefeito, o voto todo vai pra chapa. Sem cargo proprio, o vice
# nao tem secao de mapa/ranking (mesmo raciocinio de nao ter secao de
# Vice-Governador nas eleicoes gerais).
ANOS_ELEICOES_MUNICIPAIS <- c(2020, 2024)
CARGOS_ALVO_MUNICIPAIS <- c("PREFEITO", "VEREADOR")

# Id de eleicao da API publica do TSE (DivulgaCandContas, sistema oficial de
# divulgacandcontas.tse.jus.br), usado por 06_perfil_candidatos.R e pelo
# equivalente municipal. Confirmado via GET
# https://divulgacandcontas.tse.jus.br/divulga/rest/v1/eleicao/ordinarias
# (essa mesma chamada ja lista 2026 -> 20322002026, pra quando o pipeline
# precisar rodar de novo).
ID_ELEICAO_DIVULGACAND <- c("2018" = 2022802018, "2022" = 2040602022)
ID_ELEICAO_DIVULGACAND_MUNICIPAL <- c("2020" = 2030402020, "2024" = 2045202024)

# DS_CARGO vem com grafia inconsistente entre anos da TSE
# (ex.: "DEPUTADO FEDERAL" em 2018 x "Deputado Federal" em 2022).
normalizar_cargo <- function(x) toupper(trimws(x))

# maiuscula, sem acento, sem espaco nas pontas -- chave de join entre bases da
# TSE e do geobr, que variam acentuacao/capitalizacao entre si e entre anos.
normalizar_texto <- function(x) {
  x <- toupper(trimws(x))
  chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC", x)
}

# Situacao final do candidato, a partir de DS_SIT_TOT_TURNO. Categorias
# observadas na base real (2018/2022, PE): ELEITO, ELEITO POR QP, ELEITO POR
# MEDIA, SUPLENTE, 1o SUPLENTE, 2o SUPLENTE, NAO ELEITO, #NULO.
classificar_situacao <- function(ds_sit_tot_turno) {
  s <- normalizar_texto(ds_sit_tot_turno)
  dplyr::case_when(
    grepl("^ELEITO", s) ~ "eleito",
    grepl("SUPLENTE", s) ~ "suplente",
    grepl("^NAO ELEITO", s) ~ "nao_eleito",
    TRUE ~ NA_character_
  )
}

# GET json da API DivulgaCandContas com um User-Agent de navegador. Passou a
# ser NECESSARIO em 2026: jsonlite::fromJSON(url) puro (sem header nenhum,
# como o pipeline usava antes) comecou a levar 403 Forbidden do WAF do TSE --
# a mesma URL funciona normalmente com um User-Agent de navegador via httr.
# Usado por 06_perfil_candidatos.R e pelo script equivalente municipal.
buscar_json_tse <- function(url) {
  resp <- httr::GET(url, httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  ))
  httr::stop_for_status(resp)
  jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
}

# Mesmo WAF/User-Agent do buscar_json_tse() acima, mas pra baixar um arquivo
# binario (foto de candidato) em vez de JSON -- download.file() puro leva 403
# Forbidden sem header nenhum (confirmado ao vivo), exatamente como o JSON;
# so nao dava pra perceber antes porque as fotos das eleicoes gerais ja
# tinham sido baixadas e cacheadas em www/fotos/ ANTES do WAF entrar em vigor
# (o file.exists() do cache mascarava a regressao). Usado por
# 06_perfil_candidatos.R e pelo script equivalente municipal.
baixar_arquivo_tse <- function(url, destino) {
  resp <- httr::GET(url, httr::add_headers(
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  ))
  httr::stop_for_status(resp)
  writeBin(httr::content(resp, as = "raw"), destino)
  invisible(TRUE)
}

# Retry generico p/ download de rede: usado tanto pelo electionsBR (TSE)
# quanto pelo geobr. temp = FALSE nas chamadas ao electionsBR e essencial --
# com temp = TRUE (default) o pacote reaproveita o zip ja salvo no tempdir da
# sessao, entao um download corrompido (conexao cai no meio) fica "preso" e
# TODAS as tentativas seguintes falham igual, sem nunca rebaixar o arquivo.
baixar_com_retry <- function(fn, ..., tentativas = 5, espera_seg = 15) {
  for (i in seq_len(tentativas)) {
    resultado <- tryCatch(fn(...), error = function(e) e)
    if (inherits(resultado, "error")) {
      msg <- conditionMessage(resultado)
      cat(sprintf("  tentativa %d/%d falhou (erro): %s\n", i, tentativas, msg))
    } else if (is.data.frame(resultado) && nrow(resultado) == 0) {
      msg <- "download retornou 0 linhas (zip corrompido/parcial)"
      cat(sprintf("  tentativa %d/%d falhou: %s\n", i, tentativas, msg))
    } else {
      return(resultado)
    }
    if (i < tentativas) Sys.sleep(espera_seg)
  }
  stop(sprintf("Falha apos %d tentativas: %s", tentativas, msg))
}
