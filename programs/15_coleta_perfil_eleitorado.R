# =============================================================================
# Perfil demografico do eleitorado por SECAO eleitoral, um snapshot por ano de
# eleicao do dashboard (2018/2022 gerais, 2020/2024 municipais) -- fonte do
# indice de representacao demografica (16_consolidar_secao.R). NAO usa
# electionsBR (o wrapper elections_tse(type="voter_profile_by_section") bateu
# num erro de extracao de zip consistente, testado ao vivo pra PE 2022/2024,
# nao e cache corrompido de sessao porque temp=FALSE ja elimina essa causa) --
# baixa direto do CDN do TSE via buscar_arquivo_tse() (mesmo WAF/User-Agent ja
# usado pra DivulgaCandContas), descoberto o padrao de URL via
# https://dadosabertos.tse.jus.br/api/3/action/package_show?id=eleitorado-<ano>.
#
# So usa o ANO especifico (nao o dataset "eleitorado-atual", que e um
# snapshot do cadastro em vigor HOJE, sem correspondencia com o eleitorado de
# quando cada eleicao passada aconteceu de fato).
#
# Raca/cor (DS_RACA_COR) e baixada mas NUNCA usada no indice -- e autodeclarada
# desde 8/nov/2022 so, cobertura medida ao vivo pra PE: 2018/2020 = 0% (coleta
# nao existia), 2022 tambem essencialmente 0 (snapshot de antes da coleta
# comecar), 2024 = so 9,6% informado. Genero/faixa etaria/escolaridade, ao
# contrario, ficam >99,9% informados em todo ano testado.
#
# Gotcha de schema: os nomes de coluna MUDAM entre o arquivo "atual" e os por
# ano, e possivelmente entre anos diferentes tambem (confirmado nao testar
# assumindo -- ja pegou 1x: DS_COR_RACA/QT_ELEITORES no "atual" viram
# DS_RACA_COR/QT_ELEITORES_PERFIL no arquivo de 2024). match_coluna() abaixo
# usa padrao (regex), nao nome exato, pra nao quebrar se variar de novo.
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(dplyr)

dir.create("data/raw/eleitorado", showWarnings = FALSE, recursive = TRUE)

ANOS_PERFIL_ELEITORADO <- sort(unique(c(ANOS_ELEICOES_GERAIS, ANOS_ELEICOES_MUNICIPAIS)))

match_coluna <- function(nomes, padrao) {
  achado <- grep(padrao, nomes, value = TRUE)
  if (length(achado) < 1) {
    stop(sprintf("Nenhuma coluna bate com o padrao '%s' -- schema do TSE mudou de novo, confira o CSV bruto", padrao))
  }
  achado[1]
}

baixar_e_ler_ano <- function(ano) {
  zip_destino <- sprintf("data/raw/eleitorado/perfil_eleitor_secao_%d_%s.zip", ano, UF_ALVO)
  dir_extraido <- sprintf("data/raw/eleitorado/%d", ano)

  if (!file.exists(zip_destino)) {
    url <- sprintf("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_secao/perfil_eleitor_secao_%d_%s.zip", ano, UF_ALVO)
    cat(sprintf("Baixando perfil do eleitorado %d/%s...\n", ano, UF_ALVO))
    baixar_arquivo_tse(url, zip_destino)
  } else {
    cat(sprintf("Zip ja baixado: %s\n", zip_destino))
  }

  if (!dir.exists(dir_extraido) || length(list.files(dir_extraido)) == 0) {
    dir.create(dir_extraido, showWarnings = FALSE, recursive = TRUE)
    unzip(zip_destino, exdir = dir_extraido)
  }

  csv_path <- list.files(dir_extraido, pattern = "\\.csv$", full.names = TRUE)[1]
  bruto <- read.csv2(csv_path, fileEncoding = "latin1", stringsAsFactors = FALSE)

  col_qt        <- match_coluna(names(bruto), "^QT_ELEITORES(_PERFIL)?$")
  col_genero    <- match_coluna(names(bruto), "^DS_GENERO$")
  col_faixa     <- match_coluna(names(bruto), "^DS_FAIXA_ETARIA$")
  col_escolar   <- match_coluna(names(bruto), "^DS_GRAU_(ESCOLARIDADE|INSTRUCAO)$")

  bruto %>%
    transmute(
      ano = ano,
      chave_municipio = normalizar_texto(NM_MUNICIPIO),
      chave_secao = paste(normalizar_texto(NM_MUNICIPIO), NR_ZONA, NR_SECAO, ano, sep = "|"),
      genero = .data[[col_genero]],
      faixa_etaria = .data[[col_faixa]],
      escolaridade = .data[[col_escolar]],
      eleitores = .data[[col_qt]]
    )
}

perfil_todos <- bind_rows(lapply(ANOS_PERFIL_ELEITORADO, baixar_e_ler_ano))

# data/intermediate/, nao data/processed/: este crosstab (13M+ linhas, ~67MB)
# nunca e lido pelo app Shiny em runtime, so por
# programs/17_composicao_eleitorado.R e programs/13_export_dist_data.R (ver
# carregar_perfil_eleitorado_secao(), R/dados.R). Fora de data/processed/ pra
# nao ir pro deploy (.rscignore) nem pro git (.gitignore) por engano.
dir.create("data/intermediate", showWarnings = FALSE, recursive = TRUE)
saveRDS(perfil_todos, "data/intermediate/perfil_eleitorado_secao.rds")

cat(sprintf("\nPerfil do eleitorado consolidado: %d linhas (crosstab por secao/genero/faixa/escolaridade), %d eleitores no total.\n",
            nrow(perfil_todos), sum(perfil_todos$eleitores)))
cat("Anos incluidos:", paste(ANOS_PERFIL_ELEITORADO, collapse = ", "), "\n")
