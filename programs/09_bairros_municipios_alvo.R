# =============================================================================
# Define o escopo final da secao municipal (municipios com candidato do NOVO
# E com geometria de bairro no IBGE) e baixa os poligonos de bairro desses
# municipios -> data/processed/bairros_pe.rds.
#
# Municipio com candidato do NOVO mas SEM bairro no IBGE fica de FORA da aba
# municipal (decisao explicita do usuario) -- este script e o unico lugar
# onde essa exclusao acontece, e ela fica logada no console (nunca
# silenciosa).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(geobr)
library(sf)
library(dplyr)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

municipios_pe <- readRDS("data/processed/municipios_pe.rds")
stopifnot("Rode antes 01_municipios_pe.R" = !is.null(municipios_pe))

# --- 1. Municipios onde o NOVO teve candidato (Prefeito/Vereador, 1o turno,
# 2020 ou 2024) ---------------------------------------------------------

# select() antes do bind_rows: o layout do TSE varia coluna a coluna entre
# 2020 e 2024 (ex.: NR_CPF_CANDIDATO vem como character num ano e double no
# outro) -- so precisamos de um punhado de colunas aqui, entao selecionar
# cedo evita o bind_rows quebrar por causa de colunas que nem usamos.
candidatos_por_ano <- lapply(ANOS_ELEICOES_MUNICIPAIS, function(ano) {
  cand <- readRDS(sprintf("data/raw/candidate_%d.rds", ano))
  cand %>%
    filter(SG_PARTIDO == PARTIDO_ALVO,
           normalizar_cargo(DS_CARGO) %in% CARGOS_ALVO_MUNICIPAIS,
           NR_TURNO == 1) %>%
    mutate(chave_municipio = normalizar_texto(NM_UE)) %>%
    select(ano = ANO_ELEICAO, chave_municipio, NM_UE, DS_CARGO, SQ_CANDIDATO, NR_CANDIDATO,
           NM_CANDIDATO, NM_URNA_CANDIDATO, DS_SIT_TOT_TURNO)
})
candidatos_municipais_raw <- bind_rows(candidatos_por_ano)
municipios_com_candidato <- sort(unique(candidatos_municipais_raw$chave_municipio))

cat("Municipios com candidato do NOVO (Prefeito/Vereador, 2020/2024):\n")
cat(" ", paste(municipios_com_candidato, collapse = ", "), "\n")
cat("Total:", length(municipios_com_candidato), "\n\n")

# --- 2. Poligonos de bairro (IBGE via geobr) pra PE inteiro, depois filtra
# so os municipios com candidato ------------------------------------------

cat("Baixando poligonos de bairro (geobr::read_neighborhood, IBGE 2022)...\n")
bairros_pe_raw <- baixar_com_retry(read_neighborhood, code_muni = "PE", year = 2022, showProgress = FALSE)

bairros_pe_raw <- bairros_pe_raw %>%
  mutate(chave_municipio = normalizar_texto(name_muni)) %>%
  filter(chave_municipio %in% municipios_com_candidato)

municipios_com_bairro <- sort(unique(bairros_pe_raw$chave_municipio))
municipios_excluidos <- setdiff(municipios_com_candidato, municipios_com_bairro)

cat("\nMunicipios com candidato E geometria de bairro (escopo final da aba):\n")
cat(" ", paste(municipios_com_bairro, collapse = ", "), "\n")
if (length(municipios_excluidos) > 0) {
  cat("\nEXCLUIDOS da aba municipal (candidato do NOVO, mas sem bairro no IBGE):\n")
  cat(" ", paste(municipios_excluidos, collapse = ", "), "\n")
} else {
  cat("\nNenhum municipio excluido -- todos com candidato tem geometria de bairro.\n")
}

# --- 3. Prepara geometria final ------------------------------------------

# Alguns bairros vem em mais de uma linha no shapefile do IBGE (o mesmo
# code_neighborhood/nome aparece 2x -- ex.: "Ilha Joana Bezerra" e "Cohab" no
# Recife, "Dom Helder Camara" em Garanhuns, este ultimo ate espalhado por 2
# distritos diferentes) -- sao partes desconexas do mesmo bairro, nao bairros
# distintos. group_by + st_union junta as partes numa geometria so por
# chave_bairro antes do stopifnot de unicidade.
bairros_pe <- bairros_pe_raw %>%
  st_transform(4326) %>%
  mutate(chave_bairro = paste0(chave_municipio, " | ", normalizar_texto(name_neighborhood))) %>%
  group_by(chave_bairro, code_muni, name_muni, chave_municipio, code_neighborhood, name_neighborhood) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

stopifnot("chave_bairro deveria ser unica" = !any(duplicated(bairros_pe$chave_bairro)))

saveRDS(bairros_pe, "data/processed/bairros_pe.rds")
cat("\nbairros_pe.rds salvo:", nrow(bairros_pe), "bairros em", length(municipios_com_bairro), "municipios\n")
