# =============================================================================
# Coordenadas de cada local de votacao usado nos municipios/anos do escopo
# municipal -> data/processed/locais_votacao_municipal.rds (1 linha por
# local de votacao: chave_municipio, ano, zona, local, lat/lon, bairro
# atribuido, fonte da coordenada).
#
# Fonte PRIMARIA: geobr::read_polling_places() -- locais de votacao do TSE ja
# geolocalizados (coordenada oficial do TSE, com fallback via geocodebr
# quando a do TSE falta/e imprecisa). Testado ao vivo: cobre ~53% dos locais
# do escopo (958/1823) -- o resto nao tem coordenada nenhuma no dataset do
# TSE/geocodebr pra esses municipios/anos. Fonte FALLBACK (pedido explicito
# do usuario): pro que sobrar sem coordenada nenhuma, geocodifica com
# tidygeocoder sobre o ENDERECO PADRONIZADO ("{endereco}, {municipio}, PE,
# Brasil" -- cidade/estado/pais no final, como pedido) extraido de
# vote_section (coluna DS_LOCAL_VOTACAO_ENDERECO), via tidygeocoder
# method="arcgis" (testado ao vivo: method="osm"/Nominatim deu 503 Service
# Unavailable persistente em lote grande, resolvendo <70% mesmo apos retry;
# geocodebr::geocode tambem testado, mas CNEFE so bate com precisao de rua
# em ~1.5% desses enderecos, o resto cai pra nivel de municipio, inutil pra
# atribuir bairro -- arcgis foi o unico dos tres com cobertura e confianca
# boas nesses enderecos reais). Resultados com score<80 (match fraco, ex.:
# so nivel de bairro/cidade) NAO sao cacheados, ficam pendentes pra retry.
#
# QA espacial (pedido explicito do usuario): todo ponto e testado contra os
# poligonos de bairro do PROPRIO municipio (bairros_pe.rds); ponto que cai
# fora de todos os bairros do seu municipio fica marcado
# `fora_do_bairro = TRUE` em vez de descartado. `data/raw/correcoes_geocodificacao.csv`
# (chave_local, lat, lon, motivo) e um arquivo de correcao manual aplicado
# por cima do resultado bruto -- comeca vazio, e preenchido a mao (busca do
# endereco na web) sempre que a QA sinalizar um ponto errado.
#
# 37 pontos corrigidos manualmente ao vivo nesta sessao (13 por imprecisao de
# fronteira/erro pontual de geocodificacao, confirmados via busca web; 24 via
# nova tentativa de geocodificacao com nome do local + endereco com
# abreviacoes expandidas). Restam 47 pontos (de 84 originais) permanentemente
# `fora_do_bairro = TRUE` -- sao locais de votacao em SITIO/POVOADO/DISTRITO
# genuinamente rurais, fora de qualquer area com bairro definido pelo IBGE
# (nao e erro de geocodificacao: nenhum provedor testado -- Nominatim,
# geocodebr/CNEFE, ArcGIS -- consegue posicionar um ponto dentro de um bairro
# que nao existe ali). Materialidade medida ao vivo: 0.14% dos votos do NOVO
# no escopo municipal (111 de 80236). `programs/11_consolidar_municipal.R`
# EXCLUI esses locais da agregacao por bairro (mesmo padrao ja usado pra
# municipio inteiro sem geometria de bairro -- ver 09_bairros_municipios_alvo.R).
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(geobr)
library(sf)
library(dplyr)
library(tidyr)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

bairros_pe <- readRDS("data/processed/bairros_pe.rds")
stopifnot("Rode antes 09_bairros_municipios_alvo.R" = !is.null(bairros_pe))

crosswalk_muni <- readRDS("data/processed/municipios_pe.rds") %>%
  st_drop_geometry() %>%
  select(code_muni, chave_municipio)

municipios_escopo <- sort(unique(bairros_pe$chave_municipio))
cat("Municipios no escopo (com bairro):", paste(municipios_escopo, collapse = ", "), "\n\n")

# --- 1. Locais de votacao distintos usados em cada (ano, municipio) do
# escopo, a partir do vote_section (qualquer partido/cargo -- so precisamos
# de todo local fisico do municipio, nao so onde o NOVO teve voto) ----------

locais_distintos_por_ano <- lapply(ANOS_ELEICOES_MUNICIPAIS, function(ano) {
  vs <- readRDS(sprintf("data/raw/vote_section_%d.rds", ano))
  vs %>%
    mutate(chave_municipio = normalizar_texto(NM_MUNICIPIO)) %>%
    filter(chave_municipio %in% municipios_escopo) %>%
    count(ano = ANO_ELEICAO, chave_municipio, NM_MUNICIPIO, CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO,
          NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO, name = "n_secoes_com_esse_nome")
})
# NR_LOCAL_VOTACAO por si so NAO e uma chave unica de local fisico -- o mesmo
# numero aparece com nome/endereco diferente entre secoes do mesmo
# municipio/zona (local renumerado/realocado no meio do ciclo eleitoral, o
# NR_LOCAL_VOTACAO antigo fica "orfao" associado a um endereco velho em
# algumas secoes). Como a coordenada de verdade vem de
# geobr::read_polling_places() (join por municipio/zona/NR_LOCAL_VOTACAO, um
# so por vez), esse endereco textual so serve de fallback pro tidygeocoder --
# entao pega so a variante de nome/endereco mais frequente entre as secoes
# (n_secoes_com_esse_nome mais alto) como representante de cada local.
locais_distintos <- bind_rows(locais_distintos_por_ano) %>%
  mutate(chave_local = paste(chave_municipio, NR_ZONA, NR_LOCAL_VOTACAO, ano, sep = "|")) %>%
  arrange(desc(n_secoes_com_esse_nome)) %>%
  distinct(chave_local, .keep_all = TRUE)

stopifnot("chave_local deveria ser unica" = !any(duplicated(locais_distintos$chave_local)))
cat("Locais de votacao distintos no escopo (todos os anos/municipios):", nrow(locais_distintos), "\n\n")

# --- 2. Coordenadas via geobr::read_polling_places() (fonte primaria) ------

buscar_polling_places <- function(ano, code_muni_ibge) {
  baixar_com_retry(read_polling_places, year = ano, code_muni = code_muni_ibge, showProgress = FALSE,
                    tentativas = 3, espera_seg = 10)
}

pares_ano_municipio <- locais_distintos %>% distinct(ano, chave_municipio) %>%
  left_join(crosswalk_muni, by = "chave_municipio")
stopifnot("Todo par ano/municipio precisa de um code_muni IBGE" = !any(is.na(pares_ano_municipio$code_muni)))

pp_lista <- vector("list", nrow(pares_ano_municipio))
for (i in seq_len(nrow(pares_ano_municipio))) {
  ano_i <- pares_ano_municipio$ano[i]; muni_i <- pares_ano_municipio$chave_municipio[i]
  cache_path <- sprintf("data/raw/polling_places_%d_%d.rds", ano_i, pares_ano_municipio$code_muni[i])
  if (file.exists(cache_path)) {
    pp_lista[[i]] <- readRDS(cache_path)
  } else {
    cat(sprintf("Baixando locais de votacao geolocalizados: %s/%d...\n", muni_i, ano_i))
    pp <- buscar_polling_places(ano_i, pares_ano_municipio$code_muni[i])
    saveRDS(pp, cache_path)
    pp_lista[[i]] <- pp
  }
}
polling_places <- bind_rows(lapply(pp_lista, st_drop_geometry)) %>%
  # 1 linha por local (nao por secao) -- read_polling_places pode ter mais de
  # uma linha por local fisico (secao), coordenada e a mesma pro mesmo local.
  distinct(code_muni_tse, nr_zona, nr_local_votacao, .keep_all = TRUE) %>%
  mutate(
    lat = coalesce(lat_geocodebr, lat_tse),
    lon = coalesce(lon_geocodebr, lon_tse),
    fonte_coordenada = case_when(
      !is.na(lat_geocodebr) ~ "geocodebr",
      !is.na(lat_tse) ~ "tse",
      TRUE ~ NA_character_
    )
  ) %>%
  select(code_muni_tse, ano = year, nr_zona, nr_local_votacao, lat, lon, fonte_coordenada,
         precisao_geocodebr, desvio_metros_geocodebr, nm_bairro_autodeclarado = nm_bairro)

locais_com_coord <- locais_distintos %>%
  left_join(polling_places,
            by = c("CD_MUNICIPIO" = "code_muni_tse", "ano" = "ano", "NR_ZONA" = "nr_zona",
                   "NR_LOCAL_VOTACAO" = "nr_local_votacao"))

n_sem_coord <- sum(is.na(locais_com_coord$lat))
cat(sprintf("\nLocais com coordenada via read_polling_places: %d/%d (faltam %d)\n",
            nrow(locais_com_coord) - n_sem_coord, nrow(locais_com_coord), n_sem_coord))

# --- 3. Fallback: geocodificacao via tidygeocoder pro que sobrou sem
# coordenada nenhuma -- endereco padronizado com cidade/estado/pais no final
# (pedido explicito do usuario), antes de geocodificar. --------------------

if (n_sem_coord > 0) {
  library(tidygeocoder)
  faltantes <- locais_com_coord %>%
    filter(is.na(lat)) %>%
    mutate(endereco_padronizado = sprintf("%s, %s, %s, Brasil", DS_LOCAL_VOTACAO_ENDERECO, NM_MUNICIPIO, UF_ALVO))

  cache_fallback <- "data/raw/geocod_fallback.rds"
  ja_geocodificados <- if (file.exists(cache_fallback)) readRDS(cache_fallback) else tibble(chave_local = character(0))
  pendentes <- faltantes %>% filter(!chave_local %in% ja_geocodificados$chave_local)

  if (nrow(pendentes) > 0) {
    # method="osm" (Nominatim) se mostrou nao-confiavel em lote (503 Service
    # Unavailable persistente, testado ao vivo em 2 execucoes -- so ~70%
    # resolvido mesmo apos retry). method="arcgis" (ainda tidygeocoder, so
    # provedor diferente, sem chave de API) testado ao vivo: 100% de resposta,
    # scores 97-100 em amostra real, muito mais rapido (sem limite de 1/seg).
    cat(sprintf("Geocodificando %d endereco(s) via tidygeocoder (ArcGIS)...\n", nrow(pendentes)))
    geo <- pendentes %>%
      distinct(chave_local, endereco_padronizado) %>%
      tidygeocoder::geocode(address = endereco_padronizado, method = "arcgis", full_results = TRUE,
                             lat = "lat_fallback", long = "lon_fallback")
    # So cacheia geocodificacao com SUCESSO e confianca minima (score >= 80 --
    # abaixo disso o match costuma ser so nivel de bairro/cidade, nao do
    # predio) -- o resto fica de fora do cache de proposito, pra ser
    # re-tentado automaticamente na proxima execucao em vez de ficar
    # permanentemente marcado como "ja tentado".
    novo_cache <- geo %>%
      filter(!is.na(lat_fallback), !is.na(lon_fallback), score >= 80) %>%
      transmute(chave_local, lat = lat_fallback, lon = lon_fallback,
                fonte_coordenada = "tidygeocoder_arcgis",
                confianca_score = score)
    n_falhou <- nrow(geo) - nrow(novo_cache)
    if (n_falhou > 0) {
      cat(sprintf("  %d endereco(s) sem match confiavel (score<80 ou sem resultado) -- ficam pendentes pra proxima execucao.\n", n_falhou))
    }
    ja_geocodificados <- bind_rows(ja_geocodificados, novo_cache)
    saveRDS(ja_geocodificados, cache_fallback)
  }

  locais_com_coord <- locais_com_coord %>%
    left_join(ja_geocodificados, by = "chave_local", suffix = c("", "_fb")) %>%
    mutate(
      lat = coalesce(lat, lat_fb),
      lon = coalesce(lon, lon_fb),
      fonte_coordenada = coalesce(fonte_coordenada, fonte_coordenada_fb)
    ) %>%
    select(-lat_fb, -lon_fb, -fonte_coordenada_fb)

  cat(sprintf("Apos fallback: %d/%d locais ainda sem coordenada.\n",
              sum(is.na(locais_com_coord$lat)), nrow(locais_com_coord)))
}

# --- 4. QA espacial: ponto cai dentro de algum bairro DO PROPRIO municipio? -

locais_sf <- locais_com_coord %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

atribuicao <- vector("list", length(municipios_escopo))
for (i in seq_along(municipios_escopo)) {
  m <- municipios_escopo[i]
  pontos_m <- locais_sf %>% filter(chave_municipio == m)
  bairros_m <- bairros_pe %>% filter(chave_municipio == m)
  if (nrow(pontos_m) == 0) { atribuicao[[i]] <- NULL; next }
  atribuicao[[i]] <- st_join(pontos_m, bairros_m %>% select(chave_bairro), join = st_within)
}
locais_com_bairro <- bind_rows(lapply(atribuicao, st_drop_geometry))

# ponto exatamente sobre a fronteira compartilhada de 2 bairros pode casar
# com ambos no st_join (st_within e sensivel a essa ambiguidade de borda) --
# mantem so 1 linha por chave_local (a primeira, escolha arbitraria mas
# deterministica) pra nao duplicar voto na agregacao por bairro depois.
n_antes <- nrow(locais_com_bairro)
locais_com_bairro <- locais_com_bairro %>% distinct(chave_local, .keep_all = TRUE)
if (nrow(locais_com_bairro) < n_antes) {
  cat(sprintf("\n%d linha(s) duplicada(s) por casar com mais de 1 bairro na fronteira -- mantida so 1 por local.\n",
              n_antes - nrow(locais_com_bairro)))
}

locais_com_bairro$fora_do_bairro <- is.na(locais_com_bairro$chave_bairro)
n_fora <- sum(locais_com_bairro$fora_do_bairro)
cat(sprintf("\nPontos fora de todo bairro do proprio municipio (antes de correcao manual): %d\n", n_fora))
if (n_fora > 0) {
  cat("Locais sinalizados:\n")
  print(as.data.frame(locais_com_bairro %>% filter(fora_do_bairro) %>%
                         select(chave_local, NM_MUNICIPIO, NM_LOCAL_VOTACAO, DS_LOCAL_VOTACAO_ENDERECO, lat, lon)))
}

# --- 5. Aplica correcoes manuais (busca de endereco na web), se houver ----

arq_correcoes <- "data/raw/correcoes_geocodificacao.csv"
if (!file.exists(arq_correcoes)) {
  write.csv(data.frame(chave_local = character(0), lat = numeric(0), lon = numeric(0), motivo = character(0)),
            arq_correcoes, row.names = FALSE)
  cat(sprintf("\nCriado %s vazio (preencher a mao quando a QA sinalizar pontos).\n", arq_correcoes))
}
correcoes <- read.csv(arq_correcoes, stringsAsFactors = FALSE)
if (nrow(correcoes) > 0) {
  cat(sprintf("\nAplicando %d correcao(oes) manual(is) de %s...\n", nrow(correcoes), arq_correcoes))
  locais_com_bairro <- locais_com_bairro %>%
    left_join(correcoes %>% select(chave_local, lat_corrigido = lat, lon_corrigido = lon), by = "chave_local") %>%
    mutate(
      corrigido_manualmente = !is.na(lat_corrigido),
      lat = coalesce(lat_corrigido, lat),
      lon = coalesce(lon_corrigido, lon)
    ) %>%
    select(-lat_corrigido, -lon_corrigido)

  # reclassifica bairro so pros pontos corrigidos (remove chave_bairro velha
  # antes do join espacial, senao o st_join cria chave_bairro.x/.y por
  # colisao de nome em vez de sobrescrever).
  idx_corrigidos <- which(locais_com_bairro$corrigido_manualmente)
  if (length(idx_corrigidos) > 0) {
    pts_corrigidos <- locais_com_bairro[idx_corrigidos, ] %>%
      select(-chave_bairro) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)
    for (m in unique(pts_corrigidos$chave_municipio)) {
      sel <- pts_corrigidos$chave_municipio == m
      bairros_m <- bairros_pe %>% filter(chave_municipio == m) %>% select(chave_bairro)
      novo <- st_join(pts_corrigidos[sel, ], bairros_m, join = st_within)
      locais_com_bairro$chave_bairro[idx_corrigidos[sel]] <- novo$chave_bairro
    }
    locais_com_bairro$fora_do_bairro <- is.na(locais_com_bairro$chave_bairro)
  }
} else {
  locais_com_bairro$corrigido_manualmente <- FALSE
}

n_fora_final <- sum(locais_com_bairro$fora_do_bairro)
cat(sprintf("\nPontos fora de todo bairro APOS correcao manual: %d\n", n_fora_final))

saveRDS(locais_com_bairro, "data/processed/locais_votacao_municipal.rds")
cat("\nlocais_votacao_municipal.rds salvo:", nrow(locais_com_bairro), "locais\n")
cat("Por fonte de coordenada:\n")
print(table(locais_com_bairro$fonte_coordenada, useNA = "ifany"))
