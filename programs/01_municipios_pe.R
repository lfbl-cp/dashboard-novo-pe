# =============================================================================
# Geometria dos 185 municipios de PE (geobr) -> data/processed/municipios_pe.rds
#
# IMPORTANTE: geobr 1.9.1 tem a URL de metadata quebrada (aponta pro GitHub org
# antigo "ipeaGIT/geobr", que hoje da redirect 301 pra "ipea/geobr" -- e o
# check_connection() do geobr trata qualquer status != 200 como "servidor fora
# do ar"). Rodar `install.packages("geobr")` antes se a versao instalada for
# anterior a 2.0.1.
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

source("programs/00_utils.R")
library(geobr)
library(sf)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# O servidor de dados do geobr tem falhas transitorias de conexao (observado
# em teste real) -- mesmo padrao de retry dos downloads da TSE.
baixar_municipios_pe <- function(tentativas = 5, espera_seg = 15) {
  for (i in seq_len(tentativas)) {
    pe <- tryCatch(
      read_municipality(code_muni = "PE", year = 2020, showProgress = FALSE),
      error = function(e) e
    )
    if (!inherits(pe, "error") && is.data.frame(pe) && nrow(pe) > 0) return(pe)
    cat(sprintf("  tentativa %d/%d de baixar geometria do geobr falhou\n", i, tentativas))
    if (i < tentativas) Sys.sleep(espera_seg)
  }
  stop("Nao foi possivel baixar a geometria dos municipios de PE via geobr apos varias tentativas.")
}

cat("Baixando geometria dos municipios de PE (geobr)...\n")
municipios <- baixar_municipios_pe()

# geobr entrega SIRGAS2000; leaflet espera WGS84 (EPSG:4326).
municipios <- st_transform(municipios, 4326)
municipios$chave_municipio <- normalizar_texto(municipios$name_muni)

stopifnot("geobr nao retornou os 185 municipios de PE" = nrow(municipios) == 185)

saveRDS(municipios, "data/processed/municipios_pe.rds")
cat("municipios_pe.rds salvo:", nrow(municipios), "municipios\n")
