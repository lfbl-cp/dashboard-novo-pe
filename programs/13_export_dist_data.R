# =============================================================================
# Exporta os dados do Dashboard Novo (app Shiny) para JSON/GeoJSON estaticos,
# consumidos pelo site sem backend em dist/ (ver ADAPTAR_PROJETO_GAUSS.md e o
# precedente ja validado em Mapeador de liderancas/programs/export_dist_data.R
# -- mesmo padrao: jsonlite::write_json() pros data.frames, sf::st_write()
# pra geometria, fetch() no navegador em vez de servidor R).
#
# So fonte R/dados.R e R/mapa.R (tem calcular_lisa()) -- NAO fonte app.R nem
# R/secao.R/R/ficha.R/R/perfil.R, que geram HTML via tags$ e sao exatamente o
# que este export substitui (a logica deles foi reescrita em dist/js/app.js).
# As linhas de carga de dados de app.R (MUNICIPIOS, CANDIDATOS, ...) sao
# replicadas aqui embaixo.
#
# Rodar com working directory = Dashboard Novo/ (raiz do projeto):
#   Rscript programs/13_export_dist_data.R
# =============================================================================

library(dplyr)
library(tidyr)
library(sf)
library(jsonlite)

source("R/dados.R")
source("R/mapa.R")

dir.create("dist/data", recursive = TRUE, showWarnings = FALSE)
dir.create("dist/fotos", recursive = TRUE, showWarnings = FALSE)

# ── Carga (mesmas linhas de app.R, sem montar UI/servidor Shiny) ────────────

MUNICIPIOS <- carregar_municipios()
CANDIDATOS <- carregar_candidatos()
VOTOS_CANDIDATO_MUNICIPIO <- carregar_votos_candidato_municipio()
VOTOS_PARTIDO_MUNICIPIO <- carregar_votos_partido_municipio()
TOTAL_VALIDOS_MUNICIPIO <- carregar_total_validos_municipio()
PERFIL_CANDIDATOS <- carregar_perfil_candidatos()
ANOS <- anos_disponiveis(CANDIDATOS)

BAIRROS <- carregar_bairros()
CANDIDATOS_MUNICIPAIS <- carregar_candidatos_municipais()
VOTOS_CANDIDATO_BAIRRO <- carregar_votos_candidato_bairro()
VOTOS_PARTIDO_BAIRRO <- carregar_votos_partido_bairro()
TOTAL_VALIDOS_BAIRRO <- carregar_total_validos_bairro()
PERFIL_CANDIDATOS_MUNICIPAIS <- carregar_perfil_candidatos_municipais()
ANOS_MUNICIPAIS <- anos_disponiveis(CANDIDATOS_MUNICIPAIS)
MUNICIPIOS_MUNICIPAIS <- municipios_municipais_disponiveis(BAIRROS)

MUNICIPIO_MUNICIPAL_PADRAO <- VOTOS_CANDIDATO_BAIRRO %>%
  left_join(BAIRROS %>% st_drop_geometry() %>% select(chave_bairro, chave_municipio), by = "chave_bairro") %>%
  count(chave_municipio, wt = votos_nominais, name = "votos") %>%
  slice_max(votos, n = 1) %>%
  pull(chave_municipio)

# Voto por SECAO (gerais + municipal juntos) + perfil demografico do
# eleitorado por secao -- so pro indice de representacao ("Perfil do
# eleitorado", ver secao dedicada mais abaixo). PERFIL_ELEITORADO_SECAO tem
# +13M linhas (~27s pra carregar) -- custo pago 1x aqui na exportacao, nunca
# pelo navegador do usuario final.
VOTOS_CANDIDATO_SECAO <- carregar_votos_candidato_secao()
VOTOS_PARTIDO_SECAO <- carregar_votos_partido_secao()
PERFIL_ELEITORADO_SECAO <- carregar_perfil_eleitorado_secao()

escrever_json <- function(x, arquivo) {
  write_json(x, file.path("dist/data", arquivo), dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
  cat(sprintf("  %s (%d linhas)\n", arquivo, if (is.data.frame(x)) nrow(x) else length(x)))
}

cat("== candidatos ==\n")
escrever_json(
  CANDIDATOS %>% transmute(ano, cargo, sqCandidato = as.character(sq_candidato),
                            nrCandidato = nr_candidato, nmCandidato = nm_candidato,
                            nmUrna = nm_urna, situacao),
  "candidatos_gerais.json"
)
escrever_json(
  CANDIDATOS_MUNICIPAIS %>% transmute(ano, cargo, chaveMunicipio = chave_municipio,
                                       sqCandidato = as.character(sq_candidato),
                                       nrCandidato = nr_candidato, nmCandidato = nm_candidato,
                                       nmUrna = nm_urna, situacao),
  "candidatos_municipais.json"
)

cat("== votos ==\n")
votos_gerais <- list(
  candidato = VOTOS_CANDIDATO_MUNICIPIO %>%
    transmute(ano, cargo, chaveMunicipio = chave_municipio, sqCandidato = as.character(sq_candidato),
              nmUrna = nm_urna, votosNominais = votos_nominais),
  partido = VOTOS_PARTIDO_MUNICIPIO %>%
    transmute(ano, cargo, chaveMunicipio = chave_municipio, sgPartido = sg_partido,
              votosLegenda = votos_legenda, votosNominais = votos_nominais,
              votosValidosPartido = votos_validos_partido),
  totalValidos = TOTAL_VALIDOS_MUNICIPIO %>%
    transmute(ano, cargo, chaveMunicipio = chave_municipio, totalValidos = total_validos)
)
write_json(votos_gerais, "dist/data/votos_gerais.json", dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
cat(sprintf("  votos_gerais.json (candidato=%d partido=%d totalValidos=%d)\n",
            nrow(votos_gerais$candidato), nrow(votos_gerais$partido), nrow(votos_gerais$totalValidos)))

votos_municipais <- list(
  candidato = VOTOS_CANDIDATO_BAIRRO %>%
    transmute(ano, cargo, chaveBairro = chave_bairro, sqCandidato = as.character(sq_candidato),
              votosNominais = votos_nominais),
  partido = VOTOS_PARTIDO_BAIRRO %>%
    transmute(ano, cargo, chaveBairro = chave_bairro, sgPartido = sg_partido,
              votosLegenda = votos_legenda, votosNominais = votos_nominais,
              votosValidosPartido = votos_validos_partido),
  totalValidos = TOTAL_VALIDOS_BAIRRO %>%
    transmute(ano, cargo, chaveBairro = chave_bairro, totalValidos = total_validos)
)
write_json(votos_municipais, "dist/data/votos_municipais.json", dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
cat(sprintf("  votos_municipais.json (candidato=%d partido=%d totalValidos=%d)\n",
            nrow(votos_municipais$candidato), nrow(votos_municipais$partido), nrow(votos_municipais$totalValidos)))

cat("== perfis (gerais + municipais, chave = sqCandidato) ==\n")
perfil_para_lista <- function(perfil) {
  setNames(
    lapply(seq_len(nrow(perfil)), function(i) {
      list(
        idade = perfil$idade[i], genero = perfil$genero[i], estadoCivil = perfil$estado_civil[i],
        corRaca = perfil$cor_raca[i], grauInstrucao = perfil$grau_instrucao[i], ocupacao = perfil$ocupacao[i],
        gastoCampanha = perfil$gasto_campanha[i], redes = as.list(perfil$redes[[i]]),
        fotoPath = perfil$foto_path[i], fotoDisponivel = perfil$foto_disponivel[i]
      )
    }),
    as.character(perfil$sq_candidato)
  )
}
perfis <- c(perfil_para_lista(PERFIL_CANDIDATOS), perfil_para_lista(PERFIL_CANDIDATOS_MUNICIPAIS))
write_json(perfis, "dist/data/perfil_candidatos.json", auto_unbox = TRUE, na = "null", digits = NA)
cat(sprintf("  perfil_candidatos.json (%d candidatos)\n", length(perfis)))

cat("== geometrias ==\n")
mun_geo <- MUNICIPIOS %>% select(chaveMunicipio = chave_municipio, nmMunicipio = name_muni) %>% st_transform(4326)
if (file.exists("dist/data/municipios.geojson")) file.remove("dist/data/municipios.geojson")
st_write(mun_geo, "dist/data/municipios.geojson", driver = "GeoJSON", quiet = TRUE)
cat(sprintf("  municipios.geojson (%d municipios)\n", nrow(mun_geo)))

bairros_geo <- BAIRROS %>%
  select(chaveBairro = chave_bairro, chaveMunicipio = chave_municipio,
         nmMunicipio = name_muni, nmBairro = name_neighborhood) %>%
  st_transform(4326)
if (file.exists("dist/data/bairros.geojson")) file.remove("dist/data/bairros.geojson")
st_write(bairros_geo, "dist/data/bairros.geojson", driver = "GeoJSON", quiet = TRUE)
cat(sprintf("  bairros.geojson (%d bairros, %d municipios)\n", nrow(bairros_geo), n_distinct(bairros_geo$chaveMunicipio)))

# ── LISA pre-calculado -------------------------------------------------------
# spdep::localmoran() (dentro de calcular_lisa(), R/mapa.R) nao roda no
# navegador -- pre-calcula AQUI pra cada combinacao (nivel, cargo, ano,
# [municipio], escopo) que o app pode pedir, guarda so a classificacao final
# (chaveMunicipio/chaveBairro -> classe) numa lista indexada por uma chave
# composta em texto -- lisa[chaveCombo][chaveFeature] em JS. Mesma funcao
# calcular_lisa() do app Shiny -- 0 duplicacao de logica estatistica entre R
# e JS.

cat("== LISA ==\n")
lisa <- list()
n_lisa <- 0

for (c in CARGOS) {
  for (ano in ANOS) {
    candidatos_sel <- candidatos_por_cargo_ano(CANDIDATOS, c$cargo, ano)
    escopos <- c("TODOS", as.character(candidatos_sel$sq_candidato))
    for (escopo in escopos) {
      valores <- valores_mapa(MUNICIPIOS, VOTOS_CANDIDATO_MUNICIPIO, VOTOS_PARTIDO_MUNICIPIO,
                               TOTAL_VALIDOS_MUNICIPIO, c$cargo, ano, escopo)
      sf_ord <- MUNICIPIOS %>% select(chave_municipio) %>% left_join(valores, by = "chave_municipio")
      classes <- as.character(calcular_lisa(sf_ord, sf_ord$pct_validos))
      chave_combo <- paste("geral", c$cargo, ano, escopo, sep = "|")
      lisa[[chave_combo]] <- setNames(as.list(classes), sf_ord$chave_municipio)
      n_lisa <- n_lisa + 1
    }
  }
}

for (c in CARGOS_MUNICIPAIS) {
  for (ano in ANOS_MUNICIPAIS) {
    for (municipio in MUNICIPIOS_MUNICIPAIS$chave_municipio) {
      bairros_sel <- BAIRROS %>% filter(chave_municipio == municipio)
      if (nrow(bairros_sel) == 0) next
      chaves_bairro_sel <- bairros_sel %>% st_drop_geometry() %>% select(chave_bairro)

      candidatos_sel <- candidatos_por_cargo_ano_municipio(CANDIDATOS_MUNICIPAIS, c$cargo, ano, municipio)
      if (nrow(candidatos_sel) == 0) next # sem candidato aqui -- app nem mostra a secao (ver secao_ui_municipal())

      escopos <- if (cargo_e_proporcional(c$cargo)) {
        c("TODOS", as.character(candidatos_sel$sq_candidato))
      } else {
        as.character(candidatos_sel$sq_candidato[1]) # majoritario: 1 candidato so, sem visao de partido separada
      }

      votos_candidato_sel <- VOTOS_CANDIDATO_BAIRRO %>% semi_join(chaves_bairro_sel, by = "chave_bairro")
      votos_partido_sel <- VOTOS_PARTIDO_BAIRRO %>% semi_join(chaves_bairro_sel, by = "chave_bairro")
      total_validos_sel <- TOTAL_VALIDOS_BAIRRO %>% semi_join(chaves_bairro_sel, by = "chave_bairro")

      for (escopo in escopos) {
        valores <- valores_mapa_bairro(bairros_sel, votos_candidato_sel, votos_partido_sel,
                                        total_validos_sel, c$cargo, ano, escopo)
        sf_ord <- bairros_sel %>% select(chave_bairro) %>% left_join(valores, by = "chave_bairro")
        classes <- as.character(calcular_lisa(sf_ord, sf_ord$pct_validos))
        chave_combo <- paste("municipal", c$cargo, ano, municipio, escopo, sep = "|")
        lisa[[chave_combo]] <- setNames(as.list(classes), sf_ord$chave_bairro)
        n_lisa <- n_lisa + 1
      }
    }
  }
}

write_json(lisa, "dist/data/lisa.json", auto_unbox = TRUE, na = "null", digits = NA)
cat(sprintf("  lisa.json (%d combinacoes)\n", n_lisa))

# ── Locais de votacao (mapa de Densidade, so aba municipal) ------------------
# Tabelas pequenas (milhares de linhas, nao milhoes) -- ao contrario do
# indice de representacao abaixo, aqui NAO precisa pre-agregar nada: exporta
# locais_votacao_municipal.rds + votos_candidato_local.rds/votos_partido_local.rds
# quase brutos (so renomeando campo pra camelCase) e o JS reproduz
# pontos_calor_bairro() (R/dados.R) no navegador, mesma logica de
# valoresPorChave() ja portada em dist/js/app.js.

cat("== locais de votacao (mapa de densidade) ==\n")
LOCAIS_VOTACAO_MUNICIPAL <- carregar_locais_votacao_municipal()
VOTOS_CANDIDATO_LOCAL <- carregar_votos_candidato_local()
VOTOS_PARTIDO_LOCAL <- carregar_votos_partido_local()

locais_votacao <- list(
  locais = LOCAIS_VOTACAO_MUNICIPAL %>%
    transmute(chaveLocal = chave_local, chaveMunicipio = chave_municipio, lat, lon),
  votos = list(
    candidato = VOTOS_CANDIDATO_LOCAL %>%
      transmute(ano, cargo, chaveLocal = chave_local, sqCandidato = as.character(sq_candidato), votosNominais = votos_nominais),
    partido = VOTOS_PARTIDO_LOCAL %>%
      transmute(ano, cargo, chaveLocal = chave_local, votosValidosPartido = votos_validos_partido)
  )
)
write_json(locais_votacao, "dist/data/locais_votacao.json", dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
cat(sprintf("  locais_votacao.json (locais=%d candidato=%d partido=%d)\n",
            nrow(locais_votacao$locais), nrow(locais_votacao$votos$candidato), nrow(locais_votacao$votos$partido)))

# ── Perfil do eleitorado (indice de sobre/sub-representacao demografica) ----
# Precalculado igual ao LISA acima e pelo MESMO motivo: a agregacao
# (composicao_eleitorado_secao(), R/dados.R) roda sobre uma tabela de
# +13M linhas -- caro demais pro navegador (e o proprio motivo de uma rodada
# anterior de performance ter quase inviabilizado o app Shiny, ver historico
# de R/secao.R::registrar_perfil_eleitorado()). Aqui roda 1x na exportacao,
# nao por usuario. Mesma otimizacao aplicada la: composicao calculada 1x por
# (nivel, cargo, ano, [municipio], eixo) e reaproveitada entre TODOS os
# escopos (TODOS + cada candidato), em vez de recalcular o lado caro
# (eleitorado) a cada escopo.
cat("== perfil do eleitorado (indice de representacao) ==\n")
EIXOS_PERFIL <- c("genero", "faixa_etaria", "escolaridade")
perfil_eleitorado <- list()
n_perfil <- 0

for (c in CARGOS) {
  for (ano in ANOS) {
    candidatos_sel <- candidatos_por_cargo_ano(CANDIDATOS, c$cargo, ano)
    escopos <- c("TODOS", as.character(candidatos_sel$sq_candidato))
    for (eixo in EIXOS_PERFIL) {
      composicao <- calcular_composicao_eleitorado_secao(PERFIL_ELEITORADO_SECAO, ano, eixo)
      for (escopo in escopos) {
        indice <- indice_representacao(VOTOS_CANDIDATO_SECAO, VOTOS_PARTIDO_SECAO, composicao, c$cargo, ano, escopo)
        if (nrow(indice) == 0) next
        chave_combo <- paste("geral", c$cargo, ano, escopo, eixo, sep = "|")
        perfil_eleitorado[[chave_combo]] <- indice %>% transmute(categoria, indice, fracaoPonderada = fracao_ponderada)
        n_perfil <- n_perfil + 1
      }
    }
  }
}

for (c in CARGOS_MUNICIPAIS) {
  for (ano in ANOS_MUNICIPAIS) {
    for (municipio in MUNICIPIOS_MUNICIPAIS$chave_municipio) {
      candidatos_sel <- candidatos_por_cargo_ano_municipio(CANDIDATOS_MUNICIPAIS, c$cargo, ano, municipio)
      if (nrow(candidatos_sel) == 0) next # sem candidato aqui -- mesmo criterio do LISA acima

      escopos <- if (cargo_e_proporcional(c$cargo)) {
        c("TODOS", as.character(candidatos_sel$sq_candidato))
      } else {
        as.character(candidatos_sel$sq_candidato[1]) # majoritario: 1 candidato so
      }

      for (eixo in EIXOS_PERFIL) {
        composicao <- calcular_composicao_eleitorado_secao(PERFIL_ELEITORADO_SECAO, ano, eixo, chave_municipio_sel = municipio)
        for (escopo in escopos) {
          indice <- indice_representacao(VOTOS_CANDIDATO_SECAO, VOTOS_PARTIDO_SECAO, composicao, c$cargo, ano, escopo, chave_municipio_sel = municipio)
          if (nrow(indice) == 0) next
          chave_combo <- paste("municipal", c$cargo, ano, municipio, escopo, eixo, sep = "|")
          perfil_eleitorado[[chave_combo]] <- indice %>% transmute(categoria, indice, fracaoPonderada = fracao_ponderada)
          n_perfil <- n_perfil + 1
        }
      }
    }
  }
}

write_json(perfil_eleitorado, "dist/data/perfil_eleitorado.json", dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA)
cat(sprintf("  perfil_eleitorado.json (%d combinacoes)\n", n_perfil))

# ── meta.json ------------------------------------------------------------

meta <- list(
  partidoAlvo = PARTIDO_ALVO,
  anosGerais = ANOS,
  anosMunicipais = ANOS_MUNICIPAIS,
  cargosGerais = lapply(CARGOS, function(c) list(id = c$id, cargo = c$cargo, titulo = c$titulo)),
  cargosMunicipais = lapply(CARGOS_MUNICIPAIS, function(c) {
    list(id = c$id, cargo = c$cargo, titulo = c$titulo, proporcional = cargo_e_proporcional(c$cargo))
  }),
  municipiosMunicipais = MUNICIPIOS_MUNICIPAIS %>% transmute(chaveMunicipio = chave_municipio, nmMunicipio = name_muni),
  municipioMunicipalPadrao = MUNICIPIO_MUNICIPAL_PADRAO
)
write_json(meta, "dist/data/meta.json", auto_unbox = TRUE, na = "null", digits = NA, dataframe = "rows")
cat("  meta.json\n")

# ── Fotos -------------------------------------------------------------------

cat("== fotos ==\n")
arquivos_fotos <- list.files("www/fotos", full.names = TRUE)
file.copy(arquivos_fotos, "dist/fotos", overwrite = TRUE)
cat(sprintf("  %d fotos copiadas\n", length(arquivos_fotos)))

cat("\nOK -- exportado para dist/data/ e dist/fotos/\n")
