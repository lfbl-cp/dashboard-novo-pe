# =============================================================================
# Pre-calcula a composicao do eleitorado (genero/faixa_etaria/escolaridade)
# usada pelo radar/rosca "Perfil do eleitorado" do APP SHINY (R/secao.R) --
# mesma otimizacao ja aplicada ao site estatico em
# programs/13_export_dist_data.R, so que aqui o resultado fica em .rds pro
# proprio app.R ler, em vez de JSON pro navegador.
#
# Sem isso, cada sessao do Shiny recalculava calcular_composicao_eleitorado_secao()
# (R/dados.R) na hora, escaneando perfil_eleitorado_secao.rds (13M+ linhas) 3x
# (uma por eixo) so pra abrir a secao "Deputado Federal" -- ~20-40s de tela em
# branco (sem spinner) antes do 1o output aparecer, media com chromote headless
# em ago/2026. Rodando aqui 1x (poucas dezenas de combinacoes, nao por sessao
# de usuario), o app.R passa a so fazer um filter() nas tabelas ja prontas.
#
# So precisa rodar de novo se perfil_eleitorado_secao.rds
# (15_coleta_perfil_eleitorado.R) ou a lista de municipios/anos do dashboard
# mudar -- mesmo gatilho de 16_consolidar_secao.R, que le a mesma fonte.
#
# Rodar com working directory = Dashboard Novo/ (raiz do projeto):
#   Rscript programs/17_composicao_eleitorado.R
# =============================================================================

args_cmd <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("--file=", "", args_cmd[grep("--file=", args_cmd)])
raiz <- normalizePath(file.path(dirname(arquivo_script), ".."))
setwd(raiz)

suppressMessages({library(dplyr); library(tidyr); library(sf)})
source("R/dados.R")

PERFIL <- carregar_perfil_eleitorado_secao()
CANDIDATOS <- carregar_candidatos()
CANDIDATOS_MUNICIPAIS <- carregar_candidatos_municipais()
BAIRROS <- carregar_bairros()

ANOS_GERAIS <- anos_disponiveis(CANDIDATOS)
ANOS_MUNICIPAIS <- anos_disponiveis(CANDIDATOS_MUNICIPAIS)
MUNICIPIOS_ALVO <- municipios_municipais_disponiveis(BAIRROS)$chave_municipio
EIXOS <- c("genero", "faixa_etaria", "escolaridade")

# Uma linha por (ano, escopo) -- escopo = ESCOPO_GERAL (PE inteiro, eleicoes
# gerais) ou a chave de 1 dos 8 municipios da aba municipal. NAO faz produto
# cruzado ano-geral x municipio nem ano-municipal x GERAL: o app nunca pede
# essas combinacoes (Deputado Federal e sempre PE inteiro; Vereador/Prefeito
# sempre escopado a 1 municipio).
combos <- bind_rows(
  tibble(ano = ANOS_GERAIS, escopo = ESCOPO_GERAL),
  tidyr::expand_grid(ano = ANOS_MUNICIPAIS, escopo = MUNICIPIOS_ALVO)
)

cat(sprintf("Calculando %d combinacoes (ano x escopo) x %d eixos = %d chamadas...\n",
            nrow(combos), length(EIXOS), nrow(combos) * length(EIXOS)))

resultado_por_combo <- lapply(seq_len(nrow(combos)), function(i) {
  ano_i <- combos$ano[i]
  escopo_i <- combos$escopo[i]
  municipio_sel <- if (escopo_i == ESCOPO_GERAL) NULL else escopo_i

  cat(sprintf("  [%d/%d] ano=%s escopo=%s\n", i, nrow(combos), ano_i, escopo_i))

  lapply(EIXOS, function(eixo) {
    r <- calcular_composicao_eleitorado_secao(PERFIL, ano_i, eixo, chave_municipio_sel = municipio_sel)
    list(
      composicao = r$composicao_secao %>% mutate(ano = ano_i, eixo = eixo, escopo = escopo_i),
      denom = r$denom %>% mutate(ano = ano_i, eixo = eixo, escopo = escopo_i)
    )
  })
})

resultado_flat <- unlist(resultado_por_combo, recursive = FALSE)
composicao_eleitorado_secao_pre <- bind_rows(lapply(resultado_flat, `[[`, "composicao")) %>%
  select(ano, eixo, escopo, chave_secao, categoria, eleitores, fracao)
denom_eleitorado_secao_pre <- bind_rows(lapply(resultado_flat, `[[`, "denom")) %>%
  select(ano, eixo, escopo, categoria, fracao_geral)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(composicao_eleitorado_secao_pre, "data/processed/composicao_eleitorado_secao.rds")
saveRDS(denom_eleitorado_secao_pre, "data/processed/denom_eleitorado_secao.rds")

cat(sprintf(
  "\nOK -- composicao_eleitorado_secao.rds (%d linhas) e denom_eleitorado_secao.rds (%d linhas) gravados em data/processed/.\n",
  nrow(composicao_eleitorado_secao_pre), nrow(denom_eleitorado_secao_pre)
))
