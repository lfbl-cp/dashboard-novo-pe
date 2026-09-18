library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(ggplot2)
library(scales)

source("R/dados.R")
source("R/mapa.R")
source("R/ficha.R")
source("R/perfil.R")
source("R/secao.R")

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

# Locais de votacao geocodificados + voto do NOVO por local -- so pro mapa de
# Densidade (heatmap) da aba municipal, ver montar_mapa_calor_bairro() em
# R/mapa.R.
LOCAIS_VOTACAO_MUNICIPAL <- carregar_locais_votacao_municipal()
VOTOS_CANDIDATO_LOCAL <- carregar_votos_candidato_local()
VOTOS_PARTIDO_LOCAL <- carregar_votos_partido_local()

# Voto do NOVO por SECAO (gerais + municipal juntos) + composicao do
# eleitorado por secao, JA PRE-CALCULADA (programs/17_composicao_eleitorado.R)
# -- so pro radar "Perfil do eleitorado" (indice_representacao(), R/dados.R).
# Nao depende de bairro/geometria, entao e carregado 1x e compartilhado pelas
# duas abas. NUNCA le perfil_eleitorado_secao.rds (13M+ linhas) direto aqui --
# isso travava a 1a renderizacao de cada secao em ~20-40s (group_by sobre a
# tabela inteira, medido com chromote headless em ago/2026); ver
# carregar_composicao_eleitorado_pre()/composicao_eleitorado_secao() em
# R/dados.R.
VOTOS_CANDIDATO_SECAO <- carregar_votos_candidato_secao()
VOTOS_PARTIDO_SECAO <- carregar_votos_partido_secao()
COMPOSICAO_ELEITORADO_PRE <- carregar_composicao_eleitorado_pre()

# Municipio pre-selecionado ao abrir a aba municipal = onde o NOVO teve mais
# votos no total (todos os cargos/anos somados) -- pedido do usuario, em vez
# de cair no 1o municipio em ordem alfabetica.
MUNICIPIO_MUNICIPAL_PADRAO <- VOTOS_CANDIDATO_BAIRRO %>%
  left_join(BAIRROS %>% st_drop_geometry() %>% select(chave_bairro, chave_municipio), by = "chave_bairro") %>%
  count(chave_municipio, wt = votos_nominais, name = "votos") %>%
  slice_max(votos, n = 1) %>%
  pull(chave_municipio)

# ── UI ────────────────────────────────────────────────────────────────────
# Casca autoral (nao bslib::page_navbar), mesmo padrao de "Dashboard economia
# PE": cabecalho fino, barra de filtro sticky, conteudo numa coluna central
# com vao nas laterais. A barra de filtro sticky so tem o alternador Eleicoes
# gerais/municipais (`visao_eleicao`, nivel mais alto do app). O filtro de Ano
# das eleicoes GERAIS vive dentro do atalho "Ano" de cada secao (ver
# `secao_ui()`, R/secao.R) -- e um <select> de verdade, so estilizado pra
# parecer o mesmo tipo de pill que "Partido"/"Candidatos" ao lado. CARGOS tem
# hoje uma unica secao (Deputado Federal) porque o NOVO nunca lancou candidato
# a Estadual/Senador em PE, mas o loop suporta voltar a ter mais de uma sem
# mudar estrutura -- **exceto o filtro de Ano, que e GLOBAL**: se `CARGOS`
# voltar a ter mais de uma entrada, o `selectInput("f_ano", ...)` de
# `secao_ui()` nao pode ser repetido em cada secao (2 elementos com o mesmo id
# quebram o binding do Shiny); precisaria mover pra um lugar unico antes disso.
#
# Eleicoes MUNICIPAIS (Vereador/Prefeito, por bairro): o mapa de bairro so faz
# sentido dentro de UM municipio por vez, entao alem do Ano tem um filtro de
# Municipio -- os dois sao GLOBAIS por igual motivo do `f_ano` acima
# (`atalhos_municipal_ui()`, R/secao.R, renderizado 1x fora do loop de
# CARGOS_MUNICIPAIS). So os 8 municipios com candidato do NOVO E geometria de
# bairro no IBGE aparecem no seletor (municipio com candidato mas sem bairro
# fica de fora da aba inteira -- decisao do usuario, ver
# 09_bairros_municipios_alvo.R).

ui <- bslib::page(
  title = "NOVO em Pernambuco",
  theme = bslib::bs_theme(version = 5, primary = "#FF6A13"),
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),

  tags$div(class = "app-shell",
    tags$header(class = "app-header",
      tags$div(class = "app-header-inner",
        tags$h1("NOVO em Pernambuco"),
        bslib::input_dark_mode(id = "modo_escuro", mode = "light")
      )
    ),
    tags$div(class = "filtro-bar",
      tags$div(class = "filtro-bar-inner",
        tags$div(class = "visao-toggle",
          radioButtons("visao_eleicao", NULL,
                       choices = c("Eleições gerais" = "gerais", "Eleições municipais" = "municipais"),
                       selected = "gerais", inline = TRUE)
        )
      )
    ),
    tags$main(class = "app-main",
      conditionalPanel("input.visao_eleicao == 'gerais'",
        lapply(CARGOS, function(c) secao_ui(c$id, c$titulo, ANOS))
      ),
      conditionalPanel("input.visao_eleicao == 'municipais'",
        atalhos_municipal_ui(ANOS_MUNICIPAIS, MUNICIPIOS_MUNICIPAIS, MUNICIPIO_MUNICIPAL_PADRAO),
        lapply(CARGOS_MUNICIPAIS, function(c) secao_ui_municipal(c$id, c$titulo, c$cargo))
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  modo_atual <- reactive({
    if (is.null(input$modo_escuro)) "light" else input$modo_escuro
  })

  ano_sel <- reactive({ req(input$f_ano); as.numeric(input$f_ano) })

  # Clique no mapa de QUALQUER secao abre a ficha do municipio (resumo do
  # NOVO nas corridas de CARGOS no ano selecionado) -- implementado uma vez
  # aqui, repassado a todas as instancias do modulo.
  ao_clicar_municipio <- function(chave_municipio) {
    linha <- MUNICIPIOS %>% st_drop_geometry() %>% filter(chave_municipio == !!chave_municipio)
    if (nrow(linha) != 1) return(invisible(NULL))
    resumo <- ficha_municipio(VOTOS_PARTIDO_MUNICIPIO, TOTAL_VALIDOS_MUNICIPIO, chave_municipio, ano_sel())
    abrir_ficha_modal(linha$name_muni, ano_sel(), resumo)
  }

  # lapply, nao for: com for() os args de secao_server() so seriam forcados
  # (lazy eval) quando os reactives/render* internos rodassem de verdade, ja
  # depois do loop terminado -- as 3 secoes acabariam todas lendo o ULTIMO
  # valor de `c` (Senador). lapply cria um binding de `c` por chamada.
  invisible(lapply(CARGOS, function(c) {
    secao_server(c$id, c$cargo, c$titulo, CANDIDATOS, VOTOS_CANDIDATO_MUNICIPIO, VOTOS_PARTIDO_MUNICIPIO,
                 TOTAL_VALIDOS_MUNICIPIO, MUNICIPIOS, PERFIL_CANDIDATOS, VOTOS_CANDIDATO_SECAO,
                 VOTOS_PARTIDO_SECAO, COMPOSICAO_ELEITORADO_PRE, ano_sel, modo_atual, ao_clicar_municipio)
  }))

  ano_municipal_sel <- reactive({ req(input$f_ano_municipal); as.numeric(input$f_ano_municipal) })
  municipio_municipal_sel <- reactive({ req(input$f_municipio_municipal); input$f_municipio_municipal })

  # Vereador (proporcional) usa o modulo com Bloco 1 (partido) + Bloco 2
  # (candidato); Prefeito (majoritario, no maximo 1 candidato do NOVO por
  # municipio/eleicao) usa a variante de bloco unico -- ver comentario em
  # secao_server_municipal_majoritario(), R/secao.R.
  invisible(lapply(CARGOS_MUNICIPAIS, function(c) {
    servidor <- if (cargo_e_proporcional(c$cargo)) secao_server_municipal else secao_server_municipal_majoritario
    servidor(c$id, c$cargo, c$titulo, CANDIDATOS_MUNICIPAIS, VOTOS_CANDIDATO_BAIRRO,
             VOTOS_PARTIDO_BAIRRO, TOTAL_VALIDOS_BAIRRO, BAIRROS, PERFIL_CANDIDATOS_MUNICIPAIS,
             LOCAIS_VOTACAO_MUNICIPAL, VOTOS_CANDIDATO_LOCAL, VOTOS_PARTIDO_LOCAL,
             VOTOS_CANDIDATO_SECAO, VOTOS_PARTIDO_SECAO, COMPOSICAO_ELEITORADO_PRE,
             ano_municipal_sel, municipio_municipal_sel, modo_atual)
  }))
}

shinyApp(ui, server)
