# Modulo Shiny reutilizavel: uma secao completa do dashboard (um cargo).
# Uma instancia deste modulo por cargo em CARGOS (R/dados.R) -- hoje so
# Deputado Federal, unico cargo em que o NOVO lancou candidato em PE.
#
# Cada secao tem 2 blocos:
#   Bloco 1 "Visao do partido" -- sempre em nivel de partido (nao depende de
#     nenhum filtro de candidato): KPIs, mapa+ranking, cards de votos por
#     eleicao.
#   Bloco 2 "Visao por candidato" -- filtro local dedicado (sem opcao
#     "Todos"): mapa+ranking escopados aquele candidato + card de perfil
#     (demografia, foto, redes sociais -- R/perfil.R).
#
# Depende de R/dados.R, R/mapa.R e R/perfil.R, ja fonte-ados por app.R antes
# deste arquivo.

library(shiny)
library(leaflet)
library(ggplot2)
library(scales)
library(dplyr)

# plotly NAO e anexado (library()) de proposito: plotly::filter() mascara
# stats::filter, e como dplyr e carregado antes dele em app.R, um plotly
# anexado passaria a ficar na FRENTE de dplyr na search path -- todo filter()
# sem prefixo em R/dados.R (dezenas de chamadas) silenciosamente viraria
# stats::filter via plotly, quebrando o filtro de dados do app inteiro sem
# erro nenhum. Por isso as funcoes de plotly usadas aqui sao sempre com
# prefixo plotly::.

COR_LINHA <- "#FF6A13"

# --- UI ------------------------------------------------------------------

# Bloco "Perfil do eleitorado": rosca (genero) + 2 radares (faixa etaria,
# escolaridade) sempre visiveis lado a lado, mesma largura de coluna cada --
# substitui o antigo toggle (radioButtons + 1 grafico por vez). `sufixo`:
# "partido" (Bloco 1) ou "candidato" (Bloco 2) -- define os ids de output
# esperados do lado do servidor (rosca_<sufixo>, radar_<sufixo>_faixa_etaria,
# radar_<sufixo>_escolaridade). `ns`: NS(id) na UI estatica, session$ns nas
# UIs dinamicas (Bloco 2 e o bloco unico de Prefeito) -- mesma assinatura.
painel_perfil_eleitorado_ui <- function(ns, sufixo) {
  tags$div(class = "painel painel-perfil-eleitorado",
    tags$h3("Perfil do eleitorado", class = "painel-titulo"),
    tags$div(class = "perfil-eleitorado-grid",
      tags$div(class = "perfil-eleitorado-item",
        tags$div(class = "perfil-eleitorado-item-titulo", "Gênero"),
        plotly::plotlyOutput(ns(paste0("rosca_", sufixo)), height = "260px")
      ),
      tags$div(class = "perfil-eleitorado-item",
        tags$div(class = "perfil-eleitorado-item-titulo", "Faixa etária"),
        plotly::plotlyOutput(ns(paste0("radar_", sufixo, "_faixa_etaria")), height = "260px")
      ),
      tags$div(class = "perfil-eleitorado-item",
        tags$div(class = "perfil-eleitorado-item-titulo", "Escolaridade"),
        plotly::plotlyOutput(ns(paste0("radar_", sufixo, "_escolaridade")), height = "260px")
      )
    )
  )
}

secao_ui <- function(id, titulo, anos) {
  ns <- NS(id)
  id_partido <- paste0("sec-", id, "-partido")
  id_candidato <- paste0("sec-", id, "-candidato")

  tags$section(id = paste0("sec-", id), class = "secao",
    tags$h2(titulo),
    tags$nav(class = "secao-atalhos",
      # Filtro de Ano GLOBAL (input$f_ano, sem ns() -- nao e por secao) no
      # lugar de um atalho de link: um <select> de verdade, estilizado (ver
      # custom.css) pra parecer o mesmo tipo de pill que "Partido"/
      # "Candidatos" ao lado -- a pill mostra o ano selecionado (ex.: "2022"),
      # nao um rotulo fixo "Ano". `selectize = FALSE` de proposito: lista
      # curta (2-3 anos), um <select> nativo estiliza como pill muito mais
      # facil que o widget selectize.js (que traz sua propria marcacao extra).
      selectInput("f_ano", NULL, choices = anos, selected = max(anos),
                  selectize = FALSE, width = "auto"),
      tags$a(href = paste0("#", id_partido), "Partido"),
      tags$a(href = paste0("#", id_candidato), "Candidatos")
    ),
    uiOutput(ns("aviso_vazio")),

    tags$div(class = "bloco-titulo", id = id_partido, "Visão do partido"),
    tags$div(class = "kpi-row kpi-row--5",
      uiOutput(ns("kpi_legenda")), uiOutput(ns("kpi_nominais")), uiOutput(ns("kpi_candidatos")),
      uiOutput(ns("kpi_eleitos")), uiOutput(ns("kpi_suplentes"))
    ),
    tags$div(class = "painel painel-votos-eleicao",
      tags$h3("Total de votos por eleição", class = "painel-titulo"),
      uiOutput(ns("votos_eleicao"))
    ),
    tags$div(class = "mapa-ranking-par",
      tags$div(class = "painel painel-mapa",
        tags$div(class = "painel-titulo-linha",
          tags$h3("Onde", class = "painel-titulo"),
          radioButtons(ns("tipo_mapa"), NULL,
            choices = c("Coroplético" = "coropletico", "LISA" = "lisa"),
            selected = "coropletico", inline = TRUE)
        ),
        leafletOutput(ns("mapa_partido"), height = "420px")
      ),
      tags$div(class = "painel painel-ranking",
        tags$h3("Ranking — municípios (votos)", class = "painel-titulo"),
        plotly::plotlyOutput(ns("ranking_partido"), height = "420px")
      )
    ),
    painel_perfil_eleitorado_ui(ns, "partido"),

    tags$div(class = "bloco-titulo", id = id_candidato, "Visão por candidato"),
    uiOutput(ns("bloco_candidato"))
  )
}

# --- Server ----------------------------------------------------------------

# `cargo`: um dos valores de CARGOS_ALVO (R/dados.R). `ano`: reactive() ->
# ano selecionado (filtro GLOBAL). `modo_atual`: reactive() -> "light"/"dark".
# `ao_clicar_municipio`: function(chave_municipio) chamada quando o usuario
# clica num municipio de qualquer mapa desta secao -- implementada em app.R
# (drill-down + ficha modal), nao aqui, porque e compartilhada por todas as
# secoes.
secao_server <- function(id, cargo, titulo, candidatos, votos_candidato_municipio, votos_partido_municipio,
                          total_validos_municipio, municipios, perfil_candidatos, votos_candidato_secao,
                          votos_partido_secao, composicao_pre, ano, modo_atual, ao_clicar_municipio) {
  moduleServer(id, function(input, output, session) {

    candidatos_sel <- reactive({
      req(ano())
      candidatos_por_cargo_ano(candidatos, cargo, ano())
    })

    # Do mais votado pro menos votado -- mesma base usada no ranking/mapa,
    # ordem preservada pelo selectInput (setNames nao reordena).
    candidatos_ordenados <- reactive({
      req(ano())
      candidatos_com_votos(candidatos_sel(), votos_candidato_municipio, cargo, ano())
    })

    observeEvent(list(ano()), {
      updateSelectInput(session, "candidato_perfil", choices = opcoes_candidato_individual(candidatos_ordenados()))
    })

    tem_candidato <- reactive(nrow(candidatos_sel()) > 0)

    output$aviso_vazio <- renderUI({
      if (tem_candidato()) return(NULL)
      div(class = "aviso-vazio",
          sprintf("O NOVO não lançou candidato a %s em %s nesta eleição.", titulo, ano()))
    })

    # -- Bloco 1: partido (nunca depende de candidato) ---------------------

    output$kpi_legenda <- renderUI({
      v <- votos_legenda_total(votos_partido_municipio, cargo, ano())
      kpi_tile(if (is.na(v)) "Não aplicável" else formatar_votos(v), "Votos na legenda")
    })
    output$kpi_nominais <- renderUI({
      v <- votos_nominais_total(votos_candidato_municipio, cargo, ano(), "TODOS")
      kpi_tile(formatar_votos(v), "Votos nominais")
    })
    output$kpi_candidatos <- renderUI({
      kpi_tile(nrow(candidatos_sel()), "Nº candidatos")
    })
    output$kpi_eleitos <- renderUI({
      kpi_tile(contagem_situacao(candidatos_sel(), "TODOS", "eleito"), "Nº eleitos")
    })
    output$kpi_suplentes <- renderUI({
      kpi_tile(contagem_situacao(candidatos_sel(), "TODOS", "suplente"), "Nº suplentes")
    })

    valores_partido <- reactive({
      req(ano())
      valores_mapa(municipios, votos_candidato_municipio, votos_partido_municipio,
                   total_validos_municipio, cargo, ano(), "TODOS")
    })

    output$mapa_partido <- renderLeaflet({
      montar_mapa(municipios, valores_partido(), tipo = input$tipo_mapa %||% "coropletico", modo = modo_atual())
    })

    observeEvent(input$mapa_partido_shape_click, {
      ao_clicar_municipio(input$mapa_partido_shape_click$id)
    })

    output$ranking_partido <- plotly::renderPlotly({
      grafico_ranking_interativo(ranking_municipios(valores_partido(), n = 10), modo_atual())
    })

    composicoes_perfil <- composicoes_eleitorado(composicao_pre, ano)
    registrar_perfil_eleitorado(output, "partido", votos_candidato_secao, votos_partido_secao,
                                 composicoes_perfil, cargo, ano, function() "TODOS", modo_atual)

    output$votos_eleicao <- renderUI({
      serie <- votos_totais_por_ano(votos_partido_municipio, cargo, anos_disponiveis(candidatos))
      tags$div(class = "votos-eleicao-row",
        lapply(seq_len(nrow(serie)), function(i) {
          kpi_tile(formatar_votos(serie$votos[i]), sprintf("Votos em %d", serie$ano[i]))
        })
      )
    })

    # -- Bloco 2: por candidato ----------------------------------------------

    candidato_sel_perfil <- reactive({
      if (is.null(input$candidato_perfil) || !nzchar(input$candidato_perfil)) return(NULL)
      input$candidato_perfil
    })

    output$bloco_candidato <- renderUI({
      if (!tem_candidato()) {
        return(div(class = "aviso-vazio",
                    sprintf("Sem candidato do NOVO a %s em %s para detalhar.", titulo, ano())))
      }
      tagList(
        selectInput(session$ns("candidato_perfil"), "Candidato",
                    choices = opcoes_candidato_individual(candidatos_ordenados()), width = "260px"),
        # Mesma proporcao/altura do mapa do Bloco 1 (.mapa-ranking-par,
        # 1.6fr:1fr, 420px) -- aqui a coluna da direita nao e so o ranking,
        # e perfil+ranking empilhados dividindo os mesmos 420px do mapa
        # (.lateral-candidato fixa essa altura total em custom.css).
        tags$div(class = "mapa-ranking-par",
          tags$div(class = "painel painel-mapa",
            tags$div(class = "painel-titulo-linha",
              tags$h3("Onde", class = "painel-titulo"),
              radioButtons(session$ns("tipo_mapa_candidato"), NULL,
                choices = c("Coroplético" = "coropletico", "LISA" = "lisa"),
                selected = "coropletico", inline = TRUE)
            ),
            leafletOutput(session$ns("mapa_candidato"), height = "420px")
          ),
          tags$div(class = "lateral-candidato",
            uiOutput(session$ns("perfil")),
            tags$div(class = "painel painel-ranking",
              tags$h3("Ranking — municípios (votos)", class = "painel-titulo"),
              # 184px: o que sobra da altura do mapa (~503px) depois do
              # perfil (248px, custom.css) + gap + padding/titulo deste
              # painel -- ver comentario em .lateral-candidato no custom.css.
              plotly::plotlyOutput(session$ns("ranking_candidato"), height = "184px")
            )
          )
        ),
        painel_perfil_eleitorado_ui(session$ns, "candidato")
      )
    })

    registrar_perfil_eleitorado(output, "candidato", votos_candidato_secao, votos_partido_secao,
                                 composicoes_perfil, cargo, ano, candidato_sel_perfil, modo_atual)

    valores_candidato <- reactive({
      req(ano(), candidato_sel_perfil())
      valores_mapa(municipios, votos_candidato_municipio, votos_partido_municipio,
                   total_validos_municipio, cargo, ano(), candidato_sel_perfil())
    })

    output$mapa_candidato <- renderLeaflet({
      req(candidato_sel_perfil())
      montar_mapa(municipios, valores_candidato(), tipo = input$tipo_mapa_candidato %||% "coropletico", modo = modo_atual())
    })

    observeEvent(input$mapa_candidato_shape_click, {
      ao_clicar_municipio(input$mapa_candidato_shape_click$id)
    })

    output$ranking_candidato <- plotly::renderPlotly({
      req(candidato_sel_perfil())
      grafico_ranking_interativo(ranking_municipios(valores_candidato(), n = 10), modo_atual(),
                                  tamanho_municipio = 8)
    })

    output$perfil <- renderUI({
      req(candidato_sel_perfil())
      cand <- candidatos_sel() %>% filter(sq_candidato == as.numeric(candidato_sel_perfil()))
      if (nrow(cand) != 1) return(NULL)
      perfil <- perfil_candidato(perfil_candidatos, candidato_sel_perfil())
      votos_cand <- votos_nominais_total(votos_candidato_municipio, cargo, ano(), candidato_sel_perfil())
      ui_perfil_candidato(cand$nm_urna, cand$situacao, votos_cand, perfil)
    })
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Composicao do eleitorado (genero/faixa_etaria/escolaridade), 1 reactive
# por eixo -- criada 1x por secao (fora de registrar_perfil_eleitorado()) e
# passada pras 2 chamadas dela (partido E candidato) da mesma secao, pra
# COMPARTILHAR o mesmo objeto reactive entre partido/candidato (evita 2
# chamadas identicas a composicao_eleitorado_secao() por secao). O lado caro
# de verdade (scan + group_by sobre perfil_eleitorado_secao.rds, 13M+ linhas)
# ja nao roda mais aqui -- composicao_eleitorado_secao() (R/dados.R) so
# filtra as tabelas pre-calculadas por programs/17_composicao_eleitorado.R.
composicoes_eleitorado <- function(composicao_pre, ano, chave_municipio_sel = function() NULL,
                                     municipio_req = function() TRUE) {
  eixos <- c("genero", "faixa_etaria", "escolaridade")
  setNames(lapply(eixos, function(eixo) {
    reactive({
      req(ano(), municipio_req())
      composicao_eleitorado_secao(composicao_pre, ano(), eixo, chave_municipio_sel = chave_municipio_sel())
    })
  }), eixos)
}

# Liga os 3 outputs (rosca_<sufixo>, radar_<sufixo>_faixa_etaria/
# escolaridade) do bloco "Perfil do eleitorado" (painel_perfil_eleitorado_ui()
# acima) -- chamada 1x por bloco (partido OU candidato) em cada um dos 5
# pontos do arquivo que precisam dele. `composicoes`: saida de
# composicoes_eleitorado() acima, a MESMA lista passada nas 2 chamadas
# (partido e candidato) de uma secao. `candidato_sel`: function()/reactive()
# -> "TODOS" (visao de partido) ou a chave do candidato selecionado (visao
# por candidato, NULL enquanto nao selecionado -- req() segura a espera).
registrar_perfil_eleitorado <- function(output, sufixo, votos_candidato_secao, votos_partido_secao,
                                          composicoes, cargo, ano, candidato_sel, modo_atual,
                                          chave_municipio_sel = function() NULL) {
  calcular_indice <- function(eixo) {
    req(ano(), candidato_sel())
    indice_representacao(votos_candidato_secao, votos_partido_secao, composicoes[[eixo]](),
                          cargo, ano(), candidato_sel(), chave_municipio_sel = chave_municipio_sel())
  }
  indice_genero <- reactive(calcular_indice("genero"))
  indice_faixa_etaria <- reactive(calcular_indice("faixa_etaria"))
  indice_escolaridade <- reactive(calcular_indice("escolaridade"))

  output[[paste0("rosca_", sufixo)]] <- plotly::renderPlotly(grafico_rosca_genero(indice_genero(), modo_atual()))
  output[[paste0("radar_", sufixo, "_faixa_etaria")]] <-
    plotly::renderPlotly(grafico_radar(indice_faixa_etaria(), modo_atual(), "faixa_etaria"))
  output[[paste0("radar_", sufixo, "_escolaridade")]] <-
    plotly::renderPlotly(grafico_radar(indice_escolaridade(), modo_atual(), "escolaridade"))
}

# =============================================================================
# Aba municipal (Vereador/Prefeito, por bairro) -- mesma estrutura de 2 blocos
# da secao_ui()/secao_server() gerais, mas com uma dimensao a mais: o mapa de
# bairro so faz sentido dentro de UM municipio por vez, entao alem do filtro
# de Ano (ja existente nas gerais) tem um filtro de MUNICIPIO -- os dois sao
# GLOBAIS (inputs sem namespace f_ano_municipal/f_municipio_municipal),
# renderizados 1x em atalhos_municipal_ui() FORA do loop de cargos (Vereador +
# Prefeito = 2 secoes; se cada uma renderizasse seu proprio par de inputs,
# 2 elementos com o mesmo id quebrariam o binding do Shiny -- mesmo cuidado ja
# documentado sobre o filtro de Ano das eleicoes gerais em app.R).
# =============================================================================

# Filtro de Ano + Municipio da aba municipal, na mesma estetica de pill dos
# atalhos das eleicoes gerais (ver secao_ui() acima e custom.css).
# `municipio_padrao`: municipio pre-selecionado ao abrir a aba (o mais votado
# pelo NOVO no total, calculado em app.R) -- pedido do usuario, em vez de cair
# no 1o da lista em ordem alfabetica.
atalhos_municipal_ui <- function(anos, municipios, municipio_padrao) {
  tags$nav(class = "secao-atalhos",
    selectInput("f_ano_municipal", NULL, choices = anos, selected = max(anos),
                selectize = FALSE, width = "auto"),
    selectInput("f_municipio_municipal", NULL,
                choices = setNames(municipios$chave_municipio, municipios$name_muni),
                selected = municipio_padrao, selectize = FALSE, width = "auto"),
    lapply(CARGOS_MUNICIPAIS, function(c) tags$a(href = paste0("#sec-", c$id), c$titulo))
  )
}

# Cargo PROPORCIONAL (Vereador): mesma estrutura de 2 blocos das eleicoes
# gerais (Visao do partido + Visao por candidato, com filtro de Candidato) --
# faz sentido aqui porque varios candidatos do NOVO disputam ao mesmo tempo, e
# "partido" (soma de todos) e "candidato" (um so) sao valores DIFERENTES.
secao_ui_municipal <- function(id, titulo, cargo) {
  ns <- NS(id)

  if (!cargo_e_proporcional(cargo)) {
    # Cargo MAJORITARIO (Prefeito): o partido só pode registrar 1 candidato
    # por município/ano nessa disputa -- "visão do partido" e "visão por
    # candidato" seriam sempre o mesmo número, então não faz sentido
    # apresentar como um recorte de partido à parte (pedido explícito do
    # usuário). 1 bloco só, direto no candidato (sem dropdown de escolha, sem
    # card de "Nº suplentes" -- não existe suplente de prefeito). Todo o
    # conteúdo (inclusive o <h2>) fica atrás de um uiOutput só: se o
    # município/ano selecionado não tiver candidato do NOVO a prefeito, a
    # seção inteira desaparece em vez de mostrar cards zerados.
    return(tags$section(id = paste0("sec-", id), class = "secao", uiOutput(ns("conteudo"))))
  }

  id_partido <- paste0("sec-", id, "-partido")
  id_candidato <- paste0("sec-", id, "-candidato")

  tags$section(id = paste0("sec-", id), class = "secao",
    tags$h2(titulo),
    uiOutput(ns("aviso_vazio")),

    tags$div(class = "bloco-titulo", id = id_partido, "Visão do partido"),
    tags$div(class = "kpi-row kpi-row--5",
      uiOutput(ns("kpi_legenda")), uiOutput(ns("kpi_nominais")), uiOutput(ns("kpi_candidatos")),
      uiOutput(ns("kpi_eleitos")), uiOutput(ns("kpi_suplentes"))
    ),
    tags$div(class = "painel painel-votos-eleicao",
      tags$h3("Total de votos por eleição", class = "painel-titulo"),
      uiOutput(ns("votos_eleicao"))
    ),
    tags$div(class = "mapa-ranking-par",
      tags$div(class = "painel painel-mapa",
        tags$div(class = "painel-titulo-linha",
          tags$h3("Onde", class = "painel-titulo"),
          radioButtons(ns("tipo_mapa"), NULL,
            choices = c("Coroplético" = "coropletico", "LISA" = "lisa", "Densidade" = "densidade"),
            selected = "coropletico", inline = TRUE)
        ),
        leafletOutput(ns("mapa_partido"), height = "420px")
      ),
      tags$div(class = "painel painel-ranking",
        tags$h3("Ranking — bairros (votos)", class = "painel-titulo"),
        plotly::plotlyOutput(ns("ranking_partido"), height = "420px")
      )
    ),
    painel_perfil_eleitorado_ui(ns, "partido"),

    tags$div(class = "bloco-titulo", id = id_candidato, "Visão por candidato"),
    uiOutput(ns("bloco_candidato"))
  )
}

# `ano`/`municipio`: reactive() -> filtros GLOBAIS (f_ano_municipal/
# f_municipio_municipal, ver atalhos_municipal_ui()). `bairros`: geometria de
# bairro de TODOS os municipios da aba (filtrada ao municipio selecionado
# aqui dentro, 1x, reaproveitada pelos 2 blocos). `votos_candidato_bairro`/
# `votos_partido_bairro`/`total_validos_bairro`: tabelas completas (todos os
# municipios) -- tambem filtradas ao municipio selecionado aqui dentro, antes
# de repassar pras mesmas funcoes de dados.R/secao.R ja usadas pelas
# eleicoes gerais (candidatos_com_votos, votos_legenda_total,
# votos_nominais_total, votos_totais_por_ano, contagem_situacao) sem precisar
# de nenhuma versao "municipal" delas -- a diferenca de nivel geografico fica
# toda resolvida ANTES de chegar nessas funcoes.
secao_server_municipal <- function(id, cargo, titulo, candidatos, votos_candidato_bairro, votos_partido_bairro,
                                     total_validos_bairro, bairros, perfil_candidatos, locais_votacao,
                                     votos_candidato_local, votos_partido_local, votos_candidato_secao,
                                     votos_partido_secao, composicao_pre, ano, municipio, modo_atual) {
  moduleServer(id, function(input, output, session) {

    bairros_sel <- reactive({
      req(municipio())
      bairros %>% filter(chave_municipio == municipio())
    })
    chaves_bairro_sel <- reactive(bairros_sel() %>% st_drop_geometry() %>% select(chave_bairro))

    votos_candidato_sel <- reactive(votos_candidato_bairro %>% semi_join(chaves_bairro_sel(), by = "chave_bairro"))
    votos_partido_sel   <- reactive(votos_partido_bairro   %>% semi_join(chaves_bairro_sel(), by = "chave_bairro"))
    total_validos_sel   <- reactive(total_validos_bairro   %>% semi_join(chaves_bairro_sel(), by = "chave_bairro"))

    # Locais de votacao (mapa de densidade) sao filtrados por chave_municipio
    # direto (a propria coluna ja existe em locais_votacao_municipal.rds), nao
    # via semi_join de bairro como as tabelas acima -- os votos por local
    # (votos_candidato_local/votos_partido_local) ja saem restritos aos locais
    # com bairro resolvido la no ETL (11_consolidar_municipal.R), entao um
    # semi_join pela chave_local de locais_sel() e suficiente pra alinhar os 3.
    locais_sel <- reactive({
      req(municipio())
      locais_votacao %>% filter(chave_municipio == municipio())
    })
    chaves_local_sel <- reactive(locais_sel() %>% select(chave_local))

    votos_candidato_local_sel <- reactive(votos_candidato_local %>% semi_join(chaves_local_sel(), by = "chave_local"))
    votos_partido_local_sel   <- reactive(votos_partido_local   %>% semi_join(chaves_local_sel(), by = "chave_local"))

    candidatos_sel <- reactive({
      req(ano(), municipio())
      candidatos_por_cargo_ano_municipio(candidatos, cargo, ano(), municipio())
    })

    candidatos_ordenados <- reactive({
      req(ano())
      candidatos_com_votos(candidatos_sel(), votos_candidato_sel(), cargo, ano())
    })

    observeEvent(list(ano(), municipio()), {
      updateSelectInput(session, "candidato_perfil", choices = opcoes_candidato_individual(candidatos_ordenados()))
    })

    tem_candidato <- reactive(nrow(candidatos_sel()) > 0)

    output$aviso_vazio <- renderUI({
      if (tem_candidato()) return(NULL)
      div(class = "aviso-vazio",
          sprintf("O NOVO não lançou candidato a %s neste município em %s.", titulo, ano()))
    })

    # -- Bloco 1: partido (nunca depende de candidato) ---------------------

    output$kpi_legenda <- renderUI({
      v <- votos_legenda_total(votos_partido_sel(), cargo, ano())
      kpi_tile(if (is.na(v)) "Não aplicável" else formatar_votos(v), "Votos na legenda")
    })
    output$kpi_nominais <- renderUI({
      v <- votos_nominais_total(votos_candidato_sel(), cargo, ano(), "TODOS")
      kpi_tile(formatar_votos(v), "Votos nominais")
    })
    output$kpi_candidatos <- renderUI({
      kpi_tile(nrow(candidatos_sel()), "Nº candidatos")
    })
    output$kpi_eleitos <- renderUI({
      kpi_tile(contagem_situacao(candidatos_sel(), "TODOS", "eleito"), "Nº eleitos")
    })
    output$kpi_suplentes <- renderUI({
      kpi_tile(contagem_situacao(candidatos_sel(), "TODOS", "suplente"), "Nº suplentes")
    })

    valores_partido <- reactive({
      req(ano())
      valores_mapa_bairro(bairros_sel(), votos_candidato_sel(), votos_partido_sel(),
                           total_validos_sel(), cargo, ano(), "TODOS")
    })

    pontos_partido <- reactive({
      req(ano())
      pontos_calor_bairro(locais_sel(), votos_candidato_local_sel(), votos_partido_local_sel(),
                           cargo, ano(), "TODOS")
    })

    output$mapa_partido <- renderLeaflet({
      tipo <- input$tipo_mapa %||% "coropletico"
      if (tipo == "densidade") {
        montar_mapa_calor_bairro(bairros_sel(), pontos_partido(), modo = modo_atual())
      } else {
        montar_mapa_bairro(bairros_sel(), valores_partido(), tipo = tipo, modo = modo_atual())
      }
    })

    output$ranking_partido <- plotly::renderPlotly({
      grafico_ranking_interativo(ranking_bairros(valores_partido(), n = 10), modo_atual())
    })

    composicoes_perfil <- composicoes_eleitorado(composicao_pre, ano,
                                                   chave_municipio_sel = municipio, municipio_req = municipio)
    registrar_perfil_eleitorado(output, "partido", votos_candidato_secao, votos_partido_secao,
                                 composicoes_perfil, cargo, ano, function() "TODOS", modo_atual,
                                 chave_municipio_sel = municipio)

    output$votos_eleicao <- renderUI({
      serie <- votos_totais_por_ano(votos_partido_sel(), cargo, anos_disponiveis(candidatos))
      tags$div(class = "votos-eleicao-row",
        lapply(seq_len(nrow(serie)), function(i) {
          kpi_tile(formatar_votos(serie$votos[i]), sprintf("Votos em %d", serie$ano[i]))
        })
      )
    })

    # -- Bloco 2: por candidato ----------------------------------------------

    candidato_sel_perfil <- reactive({
      if (is.null(input$candidato_perfil) || !nzchar(input$candidato_perfil)) return(NULL)
      input$candidato_perfil
    })

    output$bloco_candidato <- renderUI({
      if (!tem_candidato()) {
        return(div(class = "aviso-vazio",
                    sprintf("Sem candidato do NOVO a %s neste município em %s para detalhar.", titulo, ano())))
      }
      tagList(
        selectInput(session$ns("candidato_perfil"), "Candidato",
                    choices = opcoes_candidato_individual(candidatos_ordenados()), width = "260px"),
        tags$div(class = "mapa-ranking-par",
          tags$div(class = "painel painel-mapa",
            tags$div(class = "painel-titulo-linha",
              tags$h3("Onde", class = "painel-titulo"),
              radioButtons(session$ns("tipo_mapa_candidato"), NULL,
                choices = c("Coroplético" = "coropletico", "LISA" = "lisa", "Densidade" = "densidade"),
                selected = "coropletico", inline = TRUE)
            ),
            leafletOutput(session$ns("mapa_candidato"), height = "420px")
          ),
          tags$div(class = "lateral-candidato",
            uiOutput(session$ns("perfil")),
            tags$div(class = "painel painel-ranking",
              tags$h3("Ranking — bairros (votos)", class = "painel-titulo"),
              plotly::plotlyOutput(session$ns("ranking_candidato"), height = "184px")
            )
          )
        ),
        painel_perfil_eleitorado_ui(session$ns, "candidato")
      )
    })

    registrar_perfil_eleitorado(output, "candidato", votos_candidato_secao, votos_partido_secao,
                                 composicoes_perfil, cargo, ano, candidato_sel_perfil, modo_atual,
                                 chave_municipio_sel = municipio)

    valores_candidato <- reactive({
      req(ano(), candidato_sel_perfil())
      valores_mapa_bairro(bairros_sel(), votos_candidato_sel(), votos_partido_sel(),
                           total_validos_sel(), cargo, ano(), candidato_sel_perfil())
    })

    pontos_candidato <- reactive({
      req(ano(), candidato_sel_perfil())
      pontos_calor_bairro(locais_sel(), votos_candidato_local_sel(), votos_partido_local_sel(),
                           cargo, ano(), candidato_sel_perfil())
    })

    output$mapa_candidato <- renderLeaflet({
      req(candidato_sel_perfil())
      tipo <- input$tipo_mapa_candidato %||% "coropletico"
      if (tipo == "densidade") {
        montar_mapa_calor_bairro(bairros_sel(), pontos_candidato(), modo = modo_atual())
      } else {
        montar_mapa_bairro(bairros_sel(), valores_candidato(), tipo = tipo, modo = modo_atual())
      }
    })

    output$ranking_candidato <- plotly::renderPlotly({
      req(candidato_sel_perfil())
      grafico_ranking_interativo(ranking_bairros(valores_candidato(), n = 10), modo_atual(), tamanho_municipio = 8)
    })

    output$perfil <- renderUI({
      req(candidato_sel_perfil())
      cand <- candidatos_sel() %>% filter(sq_candidato == as.numeric(candidato_sel_perfil()))
      if (nrow(cand) != 1) return(NULL)
      perfil <- perfil_candidato(perfil_candidatos, candidato_sel_perfil())
      votos_cand <- votos_nominais_total(votos_candidato_sel(), cargo, ano(), candidato_sel_perfil())
      ui_perfil_candidato(cand$nm_urna, cand$situacao, votos_cand, perfil)
    })
  })
}

# Variante de secao_server_municipal() pra cargo MAJORITARIO (Prefeito): por
# lei eleitoral um partido registra no maximo 1 candidato a prefeito por
# municipio/eleicao, entao "visao do partido" (soma de todos) e "visao por
# candidato" sao sempre o MESMO numero -- nao ha dropdown de candidato (pega
# a unica linha de candidatos_sel() direto) nem card de "Nº suplentes" (nao
# existe suplente de prefeito). Quando o municipio/ano selecionado nao tem
# candidato do NOVO, output$conteudo devolve NULL e a secao inteira
# desaparece (ver secao_ui_municipal()).
secao_server_municipal_majoritario <- function(id, cargo, titulo, candidatos, votos_candidato_bairro,
                                                 votos_partido_bairro, total_validos_bairro, bairros,
                                                 perfil_candidatos, locais_votacao, votos_candidato_local,
                                                 votos_partido_local, votos_candidato_secao, votos_partido_secao,
                                                 composicao_pre, ano, municipio, modo_atual) {
  moduleServer(id, function(input, output, session) {

    bairros_sel <- reactive({
      req(municipio())
      bairros %>% filter(chave_municipio == municipio())
    })
    chaves_bairro_sel <- reactive(bairros_sel() %>% st_drop_geometry() %>% select(chave_bairro))

    votos_candidato_sel <- reactive(votos_candidato_bairro %>% semi_join(chaves_bairro_sel(), by = "chave_bairro"))
    votos_partido_sel   <- reactive(votos_partido_bairro   %>% semi_join(chaves_bairro_sel(), by = "chave_bairro"))
    total_validos_sel   <- reactive(total_validos_bairro   %>% semi_join(chaves_bairro_sel(), by = "chave_bairro"))

    locais_sel <- reactive({
      req(municipio())
      locais_votacao %>% filter(chave_municipio == municipio())
    })
    chaves_local_sel <- reactive(locais_sel() %>% select(chave_local))

    votos_candidato_local_sel <- reactive(votos_candidato_local %>% semi_join(chaves_local_sel(), by = "chave_local"))
    votos_partido_local_sel   <- reactive(votos_partido_local   %>% semi_join(chaves_local_sel(), by = "chave_local"))

    candidatos_sel <- reactive({
      req(ano(), municipio())
      candidatos_por_cargo_ano_municipio(candidatos, cargo, ano(), municipio())
    })

    candidato_sel <- reactive({
      cs <- candidatos_sel()
      if (nrow(cs) == 0) return(NULL)
      as.character(cs$sq_candidato[1])
    })

    output$conteudo <- renderUI({
      req(ano(), municipio())
      if (is.null(candidato_sel())) return(NULL)

      tagList(
        tags$h2(titulo),
        tags$div(class = "kpi-row kpi-row--4",
          uiOutput(session$ns("kpi_legenda")), uiOutput(session$ns("kpi_nominais")),
          uiOutput(session$ns("kpi_candidatos")), uiOutput(session$ns("kpi_eleitos"))
        ),
        tags$div(class = "painel painel-votos-eleicao",
          tags$h3("Total de votos por eleição", class = "painel-titulo"),
          uiOutput(session$ns("votos_eleicao"))
        ),
        tags$div(class = "mapa-ranking-par",
          tags$div(class = "painel painel-mapa",
            tags$div(class = "painel-titulo-linha",
              tags$h3("Onde", class = "painel-titulo"),
              radioButtons(session$ns("tipo_mapa"), NULL,
                choices = c("Coroplético" = "coropletico", "LISA" = "lisa", "Densidade" = "densidade"),
                selected = "coropletico", inline = TRUE)
            ),
            leafletOutput(session$ns("mapa_candidato"), height = "420px")
          ),
          tags$div(class = "lateral-candidato",
            uiOutput(session$ns("perfil")),
            tags$div(class = "painel painel-ranking",
              tags$h3("Ranking — bairros (votos)", class = "painel-titulo"),
              plotly::plotlyOutput(session$ns("ranking_candidato"), height = "184px")
            )
          )
        ),
        painel_perfil_eleitorado_ui(session$ns, "candidato")
      )
    })

    output$kpi_legenda <- renderUI({
      v <- votos_legenda_total(votos_partido_sel(), cargo, ano())
      kpi_tile(if (is.na(v)) "Não aplicável" else formatar_votos(v), "Votos na legenda")
    })
    output$kpi_nominais <- renderUI({
      v <- votos_nominais_total(votos_candidato_sel(), cargo, ano(), "TODOS")
      kpi_tile(formatar_votos(v), "Votos nominais")
    })
    output$kpi_candidatos <- renderUI({
      kpi_tile(nrow(candidatos_sel()), "Nº candidatos")
    })
    output$kpi_eleitos <- renderUI({
      kpi_tile(contagem_situacao(candidatos_sel(), "TODOS", "eleito"), "Nº eleitos")
    })

    output$votos_eleicao <- renderUI({
      serie <- votos_totais_por_ano(votos_partido_sel(), cargo, anos_disponiveis(candidatos))
      tags$div(class = "votos-eleicao-row",
        lapply(seq_len(nrow(serie)), function(i) {
          kpi_tile(formatar_votos(serie$votos[i]), sprintf("Votos em %d", serie$ano[i]))
        })
      )
    })

    composicoes_perfil <- composicoes_eleitorado(composicao_pre, ano,
                                                   chave_municipio_sel = municipio, municipio_req = municipio)
    registrar_perfil_eleitorado(output, "candidato", votos_candidato_secao, votos_partido_secao,
                                 composicoes_perfil, cargo, ano, candidato_sel, modo_atual,
                                 chave_municipio_sel = municipio)

    valores_candidato <- reactive({
      req(ano(), candidato_sel())
      valores_mapa_bairro(bairros_sel(), votos_candidato_sel(), votos_partido_sel(),
                           total_validos_sel(), cargo, ano(), candidato_sel())
    })

    pontos_candidato <- reactive({
      req(ano(), candidato_sel())
      pontos_calor_bairro(locais_sel(), votos_candidato_local_sel(), votos_partido_local_sel(),
                           cargo, ano(), candidato_sel())
    })

    output$mapa_candidato <- renderLeaflet({
      req(candidato_sel())
      tipo <- input$tipo_mapa %||% "coropletico"
      if (tipo == "densidade") {
        montar_mapa_calor_bairro(bairros_sel(), pontos_candidato(), modo = modo_atual())
      } else {
        montar_mapa_bairro(bairros_sel(), valores_candidato(), tipo = tipo, modo = modo_atual())
      }
    })

    output$ranking_candidato <- plotly::renderPlotly({
      req(candidato_sel())
      grafico_ranking_interativo(ranking_bairros(valores_candidato(), n = 10), modo_atual(), tamanho_municipio = 8)
    })

    output$perfil <- renderUI({
      req(candidato_sel())
      cand <- candidatos_sel() %>% filter(sq_candidato == as.numeric(candidato_sel()))
      if (nrow(cand) != 1) return(NULL)
      perfil <- perfil_candidato(perfil_candidatos, candidato_sel())
      votos_cand <- votos_nominais_total(votos_candidato_sel(), cargo, ano(), candidato_sel())
      ui_perfil_candidato(cand$nm_urna, cand$situacao, votos_cand, perfil)
    })
  })
}

# --- Helpers de apresentacao -------------------------------------------------

kpi_tile <- function(valor_texto, rotulo) {
  tags$div(class = "kpi-tile",
    tags$div(class = "kpi-valor", valor_texto),
    tags$div(class = "kpi-rotulo", rotulo)
  )
}

# Grafico de ranking (municipios por votos) -- usado nos 2 blocos.
# `tamanho_municipio`: tamanho de fonte dos nomes de municipio (eixo y, com
# coord_flip) -- por padrao segue o base_size do tema (13), mas o Bloco 2
# passa um valor menor porque o grafico ali divide uma coluna bem mais
# estreita e baixa (184px) com o card de perfil.
grafico_ranking <- function(rk, modo, tamanho_municipio = 10.4) {
  if (nrow(rk) == 0 || all(rk$votos == 0)) return(NULL)
  # rk ja chega ordenado desc(votos) (ranking_municipios) -- a 1a linha e o
  # municipio mais votado, marcado para destaque antes de inverter os niveis
  # do factor (coord_flip espera a ordem invertida pra ficar de cima pra baixo).
  rk <- rk %>%
    mutate(destaque = row_number() == 1,
           municipio = factor(municipio, levels = rev(municipio)))
  cor_texto <- if (modo == "dark") "#ffffff" else "#0b0b0b"
  cor_grade <- if (modo == "dark") "#2c2c2a" else "#e1e0d9"
  # Mesmo --accent do custom.css (claro/escuro) -- antes era um hex fixo,
  # entao a barra do ranking ficava com a cor do modo claro mesmo no escuro.
  cor_barra <- if (modo == "dark") "#FF9800" else COR_LINHA
  # Municipio mais votado ganha um laranja mais escuro/queimado -- distinto o
  # bastante da barra padrao pra chamar atencao, mas nao tao escuro a ponto de
  # se confundir com preto (nem no modo escuro).
  cor_destaque <- if (modo == "dark") "#B33F00" else "#8A3200"

  ggplot(rk, aes(x = municipio, y = votos, fill = destaque,
                 text = paste0(municipio, ": ", formatar_votos(votos), " votos"))) +
    geom_col(width = 0.68) +
    scale_fill_manual(values = c(`TRUE` = cor_destaque, `FALSE` = cor_barra), guide = "none") +
    coord_flip() +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(x = NULL, y = "Votos") +
    theme_minimal(base_size = 13) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major.x = element_line(color = cor_grade, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.x = element_text(color = cor_texto),
      axis.text.y = element_text(color = cor_texto, size = tamanho_municipio),
      axis.title = element_text(color = cor_texto)
    )
}

# Versao interativa de grafico_ranking() (tooltip com votos ao passar o
# mouse) -- ggplotly() reaproveita o ggplot inteiro (cores, destaque do mais
# votado, tema claro/escuro) e so troca o dispositivo estatico por um widget
# htmlwidgets. Sem modebar/zoom: aqui o objetivo e so o tooltip, nao virar um
# grafico exploravel (mantem a mesma sobriedade visual do resto do dashboard).
grafico_ranking_interativo <- function(rk, modo, tamanho_municipio = 10.4) {
  p <- grafico_ranking(rk, modo, tamanho_municipio)
  if (is.null(p)) return(NULL)
  cor_texto <- if (modo == "dark") "#ffffff" else "#0b0b0b"
  fundo_dica <- if (modo == "dark") "#21242b" else "#ffffff"

  suppressWarnings(plotly::ggplotly(p, tooltip = "text")) %>%
    plotly::layout(
      showlegend = FALSE,
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      xaxis = list(fixedrange = TRUE),
      yaxis = list(fixedrange = TRUE),
      hoverlabel = list(bgcolor = fundo_dica, font = list(color = cor_texto))
    ) %>%
    plotly::config(displayModeBar = FALSE, scrollZoom = FALSE)
}

# --- "Perfil do eleitorado" (rosca de genero + radar de faixa etaria/
# escolaridade, indice de representacao demografica) ------------------------
# Rotulo/ordem de exibicao por eixo -- FEMININO/MASCULINO (ordem TSE, ja vem
# so com essas 2 categorias uteis, ver R/dados.R::indice_representacao()),
# GRUPOS_FAIXA_ETARIA (constante de R/dados.R, jovem->idoso) e escolaridade
# em ordem crescente de nivel (nao a ordem alfabetica que sairia de um
# distinct() cru).
ORDEM_CATEGORIA_EIXO <- list(
  genero = c("FEMININO", "MASCULINO"),
  faixa_etaria = GRUPOS_FAIXA_ETARIA,
  escolaridade = c("ANALFABETO", "LÊ E ESCREVE", "ENSINO FUNDAMENTAL INCOMPLETO", "ENSINO FUNDAMENTAL COMPLETO",
                    "ENSINO MÉDIO INCOMPLETO", "ENSINO MÉDIO COMPLETO", "SUPERIOR INCOMPLETO", "SUPERIOR COMPLETO")
)

# Rosca de genero: laranja mais claro para Mulheres, mais escuro para Homens
# (pedido explicito) -- fixa nos 2 modos (claro/escuro), nao e um token de
# accent que muda com o tema, e sim uma cor semantica por categoria.
COR_ROSCA_GENERO <- c(FEMININO = "#FFC98A", MASCULINO = "#B33F00")
ROTULO_GENERO <- c(FEMININO = "Mulheres", MASCULINO = "Homens")

# `indice_df`: saida de indice_representacao(eixo = "genero") (categoria,
# indice, fracao_ponderada). Fatia = fracao_ponderada (participacao de cada
# genero no total de votos do NOVO nas secoes, ponderada por voto) -- ao
# contrario do radar (raio = indice, comparacao com o eleitorado geral), a
# rosca mostra a COMPOSICAO em si; o indice de sobre/sub-representacao fica
# no hover, pra nao perder a comparacao que da sentido ao numero.
grafico_rosca_genero <- function(indice_df, modo) {
  if (nrow(indice_df) == 0) return(NULL)
  ordem <- ORDEM_CATEGORIA_EIXO$genero
  indice_df <- indice_df %>%
    mutate(categoria = factor(categoria, levels = ordem)) %>%
    filter(!is.na(categoria)) %>%
    arrange(categoria)
  if (nrow(indice_df) < 2) return(NULL)

  cor_texto <- if (modo == "dark") "#ffffff" else "#0b0b0b"
  fundo_dica <- if (modo == "dark") "#21242b" else "#ffffff"
  cor_borda_fatia <- if (modo == "dark") "#15161a" else "#ffffff"
  categoria_chr <- as.character(indice_df$categoria)

  plotly::plot_ly(
    labels = ROTULO_GENERO[categoria_chr],
    values = indice_df$fracao_ponderada,
    type = "pie", hole = 0.62, sort = FALSE,
    marker = list(colors = unname(COR_ROSCA_GENERO[categoria_chr]),
                  line = list(color = cor_borda_fatia, width = 2)),
    text = sprintf("%s: %s%% dos votos do NOVO (%sx a média do eleitorado)",
                    ROTULO_GENERO[categoria_chr],
                    format(round(indice_df$fracao_ponderada * 100, 1), decimal.mark = ","),
                    format(round(indice_df$indice, 2), decimal.mark = ",")),
    hoverinfo = "text", textinfo = "percent", textfont = list(color = cor_texto)
  ) %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1,
                    font = list(color = cor_texto, size = 11)),
      margin = list(t = 10, b = 10, l = 10, r = 10),
      hoverlabel = list(bgcolor = fundo_dica, font = list(color = cor_texto))
    ) %>%
    plotly::config(displayModeBar = FALSE)
}

# `indice_df`: saida de dados.R::indice_representacao() (categoria, indice).
# Raio = indice (1.0 = mesma proporcao do eleitorado geral, centro visual do
# radar -- por isso o circulo pontilhado de referencia, sem ele o leitor nao
# tem como saber se um valor e "alto" ou "baixo" so olhando a forma).
# scatterpolar (plotly, so com prefixo -- mesmo motivo do plotly:: em
# grafico_ranking_interativo()): sem lib de radar chart nova no projeto.
grafico_radar <- function(indice_df, modo, eixo) {
  if (nrow(indice_df) == 0) return(NULL)
  ordem <- ORDEM_CATEGORIA_EIXO[[eixo]]
  indice_df <- indice_df %>%
    mutate(categoria = factor(categoria, levels = ordem)) %>%
    filter(!is.na(categoria)) %>%
    arrange(categoria)
  # So chamado com faixa_etaria (5 categorias) e escolaridade (ate 8) --
  # genero (sempre 2 categorias, radar ficaria com poligono degenerado) usa
  # grafico_rosca_genero() em vez deste. Guarda de seguranca: com menos de 2
  # categorias nao ha "teia" nenhuma pra desenhar.
  if (nrow(indice_df) < 2) return(NULL)

  theta <- c(as.character(indice_df$categoria), as.character(indice_df$categoria[1]))
  r <- c(indice_df$indice, indice_df$indice[1])

  cor_linha <- if (modo == "dark") "#FF9800" else COR_LINHA
  cor_texto <- if (modo == "dark") "#ffffff" else "#0b0b0b"
  cor_grade <- if (modo == "dark") "#2c2c2a" else "#e1e0d9"
  fundo_dica <- if (modo == "dark") "#21242b" else "#ffffff"

  plotly::plot_ly() %>%
    plotly::add_trace(
      type = "scatterpolar", mode = "lines", r = rep(1, length(theta)), theta = theta,
      line = list(color = cor_grade, dash = "dot", width = 1),
      hoverinfo = "skip", showlegend = FALSE
    ) %>%
    plotly::add_trace(
      type = "scatterpolar", mode = "lines+markers", fill = "toself",
      r = r, theta = theta,
      line = list(color = cor_linha), fillcolor = paste0(cor_linha, "33"),
      marker = list(color = cor_linha, size = 7),
      text = sprintf("%s: %sx a média", theta, format(round(r, 2), decimal.mark = ",")),
      hoverinfo = "text", showlegend = FALSE
    ) %>%
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      # Margem generosa + fonte menor no rotulo angular: cada radar agora
      # divide 1/3 da largura do painel (3 graficos lado a lado, ver
      # painel_perfil_eleitorado_ui()) -- sem isso os rotulos mais longos de
      # escolaridade ("ENSINO FUNDAMENTAL INCOMPLETO" etc.) ficavam cortados
      # na borda do widget.
      margin = list(t = 40, b = 40, l = 60, r = 60),
      polar = list(
        bgcolor = "rgba(0,0,0,0)",
        radialaxis = list(gridcolor = cor_grade, linecolor = cor_grade, tickfont = list(color = cor_texto, size = 9)),
        angularaxis = list(gridcolor = cor_grade, linecolor = cor_grade, tickfont = list(color = cor_texto, size = 9))
      ),
      hoverlabel = list(bgcolor = fundo_dica, font = list(color = cor_texto))
    ) %>%
    plotly::config(displayModeBar = FALSE)
}
