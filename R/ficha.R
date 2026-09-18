# Ficha do municipio (modal ao clicar num municipio de qualquer secao) --
# mesmo padrao de "Dashboard economia PE"/R/ficha.R: resumo curado (aqui, o
# NOVO nas corridas de CARGOS no ano selecionado), nao a lista completa de dados.

library(shiny)

linha_ficha <- function(item) {
  legenda_txt <- if (is.na(item$votos_legenda)) "não aplicável" else paste0(formatar_votos(item$votos_legenda), " votos de legenda")
  tags$tr(
    tags$td(item$titulo, class = "ficha-rotulo"),
    tags$td(
      tags$div(paste0(formatar_votos(item$votos_nominais), " votos nominais")),
      tags$div(class = "text-muted", style = "font-size: 0.85em;", legenda_txt),
      class = "ficha-valor"
    ),
    tags$td(formatar_pct(item$pct_validos), class = "ficha-valor")
  )
}

abrir_ficha_modal <- function(nome_municipio, ano_sel, resumo) {
  corpo <- tags$table(class = "ficha-tabela",
    tags$thead(tags$tr(tags$th("Cargo"), tags$th("Votos do NOVO"), tags$th("% dos válidos"))),
    tags$tbody(lapply(resumo, linha_ficha))
  )

  showModal(modalDialog(
    title = tags$div(
      tags$strong(nome_municipio),
      tags$div(class = "text-muted", style = "font-size: 0.85em;", paste("Eleição de", ano_sel))
    ),
    corpo,
    easyClose = TRUE,
    footer = modalButton("Fechar")
  ))
}
