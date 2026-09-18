# Renderizacao do card de perfil de candidato (Bloco 2 de cada secao) --
# adaptado do padrao de card de perfil do projeto "Mapeador de liderancas"
# (avatar + badge de situacao + stat de votos + grid de demografia + redes
# sociais), usando os dados de R/dados.R::perfil_candidato() /
# classificar_rede_social(). Mesmo espirito de separacao de responsabilidade
# que R/ficha.R ja tem pro modal de municipio.

library(shiny)

capitalizar <- function(x) {
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  x <- tolower(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

formatar_genero <- function(g) {
  if (is.na(g)) return(NA_character_)
  dplyr::case_when(
    grepl("^MASC", g) ~ "Masculino",
    grepl("^FEM", g) ~ "Feminino",
    TRUE ~ capitalizar(g)
  )
}

avatar_candidato <- function(nm_urna, foto_path, foto_disponivel) {
  if (isTRUE(foto_disponivel) && !is.na(foto_path)) {
    tags$img(src = foto_path, class = "perfil-avatar", alt = nm_urna)
  } else {
    tags$div(class = "perfil-avatar perfil-avatar--fallback", substr(nm_urna, 1, 1))
  }
}

badge_situacao <- function(situacao) {
  rotulo <- switch(situacao,
    eleito = "Eleito",
    suplente = "Suplente",
    nao_eleito = "Não eleito",
    "Situação não informada"
  )
  classe <- switch(situacao,
    eleito = "perfil-badge perfil-badge--eleito",
    suplente = "perfil-badge perfil-badge--suplente",
    "perfil-badge perfil-badge--nao-eleito"
  )
  tags$span(class = classe, rotulo)
}

# Fatos demograficos como uma linha corrida ("23 anos · Masculino · ..."), nao
# uma grade rotulo/valor -- o card de perfil aqui divide a altura do mapa com
# o ranking (R/secao.R), entao precisa do formato mais compacto verticalmente
# que ainda cabe todo mundo (uma grade de 6 linhas nao cabia no espaco).
fatos_compactos <- function(...) {
  fatos <- unlist(list(...))
  fatos <- fatos[!is.na(fatos) & nzchar(fatos)]
  if (length(fatos) == 0) return(NULL)
  paste(fatos, collapse = " · ")
}

pills_redes <- function(redes) {
  if (length(redes) == 0 || all(!nzchar(redes))) {
    return(tags$span(class = "text-muted", style = "font-style: italic; font-size: 0.85em;",
                      "Nenhuma rede social declarada"))
  }
  tags$div(class = "perfil-redes",
    lapply(redes, function(url) {
      tags$a(class = "perfil-rede-pill", href = url, target = "_blank", rel = "noopener",
             classificar_rede_social(url))
    })
  )
}

# Perfil compacto: fica do lado direito do mapa, empilhado ACIMA do ranking
# (R/secao.R monta essa pilha), dividindo os mesmos 420px de altura do mapa.
# Por isso e deliberadamente denso -- cabecalho pequeno, stat inline (nao mais
# o numero gigante do KPI), fatos demograficos em linha corrida em vez de
# grade. `overflow-y: auto` no painel (custom.css) e uma rede de seguranca
# pro caso raro de um nome de ocupacao muito longo estourar a altura, em vez
# de quebrar o alinhamento com o mapa.
ui_perfil_candidato <- function(nm_urna, situacao, votos_nominais, perfil) {
  tags$div(class = "painel painel-perfil",
    tags$h3("Perfil do candidato", class = "painel-titulo"),
    tags$div(class = "perfil-cabecalho",
      avatar_candidato(nm_urna, perfil$foto_path, perfil$foto_disponivel),
      tags$div(
        tags$div(class = "perfil-nome", nm_urna),
        badge_situacao(situacao)
      )
    ),
    tags$div(class = "perfil-stat-inline",
      tags$strong(formatar_votos(votos_nominais)), " votos nominais"
    ),
    tags$div(class = "perfil-fatos",
      fatos_compactos(
        if (is.na(perfil$idade)) NA else paste0(perfil$idade, " anos"),
        formatar_genero(perfil$genero),
        capitalizar(perfil$estado_civil),
        capitalizar(perfil$cor_raca),
        perfil$grau_instrucao,
        perfil$ocupacao
      )
    ),
    pills_redes(perfil$redes[[1]])
  )
}
