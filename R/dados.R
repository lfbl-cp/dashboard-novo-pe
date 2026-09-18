# Carregamento dos dados processados + funcoes de apoio do app (filtro
# candidato/cargo/ano, valores do mapa, ranking, KPIs, serie temporal). Mesmo
# papel do R/dados.R da referencia "Dashboard economia PE", adaptado: aqui nao
# ha filtro geografico em cascata (o mapa ja e a unidade municipio) -- o que
# se repete por "categoria" e o CARGO, nao uma regiao.

library(dplyr)
library(tidyr)
library(sf)

PARTIDO_ALVO <- "NOVO"

# id (usado em NS()/ancora) -> rotulo de exibicao. Ordem = ordem das secoes.
# O NOVO nunca lancou candidato a Deputado Estadual nem Senador em PE (2018/2022)
# -- so Deputado Federal tem candidatos -- por isso as outras duas secoes foram
# removidas em vez de ficarem sempre com o aviso de estado vazio.
CARGOS <- list(
  list(id = "federal", cargo = "DEPUTADO FEDERAL", titulo = "Deputado Federal")
)

# Eleicoes municipais: Vereador (proporcional, como Dep. Federal/Estadual) e
# Prefeito (majoritario, sem voto de legenda). PREFEITO entra em
# cargo_e_proporcional() = FALSE, cai no mesmo caminho "Nao aplicavel" que
# Senador ja usa nas eleicoes gerais.
CARGOS_MUNICIPAIS <- list(
  list(id = "vereador", cargo = "VEREADOR", titulo = "Vereador"),
  list(id = "prefeito", cargo = "PREFEITO", titulo = "Prefeito")
)

cargo_e_proporcional <- function(cargo) cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "VEREADOR")

normalizar_texto <- function(x) {
  x <- toupper(trimws(x))
  chartr("ÁÀÂÃÄÉÈÊËÍÌÎÏÓÒÔÕÖÚÙÛÜÇ", "AAAAAEEEEIIIIOOOOOUUUUC", x)
}

carregar_candidatos <- function() readRDS("data/processed/candidatos_novo.rds")
carregar_votos_candidato_municipio <- function() readRDS("data/processed/votos_candidato_municipio.rds")
carregar_votos_partido_municipio <- function() readRDS("data/processed/votos_partido_municipio.rds")
carregar_total_validos_municipio <- function() readRDS("data/processed/total_validos_municipio.rds")
carregar_municipios <- function() readRDS("data/processed/municipios_pe.rds")
carregar_perfil_candidatos <- function() readRDS("data/processed/perfil_candidatos.rds")

carregar_bairros <- function() readRDS("data/processed/bairros_pe.rds")
carregar_candidatos_municipais <- function() readRDS("data/processed/candidatos_novo_municipais.rds")
carregar_votos_candidato_bairro <- function() readRDS("data/processed/votos_candidato_bairro.rds")
carregar_votos_partido_bairro <- function() readRDS("data/processed/votos_partido_bairro.rds")
carregar_total_validos_bairro <- function() readRDS("data/processed/total_validos_bairro.rds")
carregar_perfil_candidatos_municipais <- function() readRDS("data/processed/perfil_candidatos_municipais.rds")
carregar_locais_votacao_municipal <- function() readRDS("data/processed/locais_votacao_municipal.rds")
carregar_votos_candidato_local <- function() readRDS("data/processed/votos_candidato_local.rds")
carregar_votos_partido_local <- function() readRDS("data/processed/votos_partido_local.rds")

carregar_votos_candidato_secao <- function() readRDS("data/processed/votos_candidato_secao.rds")
carregar_votos_partido_secao <- function() readRDS("data/processed/votos_partido_secao.rds")

# So usado pelo pipeline de ETL (programs/17_composicao_eleitorado.R,
# programs/13_export_dist_data.R) -- o app Shiny NUNCA carrega este arquivo
# (13M+ linhas) em runtime, ver carregar_composicao_eleitorado_pre() abaixo.
# Fica em data/intermediate/ (nao data/processed/) exatamente por isso: nao e
# um dado que o app le, e nao devia ir pro deploy (rsconnect, ver
# .rscignore) nem pro git (67MB, so relevante enquanto se reprocessa o
# pipeline -- ver .gitignore).
carregar_perfil_eleitorado_secao <- function() readRDS("data/intermediate/perfil_eleitorado_secao.rds")

# Composicao do eleitorado JA PRE-CALCULADA por programs/17_composicao_eleitorado.R
# (uma linha por ano/eixo/escopo/categoria, ver ESCOPO_GERAL abaixo) -- e o que
# o app Shiny le em vez do perfil_eleitorado_secao.rds bruto. Ver
# composicao_eleitorado_secao() mais abaixo, que so filtra essas duas tabelas.
carregar_composicao_eleitorado_pre <- function() {
  list(
    composicao = readRDS("data/processed/composicao_eleitorado_secao.rds"),
    denom = readRDS("data/processed/denom_eleitorado_secao.rds")
  )
}

anos_disponiveis <- function(candidatos) sort(unique(candidatos$ano))

# Municipios da aba municipal (so os que tem geometria de bairro no IBGE --
# ver 09_bairros_municipios_alvo.R), do mais para o menos populoso na pratica
# (ordem alfabetica e mais previsivel pro usuario que ordem de votos).
municipios_municipais_disponiveis <- function(bairros) {
  bairros %>% st_drop_geometry() %>% distinct(chave_municipio, name_muni) %>% arrange(name_muni)
}

# --- Candidato (controle local de cada secao) -------------------------------

candidatos_por_cargo_ano <- function(candidatos, cargo, ano) {
  candidatos %>% filter(cargo == !!cargo, ano == !!ano)
}

# Variante municipal: candidatos_novo_municipais.rds tem candidatos de VARIOS
# municipios juntos (Vereador de Recife e de Caruaru compartilham a mesma
# tabela) -- aqui tambem filtra pelo municipio selecionado na aba.
candidatos_por_cargo_ano_municipio <- function(candidatos, cargo, ano, chave_municipio_sel) {
  candidatos %>% filter(cargo == !!cargo, ano == !!ano, chave_municipio == !!chave_municipio_sel)
}

# candidatos_filtrados + total de votos nominais (cargo/ano), do mais votado
# pro menos votado -- base do filtro "Candidato" do Bloco 2 (a ordem do
# data.frame e preservada pelo selectInput/setNames na hora de montar as
# choices, entao basta ordenar aqui uma vez).
candidatos_com_votos <- function(candidatos_filtrados, votos_candidato_municipio, cargo, ano) {
  if (nrow(candidatos_filtrados) == 0) return(candidatos_filtrados %>% mutate(votos_total = numeric(0)))
  totais <- votos_candidato_municipio %>%
    filter(cargo == !!cargo, ano == !!ano) %>%
    group_by(sq_candidato) %>%
    summarise(votos_total = sum(votos_nominais), .groups = "drop")
  candidatos_filtrados %>%
    left_join(totais, by = "sq_candidato") %>%
    mutate(votos_total = replace_na(votos_total, 0)) %>%
    arrange(desc(votos_total))
}

# nome = rotulo exibido, valor = o que vira input$candidato (padrao inverso do
# que parece intuitivo em selectInput(choices = c(nome = valor))).
opcoes_candidato <- function(candidatos_filtrados) {
  escolhas <- c("Todos (partido)" = "TODOS")
  if (nrow(candidatos_filtrados) > 0) {
    extra <- setNames(as.character(candidatos_filtrados$sq_candidato), candidatos_filtrados$nm_urna)
    escolhas <- c(escolhas, extra)
  }
  escolhas
}

# Mesma logica de opcoes_candidato(), sem a entrada "Todos" -- usado no filtro
# "por candidato" (Bloco 2 da secao), que so faz sentido com um candidato real
# selecionado.
opcoes_candidato_individual <- function(candidatos_filtrados) {
  if (nrow(candidatos_filtrados) == 0) return(character(0))
  setNames(as.character(candidatos_filtrados$sq_candidato), candidatos_filtrados$nm_urna)
}

# --- KPIs ---------------------------------------------------------------

votos_legenda_total <- function(votos_partido_municipio, cargo, ano) {
  if (!cargo_e_proporcional(cargo)) return(NA_real_)
  votos_partido_municipio %>%
    filter(ano == !!ano, cargo == !!cargo, sg_partido == PARTIDO_ALVO) %>%
    summarise(v = sum(votos_legenda)) %>%
    pull(v)
}

votos_nominais_total <- function(votos_candidato_municipio, cargo, ano, candidato_sel) {
  base <- votos_candidato_municipio %>% filter(ano == !!ano, cargo == !!cargo)
  if (candidato_sel != "TODOS") base <- base %>% filter(sq_candidato == as.numeric(candidato_sel))
  if (nrow(base) == 0) return(0)
  sum(base$votos_nominais)
}

contagem_situacao <- function(candidatos_filtrados, candidato_sel, situacao_alvo) {
  base <- candidatos_filtrados
  if (candidato_sel != "TODOS") base <- base %>% filter(sq_candidato == as.numeric(candidato_sel))
  sum(base$situacao == situacao_alvo, na.rm = TRUE)
}

# --- Mapa / ranking (mesma base para os dois, evita divergencia) -----------

# Um dado por municipio: votos do recorte (partido ou candidato especifico),
# total de votos validos do municipio/cargo (denominador) e % resultante.
# Municipio sem nenhum voto do NOVO fica com votos = 0 (nao desaparece).
valores_mapa <- function(municipios, votos_candidato_municipio, votos_partido_municipio,
                          total_validos_municipio, cargo, ano, candidato_sel) {
  denom <- total_validos_municipio %>%
    filter(ano == !!ano, cargo == !!cargo) %>%
    select(chave_municipio, total_validos)

  if (candidato_sel == "TODOS") {
    numer <- votos_partido_municipio %>%
      filter(ano == !!ano, cargo == !!cargo, sg_partido == PARTIDO_ALVO) %>%
      select(chave_municipio, votos = votos_validos_partido)
  } else {
    numer <- votos_candidato_municipio %>%
      filter(ano == !!ano, cargo == !!cargo, sq_candidato == as.numeric(candidato_sel)) %>%
      select(chave_municipio, votos = votos_nominais)
  }

  municipios %>%
    st_drop_geometry() %>%
    select(chave_municipio, municipio = name_muni) %>%
    left_join(denom, by = "chave_municipio") %>%
    left_join(numer, by = "chave_municipio") %>%
    mutate(
      votos = replace_na(votos, 0),
      total_validos = replace_na(total_validos, 0),
      pct_validos = if_else(total_validos > 0, 100 * votos / total_validos, 0)
    )
}

ranking_municipios <- function(valores, n = 10) {
  valores %>% arrange(desc(votos)) %>% slice_head(n = n) %>% select(municipio, votos, pct_validos)
}

# --- Mapa / ranking por BAIRRO (aba municipal) -------------------------------
# Mesma logica de valores_mapa(), so que escopada a UM municipio por vez (o
# mapa de bairro so faz sentido dentro de um municipio -- ver
# montar_mapa_bairro() em R/mapa.R) e com os nomes de tabela/coluna trocados
# (chave_bairro em vez de chave_municipio). `votos_candidato_bairro`/
# `votos_partido_bairro`/`total_validos_bairro`: espera-se que ja venham
# filtrados ao municipio selecionado (feito 1x no server, reaproveitado por
# todas as chamadas -- ver secao_server_municipal).
valores_mapa_bairro <- function(bairros_municipio, votos_candidato_bairro, votos_partido_bairro,
                                  total_validos_bairro, cargo, ano, candidato_sel) {
  denom <- total_validos_bairro %>%
    filter(ano == !!ano, cargo == !!cargo) %>%
    select(chave_bairro, total_validos)

  if (candidato_sel == "TODOS") {
    numer <- votos_partido_bairro %>%
      filter(ano == !!ano, cargo == !!cargo, sg_partido == PARTIDO_ALVO) %>%
      select(chave_bairro, votos = votos_validos_partido)
  } else {
    numer <- votos_candidato_bairro %>%
      filter(ano == !!ano, cargo == !!cargo, sq_candidato == as.numeric(candidato_sel)) %>%
      select(chave_bairro, votos = votos_nominais)
  }

  bairros_municipio %>%
    st_drop_geometry() %>%
    select(chave_bairro, bairro = name_neighborhood) %>%
    left_join(denom, by = "chave_bairro") %>%
    left_join(numer, by = "chave_bairro") %>%
    mutate(
      votos = replace_na(votos, 0),
      total_validos = replace_na(total_validos, 0),
      pct_validos = if_else(total_validos > 0, 100 * votos / total_validos, 0)
    )
}

# --- Pontos para o mapa de densidade (aba municipal) ------------------------
# Base do heatmap (montar_mapa_calor_bairro(), R/mapa.R): 1 linha por local de
# votacao com voto > 0 do recorte selecionado (mesmo "TODOS" = partido, senao
# candidato especifico, de valores_mapa_bairro()), com lat/lon (de
# locais_votacao_municipal.rds, ja geocodificado) e peso = votos do NOVO ali.
# Ao contrario de valores_mapa_bairro() (sempre mostra todo bairro, mesmo com
# 0), local sem voto do recorte NAO aparece -- peso 0 nao ajuda a visualizar
# densidade, so ocupa area do kernel a toa. `votos_candidato_local`/
# `votos_partido_local` ja vem so do NOVO (ver 11_consolidar_municipal.R), ao
# contrario de votos_partido_bairro (todos os partidos, usado como
# denominador de %) -- por isso nao precisa filtrar sg_partido aqui.
pontos_calor_bairro <- function(locais_municipio, votos_candidato_local, votos_partido_local,
                                  cargo, ano, candidato_sel) {
  if (candidato_sel == "TODOS") {
    numer <- votos_partido_local %>%
      filter(ano == !!ano, cargo == !!cargo) %>%
      select(chave_local, peso = votos_validos_partido)
  } else {
    numer <- votos_candidato_local %>%
      filter(ano == !!ano, cargo == !!cargo, sq_candidato == as.numeric(candidato_sel)) %>%
      select(chave_local, peso = votos_nominais)
  }

  locais_municipio %>%
    select(chave_local, lat, lon) %>%
    inner_join(numer, by = "chave_local") %>%
    filter(peso > 0, !is.na(lat), !is.na(lon))
}

# `municipio = bairro`: reaproveita grafico_ranking()/grafico_ranking_interativo()
# (R/secao.R) sem duplicar o ggplot inteiro -- essas funcoes so leem o rotulo
# de uma coluna chamada "municipio", nao dependem de ser um municipio de
# verdade.
ranking_bairros <- function(valores, n = 10) {
  valores %>% arrange(desc(votos)) %>% slice_head(n = n) %>% transmute(municipio = bairro, votos, pct_validos)
}

# --- Votos por eleicao (cards do Bloco 1) -----------------------------------
# Total de votos validos do partido (legenda + nominal) por ano -- substitui a
# antiga serie temporal em linha, que com so 2 eleicoes (2018/2022) sugeria uma
# "tendencia" que era na verdade so 2 pontos.

votos_totais_por_ano <- function(votos_partido_municipio, cargo, anos) {
  votos_partido_municipio %>%
    filter(cargo == !!cargo, sg_partido == PARTIDO_ALVO) %>%
    group_by(ano) %>%
    summarise(votos = sum(votos_validos_partido), .groups = "drop") %>%
    complete(ano = anos, fill = list(votos = 0)) %>%
    arrange(ano)
}

# --- Perfil do candidato (Bloco 2: card de perfil) --------------------------

# Parametro NAO pode se chamar sq_candidato: dentro de filter(), o data-mask
# do dplyr da prioridade a coluna do dado sobre a variavel local de mesmo
# nome, entao `filter(sq_candidato == as.numeric(sq_candidato))` vira uma
# tautologia (compara a coluna com ela mesma) e devolve a tabela inteira sem
# filtrar nada -- slice(1) sempre pegava a 1a linha da tabela toda, nao o
# candidato pedido. Confirmado visualmente: o perfil de qualquer candidato
# mostrava sempre os dados do 1o candidato da base (Alexandra Morais, 2018).
perfil_candidato <- function(perfil_candidatos, sq_sel) {
  perfil_candidatos %>% filter(sq_candidato == as.numeric(sq_sel)) %>% slice(1)
}

# Classifica uma URL de rede social declarada por plataforma, pro pill de
# exibicao no card de perfil (mesmo espirito do Mapeador de liderancas).
classificar_rede_social <- function(url) {
  u <- tolower(url)
  dplyr::case_when(
    grepl("instagram\\.com", u) ~ "Instagram",
    grepl("facebook\\.com", u) ~ "Facebook",
    grepl("tiktok\\.com", u) ~ "TikTok",
    grepl("youtube\\.com|youtu\\.be", u) ~ "YouTube",
    grepl("twitter\\.com|(^|/)x\\.com", u) ~ "X (Twitter)",
    grepl("linkedin\\.com", u) ~ "LinkedIn",
    grepl("threads\\.net", u) ~ "Threads",
    TRUE ~ "Site/Outro"
  )
}

# --- Ficha do municipio (clique no mapa) ------------------------------------
# Sempre no nivel do PARTIDO (nao do candidato local de cada secao) -- a
# ficha resume "o que o NOVO fez aqui" nas corridas de CARGOS, no ano selecionado.

ficha_municipio <- function(votos_partido_municipio, total_validos_municipio, chave_municipio_sel, ano) {
  linhas <- lapply(CARGOS, function(c) {
    denom <- total_validos_municipio %>%
      filter(ano == !!ano, cargo == c$cargo, chave_municipio == chave_municipio_sel) %>%
      pull(total_validos)
    denom <- if (length(denom) == 0) 0 else denom

    linha_partido <- votos_partido_municipio %>%
      filter(ano == !!ano, cargo == c$cargo, sg_partido == PARTIDO_ALVO, chave_municipio == chave_municipio_sel)

    votos_nom <- if (nrow(linha_partido) == 0) 0 else linha_partido$votos_nominais[1]
    votos_leg <- if (!cargo_e_proporcional(c$cargo)) NA_real_ else if (nrow(linha_partido) == 0) 0 else linha_partido$votos_legenda[1]
    pct <- if (denom > 0) 100 * (votos_nom + (if (is.na(votos_leg)) 0 else votos_leg)) / denom else 0

    list(titulo = c$titulo, votos_nominais = votos_nom, votos_legenda = votos_leg, pct_validos = pct)
  })
  linhas
}

# --- Indice de representacao demografica (radar "Perfil do eleitorado") ----
# Pergunta: o NOVO performa acima ou abaixo da media entre eleitores de cada
# categoria demografica? Metodo escolhido com o usuario (nao correlacao): pra
# cada categoria c de um eixo (genero/faixa etaria/escolaridade),
#   numerador   = fracao_c media por SECAO, ponderada pelo voto do NOVO ali
#                 (secoes com mais voto do NOVO pesam mais)
#   denominador = fracao geral do ELEITORADO que e da categoria c, no MESMO
#                 escopo geografico (estado inteiro nas gerais, o municipio
#                 selecionado no municipal) -- nao so nas secoes onde o NOVO
#                 teve voto, senao o "geral" fica enviesado pra onde o NOVO
#                 ja tem presenca, o que descaracterizaria o indice.
#   indice = numerador / denominador (1.0 = mesma proporcao do eleitorado
#            geral; >1 sobre-representado entre o voto do NOVO; <1 sub-representado)
#
# Leitura ECOLOGICA, nao individual: estimado a partir da composicao
# demografica de cada secao (nao existe voto nominal ligado a perfil
# individual — sigilo de voto), mesma limitacao inerente de qualquer
# cruzamento TSE nesse nivel de granularidade.
#
# Raca/cor fica de fora (autodeclarada so desde nov/2022, cobertura medida ao
# vivo pra PE: 2018/2020/2022 ~0% informado, 2024 so 9,6% -- dado insuficiente
# em qualquer ano do dashboard). Faixa etaria (~24 categorias da TSE) e
# agrupada em 5 faixas mais largas -- 24 eixos deixaria o radar ilegivel.
GRUPOS_FAIXA_ETARIA <- c("16 a 24 anos", "25 a 34 anos", "35 a 44 anos", "45 a 59 anos", "60 anos ou mais")

agrupar_faixa_etaria <- function(faixa) {
  idade_min <- suppressWarnings(as.numeric(gsub("^([0-9]+).*", "\\1", faixa)))
  dplyr::case_when(
    is.na(idade_min) ~ NA_character_,
    idade_min < 25 ~ "16 a 24 anos",
    idade_min < 35 ~ "25 a 34 anos",
    idade_min < 45 ~ "35 a 44 anos",
    idade_min < 60 ~ "45 a 59 anos",
    TRUE ~ "60 anos ou mais"
  )
}

# Lado do ELEITORADO do indice de representacao (composicao por secao +
# geral) -- NAO depende de candidato, so de ano/eixo/municipio. Calculo caro
# (~5-15s por chamada, medido) porque escaneia perfil_eleitorado_secao.rds
# (13M+ linhas): por isso NUNCA roda em tempo de execucao do app Shiny --
# programs/17_composicao_eleitorado.R chama esta funcao 1x por combinacao de
# ano/eixo/escopo (poucas dezenas) e grava o resultado em
# data/processed/{composicao,denom}_eleitorado_secao.rds; o app le so essas
# tabelas ja prontas via composicao_eleitorado_secao() (definida logo abaixo
# desta funcao). programs/13_export_dist_data.R (export do site estatico)
# tambem chama esta funcao direto, pelo mesmo motivo (nunca sobre o
# navegador do usuario final).
#
# `chave_municipio_sel`: NULL nas eleicoes gerais (escopo = PE inteiro); a
# chave do municipio selecionado no municipal (escopo = so aquele municipio).
calcular_composicao_eleitorado_secao <- function(perfil_eleitorado_secao, ano, eixo = c("genero", "faixa_etaria", "escolaridade"),
                                                   chave_municipio_sel = NULL) {
  eixo <- match.arg(eixo)

  perfil_ano <- perfil_eleitorado_secao %>% filter(ano == !!ano)
  if (!is.null(chave_municipio_sel)) perfil_ano <- perfil_ano %>% filter(chave_municipio == chave_municipio_sel)
  if (nrow(perfil_ano) == 0) {
    vazio <- tibble(categoria = character(0))
    return(list(composicao_secao = vazio %>% mutate(chave_secao = character(0), eleitores = numeric(0), fracao = numeric(0)),
                denom = vazio %>% mutate(fracao_geral = numeric(0))))
  }

  perfil_ano <- if (eixo == "faixa_etaria") {
    # agrupar_faixa_etaria() usa gsub/regex -- caro se aplicado linha a linha
    # num df de milhoes de linhas (~15s medido). Calculado so sobre os poucos
    # valores DISTINTOS de faixa_etaria (~20-25) e devolvido via join, nao
    # mutate() direto na tabela inteira.
    mapa_faixa <- perfil_ano %>% distinct(faixa_etaria) %>% mutate(categoria = agrupar_faixa_etaria(faixa_etaria))
    perfil_ano %>% left_join(mapa_faixa, by = "faixa_etaria") %>% filter(!is.na(categoria))
  } else if (eixo == "genero") {
    perfil_ano %>% rename(categoria = genero) %>% filter(categoria %in% c("FEMININO", "MASCULINO"))
  } else {
    perfil_ano %>% rename(categoria = escolaridade) %>% filter(categoria != "NÃO INFORMADO")
  }

  composicao_secao <- perfil_ano %>%
    group_by(chave_secao, categoria) %>%
    summarise(eleitores = sum(eleitores), .groups = "drop") %>%
    group_by(chave_secao) %>%
    mutate(fracao = eleitores / sum(eleitores)) %>%
    ungroup()

  denom <- composicao_secao %>%
    group_by(categoria) %>%
    summarise(eleitores = sum(eleitores), .groups = "drop") %>%
    mutate(fracao_geral = eleitores / sum(eleitores)) %>%
    select(categoria, fracao_geral)

  list(composicao_secao = composicao_secao, denom = denom)
}

# Escopo usado em data/processed/{composicao,denom}_eleitorado_secao.rds pras
# eleicoes GERAIS (PE inteiro, chave_municipio_sel = NULL em
# calcular_composicao_eleitorado_secao()) -- string, nao NA, pra filtrar com
# == normal em vez de precisar de tratamento especial de NA no filter() de
# composicao_eleitorado_secao() abaixo. Nunca colide com uma chave_municipio
# de verdade (essas sao nomes de municipio normalizados, nunca "GERAL").
ESCOPO_GERAL <- "GERAL"

# Versao "leitura" de calcular_composicao_eleitorado_secao() acima: usada
# pelo app Shiny em runtime, so filtra as tabelas JA pre-calculadas por
# programs/17_composicao_eleitorado.R (carregar_composicao_eleitorado_pre()) --
# nenhum group_by sobre a tabela de 13M linhas acontece aqui, por isso e
# rapida o bastante pra rodar dentro de um reactive() sem travar a UI.
# `composicao_pre`: saida de carregar_composicao_eleitorado_pre() (list com
# $composicao e $denom). Mesmo contrato de retorno da versao "calcular".
composicao_eleitorado_secao <- function(composicao_pre, ano, eixo = c("genero", "faixa_etaria", "escolaridade"),
                                          chave_municipio_sel = NULL) {
  eixo <- match.arg(eixo)
  escopo_sel <- if (is.null(chave_municipio_sel)) ESCOPO_GERAL else chave_municipio_sel

  list(
    composicao_secao = composicao_pre$composicao %>%
      filter(ano == !!ano, eixo == !!eixo, escopo == !!escopo_sel) %>%
      select(chave_secao, categoria, eleitores, fracao),
    denom = composicao_pre$denom %>%
      filter(ano == !!ano, eixo == !!eixo, escopo == !!escopo_sel) %>%
      select(categoria, fracao_geral)
  )
}

# Lado do VOTO: pesa a composicao do eleitorado (`composicao`, saida de
# composicao_eleitorado_secao() acima) pelo voto do partido ("TODOS") ou de
# um candidato especifico na mesma secao/escopo. `chave_municipio_sel` deve
# bater com o que foi passado pra composicao_eleitorado_secao() -- os dois
# lados do indice precisam do MESMO escopo geografico, senao a comparacao
# nao faz sentido.
indice_representacao <- function(votos_candidato_secao, votos_partido_secao, composicao,
                                   cargo, ano, candidato_sel, chave_municipio_sel = NULL) {
  votos_secao <- if (candidato_sel == "TODOS") {
    votos_partido_secao %>% filter(ano == !!ano, cargo == !!cargo) %>%
      select(chave_municipio, chave_secao, votos = votos_validos_partido)
  } else {
    votos_candidato_secao %>% filter(ano == !!ano, cargo == !!cargo, sq_candidato == as.numeric(candidato_sel)) %>%
      select(chave_municipio, chave_secao, votos = votos_nominais)
  }
  if (!is.null(chave_municipio_sel)) votos_secao <- votos_secao %>% filter(chave_municipio == chave_municipio_sel)
  votos_secao <- votos_secao %>% filter(votos > 0) %>% select(chave_secao, votos)

  if (nrow(votos_secao) == 0 || nrow(composicao$composicao_secao) == 0) {
    return(tibble(categoria = character(0), indice = numeric(0), fracao_ponderada = numeric(0)))
  }

  numer <- composicao$composicao_secao %>%
    inner_join(votos_secao, by = "chave_secao") %>%
    group_by(categoria) %>%
    summarise(fracao_ponderada = sum(fracao * votos) / sum(votos), .groups = "drop")

  numer %>%
    inner_join(composicao$denom, by = "categoria") %>%
    mutate(indice = fracao_ponderada / fracao_geral) %>%
    select(categoria, indice, fracao_ponderada)
}
