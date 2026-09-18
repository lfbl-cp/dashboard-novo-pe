# Mapa de PE por municipio (leaflet) -- coropletico (cor continua, % de votos
# validos do NOVO) ou LISA (cluster espacial via spdep). Mesmo padrao da
# referencia "Dashboard economia PE": legenda HTML autoral via addControl (nao
# addLegend(), que fica alta demais e tampa municipio de verdade), fitBounds
# sobre o bbox real da malha, variante clara/escura do basemap+paleta.
#
# Diferenca proposital vs. a referencia: aqui todo municipio SEMPRE tem um
# valor (0 quando o NOVO nao teve voto ali), entao nao existe o conceito de
# "sem dado" nem de "fora do filtro regional esmaecido" -- nao ha filtro
# geografico neste dashboard, o mapa inteiro reflete o cargo/ano/candidato
# selecionados na secao.

library(leaflet)
library(sf)
library(spdep)

# Mesma polaridade nos dois modos (claro = baixo, escuro = alto), cores
# diferentes por modo -- no escuro o tom "alto" precisa ser mais saturado pra
# nao sumir contra o basemap escuro (mesma logica da referencia, cor do NOVO
# em vez de azul).
ESQUEMA_MAPA <- list(
  light = list(
    tile        = "CartoDB.Positron",
    cor_borda   = "#FFFFFF",
    cor_destaque = "#7A2E00",
    paleta_valor = c("#FFF3E0", "#FFCC80", "#FF9800", "#E65100", "#7A2E00")
  ),
  dark = list(
    tile        = "CartoDB.DarkMatter",
    cor_borda   = "#1A1C21",
    cor_destaque = "#FFB74D",
    paleta_valor = c("#4A3420", "#C9691E", "#FF9800", "#FFB74D", "#FFE0B2")
  )
)

CORES_LISA <- c(
  "Alto-Alto"        = "#D7191C",
  "Baixo-Baixo"      = "#2C7BB6",
  "Alto-Baixo"       = "#FDAE61",
  "Baixo-Alto"       = "#ABD9E9",
  "Nao significante" = "#BDBDBD"
)
LISA_NIVEIS <- names(CORES_LISA)

formatar_pct <- function(x) paste0(format(round(x, 1), decimal.mark = ",", trim = TRUE), "%")
formatar_votos <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)

# --- LISA --------------------------------------------------------------------
# Classificacao de cluster (Moran local) padrao GeoDa: quadrante pelo sinal da
# variavel centralizada e de sua defasagem espacial (media dos vizinhos),
# significante quando p < alpha; senao "Nao significante".
calcular_lisa <- function(sf_obj, variavel, alpha = 0.05) {
  stopifnot(nrow(sf_obj) == length(variavel))
  variavel[is.na(variavel)] <- 0

  vizinhos <- poly2nb(sf_obj, queen = TRUE)
  pesos <- nb2listw(vizinhos, style = "W", zero.policy = TRUE)

  # variavel constante (secao sem nenhum voto do NOVO em lugar nenhum) faz
  # scale() dividir por sd = 0 e devolver NaN -- sem variancia nao ha cluster
  # pra detectar, entao a resposta correta e "tudo zero" (nao significante).
  desvio <- stats::sd(variavel)
  z <- if (is.na(desvio) || desvio == 0) rep(0, length(variavel)) else as.numeric(scale(variavel))
  z_lag <- lag.listw(pesos, z, zero.policy = TRUE)

  moran_local <- localmoran(variavel, pesos, zero.policy = TRUE)
  col_p <- grep("^Pr\\(", colnames(moran_local))[1]
  p_valor <- moran_local[, col_p]
  p_valor[is.na(p_valor)] <- 1

  quadrante <- dplyr::case_when(
    z >= 0 & z_lag >= 0 ~ "Alto-Alto",
    z < 0 & z_lag < 0 ~ "Baixo-Baixo",
    z >= 0 & z_lag < 0 ~ "Alto-Baixo",
    z < 0 & z_lag >= 0 ~ "Baixo-Alto"
  )
  categoria <- ifelse(p_valor < alpha, quadrante, "Nao significante")
  factor(categoria, levels = LISA_NIVEIS)
}

# --- legendas HTML autorais (addControl, nao addLegend) ---------------------

legenda_coropletico_html <- function(dominio, esquema) {
  gradiente <- paste(esquema$paleta_valor, collapse = ", ")
  htmltools::HTML(sprintf(
    '<div class="mapa-legenda">
       <div class="mapa-legenda-titulo">%% dos votos válidos</div>
       <div class="mapa-legenda-barra" style="background: linear-gradient(to right, %s);"></div>
       <div class="mapa-legenda-rotulos"><span>%s</span><span>%s</span></div>
     </div>',
    gradiente, formatar_pct(min(dominio, 0)), formatar_pct(max(dominio, 0.01))
  ))
}

legenda_lisa_html <- function() {
  linhas <- paste(sprintf(
    '<div class="mapa-legenda-item"><span class="mapa-legenda-swatch" style="background:%s"></span>%s</div>',
    unname(CORES_LISA), names(CORES_LISA)
  ), collapse = "")
  htmltools::HTML(sprintf('<div class="mapa-legenda"><div class="mapa-legenda-titulo">Cluster LISA</div>%s</div>', linhas))
}

# --- mapa ---------------------------------------------------------------
# `municipios`: sempre os 185 (nunca abre buraco no mapa). `valores`: saida de
# dados.R::valores_mapa() (1 linha por municipio, com chave_municipio,
# municipio, votos, pct_validos). `tipo`: "coropletico" ou "lisa". `modo`:
# "light"/"dark".
montar_mapa <- function(municipios, valores, tipo = "coropletico", modo = "light") {
  esquema <- ESQUEMA_MAPA[[if (modo == "dark") "dark" else "light"]]

  d <- municipios %>% select(chave_municipio) %>% left_join(valores, by = "chave_municipio")

  if (tipo == "lisa") {
    d$lisa <- calcular_lisa(d, d$pct_validos)
    # unname() e essencial: vetor de cor NOMEADO quebra a serializacao do
    # widget leaflet (pinta tudo de preto, sem erro nenhum no console).
    cores <- unname(CORES_LISA[as.character(d$lisa)])
    rotulo <- sprintf("<b>%s</b><br/>%% dos válidos: %s<br/>Cluster: %s",
                       d$municipio, formatar_pct(d$pct_validos), d$lisa)
    controle_legenda <- legenda_lisa_html()
  } else {
    pal <- colorNumeric(esquema$paleta_valor, domain = c(0, max(d$pct_validos, 1)))
    cores <- pal(d$pct_validos)
    rotulo <- sprintf("<b>%s</b><br/>Votos: %s<br/>%% dos válidos: %s",
                       d$municipio, formatar_votos(d$votos), formatar_pct(d$pct_validos))
    controle_legenda <- legenda_coropletico_html(d$pct_validos, esquema)
  }

  # Fernando de Noronha fica ~1000km a leste do continente -- se entrar no
  # bbox do fitBounds(), forca o zoom a encolher o estado inteiro so pra
  # caber o arquipelago. Continua no mapa (colorido, clicavel), so nao dita
  # o enquadramento inicial.
  bbox <- st_bbox(municipios[municipios$name_muni != "Fernando de Noronha", ])

  leaflet(d, options = leafletOptions(minZoom = 6, maxZoom = 11)) %>%
    addProviderTiles(esquema$tile) %>%
    addPolygons(
      fillColor = cores, fillOpacity = 0.85, color = esquema$cor_borda, weight = 0.6,
      layerId = ~chave_municipio, label = lapply(rotulo, htmltools::HTML),
      highlightOptions = highlightOptions(weight = 2.2, color = esquema$cor_destaque, bringToFront = TRUE)
    ) %>%
    addControl(html = controle_legenda, position = "bottomright", className = "leaflet-control mapa-legenda-ctrl") %>%
    fitBounds(
      lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]],
      options = list(paddingBottomRight = c(10, 90), paddingTopLeft = c(10, 10))
    )
}

# --- mapa por BAIRRO (aba municipal) -----------------------------------------
# Funcao PARALELA a montar_mapa(), nao uma versao parametrizada dela: a chave
# composta de bairro (chave_bairro, unico por bairro+municipio) e o rotulo
# (nome do bairro, nao do municipio) mudam o suficiente pra justificar uma
# funcao propria em vez de sobrecarregar montar_mapa() com mais um parametro
# de "nivel". calcular_lisa() e as legendas HTML sao reaproveitadas sem
# mudanca nenhuma (ja sao genericas o bastante).
#
# `bairros_municipio`: geometria de UM municipio so (poligonos de bairro,
# ja filtrados -- o mapa de bairro nao faz sentido pro estado inteiro).
# `valores`: saida de dados.R::valores_mapa_bairro() (chave_bairro, bairro,
# votos, pct_validos). Zoom min/max mais fechado que montar_mapa() (bairro
# e uma escala bem menor que municipio).
montar_mapa_bairro <- function(bairros_municipio, valores, tipo = "coropletico", modo = "light") {
  esquema <- ESQUEMA_MAPA[[if (modo == "dark") "dark" else "light"]]

  d <- bairros_municipio %>% select(chave_bairro) %>% left_join(valores, by = "chave_bairro")

  if (tipo == "lisa") {
    d$lisa <- calcular_lisa(d, d$pct_validos)
    cores <- unname(CORES_LISA[as.character(d$lisa)])
    rotulo <- sprintf("<b>%s</b><br/>%% dos válidos: %s<br/>Cluster: %s",
                       d$bairro, formatar_pct(d$pct_validos), d$lisa)
    controle_legenda <- legenda_lisa_html()
  } else {
    pal <- colorNumeric(esquema$paleta_valor, domain = c(0, max(d$pct_validos, 1)))
    cores <- pal(d$pct_validos)
    rotulo <- sprintf("<b>%s</b><br/>Votos: %s<br/>%% dos válidos: %s",
                       d$bairro, formatar_votos(d$votos), formatar_pct(d$pct_validos))
    controle_legenda <- legenda_coropletico_html(d$pct_validos, esquema)
  }

  bbox <- st_bbox(bairros_municipio)

  ajustar_encaixe_bairro(
    leaflet(d, options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
      addProviderTiles(esquema$tile) %>%
      addPolygons(
        fillColor = cores, fillOpacity = 0.85, color = esquema$cor_borda, weight = 0.6,
        layerId = ~chave_bairro, label = lapply(rotulo, htmltools::HTML),
        highlightOptions = highlightOptions(weight = 2.2, color = esquema$cor_destaque, bringToFront = TRUE)
      ) %>%
      addControl(html = controle_legenda, position = "bottomright", className = "leaflet-control mapa-legenda-ctrl") %>%
      fitBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]],
        options = list(paddingBottomRight = c(20, 100), paddingTopLeft = c(20, 20))
      ),
    bbox
  )
}

# fitBounds() roda no lado R (chute inicial, evita "mapa sem enquadramento
# nenhum" no primeiro frame) e de novo dentro do onRender -- mas so DEPOIS do
# container estabilizar de verdade, nao apos um atraso fixo (um setTimeout com
# numero magico foi tentado e falhou: o mapa do bloco "Visao do partido" e o
# 1o widget Leaflet da pagina, e o proprio radioButtons de tipo de mapa
# (input$tipo_mapa) dispara uma 2a renderizacao logo apos o valor default
# chegar do cliente -- se o timer da 1a renderizacao disparar depois do widget
# ja ter sido substituido pela 2a, ele mede/mexe numa instancia Leaflet ja
# destacada do DOM, o que rendeu zoom no teto (maxZoom) em vez do encaixe
# certo). A funcao abaixo em vez disso: (1) verifica a cada 60ms se o
# container ainda esta no DOM -- se nao estiver (foi substituido por uma
# renderizacao mais nova), desiste sem mexer em nada; (2) so aplica o
# fitBounds quando 2 leituras consecutivas do tamanho do container batem
# (sinal de que o layout da grid CSS ja assentou), garantindo o mesmo
# criterio de "pronto" pros dois mapas (partido e candidato) independente de
# quando cada um nasceu. Margem ao redor do municipio: pixels de padding no
# fitBounds, nao um "zoom - 1" fixo -- o zoom do Leaflet e sempre um numero
# inteiro (o widget arredonda pro nivel que ainda cabe o bbox inteiro), entao
# subtrair 1 na marra e instavel: em containers onde o encaixe "justo" ja
# arredondava pra baixo, o -1 mal fazia diferenca; em outros, tirava um nivel
# inteiro de detalhe e encolhia o municipio a quase nada no mapa (ja visto nos
# dois sentidos ao testar). E um padding em PIXELS FIXOS tem o mesmo problema
# por outro caminho: 40px e pouco demais numa janela estreita e de menos numa
# larga, entao o mesmo numero fixo cabe "justo" numa resolucao e "folgado"
# demais em outra (foi o que aconteceu ao testar em 2 larguras diferentes). A
# correcao e calcular o padding como PORCENTAGEM do tamanho medido do
# container (dentro do JS, onde ja se sabe larg/alt de verdade) -- af o
# respiro visual fica proporcional e consistente em qualquer resolucao de
# tela. Compartilhada entre montar_mapa_bairro() e montar_mapa_calor_bairro()
# (mesma necessidade de enquadramento: mapa de 1 municipio, container pode
# nao estar pronto no primeiro frame).
ajustar_encaixe_bairro <- function(mapa, bbox) {
  htmlwidgets::onRender(
    mapa,
    sprintf(
      "function(el, x) {
         var map = this;
         var largAnterior = -1, altAnterior = -1, tentativas = 0;
         function tentarEncaixar() {
           tentativas++;
           if (!document.body.contains(el)) return;
           var larg = el.clientWidth, alt = el.clientHeight;
           if (larg > 0 && alt > 0 && larg === largAnterior && alt === altAnterior) {
             map.invalidateSize();
             // Padding proporcional (3,5%% da largura/altura reais do
             // container), nao pixels fixos -- ver comentario acima da
             // funcao. Lado inferior-direito ganha +70px fixos por cima pra
             // sempre limpar a legenda (%% dos votos validos), que tem
             // altura fixa em CSS independente do tamanho do mapa.
             var padX = larg * 0.015, padY = alt * 0.015;
             map.fitBounds([[%f, %f], [%f, %f]], {
               paddingTopLeft: [padX, padY],
               paddingBottomRight: [padX, padY + 70]
             });
             return;
           }
           largAnterior = larg; altAnterior = alt;
           if (tentativas < 30) setTimeout(tentarEncaixar, 60);
         }
         setTimeout(tentarEncaixar, 60);
       }",
      bbox[["ymin"]], bbox[["xmin"]], bbox[["ymax"]], bbox[["xmax"]]
    )
  )
}

# --- mapa de DENSIDADE por local de votacao (aba municipal) -----------------
# 3a opcao de mapa (junto de Coropletico/LISA), so no bloco municipal --
# trabalha com PONTO (local de votacao geocodificado), nao poligono, entao nao
# reusa a logica de cor de montar_mapa_bairro(). `pontos`: saida de
# dados.R::pontos_calor_bairro() (lat, lon, peso = votos do NOVO naquele
# local) -- so locais com peso > 0 (local sem voto do recorte ja vem excluido).
# Contorno dos bairros fica visivel (sem preenchimento) por baixo do heatmap,
# como referencia geografica. leaflet.extras (addHeatmap) e usado so com
# prefixo, nunca via library() -- mesma cautela ja adotada com plotly (ver
# R/secao.R) por causa de colisao de nome com dplyr; addHeatmap() nao colide
# com nada usado aqui, mas o padrao do projeto e nao arriscar.
montar_mapa_calor_bairro <- function(bairros_municipio, pontos, modo = "light") {
  esquema <- ESQUEMA_MAPA[[if (modo == "dark") "dark" else "light"]]
  bbox <- st_bbox(bairros_municipio)

  mapa <- leaflet(options = leafletOptions(minZoom = 9, maxZoom = 18)) %>%
    addProviderTiles(esquema$tile) %>%
    addPolygons(
      data = bairros_municipio, fillOpacity = 0, color = esquema$cor_borda, weight = 0.6
    )

  if (nrow(pontos) > 0) {
    # Mesma paleta laranja (por modo claro/escuro) ja usada no coropletico
    # (ESQUEMA_MAPA$paleta_valor), pra manter a identidade visual do mapa --
    # sem isso, addHeatmap() cai no gradiente padrao azul/verde/amarelo/
    # vermelho do leaflet.heat. `gradient` aceita um vetor de cores (repassado
    # pra colorNumeric() por dentro), nao uma lista nomeada por stop 0-1 (essa
    # e a API do leaflet.heat em JS puro, nao a do wrapper leaflet.extras::
    # addHeatmap() -- passar lista nomeada quebra com erro de toPaletteFunc).
    # O 1o tom da paleta (o mais claro, quase creme) fica praticamente invisivel
    # sobre o basemap claro/CartoDB.Positron -- removido do gradiente ([-1]),
    # entao mesmo intensidade baixa ja entra num laranja perceptivel. minOpacity
    # tambem sobe do default (0.05) pra 0.35 pelo mesmo motivo: numa visao de
    # cidade inteira (zoom afastado), cada local de votacao fica isolado dos
    # vizinhos em espaco de tela, entao o kernel raramente acumula intensidade
    # alta o bastante pra "subir" no gradiente sozinho -- sem esse piso, o
    # heatmap inteiro ficava proximo de transparente.
    mapa <- mapa %>%
      leaflet.extras::addHeatmap(
        data = pontos, lng = ~lon, lat = ~lat, intensity = ~peso,
        radius = 20, blur = 24, max = max(pontos$peso),
        gradient = esquema$paleta_valor[-1], minOpacity = 0.35
      )
  }

  controle_legenda <- htmltools::HTML(
    '<div class="mapa-legenda"><div class="mapa-legenda-titulo">Densidade de votos por local de votação</div></div>'
  )

  ajustar_encaixe_bairro(
    mapa %>%
      addControl(html = controle_legenda, position = "bottomright", className = "leaflet-control mapa-legenda-ctrl") %>%
      fitBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]], lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]],
        options = list(paddingBottomRight = c(20, 100), paddingTopLeft = c(20, 20))
      ),
    bbox
  )
}
