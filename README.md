# Dashboard NOVO em Pernambuco

Dashboard Shiny com o desempenho eleitoral do partido NOVO em Pernambuco (eleições
gerais de 2018 e 2022). Segue o padrão de projeto do Gauss usado em
"Dashboard economia PE": `app.R` único + `R/` (código de runtime do app) +
`programs/` (ETL numerado, roda fora do app) + `data/raw` e `data/processed` +
`www/custom.css` com tema claro/escuro via custom properties.

A barra de filtro sticky tem o alternador **Eleições gerais / Eleições
municipais** (nível mais alto do app). O filtro de **Ano** das eleições gerais
vive dentro de cada seção, como o primeiro dos botões-atalho ("Ano" /
"Partido" / "Candidatos") — é um `<select>` de verdade estilizado pra parecer
a mesma pill dos outros 2 (mostra o ano selecionado, ex. "2022", em vez de um
rótulo fixo).

Dentro de "Eleições gerais", uma seção por cargo em que o NOVO lançou candidato
em PE — hoje só **Deputado Federal** (nunca teve candidato a Estadual nem
Senador). Cada seção tem 2 blocos:

- **Visão do partido** (sempre agregada, não depende de candidato): KPIs, mapa
  (coroplético % dos válidos ou LISA) + ranking de municípios por votos absolutos
  lado a lado (interativo — passar o mouse numa barra mostra o município e a
  contagem de votos), e cards com o total de votos em cada eleição.
- **Visão por candidato** (filtro local de Candidato): mapa + ranking escopados
  àquele candidato, e um card de perfil (foto oficial, idade, gênero, raça/cor,
  escolaridade, ocupação, redes sociais declaradas) vindo da API pública do TSE
  DivulgaCandContas — ver `programs/06_perfil_candidatos.R`.

Clicar num município (em qualquer mapa) abre uma ficha com o NOVO nas corridas
daquele cargo, no ano selecionado.

Dentro de "Eleições municipais" (Prefeito/Vereador, 2020 e 2024), a mesma
estrutura de 2 blocos se repete, mas em nível de **bairro** em vez de
município — um candidato a vereador disputa dentro de UMA cidade, então o
mapa por município não mostraria nada útil. Como o mapa de bairro só faz
sentido dentro de um município por vez, tem dois filtros GLOBAIS extras acima
das seções (Ano e Município), compartilhados pelas seções de Vereador e
Prefeito. Só os municípios onde o NOVO teve candidato **E** que têm geometria
de bairro no IBGE aparecem no seletor — hoje: Afogados da Ingazeira, Caruaru,
Garanhuns, Jaboatão dos Guararapes, Olinda, Paulista, Recife, Surubim (Carpina
e Gravatá têm candidato mas não têm bairro no IBGE, ficam de fora da aba —
decisão deliberada, não um bug; ver Notas abaixo).

## Como rodar

> `data/processed/`, `data/intermediate/` e `dist/` **não** vêm no git (ver
> `.gitignore`) -- são build artifacts, sempre regenerados a partir de
> `data/raw/` (que por sua vez é baixado do zero se não existir). Isso
> significa que **clonar o repositório sozinho não é suficiente pra rodar o
> app** -- o passo 1 abaixo é obrigatório antes de `shiny::runApp(".")`, não
> só "pra atualizar dados".

1. **Preparar os dados** (baixa do TSE via `electionsBR` + geometria via `geobr`,
   demora alguns minutos e precisa de internet; só precisa rodar de novo se quiser
   atualizar os dados ou incluir a eleição de 2026 quando ela sair). Rodar em ordem:

   ```r
   # a partir da pasta do projeto

   # Eleições gerais
   source("programs/01_municipios_pe.R")
   source("programs/02_coleta_candidatos.R")
   source("programs/03_coleta_votos_candidato_municipio.R")
   source("programs/04_coleta_votos_partido_municipio.R")
   source("programs/05_consolidar.R")
   source("programs/06_perfil_candidatos.R")

   # Eleições municipais (por bairro)
   source("programs/07_coleta_candidatos_municipais.R")
   source("programs/08_coleta_votos_secao_municipal.R")
   source("programs/09_bairros_municipios_alvo.R")
   source("programs/10_locais_votacao_geocodificados.R")
   source("programs/11_consolidar_municipal.R")
   source("programs/12_perfil_candidatos_municipais.R")

   # Perfil do eleitorado por seção (radar "Perfil do eleitorado", gerais + municipais)
   source("programs/14_coleta_votos_secao_geral.R")
   source("programs/15_coleta_perfil_eleitorado.R")
   source("programs/16_consolidar_secao.R")
   source("programs/17_composicao_eleitorado.R")
   ```

   O passo 17 pré-calcula a composição do eleitorado (gênero/faixa etária/
   escolaridade) por ano/escopo — é o que faz o app Shiny abrir rápido; sem
   ele, `R/dados.R::composicao_eleitorado_secao()` teria que escanear
   `perfil_eleitorado_secao.rds` (13M+ linhas) toda vez que uma seção abre,
   travando a 1ª renderização por ~20-40s sem nenhum indicador de
   carregamento (ver Notas). Só precisa rodar de novo se o passo 15 rodar de
   novo ou se a lista de municípios/anos do dashboard mudar.

   Os downloads brutos ficam em cache em `data/raw/` (os scripts pulam
   candidatos/anos/locais já baixados). Os `.rds` finais usados pelo app vão para
   `data/processed/`; as fotos oficiais dos candidatos vão para `www/fotos/`
   (servidas pelo Shiny, entram no deploy normalmente).

   Os passos 06/12 buscam perfil + foto de cada candidato na API pública do TSE
   DivulgaCandContas (`divulgacandcontas.tse.jus.br`, sem autenticação — mesma
   base que alimenta o site oficial de transparência de candidaturas). Só
   precisam rodar de novo se `candidatos_novo.rds`/`candidatos_novo_municipais.rds`
   mudar (nova eleição) ou se quiser atualizar fotos/perfis já em cache. Essa
   API exige um `User-Agent` de navegador (sem ele, tanto o JSON quanto a foto
   voltam 403 Forbidden — ver `programs/00_utils.R::buscar_json_tse()` /
   `baixar_arquivo_tse()`) e, para eleições municipais, o segmento de UF na URL
   precisa ser o **código TSE do município** (`SG_UE` do candidato), não "PE"
   — com "PE" a API responde 200 mas com corpo vazio, sem erro nenhum.

   O passo 10 (`10_locais_votacao_geocodificados.R`) é o mais demorado e o único
   com uma etapa manual: geocodifica cada local de votação (endereço padronizado
   "{local}, {endereço}, {município}, PE, Brasil") via `tidygeocoder`
   (`method="arcgis"` — testado ao vivo, muito mais confiável que `method="osm"`/
   Nominatim, que dá 503 Service Unavailable persistente em lote grande) e
   confere se o ponto cai dentro de algum bairro do próprio município
   (`sf::st_within`). Local rural genuíno (sítio/povoado/distrito) que caia fora
   de qualquer bairro fica marcado `fora_do_bairro = TRUE` e é excluído da
   agregação em `11_consolidar_municipal.R` (materialidade medida ao vivo: 0,14%
   dos votos do NOVO no escopo — ver Notas). Erro de geocodificação de verdade
   (ponto capturado no município errado) é corrigido manualmente em
   `data/raw/correcoes_geocodificacao.csv` (chave_local, lat, lon, motivo) —
   esse arquivo é reaplicado a cada execução do script.

2. **Rodar o app**:

   ```r
   shiny::runApp(".")
   ```

## Estrutura

- `app.R` — UI + server num arquivo só.
- `R/dados.R` — carrega os `.rds` processados; filtro candidato/cargo/ano, valores
  do mapa/ranking, KPIs, votos por eleição, perfil de candidato, dados da ficha
  do município.
- `R/mapa.R` — paleta claro/escuro do coroplético, cálculo de LISA (`spdep`),
  montagem do leaflet (legenda HTML própria, `fitBounds` excluindo Fernando de
  Noronha do enquadramento).
- `R/ficha.R` — modal ao clicar num município.
- `R/perfil.R` — card de perfil do candidato (Visão por candidato): avatar/foto,
  badge de situação, grid de demografia, pills de redes sociais.
- `R/secao.R` — módulo Shiny reutilizável (uma instância por cargo; Bloco 1
  partido + Bloco 2 por candidato). Tem a variante geral (`secao_ui()`/
  `secao_server()`) e a municipal (`secao_ui_municipal()`/
  `secao_server_municipal()` + `atalhos_municipal_ui()`, o filtro global de
  Ano+Município da aba municipal).
- `programs/00_utils.R` a `12_perfil_candidatos_municipais.R` — pipeline de
  dados numerado, nunca fonte-ado pelo app (mesma separação da referência).
  `00-06` são as eleições gerais; `07-12` são as municipais (por bairro).
- `data/raw/` — cache dos downloads brutos da TSE/geobr (inclui `data/raw/perfil/`,
  cache por candidato da API DivulgaCandContas, e os caches intermediários da
  geocodificação municipal: `polling_places_*.rds`, `geocod_fallback.rds`,
  `correcoes_geocodificacao.csv`).
- `data/processed/` — dados já tratados que o app lê. Fora do git (build
  artifact, ver `.gitignore` e o aviso em "Como rodar" acima). `bairros_pe.rds`
  (geometria de bairro dos municípios do escopo municipal) e
  `locais_votacao_municipal.rds` (local de votação → coordenada → bairro) são
  específicos da aba municipal, junto com as versões `_bairro`/`_municipais`
  de `candidatos_novo`/`votos_candidato`/`votos_partido`/`total_validos`/
  `perfil_candidatos`; `composicao_eleitorado_secao.rds`/
  `denom_eleitorado_secao.rds` são a composição do eleitorado já pré-calculada
  por `programs/17_composicao_eleitorado.R` (ver Notas).
- `data/intermediate/` — dado tratado que só o pipeline de ETL usa, nunca o
  app em runtime: hoje só `perfil_eleitorado_secao.rds` (13M+ linhas, ~67MB),
  gerado por `15_coleta_perfil_eleitorado.R` e consumido por
  `16_consolidar_secao.R`/`17_composicao_eleitorado.R`/
  `13_export_dist_data.R`. Fora do git (`.gitignore`) e do deploy
  (`.rscignore`) pelo tamanho — reproduzível rodando o pipeline de novo.
- `www/fotos/` — fotos oficiais dos candidatos baixadas por `06_perfil_candidatos.R`
  e `12_perfil_candidatos_municipais.R`.
- `www/custom.css` — casca visual, tokens de tema claro/escuro, accent laranja.
- `.rscignore` — exclui `programs/`, `data/raw/` e `data/intermediate/` de um
  deploy (rsconnect/shinyapps.io); `www/fotos/` **não** é excluído, vai junto
  no deploy.

## Notas

- Escopo: apenas Pernambuco. `CARGOS` em `R/dados.R` lista só os cargos com
  candidato do NOVO em PE (hoje, só Deputado Federal) — Estadual e Senador nunca
  tiveram candidato lá e por isso não viram seção; se isso mudar numa eleição
  futura, basta adicionar o cargo de volta a `CARGOS`. A seção ainda mostra um
  aviso de estado vazio caso um ANO específico não tenha candidato.
- "Votos na legenda" só existe para cargos proporcionais (Dep. Estadual/Federal);
  para Senador (majoritário) o KPI mostraria "Não aplicável".
- O mapa LISA usa contiguidade queen (`spdep::poly2nb`); Fernando de Noronha é uma
  ilha sem vizinhos e por isso sempre entra como "Não significante".
- Para atualizar quando sair o resultado de 2026: adicionar `2026` a
  `ANOS_ELEICOES_GERAIS` em `programs/00_utils.R` e o id de eleição correspondente
  a `ID_ELEICAO_DIVULGACAND` (mesmo arquivo — o valor de 2026 já é conhecido,
  `20322002026`, obtido de `/eleicao/ordinarias` da API), e rodar o pipeline de
  novo (o filtro global de Ano no app já se adapta sozinho aos anos presentes
  nos dados).
- Os rankings (partido e candidato) são gráficos `plotly` (tooltip com município +
  votos ao passar o mouse), mas o pacote **não é anexado via `library(plotly)`**
  em nenhum arquivo — só usado com prefixo (`plotly::ggplotly()` etc., em
  `R/secao.R`). Motivo: `plotly::filter()` mascara `stats::filter`, e como
  `dplyr` é carregado antes na `search path` do app, um `library(plotly)`
  anexado ficaria à frente de `dplyr` — todo `filter()` sem prefixo em
  `R/dados.R` passaria a resolver para `stats::filter` via plotly, quebrando o
  filtro de dados do app inteiro sem erro visível.
- A API DivulgaCandContas não é documentada oficialmente pelo TSE (é a mesma que
  o site público usa, sem chave/autenticação, mas o formato da resposta pode
  mudar sem aviso). `06_perfil_candidatos.R`/`12_perfil_candidatos_municipais.R`
  já tratam falha por candidato sem derrubar o pipeline (perfil fica com campos
  `NA`, sem foto). O payload também traz `gastoCampanha1T`/`gastoCampanha2T`,
  mas eles vieram **idênticos para todos os candidatos do mesmo cargo/ano** nos
  testes — é o teto legal de gasto de campanha, não o valor declarado por
  candidato — por isso não é exibido no card de perfil (não diferenciaria um
  candidato do outro).
- **Cobertura de bairro é parcial** (`geobr::read_neighborhood()`, IBGE): só
  ~720 municípios no Brasil têm polígono de bairro. Município com candidato do
  NOVO mas sem essa geometria (Carpina, Gravatá) fica de fora da aba municipal
  inteira — decisão explícita do usuário, não um fallback silencioso;
  `09_bairros_municipios_alvo.R` loga a exclusão no console toda vez que roda.
- **Locais de votação em zona rural genuína** (sítio/povoado/distrito) não têm
  bairro nenhum pra cair dentro — nenhum geocodificador (testados: Nominatim,
  `geocodebr`/CNEFE do IBGE, ArcGIS) resolve isso, porque não é erro de
  geocodificação, é ausência real de malha urbana ali. Esses locais ficam
  marcados `fora_do_bairro = TRUE` em `locais_votacao_municipal.rds` e são
  excluídos da agregação por bairro em `11_consolidar_municipal.R` — medido ao
  vivo (ago/2026): 0,14% dos votos do NOVO no escopo municipal, considerado
  negligenciável.
- O `id_eleicao` da API DivulgaCandContas pra municipais
  (`ID_ELEICAO_DIVULGACAND_MUNICIPAL` em `00_utils.R`) é diferente do de
  eleições gerais — confirmado via `/eleicao/ordinarias`
  (`tipoAbrangencia: "M"`). Quando sair a eleição municipal de 2028, adicionar
  o ano/id ali e em `ANOS_ELEICOES_MUNICIPAIS`, do mesmo jeito descrito acima
  pra 2026 nas gerais.
- Eleição municipal só entra no escopo em **1º turno** (`NR_TURNO == 1`,
  filtrado em todos os pulls municipais) — mesmo critério já usado pras
  eleições gerais (Governador/Presidente também só contam 1º turno).
  VICE-PREFEITO fica de fora de `CARGOS_ALVO_MUNICIPAIS`: é um cargo
  registrado no TSE (tem candidato/perfil próprio), mas não recebe voto
  próprio — o eleitor vota no número do Prefeito, o voto vai todo pra chapa.
