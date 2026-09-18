# Sobre o Dashboard NOVO em Pernambuco

Este projeto é um painel de análise do desempenho eleitoral do partido **NOVO**
em Pernambuco, construído para apoiar o mapeamento do espaço político do
partido no estado — onde ele já tem força, onde é irrelevante, e o perfil de
eleitorado que historicamente o acompanha.

## O que o painel mostra

- **Eleições gerais (2018 e 2022)** — desempenho de Deputado Federal por
  município: votos de legenda e nominais, mapa coroplético (% dos votos
  válidos) e mapa LISA (clusters estatisticamente significantes), ranking de
  municípios, e um perfil detalhado de cada candidato (foto oficial, idade,
  gênero, escolaridade, redes sociais).
- **Eleições municipais (2020 e 2024)** — o mesmo tipo de análise para
  Vereador e Prefeito, só que em nível de **bairro** em vez de município,
  nos 8 municípios onde o NOVO teve candidato e existe geometria de bairro
  disponível no IBGE (Recife, Olinda, Paulista, Jaboatão dos Guararapes,
  Caruaru, Garanhuns, Surubim, Afogados da Ingazeira).
- **Perfil do eleitorado** — em toda seção, um índice de sobre/sub-
  representação: o eleitor do NOVO, por gênero/faixa etária/escolaridade, é
  mais ou menos parecido com o eleitorado geral daquele recorte?

## Como os dados chegam até aqui

Tudo vem de fontes públicas oficiais:

- **TSE** (`electionsBR` + downloads diretos do CDN/API) — votos por seção,
  candidatos, e perfil demográfico do eleitorado por seção eleitoral.
- **API DivulgaCandContas do TSE** — foto oficial e dados de perfil de cada
  candidato.
- **IBGE, via `geobr`** — geometria de município e de bairro.

O pipeline completo de coleta/tratamento fica em `programs/` (numerado,
passo a passo) e está documentado com todos os detalhes técnicos em
[README.md](README.md).

## Duas formas de acessar

1. **App Shiny** (`app.R`) — versão interativa completa, roda localmente em
   R (`shiny::runApp(".")`).
2. **Site estático** (`dist/`) — a mesma análise exportada para
   HTML/JS/JSON puro (sem precisar de um servidor R rodando), publicada no
   painel Gauss. Gerado por `atualizar_site.R`.

## Autoria

Projeto desenvolvido para a pré-campanha de Eduardo Inojosa a deputado
estadual por Pernambuco.
