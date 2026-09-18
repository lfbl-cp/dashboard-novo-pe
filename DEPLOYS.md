# Registro de publicações

O deploy do site estático (upload manual de `site.zip` no painel de
hospedagem externo) não deixa nenhum rastro automático — ao contrário do
shinyapps.io (`rsconnect/`, tem timestamp do último deploy) e do app Shiny
local (não precisa de deploy). Por isso esse registro é manual: depois de
publicar o site estático, adicione uma linha aqui.

Antes de publicar (qualquer uma das 3 versões), rode `Rscript
checklist_deploy.R` — ver README.md, seção "Publicar".

| Data | O que mudou | Publicado onde |
|------|-------------|-----------------|
| (preencher) | Registro começa a partir de agora — publicações anteriores (site estático, shinyapps.io) não têm data confirmada aqui | — |
| 2026-09-18 | Pré-cálculo da composição do eleitorado (`programs/17`, corrige ~40s de tela em branco) + correção do `.rscignore` (bundle de deploy caiu de 550MB pra 11MB) | shinyapps.io |
| (preencher) | Mesma atualização acima | site estático — **subir `site.zip` manualmente e completar esta linha** |
