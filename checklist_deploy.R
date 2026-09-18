# =============================================================================
# Checklist antes de publicar (Gauss / shinyapps.io / app Shiny local): mostra
# quando cada camada de dado foi gerada pela ultima vez, num comando so.
#
# O projeto tem 3 publicacoes independentes do MESMO dado (ver README.md,
# secao "Publicar") e nenhuma trava impede publicar uma delas com
# data/processed/ ou dist/ desatualizados em relacao a ultima rodada do
# pipeline -- foi exatamente esse tipo de dessincronia que já causou "o
# dashboard nao mostra dado nenhum" numa das 3 versoes enquanto as outras
# duas estavam certas. Este script nao AUTOMATIZA a publicacao nem troca a
# checagem manual por uma trava de verdade -- so torna visivel, sem precisar
# abrir o Explorador de arquivos e comparar data por data, o que hoje esta
# mais novo e o que esta mais velho.
#
# Rodar com working directory = Dashboard Novo/ (raiz do projeto):
#   Rscript checklist_deploy.R
# =============================================================================

mtime_mais_recente <- function(dir) {
  arquivos <- list.files(dir, recursive = TRUE, full.names = TRUE)
  if (length(arquivos) == 0) return(NA)
  max(file.info(arquivos)$mtime, na.rm = TRUE)
}

formatar <- function(t) if (is.na(t)) "-- (pasta vazia ou nao existe)" else format(t, "%Y-%m-%d %H:%M")

cat("== Checklist de publicacao -- Dashboard NOVO em Pernambuco ==\n\n")

t_raw <- mtime_mais_recente("data/raw")
t_processed <- mtime_mais_recente("data/processed")
t_dist <- mtime_mais_recente("dist")

cat(sprintf("1. Cache bruto baixado do TSE/IBGE (data/raw/)....... %s\n", formatar(t_raw)))
cat(sprintf("2. Pipeline processado (data/processed/)............. %s\n", formatar(t_processed)))
cat(sprintf("3. Site estatico exportado (dist/, p/ Gauss)......... %s\n", formatar(t_dist)))

dcf_shinyapps <- list.files("rsconnect/shinyapps.io", pattern = "\\.dcf$", recursive = TRUE, full.names = TRUE)
if (length(dcf_shinyapps) > 0) {
  cat(sprintf("4. Ultimo deploy registrado no shinyapps.io........... %s\n", formatar(file.info(dcf_shinyapps[1])$mtime)))
} else {
  cat("4. Ultimo deploy registrado no shinyapps.io........... (nenhum registro em rsconnect/ nesta maquina)\n")
}

if (file.exists("site.zip")) {
  cat(sprintf("5. site.zip local (o que SERIA publicado no Gauss)... %s\n", formatar(file.info("site.zip")$mtime)))
} else {
  cat("5. site.zip local..................................... (nao existe -- rode atualizar_site.R antes de publicar no Gauss)\n")
}

cat("\n-- Leitura --\n")
if (!is.na(t_processed) && !is.na(t_raw) && t_processed < t_raw) {
  cat("[ATENCAO] data/processed/ é mais VELHO que data/raw/ -- rode o pipeline\n")
  cat("          de consolidacao de novo (passos 05/11/16/17 no README).\n")
}
if (!is.na(t_dist) && !is.na(t_processed) && t_dist < t_processed) {
  cat("[ATENCAO] dist/ é mais VELHO que data/processed/ -- o site estatico do\n")
  cat("          Gauss ainda reflete dado antigo. Rode atualizar_site.R antes\n")
  cat("          de subir o site.zip de novo.\n")
}
if (length(dcf_shinyapps) > 0 && !is.na(t_processed) && file.info(dcf_shinyapps[1])$mtime < t_processed) {
  cat("[ATENCAO] o ultimo deploy no shinyapps.io é mais VELHO que\n")
  cat("          data/processed/ -- rode rsconnect::deployApp() de novo.\n")
}

cat("\nDepois de publicar manualmente no Gauss (upload do site.zip, fora deste\n")
cat("script), registre a data em DEPLOYS.md.\n")
