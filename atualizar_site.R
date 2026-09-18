# =============================================================================
# Atualiza o site estatico do Dashboard Novo (dist/) e empacota o ZIP pronto
# pra subir no painel Gauss (gauss.indexps.xyz/admin/menus). Ver
# ADAPTAR_PROJETO_GAUSS.md pro passo a passo de publicacao.
#
# Rodar com working directory = Dashboard Novo/ (raiz do projeto):
#   Rscript atualizar_site.R
# =============================================================================

cat("== 1/2: exportando dados (programs/13_export_dist_data.R) ==\n")
source("programs/13_export_dist_data.R")

cat("\n== 2/2: empacotando site.zip ==\n")
# Conteudo de dist/ precisa estar na RAIZ do ZIP (nao a pasta dist/ em si) --
# setwd("dist") antes do zip::zip() garante isso. zip::zip() grava os
# caminhos com "/" (funciona no servidor Linux do Gauss); NUNCA usar
# Compress-Archive do PowerShell aqui (grava com "\", quebra subpastas).
raiz <- getwd()
destino <- file.path(raiz, "site.zip")
if (file.exists(destino)) file.remove(destino)

setwd("dist")
zip::zip(destino, files = list.files("."), recurse = TRUE)
setwd(raiz)

cat(sprintf("\nOK -- %s gerado (%.1f MB)\n", destino, file.size(destino) / 1024^2))
cat("\nPróximos passos (manuais, no navegador):\n")
cat("  1. Acesse gauss.indexps.xyz/admin/menus e faça login.\n")
cat("  2. Se já existir um menu 'Dashboard Novo', DELETE-o primeiro\n")
cat("     (o Gauss não sobrescreve um menu existente ao subir um ZIP novo).\n")
cat("  3. Em 'Novo Menu Interativo', selecione site.zip e clique em 'Enviar e Descompactar'.\n")
cat("  4. Abra a URL publicada e confira se tudo carrega.\n")
