# =============================================================================
# Atualiza o site estatico do Dashboard Novo (dist/) e empacota o ZIP pronto
# pra subir manualmente no painel de hospedagem do site (ver README.md,
# secao "Publicar", pros passos e o link).
#
# Rodar com working directory = Dashboard Novo/ (raiz do projeto):
#   Rscript atualizar_site.R
# =============================================================================

cat("== 1/2: exportando dados (programs/13_export_dist_data.R) ==\n")
source("programs/13_export_dist_data.R")

cat("\n== 2/2: empacotando site.zip ==\n")
# Conteudo de dist/ precisa estar na RAIZ do ZIP (nao a pasta dist/ em si) --
# setwd("dist") antes do zip::zip() garante isso. zip::zip() grava os
# caminhos com "/" (funciona no servidor Linux de destino); NUNCA usar
# Compress-Archive do PowerShell aqui (grava com "\", quebra subpastas).
raiz <- getwd()
destino <- file.path(raiz, "site.zip")
if (file.exists(destino)) file.remove(destino)

setwd("dist")
zip::zip(destino, files = list.files("."), recurse = TRUE)
setwd(raiz)

cat(sprintf("\nOK -- %s gerado (%.1f MB)\n", destino, file.size(destino) / 1024^2))
cat("\nPróximos passos (manuais, no navegador):\n")
cat("  1. Acesse o painel de administração do site estático e faça login\n")
cat("     (ver README.md, seção 'Publicar', pro link).\n")
cat("  2. Se já existir uma versão publicada deste dashboard, apague-a primeiro\n")
cat("     (o painel não sobrescreve uma publicação existente ao subir um ZIP novo).\n")
cat("  3. Envie site.zip e extraia.\n")
cat("  4. Abra a URL publicada e confira se tudo carrega.\n")
