# =============================================================================
# TESTE INDEPENDENTE DO PACOTE vigiar.rj
# Salve este arquivo e rode:  source("teste_vigiar_rj.R")
# =============================================================================

library(vigiar.rj)

cat("\n============================================================\n")
cat("  TESTE INDEPENDENTE DO vigiar.rj\n")
cat("============================================================\n")

# ---- 1. Pacote carregado ----
cat("\n[1] Pacote carregado: vigiar.rj v", as.character(packageVersion("vigiar.rj")), "\n")

# ---- 2. Funcoes internas ----
cat("\n[2] Funcoes internas:\n")
cat("    uuid_v4(): ", uuid_v4(), "\n")
cat("    %||%: ", 1 %||% 2, "\n")
cat("    Tipos Power BI -> R: 1=", .vigiar_tipo_dado(1), " 3=", .vigiar_tipo_dado(3), " 4=", .vigiar_tipo_dado(4), "\n")

# ---- 3. Registro RJ ----
cat("\n[3] Registro RJ:\n")
rj <- vigiar_rj_municipios()
cat("    Municipios: ", nrow(rj), "\n")
cat("    Macrorregioes: ", paste(vigiar_rj_macrorregioes(), collapse=", "), "\n")
cat("    Exemplo: ", rj$municipio[1], " (", rj$codigo_ibge[1], ")\n")

# ---- 4. Conexao ----
cat("\n[4] Conectando ao VIGIAR...\n")
vigiar_conectar()
cat("    Sessao ativa: ", vigiar_sessao_ativa(), "\n")
cat("    Tabelas: ", length(vigiar_tabelas()), "\n")

# ---- 5. Download ----
cat("\n[5] Baixando dados:\n")

# df_muni (tabela pequena, todos municipios)
cat("    df_muni... ")
muni <- tryCatch(vigiar_baixar_rj("df_muni"), error = function(e) NULL)
if (!is.null(muni)) {
  cat(nrow(muni), "linhas,", length(unique(muni$MUN_COD)), "municipios\n")
} else {
  cat("ERRO\n")
}

# df_anual com strategy year
cat("    df_anual (year strategy)... ")
pm25 <- tryCatch(vigiar_baixar_rj("df_anual"), error = function(e) NULL)
if (!is.null(pm25)) {
  cat(nrow(pm25), "linhas\n")
} else {
  cat("ERRO\n")
}

# ---- 6. Validacao ----
cat("\n[6] Validacao RJ:\n")
if (!is.null(pm25) && nrow(pm25) > 0) {
  val <- vigiar_validar_rj(pm25)
} else if (!is.null(muni) && nrow(muni) > 0) {
  cat("    Validando df_muni...\n")
  val <- vigiar_validar_rj(muni)
} else {
  cat("    Sem dados para validar\n")
}

# ---- 7. Processamento ----
cat("\n[7] Processando com process_vigiar():\n")
if (!is.null(muni) && nrow(muni) > 0) {
  muni_proc <- process_vigiar(muni, tabela = "df_muni")
  cat("    df_muni processado:", ncol(muni_proc), "colunas\n")
  cat("    Colunas:", paste(names(muni_proc)[1:5], collapse=", "), "...\n")
}

# ---- 8. Resumo ----
cat("\n[8] Resumo:\n")
if (!is.null(pm25) && nrow(pm25) > 0) {
  cat("    PM2.5 medio:", mean(pm25$Media_pm25, na.rm = TRUE) |> round(1), "ug/m3\n")
  cat("    Periodo:", min(pm25$ano, na.rm = TRUE), "-", max(pm25$ano, na.rm = TRUE), "\n")
}

# ---- 9. Exportacao ----
cat("\n[9] Exportacao:\n")
tmp <- file.path(tempdir(), "teste_vigiar_rj.csv")
if (!is.null(muni)) {
  vigiar_exportar_csv(muni, tmp, overwrite = TRUE)
  cat("    CSV exportado:", tmp, "(", file.info(tmp)$size, "bytes)\n")
}

# ---- 10. Dicionario ----
cat("\n[10] Dicionario:\n")
dict <- vigiar_dicionario()
cat("    Variaveis documentadas:", nrow(dict), "\n")
cat("    Tabelas cobertas:", paste(unique(dict$table_id)[1:5], collapse=", "), "...\n")

# ---- FIM ----
cat("\n============================================================\n")
cat("  TESTE CONCLUIDO\n")
cat("============================================================\n")
vigiar_desconectar()
