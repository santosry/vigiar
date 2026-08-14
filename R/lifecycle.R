# Package: vigiar
# Lifecycle registry for the public API.
#
# Every exported function carries an explicit lifecycle status so users and
# CI can rely on what is stable. Statuses follow the lifecycle project:
#   stable       -> API is mature and will not change without deprecation
#   experimental -> API may change in a non-breaking release
#   deprecated   -> API will be removed; use the replacement instead
#
# The registry is exposed through vigiar_lifecycle() and is also used to
# keep documentation badges in sync.

VIGIAR_LIFECYCLE_STABLE <- c(
  "process_exposicao_indoor",
  "process_fracao_atribuivel",
  "process_indicadores_saude",
  "process_municipios",
  "process_pm25",
  "process_populacao_exposta",
  "process_ufs",
  "process_vigiar",
  "validate.vigiar_tbl",
  "vigiar_agregar_tempo",
  "vigiar_auditar",
  "vigiar_auditar_tudo",
  "vigiar_baixar",
  "vigiar_baixar_com_cache",
  "vigiar_baixar_principais",
  "vigiar_baixar_rj",
  "vigiar_baixar_tudo",
  "vigiar_cache_dir",
  "vigiar_cache_info",
  "vigiar_carregar_snapshot",
  "vigiar_checar_cobertura_espacial",
  "vigiar_checar_cobertura_temporal",
  "vigiar_checar_dados",
  "vigiar_checar_duplicatas",
  "vigiar_checar_ibge",
  "vigiar_checar_pm25",
  "vigiar_checar_quebra_serie",
  "vigiar_checksum",
  "vigiar_classificar_alertas",
  "vigiar_comparar_snapshots",
  "vigiar_compliance_check",
  "vigiar_conectar",
  "vigiar_convencoes",
  "vigiar_desconectar",
  "vigiar_descrever_variavel",
  "vigiar_diagnosticar_serie",
  "vigiar_diagnostico",
  "vigiar_dicionario",
  "vigiar_esquema",
  "vigiar_esquema_carregar_lock",
  "vigiar_esquema_lock",
  "vigiar_esquema_verificar",
  "vigiar_exportar_auditoria",
  "vigiar_exportar_csv",
  "vigiar_exportar_log",
  "vigiar_exportar_parquet",
  "vigiar_exportar_rds",
  "vigiar_historico_downloads",
  "vigiar_info",
  "vigiar_limpar_cache",
  "vigiar_limpar_log",
  "vigiar_log",
  "vigiar_padronizar_colunas",
  "vigiar_relatorio_diagnostico",
  "vigiar_resumo",
  "vigiar_resumo_downloads",
  "vigiar_resumo_fracao_atribuivel",
  "vigiar_resumo_indoor",
  "vigiar_resumo_log",
  "vigiar_resumo_pm25",
  "vigiar_resumo_populacao",
  "vigiar_resumo_saude",
  "vigiar_rj_macrorregioes",
  "vigiar_rj_municipios",
  "vigiar_rj_regioes_saude",
  "vigiar_rj_resumo",
  "vigiar_rj_series",
  "vigiar_salvar_snapshot",
  "vigiar_serie_temporal",
  "vigiar_sessao_ativa",
  "vigiar_snapshot",
  "vigiar_status",
  "vigiar_tabelas",
  "vigiar_tabelas_documentadas",
  "vigiar_tendencia_descritiva",
  "vigiar_validar_datas",
  "vigiar_validar_dicionario",
  "vigiar_validar_ibge",
  "vigiar_validar_rj",
  "vigiar_validar_unidades",
  "vigiar_variaveis",
  "vigiar_variaveis_nao_documentadas",
  "vigiar_variaveis_orfas",
  "vigiar_verificar_snapshot"
)

VIGIAR_LIFECYCLE_EXPERIMENTAL <- c(
  "vigiar_benchmark",
  "vigiar_benchmark_tabelas",
  "vigiar_client",
  "vigiar_comparar_schema",
  "vigiar_health_check",
  "vigiar_schema"
)

stable <- rep("stable", length(VIGIAR_LIFECYCLE_STABLE))
names(stable) <- VIGIAR_LIFECYCLE_STABLE

experimental <- rep("experimental", length(VIGIAR_LIFECYCLE_EXPERIMENTAL))
names(experimental) <- VIGIAR_LIFECYCLE_EXPERIMENTAL

VIGIAR_LIFECYCLE <- c(stable, experimental)

# Remove helper objects that should not leak into the registry surface.
rm(stable, experimental)

#' Inspect the lifecycle status of exported functions
#'
#' `r lifecycle::badge("stable")`
#'
#' Returns the lifecycle status of one or more exported functions, or a tibble
#' with the full public API registry when `fun` is `NULL`. Use this to
#' determine whether a function is safe to rely on in scripts and publications.
#'
#' @param fun Character vector of function names, or `NULL` for the full
#'   registry.
#' @return A tibble with columns `fun` and `status` when `fun` is `NULL`;
#'   otherwise a named character vector with the requested statuses.
#' @export
vigiar_lifecycle <- function(fun = NULL) {
  if (is.null(fun)) {
    return(
      tibble::tibble(
        fun = names(VIGIAR_LIFECYCLE),
        status = unname(VIGIAR_LIFECYCLE)
      )
    )
  }

  fun <- as.character(fun)
  desconhecidas <- setdiff(fun, names(VIGIAR_LIFECYCLE))
  if (length(desconhecidas) > 0) {
    stop(
      "Funcao(s) sem status de lifecycle registrado: ",
      paste(desconhecidas, collapse = ", ")
    )
  }

  VIGIAR_LIFECYCLE[fun]
}
