# Public API lifecycle registry

.VIGIAR_EXPERIMENTAL_API <- c(
  "vigiar_analisar_pm25_mensal_rj",
  "vigiar_auditar_rj_online",
  "vigiar_baixar_rj_completo",
  "vigiar_benchmark",
  "vigiar_benchmark_tabelas",
  "vigiar_compliance_check",
  "vigiar_esquema_carregar_lock",
  "vigiar_esquema_lock",
  "vigiar_esquema_verificar",
  "vigiar_esquema_verificar_critico",
  "vigiar_health_check",
  "vigiar_plot_pm25_rj"
)

.VIGIAR_DEPRECATED_API <- c(
  "vigiar_rj_macrorregioes"
)

#' Inspect the public API lifecycle status
#'
#' Every exported function is classified as `stable`, `experimental`, or
#' `deprecated`. Objects whose names start with a dot and unexported helpers are
#' internal and are not part of this table. Experimental interfaces may evolve
#' with documented migration guidance. Deprecated interfaces remain available
#' for compatibility during the current development series.
#'
#' @return A tibble with exported symbol, lifecycle status, and guidance.
#' @export
vigiar_api_status <- function() {
  symbols <- sort(getNamespaceExports("vigiar"))
  status <- ifelse(
    symbols %in% .VIGIAR_DEPRECATED_API,
    "deprecated",
    ifelse(symbols %in% .VIGIAR_EXPERIMENTAL_API, "experimental", "stable")
  )
  guidance <- rep("Covered by backwards-compatibility policy.", length(symbols))
  guidance[status == "experimental"] <- paste(
    "Interface is usable but may evolve with NEWS and migration guidance."
  )
  guidance[symbols == "vigiar_rj_macrorregioes"] <- paste(
    "Compatibility alias for vigiar_rj_regioes_saude(); use the canonical",
    "SES-RJ health-region terminology in new code."
  )
  tibble::tibble(symbol = symbols, status = status, guidance = guidance)
}
