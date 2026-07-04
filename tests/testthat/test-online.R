# vigiar: online integration tests
#
# These tests require internet access and a working VIGIAR dashboard.
# Run with:  VIGIAR_RUN_ONLINE_TESTS=true  R CMD check
# Or:        withr::local_envvar(VIGIAR_RUN_ONLINE_TESTS = "true")
#            devtools::test()

library(testthat)
library(vigiar)

# Guard: skip all online tests unless explicitly enabled
# This runs at file-level load time, before any test_that() block
online_tests <- identical(tolower(Sys.getenv("VIGIAR_RUN_ONLINE_TESTS")), "true")
if (!online_tests) {
  skip("Online tests disabled. Set VIGIAR_RUN_ONLINE_TESTS=true to run.")
}

# If we get here, online tests are enabled

.online_report_dir <- function() {
  release_id <- Sys.getenv(
    "VIGIAR_VALIDATION_RELEASE",
    unset = paste0("vigiar-", utils::packageVersion("vigiar"))
  )
  root <- Sys.getenv(
    "VIGIAR_ONLINE_REPORT_DIR",
    unset = file.path(tempdir(), "vigiar-online-rj-report")
  )
  if (!grepl("^([A-Za-z]:|/|\\\\\\\\)", root)) {
    candidates <- unique(c(getwd(), dirname(getwd()), dirname(dirname(getwd()))))
    package_root <- candidates[file.exists(file.path(candidates, "DESCRIPTION"))][1]
    if (!is.na(package_root)) {
      root <- file.path(package_root, root)
    }
  }
  out_dir <- file.path(root, paste(release_id, format(Sys.time(), "%Y%m%d-%H%M%S"), sep = "-"))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_dir
}

.online_write_report <- function(x, out_dir, name) {
  path <- file.path(out_dir, name)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  list_cols <- vapply(x, is.list, logical(1))
  x[list_cols] <- lapply(x[list_cols], function(col) {
    vapply(col, paste, collapse = "; ", FUN.VALUE = character(1))
  })
  utils::write.csv(x, path, row.names = FALSE)
  path
}

.online_temporal_summary <- function(dados) {
  years <- integer(0)
  missing_years <- integer(0)
  month_gaps <- character(0)

  if ("ano" %in% names(dados)) {
    years <- sort(unique(as.integer(dados$ano)))
    years <- years[!is.na(years)]
    if (length(years) > 0) {
      missing_years <- setdiff(seq.int(min(years), max(years)), years)
    }
  }

  if (all(c("ano", "mes") %in% names(dados))) {
    year_month <- unique(data.frame(
      ano = as.integer(dados$ano),
      mes = as.integer(dados$mes)
    ))
    year_month <- year_month[!is.na(year_month$ano) & !is.na(year_month$mes), , drop = FALSE]
    if (nrow(year_month) > 0) {
      by_year <- split(year_month$mes, year_month$ano)
      month_gaps <- unlist(Map(function(year, months) {
        missing <- setdiff(1:12, sort(unique(months)))
        if (length(missing) == 0) {
          return(character(0))
        }
        paste0(year, ":", paste(missing, collapse = "|"))
      }, names(by_year), by_year), use.names = FALSE)
    }
  }

  data.frame(
    anos_presentes = paste(years, collapse = "; "),
    anos_ausentes = paste(missing_years, collapse = "; "),
    meses_ausentes_por_ano = paste(month_gaps, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

test_that("online RJ downloads create table-grain audit reports", {
  vigiar_conectar()
  withr::defer(vigiar_desconectar(), testthat::teardown_env())

  report_dir <- .online_report_dir()
  strict <- identical(tolower(Sys.getenv("VIGIAR_REQUIRE_ONLINE_COMPLETENESS")), "true")
  tables <- c("df_anual", "df_mensal", "df_dias")
  expected_grades <- list(
    df_anual = "municipio x ano",
    df_mensal = "municipio x ano x mes",
    df_dias = c("municipio x ano x mes", "municipio x ano")
  )
  manifest <- list()
  rj_codes <- vigiar_rj_municipios()$codigo_ibge_6

  for (tab in tables) {
    dados <- suppressWarnings(vigiar_baixar_rj(
      tab,
      validar_cobertura = TRUE,
      processar = TRUE
    ))
    cov <- vigiar_rj_cobertura(dados)
    completude <- vigiar_rj_completude_tabela(dados, tabela = tab)
    ausentes <- vigiar_rj_municipios_ausentes(dados)
    temporal <- .online_temporal_summary(dados)
    checksum <- vigiar_checksum(dados)
    checked_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    incomplete_groups <- sum(!completude$completo)

    manifest[[tab]] <- cbind(
      data.frame(
        checked_at = checked_at,
        tabela = tab,
        n_rows = nrow(dados),
        n_cols = ncol(dados),
        checksum = checksum,
        completeness_grade = paste(unique(completude$grade), collapse = "; "),
        n_completeness_groups = nrow(completude),
        n_incomplete_groups = incomplete_groups,
        n_municipios_presentes = cov$n_municipios_presentes[1],
        n_municipios_esperados = cov$n_municipios_esperados[1],
        n_municipios_ausentes = cov$n_ausentes[1],
        possivel_truncamento = isTRUE(attr(dados, "vigiar_possivel_truncamento")),
        stringsAsFactors = FALSE
      ),
      temporal
    )

    .online_write_report(cov, report_dir, paste0(tab, "-coverage-general.csv"))
    .online_write_report(completude, report_dir, paste0(tab, "-coverage-table-grain.csv"))
    .online_write_report(ausentes, report_dir, paste0(tab, "-missing-municipalities.csv"))
    .online_write_report(temporal, report_dir, paste0(tab, "-temporal-gaps.csv"))

    if ("ano" %in% names(dados)) {
      .online_write_report(
        vigiar_rj_municipios_ausentes(dados, por = "ano"),
        report_dir,
        paste0(tab, "-missing-municipalities-year.csv")
      )
    }
    if (all(c("ano", "mes") %in% names(dados))) {
      .online_write_report(
        vigiar_rj_municipios_ausentes(dados, por = "ano_mes"),
        report_dir,
        paste0(tab, "-missing-municipalities-year-month.csv")
      )
    }

    message(sprintf(
      paste(
        "RJ online audit: checked_at=%s; table=%s; rows=%d;",
        "checksum=%s; grade=%s; incomplete_groups=%d;",
        "missing_municipalities=%d; missing_years=%s; missing_months=%s"
      ),
      checked_at,
      tab,
      nrow(dados),
      checksum,
      paste(unique(completude$grade), collapse = "; "),
      incomplete_groups,
      cov$n_ausentes[1],
      temporal$anos_ausentes,
      temporal$meses_ausentes_por_ano
    ))

    expect_s3_class(dados, "tbl_df")
    expect_gt(nrow(dados), 0)
    expect_s3_class(cov, "tbl_df")
    expect_s3_class(completude, "tbl_df")
    expect_s3_class(ausentes, "tbl_df")
    expect_true(any(completude$grade %in% expected_grades[[tab]]))
    expect_true(all(c(
      "n_municipios_presentes",
      "n_municipios_esperados",
      "n_ausentes",
      "completo"
    ) %in% names(completude)))
    expect_gt(cov$n_municipios_presentes[1], 0)
    expect_lte(cov$n_municipios_presentes[1], 92)
    expect_type(checksum, "character")
    expect_true(nchar(checksum) > 0)

    code_col <- intersect(c("codigo_ibge_6", "cod_municipio"), names(dados))[1]
    expect_false(is.na(code_col), info = paste(tab, "must expose a normalized municipality code"))
    codes <- as.character(dados[[code_col]])
    codes <- codes[!is.na(codes)]
    expect_true(all(codes %in% rj_codes), info = paste(tab, "must not include non-RJ municipalities"))

    if (strict) {
      expect_true(
        all(completude$completo),
        info = paste(tab, "is incomplete at table grain; inspect", report_dir)
      )
      expect_false(
        isTRUE(attr(dados, "vigiar_possivel_truncamento")),
        info = paste(tab, "has possible API truncation; inspect", report_dir)
      )
    }
  }

  manifest_path <- .online_write_report(
    do.call(rbind, manifest),
    report_dir,
    "online-rj-audit-manifest.csv"
  )
  expect_true(file.exists(manifest_path))
  message("RJ online audit reports: ", normalizePath(report_dir, winslash = "/", mustWork = FALSE))
})
