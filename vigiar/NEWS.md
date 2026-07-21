# vigiar 0.7.0

## New: Benchmark & Performance

* `vigiar_benchmark()`: compare download strategies (direct, year_asc_desc,
  minimal_columns) with timing, row counts, and success rates.
* `vigiar_benchmark_tabelas()`: multi-table benchmark for API health monitoring.
* `vigiar_health_check()`: comprehensive health check (connection, schema,
  benchmark, compliance) returning a structured report.

## New: Compliance & Auditing

* `vigiar_auditar()`: full data audit covering schema, IBGE codes, temporal
  consistency, units, coverage, and checksums. Returns structured `vigiar_audit`
  object with S3 print method.
* `vigiar_auditar_tudo()`: batch audit across multiple tables.
* `vigiar_compliance_check()`: multi-profile compliance (basico, rigoroso, rj,
  corrupcao) with outlier detection and integrity checks.
* `vigiar_checksum()`: deterministic SHA256 checksum for any data frame.
* `vigiar_exportar_auditoria()`: export audit report as JSON for archiving.
* S3 classes: `vigiar_audit`, `vigiar_audit_list`, `vigiar_compliance` with
  print methods.

## New: Structured Logging

* `.vigiar_log()`: internal structured logger with INFO/WARN/ERROR/DEBUG levels.
* `vigiar_log()`: retrieve complete operation log as tibble.
* `vigiar_limpar_log()`: clear operation log.
* `vigiar_exportar_log()`: export log to CSV or JSON.
* `vigiar_resumo_log()`: summary statistics by level and table.
* `vigiar_historico_downloads()`: download history with timestamps and row counts.
* `vigiar_resumo_downloads()`: summary of all downloads in session.
* Automatic logging integrated into `vigiar_baixar()` via `.vigiar_registrar_download()`.

## New: Reproducibility & Snapshots

* `vigiar_snapshot()`: create data snapshots with SHA256 checksums, session info,
  and parameter provenance.
* `vigiar_verificar_snapshot()`: verify snapshot integrity.
* `vigiar_salvar_snapshot()` / `vigiar_carregar_snapshot()`: save/load snapshots.
* `vigiar_comparar_snapshots()`: diff two snapshots (dimensions, columns, checksums).

## New: Local Cache

* `vigiar_cache_dir()`: configure cache directory (defaults to platform-appropriate
  location).
* `vigiar_baixar_com_cache()`: download with automatic caching and TTL.
* `vigiar_cache_info()`: list cached tables with age and checksums.
* `vigiar_limpar_cache()`: clear cache by table or age.

## New: Schema Version Locking

* `vigiar_esquema_lock()`: freeze current schema to JSON for reproducibility.
* `vigiar_esquema_carregar_lock()`: load a schema lock file.
* `vigiar_esquema_verificar()`: compare live schema against a lock, detect changes.

## Changed

* `vigiar_baixar()` UF filter now tries multiple column names (UF, sigla_uf,
  UF_SIGLA, uf, cod_uf) and falls back to IBGE code range for RJ.
* `vigiar_baixar()` now uses `cli` for messages and integrates with logging.
* DESCRIPTION: added `cli`, `openssl`, `tools` to Imports. Bumped version to 0.7.0.
* NAMESPACE: added 30+ new exports for benchmark, audit, logging, cache, snapshots.
* Removed `stats::filter` import to avoid masking `dplyr::filter`.
* Fixed man page for `vigiar_baixar.Rd` to match `uf = "RJ"` default.
* Fixed non-ASCII characters in documentation files.

## Tests

* Added comprehensive offline tests for all new features (test-new-features.R).
* Added `tests/testthat.R` for proper testthat integration.

# vigiar 0.6.0
...
