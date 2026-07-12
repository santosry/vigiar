# vigiar 0.7.1.9000

## Rio de Janeiro completeness hardening

* Fixed a false compliance pass for data containing all 92 RJ municipalities
  plus an external municipality. Audit checks now share explicit `ok`, `status`,
  `severity`, and `details` contracts; `unknown` never becomes a pass.
* `vigiar_baixar()` is now geographically generic (`uf = NULL`). RJ scope is
  explicit in `vigiar_baixar_rj()`, which filters against the official registry
  rather than a numeric range.
* Municipality-code validation now separates format, official existence,
  seven-digit check-code correspondence, and UF membership using a versioned
  national IBGE reference. Six digits are the internal standard; seven digits
  are retained for interoperability.
* The RJ fixture is checked against official IBGE and SES-RJ references,
  including 92 unique municipalities, nine SES-RJ health regions, and sentinels
  for Campos dos Goytacazes (`330100`/`3301009`), Italva, and Volta Redonda.
* `vigiar_rj_completude_tabela()` detects entirely absent internal and boundary
  periods. Expected domains can be supplied through years, months, or period
  endpoints; observed, inferred, user-specified, official, and not-applicable
  domains are not conflated.
* Spatial coverage, temporal-domain status, panel completeness, schema status,
  response completeness, truncation evidence, verification, and overall status
  are reported independently. S3 classes now expose programmatic coverage and
  completeness summaries.
* `vigiar_auditar_rj_online()` writes CSV, JSON, and RDS evidence with package
  version, commit SHA, canonical checksum, schema hash, rows, columns, coverage,
  missing municipalities, health-region gaps, temporal gaps, and conservative
  conclusions including `complete_within_inferred_domain` and
  `schema_unverified`. Plain `complete` requires an explicit expected domain.
* Truncation is represented as `no_evidence`, `possible`, `probable`,
  `confirmed`, or `unknown` with supporting evidence. Raw responses are assessed
  before client-side filters. Strict completeness converts any truncation signal
  into an actionable error.
* The DSR parser now validates response envelopes, descriptors, repeat/null
  masks, dictionaries, compacted rows, and row width. Adversarial and randomized
  repeat-mask tests protect malformed and partial-response behavior.
* Canonical checksum version 2 defines semantic table identity without losing
  double precision. Snapshots store separate data, schema, and metadata hashes;
  caches include query parameters, schema, package version, and algorithm
  versions in their keys and validate provenance on read.
* Structured logs now record connection, download, retry, schema change,
  truncation, cache, snapshot, audit, and compliance events while redacting
  cookies, tokens, resource keys, authorization values, and URL queries.
* Benchmarks now use warm-up and repeated measurements with median, p25, p75,
  min, max, success/failure counts, checksums, and environment details. The old
  `year_asc_desc` name is deprecated in favor of the honest
  `two_ended_sample`; neither strategy claims complete retrieval.
* CI now uses least-privilege read permissions, enforces a real 70% coverage
  gate with persisted evidence, and runs a separate scheduled online canary that
  archives RJ audit artifacts without blocking pull requests.
* Software metadata and citation files are synchronized at version 0.7.1.9000.
  User-facing prose and messages use technical English; historical Portuguese
  API identifiers remain for compatibility and are governed by
  `vigiar_api_status()` and `LANGUAGE_POLICY.md`.
* Scientific documentation distinguishes downloaded, filtered, covered,
  complete, verified, audited, and reproducible data. The package does not
  validate causal inference, GAM, DLNM, relative risk, machine learning,
  predictive models, or epidemiologic conclusions.

## Migration notes

* Code that relied on the old implicit RJ default must use
  `vigiar_baixar_rj()` or pass `uf = "RJ"` explicitly.
* New code should use `regiao_saude`; `macrorregiao_saude` and
  `vigiar_rj_macrorregioes()` remain compatibility aliases.
* Stored checksums created with an earlier algorithm should be regenerated and
  archived with `canonicalization_version = 2`.
* Strict online audits must provide `dominios_esperados` for temporal tables.
* Benchmark consumers must migrate from Portuguese timing columns and
  `year_asc_desc` to the structured English metrics and `two_ended_sample`.

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
