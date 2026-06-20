# vigiar 0.2.0

## New: Processing family (microdatasus architecture)

* `process_vigiar()`: generic dispatcher — auto-detects table type and
  applies the correct processor.
* `process_pm25()`: standardises PM2.5 data (annual, monthly, critical days).
* `process_populacao_exposta()`: standardises population exposure data.
* `process_indicadores_saude()`: standardises health indicators
  (Brazil, UF, municipality, quartiles).
* `process_fracao_atribuivel()`: standardises attributable fraction data.
* `process_exposicao_indoor()`: standardises indoor exposure data.
* `process_municipios()`: standardises municipality registry data.
* `process_ufs()`: generic UF-level processor.
* All `process_*()` functions standardise column names (original →
  padrão), convert types, and add metadata attributes.

## New: S3 classes

* `vigiar_tbl`: base class for all processed VIGIAR data.
* `vigiar_pm25`, `vigiar_air_quality`: air quality data.
* `vigiar_health`: health indicators.
* `vigiar_population`: population data.
* `vigiar_attributable_fraction`: attributable fractions.
* `vigiar_indoor`: indoor exposure.
* `vigiar_municipios`: municipality registry.
* `vigiar_uf`: UF-level data.
* Methods: `print()`, `summary()`, `validate()` for `vigiar_tbl`.

## New: Validation functions

* `vigiar_padronizar_colunas()`: standardise column names using the
  internal dictionary.
* `vigiar_validar_ibge()`: validate IBGE municipality codes (110001–530010).
* `vigiar_validar_datas()`: validate year/month ranges.
* `vigiar_validar_unidades()`: validate PM2.5 values (0–1000 µg/m³).

## New: Variable dictionary

* `vigiar_dicionario()`: complete variable dictionary as a tibble.
* `vigiar_variaveis()`: list variables for a data domain.
* `vigiar_descrever_variavel()`: describe a single variable.
* `vigiar_convencoes()`: open conventions documentation.
* `vigiar_schema()`: schema overview for a domain.
* `inst/extdata/vigiar_variable_dictionary.csv`: bundled dictionary.
* `data-raw/dictionary.R`: dictionary builder script.

## New: Vignettes

* `variaveis-vigiar.Rmd`: variable dictionary documentation.
* `fluxo-download-processamento.Rmd`: download → process → validate pipeline.

## New: pkgdown

* `_pkgdown.yml`: complete site structure.
* Reference organised by: Session, Raw Download, Processing, Validation,
  Dictionary, S3 Classes.

## Documentation

* README updated with download → process → validate pipeline.
* Column name mapping table added to vignettes.
* Comparison with microdatasus architecture documented.

# vigiar 0.1.0

## New features

* `vigiar_conectar()`: anonymous Power BI session with retry logic.
* `vigiar_baixar()`: download a single table → tibble.
* `vigiar_baixar_tudo()`: download all 32 tables.
* `vigiar_baixar_principais()`: shortcut for 14 key tables.
* `vigiar_info()`: table catalogue with descriptions and categories.
* `vigiar_esquema()`: column names and R types.
* `vigiar_tabelas()`: list available tables.
* `vigiar_status()`: dashboard availability check.
* `vigiar_checar_dados()`: data quality diagnostics (NAs, duplicates).
* `vigiar_diagnostico()`: sample + diagnose all tables.
* `vigiar_desconectar()` / `vigiar_sessao_ativa()`: session lifecycle.

## Technical

* Full DSR (Data Shape Response) parser: DM0 array, ValueDicts,
  R‑based row compression, gzip streaming decompression.
* Retry with exponential backoff for transient HTTP failures.
* Cookie extraction robust to multi‑header `set-cookie`.
* Internal `%||%` and `uuid_v4()` utilities.
* Online tests guarded by `VIGIAR_RUN_ONLINE_TESTS` env var.
* CI/CD: GitHub Actions for R‑CMD‑check, coverage, and lint.

## Documentation

* Comprehensive README with table catalogue.
* Vignette with examples per data category.
* CONTRIBUTING.md, CODE_OF_CONDUCT.md, SECURITY.md.
* CITATION.cff, codemeta.json.
* AI_USE_DECLARATION.md.

## Dependencies

* R ≥ 4.0.0
* httr2, jsonlite, tibble
* Suggests: testthat, dplyr, ggplot2, sf, knitr, rmarkdown, withr, cli
