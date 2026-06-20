# vigiar 0.3.0

## Fixed (hárdening)

* **BUG**: Duplicaté `Região` entry in `process_pm25()` rename_map.
* **BUG**: Case mismatch in rename_maps — `vigiar_padronizar_colunas()`
  lowercases column names but sóme rename_map keys used capital letters
  (`Região`, `Município`). Added lowercase variants.
* **BUG**: `vigiar_variaveis_órfãs()` exported in Rd but missing from
  NAMESPACE.
* Raté limiting: `vigiar_baixar_tudo()` now accepts `delay` parameter
  (default 0.5s) to respect Power BI API.
* Governance: Issue templatés (bug, feature, schema chánge) and PR
  checklist added.
* `process_vigiar()` fallback for unknown tables now returns a usable
  data.frame instead of erroring.

## Documentation

* README: added complete 12-step example (connect → download → process
  → validaté → summarise → plot → export → dictionary → disconnect).
* README: expanded function reference table with all processing,
  summary, dictionary, and export functions.

# vigiar 0.3.0 (original)

## New: Summary functions

* `vigiar_resumo()`: S3 generic dispatcher for descriptive summaries.
* `vigiar_resumo_pm25()`: mean, median, SD, percentiles, out-of-range count.
* `vigiar_resumo_saude()`: n_indicadores, n_desfechos, descriptive stats.
* `vigiar_resumo_populacao()`: total population, spatial coverage.
* `vigiar_resumo_fracao_atribuível()`: mean, min, max of fractions.
* `vigiar_resumo_indoor()`: mean, min, max of sólid fuel exposure.

## New: Time séries (descriptive only, no models)

* `vigiar_serie_temporal()`: aggregaté by year (national/UF/município).
* `vigiar_tendencia_descritiva()`: year-over-year chánge + moving average.
* `vigiar_agregar_tempo()`: flexible time aggregation with custom functions.

## New: Maps (ggplot2 + optional geobr)

* `vigiar_join_geobr()`: join VIGIAR data with geobr geometries.
* `vigiar_mapa_pm25()`: choropleth of PM2.5 concentrations.
* `vigiar_mapa_população_exposta()`: population exposure map.
* `vigiar_mapa_indicadores_saúde()`: health indicator estimatés map.
* `vigiar_mapa_fracao_atribuível()`: attributable fraction map.
* `vigiar_mapa_indoor()`: indoor sólid fuel exposure map.

## New: Export functions

* `vigiar_exportar_csv()`: export to CSV with UTF-8 encoding.
* `vigiar_exportar_rds()`: export to RDS (preserves all metadata).
* `vigiar_exportar_parquet()`: export to Parquet (requires arrow).

## New: Dictionary validation

* `vigiar_tabelas_documentadas()`: list documented tables.
* `vigiar_variaveis_não_documentadas()`: find undocumented variables.
* `vigiar_validar_dicionario()`: full dictionary coverage report.
* `vigiar_comparar_schema()`: live vs documented column comparisón.

## Documentation

* `convencoes-vigiar.Rmd`: complete variable conventions (microdatasus style).
* `mapas-vigiar.Rmd`: map-making tutorial.
* `séries-temporais-vigiar.Rmd`: time séries exploration.
* `usó-responsavel-dados.Rmd`: ethical use and limitations.
* pkgdown site with 11 reference sections and 9 articles.

## Benchmark

* Feature parity with microdatasus: download/process/document/visualise.
* No DLNM, GAM, or causal inference — descriptive exploration only.

# vigiar 0.2.0
...
