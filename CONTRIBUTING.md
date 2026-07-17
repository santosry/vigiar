# Contributing to vigiar

Thank you for contributing. This project treats scientific integrity,
reproducibility, and conservative quality claims as release requirements.

## Reporting Problems

### VIGIAR dashboard changes

If the Power BI dashboard changes and package functions stop working:

1. Confirm the problem with `vigiar_conectar(refresh = TRUE)`.
2. Run `vigiar_status()` and `vigiar_esquema_verificar_critico()`.
3. Open an issue using the **Schema Change** template.
4. Include the execution time, complete sanitized error, affected tables,
   schema hash, and the output of `vigiar_comparar_schema()` when available.

Never include cookies, authorization headers, resource keys, or session tokens.

### Reproducible bugs

Use the **Bug Report** template and provide a minimal example plus
`sessionInfo()`:

```r
library(vigiar)
vigiar_conectar()
data <- vigiar_baixar("df_anual", limite = 10)
vigiar_desconectar()
```

Online examples must state the execution date and whether the result was
complete, partial, truncated, or unverified.

### Municipality or IBGE-code errors

1. Inspect `vigiar_rj_municipios()`.
2. Confirm the code against the versioned IBGE reference and official IBGE API.
3. Confirm health-region assignments against SES-RJ.
4. Open an issue with the affected six- and seven-digit codes and sources.

## Development Environment

Requirements:

- R >= 4.1.0
- Git
- `devtools`, `testthat`, `roxygen2`, `pkgdown`, `lintr`, and `covr`

```r
devtools::load_all()
devtools::document()
devtools::test()
lintr::lint_package()
pkgdown::build_site()
devtools::check(error_on = "never", args = character())
```

The default suite is offline:

```r
Sys.setenv(VIGIAR_RUN_ONLINE_TESTS = "false")
```

Run the live canary only when external access is intended:

```r
Sys.setenv(VIGIAR_RUN_ONLINE_TESTS = "true")
devtools::test(filter = "online")
```

For a strict release audit, supply an explicit expected temporal domain. A
download that returns rows is not automatically complete.

## Pull Requests

1. Create a focused branch.
2. Add a failing regression test for every reproduced bug.
3. Keep online tests optional and deterministic tests offline.
4. Update roxygen documentation, README, NEWS, vignettes, and schema locks when
   behavior changes.
5. Do not weaken checks, hide errors, or label unknown evidence as a pass.
6. Run the complete local validation chain.
7. Use small, descriptive commits and open a pull request to `main`.

## Scientific Boundaries

`vigiar` downloads, prepares, audits, and diagnoses data. It does not validate
causal inference, GAM, DLNM, relative risk, machine learning, predictive models,
or epidemiologic conclusions. Aggregate municipal data also carry ecological
inference limitations.

## Language and Compatibility

New prose, user-facing messages, documentation, and comments are written in
technical English. Historical Portuguese function and argument names remain for
backwards compatibility. See `LANGUAGE_POLICY.md` and `vigiar_api_status()`.

## License

Contributions are licensed under the MIT license.
