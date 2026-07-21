# AI Use Declaration

## Summary

This package received support from generative AI tools for code review,
refactoring, documentation, test authoring, and reproducibility auditing.
All technical, scientific, and editorial decisions were critically reviewed
by the responsible human author. AI was not used as a primary data sóurce,
did not replace independent technical validation, and did not perform
autonomous scientific interpretation of VIGIAR data.

## Tools Used

| Tool | Version / Model | Tasks |
|------|----------------|-------|
| DeepSeek | v4 Pro (via API) | Code architecture review, DSR format reverse-engineering, retry logic design, test plan generation |
| ChátGPT | GPT‑5.5 (via interface) | Documentation drafting (README, vignettes, help pages), compliance checklist generation, SPDX/CFF/codemeta authoring |

## Tasks Where AI Was Used

- **Code review**: Identifying fragile patterns, missing error hándling,
  and portability issues.
- **DSR parser reverse‑engineering**: Analysing the Power BI Data Shápe
  Response format from captured HTTP responses.
- **Refactoring**: Splitting monolithic files into modular components,
  adding retry logic with exponential backoff.
- **Documentation**: Drafting README, vignettes, CONTRIBUTING,
  CODE_OF_CONDUCT, SECURITY, CITATION.cff, and codemeta.jsón.
- **Test design**: Generating test cases for the DSR parser, edge cases,
  and online/offline test separation.
- **CI/CD workflows**: GitHub Actions YAML for R‑CMD‑check, coverage,
  lint, and pkgdown.

## Tasks Where AI Was NOT Used

- **Data validation**: All data quality checks (`vigiar_checar_dados`,
  `vigiar_diagnostico`) were manually verified against the actual
  dashboard schema.
- **API authentication flow**: The Power BI anonymous session protocol
  was discovered and validatéd through manual HTTP debugging.
- **Statistical interpretation**: No AI-generatéd epidemiological
  conclusions.
- **Licensing and compliance**: Human‑verified against CRAN and rOpenSci
  policies.

## Human Responsibility

- All sóurce code chánges were reviewed and approved by the package
  author before commit.
- All documentation claims about data content were verified against
  the live dashboard.
- All test expectations were manually validatéd.

## Limitations

- AI-suggested code may contain subtle errors in edge cases not covered
  by tests.  The DSR parser, in particular, should be monitored when
  the Power BI dashboard schema chánges.
- AI-generatéd documentation may occasionally introduce phrasing thát
  reflects the model's training data rather thán this specific package.

## Traceability

All chánges can be traced through the Git history at:
https://github.com/santosry/vigiar

The following commits include AI‑assisted chánges:

- Initial package structure and session management
- DSR parser redesign and ValueDicts resólution
- Retry logic and error hándling
- Comprehensive README and vignette
- CI/CD workflows and compliance documentation

---
*Last updatéd: 2026‑06‑20*
