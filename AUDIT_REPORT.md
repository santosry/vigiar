# Technical and Scientific Audit Report

## Executive summary

This audit started from commit
`b01554229c9dd5a153c1dfbe9d92ce224bbd4bce` on branch
`harden-rj-completeness`. The implementation audited online is commit
`8c80af865fe3632a9664eb2cba283546853a5ca5`.

The package now treats download success, spatial coverage, expected temporal
domain, panel completeness, schema compatibility, parser integrity, response
truncation, verification, and overall audit status as separate evidence. An
unknown or unverifiable state is not promoted to a pass.

The strict live audit on 2026-07-12 used an explicit January 2010 through
December 2024 expected domain. `df_anual`, `df_mensal`, `df_dias`, and `pop`
all returned all 92 RJ municipalities in every expected table-grain group. The
four tables had `parser_status = pass`, `schema_status = pass`,
`truncation_status = no_evidence`, zero incomplete groups, and conclusion
`complete`. This is evidence of completeness for the declared municipality-time
grid at that timestamp. It is not evidence of causal validity, exposure
measurement validity, model adequacy, or future source stability.

The package is **READY FOR RELEASE CANDIDATE** after the current branch passes
the remote platform matrix. It is not classified as ready for a stable release
because the source is an external, reverse-engineered Power BI endpoint without
an availability or schema-stability guarantee, and because the repository still
has non-blocking static-style debt described below.

## Scope

The audit covered:

- `main`, draft PR #1, and branch `harden-rj-completeness`;
- package metadata, exports, S3 return contracts, documentation, and pkgdown;
- Power BI connection, query construction, filters, DSR parser, and downloads;
- RJ municipality registry, IBGE normalization, official fixtures, and joins;
- spatial coverage, temporal domains, panel completeness, and diagnostics;
- schema locks, checksums, snapshots, cache keys, logs, and provenance;
- compliance, audit, health checks, benchmark semantics, and API status;
- offline, adversarial, property-style, optional online, and CI tests;
- GitHub Actions permissions, coverage enforcement, online canary, and Pages
  deployment boundaries.

Primary implementation areas were `R/`, `tests/testthat/`, `.github/workflows/`,
`inst/extdata/`, `data-raw/`, `README.Rmd`, vignettes, `_pkgdown.yml`, and
software metadata files.

## Initial findings

1. RJ compliance could pass a dataset containing all 92 RJ municipalities plus
   an external municipality because checks used incompatible `valido`, `ok`, and
   `passed` conventions.
2. Table completeness could be calculated only from observed periods, allowing
   an entirely absent internal or boundary year/month to disappear from the
   expected grid.
3. Overall 92/92 spatial coverage could be conflated with municipality-by-time
   panel completeness.
4. Numeric-range validation could not prove that an IBGE municipality existed,
   and a seven-digit code could be accepted without validating its official
   correspondence.
5. The generic downloader implicitly scoped data to RJ, and numeric `cod_uf`
   fields could be compared with the string `RJ`.
6. Conflicting user and internal server-side filters were not represented by a
   single explicit conflict policy.
7. The historical ascending/descending strategy could be described more
   strongly than the evidence supported.
8. Truncation evidence could be assessed after a local geographic filter,
   allowing the filter to hide the raw response size signal.
9. Parser assumptions, compact rows, dictionary indices, and response envelopes
   did not have a sufficiently explicit failure contract.
10. Canonical checksum semantics, cache provenance, and snapshot component
    hashes were not versioned strongly enough for scientific reuse.
11. Audit, compliance, health, coverage, and completeness objects lacked one
    consistent programmatic assurance vocabulary.
12. The online test was a smoke test rather than a release-grade evidence
    producer, and coverage did not have an enforced threshold.
13. Workflow permissions and software metadata required synchronization.
14. Narrative language and scientific claims were inconsistent.
15. During final live validation, the critical `pop` schema lock expected
    `numeric` while the live source declared `integer`.
16. During the first strict live audit, valid Power BI inline strings after the
    100-value dictionary cap were incorrectly treated as malformed dictionary
    indices and replaced with `NA`.
17. A cached non-strict RJ result could be returned before strict parser,
    truncation, and coverage checks.
18. Repository-only metadata tests failed inside the installed-package process
    used by `covr`.
19. `vigiar_validar_codigo_municipio()` was documented but missing from the
    pkgdown reference index.
20. A legacy standalone validation runner in `inst/extdata/testar_vigiar.R`
    duplicated `testthat` and the release audit script.

## Changes implemented

### Geographic and IBGE integrity

- Six-digit IBGE municipality codes are the internal canonical representation.
- Official seven-digit codes are retained for interoperability.
- Format normalization, structural validity, official existence, seven-digit
  correspondence, and RJ membership are evaluated separately.
- A versioned national IBGE fixture contains 5,571 municipalities and exactly
  92 RJ municipalities. Its SHA-256 is
  `a86e57e34937627513142a21cde03ed0556bfddf7014420d4e6994825dbe07a1`.
- SES-RJ fixtures lock the nine official health regions.
- Sentinel tests cover Campos dos Goytacazes (`330100`/`3301009`, Norte),
  Italva (`330205`/`3302056`), Volta Redonda (`330630`/`3306305`), and rejection
  of nonexistent `330033`.
- `regiao_saude` is canonical. `macrorregiao_saude` remains a compatibility
  alias and is not presented as a distinct official grouping.

### Download and query behavior

- `vigiar_baixar()` is geographically generic with `uf = NULL`.
- `vigiar_baixar_rj()` applies explicit RJ server-side filters when supported
  and verifies the returned municipality codes against the 92-row registry.
- `vigiar_baixar_municipio()` builds a real municipality server-side filter
  when the schema supports one and records whether local subsetting was needed.
- Numeric and character UF codes are normalized correctly.
- Duplicate filters are combined by intersection; conflicting filters fail
  before the request.
- Requested scope, raw response rows, returned rows, filters, limit, strategy,
  schema hash, timestamp, verification, parser, and truncation evidence are
  preserved as metadata.
- The old ascending/descending approach is named `two_ended_sample` and does
  not claim complete retrieval.

### Completeness and audit semantics

- Coverage, temporal-domain status, panel status, schema status, parser status,
  response status, truncation status, verification status, and overall status
  are independent fields.
- Expected years, months, and period endpoints can be user-specified.
- Observed, inferred, user-specified, official, and not-applicable domains are
  not conflated.
- Missing internal and boundary periods are materialized in the expected grid.
- `df_anual` and `pop` use municipality by year; `df_mensal` uses municipality
  by year by month; `df_dias` and `df_dias_conama` use month when available.
- `require_complete = TRUE` rejects partial panels, missing municipality fields,
  parser issues, schema failures, and any possible/probable/confirmed truncation.
- Audit conclusions include `complete`, `complete_within_inferred_domain`,
  `complete_within_observed_domain`, `partial`, `truncated`, `schema_changed`,
  `schema_unverified`, and `failed`.
- Plain `complete` requires an explicit expected domain, complete expected grid,
  passing critical schema, passing parser structure, and no truncation evidence.

### Parser and response integrity

- DSR envelopes, descriptors, phases, compact rows, repeat masks, null masks,
  dictionaries, row widths, and type conversions are validated.
- Randomized repeat-mask tests and malformed-response tests exercise the parser.
- Valid hybrid dictionary encoding is supported: integer dictionary indices and
  later inline strings can coexist after the Power BI dictionary cap.
- Real structural issues are retained in `vigiar_parser_status`,
  `vigiar_parser_issue_count`, and `vigiar_parser_issues`.
- Parser evidence survives client-side filtering, processing, caching, and
  audit serialization. Structural issues block strict completeness.

### Truncation, cache, snapshots, and provenance

- Truncation is represented as `no_evidence`, `possible`, `probable`,
  `confirmed`, or `unknown`, with evidence.
- Raw response evidence is captured before client-side filtering.
- Requested limits, response metadata, continuation signals, and a labeled
  Power BI row-count heuristic contribute to the assessment.
- Cache keys include query parameters, geographic scope, schema hash, package
  version, cache-key version, and canonicalization version.
- Cache reads verify checksum and provenance. Strict cache hits rerun parser,
  truncation, and RJ coverage assertions.
- Canonical checksum version 2 preserves full numeric precision and defines
  canonical versus ordered table identity.
- Snapshots store data, schema, and metadata checksums separately.

### CI, security, metadata, and documentation

- A 70% coverage gate writes Cobertura XML, RDS, JSON, and text evidence.
- A separate scheduled/dispatchable online canary archives RJ audit artifacts.
- Check, lint, and coverage workflows use `contents: read`; Pages write and OIDC
  permissions remain limited to the deployment job.
- DESCRIPTION, CITATION, CFF, codemeta, README, and NEWS metadata are aligned at
  version `0.7.1.9000` and covered by tests.
- `vigiar_api_status()` classifies stable, experimental, and deprecated exports.
- Technical English is the narrative language; historical Portuguese function,
  argument, and field identifiers remain for compatibility.
- README and vignettes distinguish downloaded, filtered, covered, complete,
  verified, audited, and reproducible.
- The obsolete standalone validation runner was removed.

## Bugs fixed

### External municipality false pass

**Problem:** 92 RJ municipalities plus one external municipality could pass the
RJ compliance profile.
**Risk:** A geographically contaminated dataset could be approved for analysis.
**Reproduction:** Add one SP code to the complete RJ registry and run the RJ
profile.
**Root cause:** Incompatible result fields and permissive aggregation.
**Fix:** Normalize checks to explicit `ok`, `status`, `severity`, and `details`;
aggregate conservatively.
**Regression test:** The adversarial 92 RJ plus one external case must fail.

### Entirely missing period not detected

**Problem:** A month or year absent from all rows could be absent from the audit
grid.
**Risk:** A discontinuous panel could be called complete.
**Reproduction:** Remove every March row from an otherwise complete monthly
panel.
**Root cause:** The grid was based only on observed combinations.
**Fix:** Build the grid from explicit or inferred expected domains and retain
domain provenance.
**Regression test:** Internal, first, last, annual, monthly, invalid, and missing
period cases are tested.

### Geographic defaults and UF code mismatch

**Problem:** Generic downloads silently targeted RJ, and `cod_uf = 33` could be
compared with `RJ`.
**Risk:** Unexpected scope and empty filters.
**Reproduction:** Call the generic downloader without `uf`, or filter a numeric
UF-code column.
**Root cause:** Scope and field semantics were conflated.
**Fix:** Default to no geographic filter and use type-aware UF literals.
**Regression test:** Numeric, character, factor, abbreviation, and conflicting
filter cases are covered.

### Invalid official municipality acceptance

**Problem:** Numeric shape or range could be mistaken for official existence.
**Risk:** False municipality joins and incorrect RJ membership.
**Reproduction:** Use `330033` or a seven-digit code with the wrong final digit.
**Root cause:** No national official reference lookup.
**Fix:** Validate against the versioned IBGE reference and exact 6/7 mapping.
**Regression test:** Valid, nonexistent, malformed, mixed, and wrong-check-code
vectors are covered.

### Truncation evidence erased by filtering

**Problem:** A large raw response could become small after local RJ filtering.
**Risk:** The filtered result could lose the upstream response-limit signal.
**Reproduction:** Return 28,500 mixed rows and retain one RJ row.
**Root cause:** Truncation assessment occurred after local filtering.
**Fix:** Assess and preserve raw evidence before any local filter.
**Regression test:** The filtered one-row result remains marked with raw
truncation evidence.

### Power BI dictionary-cap false error

**Problem:** Inline strings after the 100-value DSR dictionary cap were treated
as non-numeric dictionary indices and replaced with `NA`.
**Risk:** Silent corruption of high-cardinality text fields and an invalid
strict audit pass despite parser warnings.
**Reproduction:** Download live `df_mensal` or `df_dias`; inspect dictionary
columns after the first 100 unique values.
**Root cause:** The parser assumed every dictionary-declared value must always be
an integer index.
**Fix:** Accept scalar inline strings as the documented observed hybrid form;
retain errors for invalid indices, shapes, masks, and widths.
**Regression test:** Mixed indexed and inline values decode without warning;
malformed dictionaries still fail or record issues.

### Parser evidence not enforced

**Problem:** Parser warnings were not part of strict audit conclusions.
**Risk:** `require_complete = TRUE` could succeed after structural parsing
issues.
**Reproduction:** Attach a parser issue to an otherwise complete 92-row panel.
**Root cause:** Parser attributes were not preserved through processing or
included in the audit contract.
**Fix:** Preserve parser evidence end to end and classify structural issues as
`failed`.
**Regression test:** Direct strict download, cache hit, JSON/RDS artifact, and
online audit paths reject parser issues.

### Stale population schema type

**Problem:** The lock expected `pop` as numeric while the live Power BI schema
declared integer.
**Risk:** A false schema-change conclusion.
**Reproduction:** Run the online audit against `pop`.
**Root cause:** The fixture did not match the current source schema.
**Fix:** Lock the raw source field as integer; processing still standardizes the
analysis field to numeric.
**Regression test:** Critical lock tests and the repeated live strict audit pass.

### Strict cache bypass

**Problem:** A valid non-strict cache hit returned before strict assertions.
**Risk:** Old parser or truncation evidence could bypass `require_complete`.
**Reproduction:** Cache a 92-row object with parser issues, then request strict
completeness.
**Root cause:** Early return on cache hit.
**Fix:** Revalidate parser, truncation, and 92/92 coverage before strict cache
return.
**Regression test:** Strict cache-hit parser bypass is rejected.

### Installed-test path failures

**Problem:** Metadata and language tests used repository-relative paths that do
not exist in the installed package used by `covr`.
**Risk:** Coverage failed even though direct tests passed.
**Reproduction:** Run `covr::package_coverage(type = "tests")`.
**Root cause:** Test execution context was assumed to be the checkout.
**Fix:** A shared helper locates the source root via an explicit inherited
environment value and skips only repository-only files when they are genuinely
not installed.
**Regression test:** Metadata tests pass directly and under the coverage gate.

## Scientific integrity assessment

### Spatial completeness

RJ membership is verified by official municipality codes, not name matching or
numeric ranges. The strict live audit found 92/92 municipalities and no external
codes in all four audited tables. Spatial presence does not by itself prove
time-panel completeness.

### Temporal and panel completeness

The package materializes explicit expected domains. The strict live audit used
2010-2024 and months 1-12 where applicable. It found 15 complete annual groups
for `df_anual`, 180 complete year-month groups for `df_mensal`, 180 for
`df_dias`, and 15 complete year groups for `pop`, with zero missing groups.
`pop` has multiple category rows per municipality-year; the audit verifies
municipality presence at that grain, not a universal category cardinality.

### IBGE validation

The national fixture is versioned, checksummed, and sourced from the IBGE
Localidades API. Six- and seven-digit forms must map exactly. Official fixture
freshness is auditable but must still be refreshed deliberately when IBGE
changes municipal codes.

### Power BI truncation and parser integrity

No truncation evidence was observed in the strict run. This is not a guarantee
that the external service can never truncate future responses. Parser structure
passed with zero issues after the hybrid-dictionary correction. Any future
parser issue or truncation evidence blocks strict completeness.

### Data provenance

Audit artifacts include timestamp, package version, commit SHA, query scope,
schema hash, canonical data checksum, dimensions, expected domain, missing
municipalities, health-region gaps, parser evidence, and truncation evidence.
Release validation artifacts should be attached to the corresponding release.

### Audit and compliance semantics

Checks use conservative categorical contracts. The precedence is fail over
unknown over pass; not-applicable is explicit. Console text is secondary to
structured fields. A passing data audit does not certify an epidemiologic model
or scientific conclusion.

## Test evidence

Local platform: Windows 11 x64, R 4.6.0, UTF-8 session.

| Check | Result |
|---|---|
| `devtools::document()` | Passed; generated Rd synchronized |
| `devtools::test()` with online disabled | 738 passed, 1 expected online skip, 0 failed, 0 warnings |
| Strict online test | 58 passed, 0 skipped, 0 failed, 0 warnings |
| Coverage gate | 75.17%, required 70.00%, passed |
| `pkgdown::build_site()` | Passed, including all reference pages, six offline articles, and this report |
| `devtools::check(error_on = "never", args = character())` | 0 errors, 0 warnings, 0 notes |
| `lintr::lint_package()` | Exited successfully; 288 diagnostics remain, mostly cross-file usage and inherited style rules |
| Metadata consistency | 13 assertions passed |
| Parser adversarial/property tests | Passed, including hybrid dictionary and malformed-response cases |
| GitHub matrix | Configured for Windows release, macOS release, Ubuntu release, devel, and oldrel; current branch result must be read from PR checks |

Strict online evidence for commit
`8c80af865fe3632a9664eb2cba283546853a5ca5`:

| Table | Rows | Expected groups | Incomplete | Municipalities | Parser | Schema | Truncation | Conclusion |
|---|---:|---:|---:|---:|---|---|---|---|
| `df_anual` | 1,380 | 15 | 0 | 92/92 | pass | pass | no_evidence | complete |
| `df_mensal` | 16,560 | 180 | 0 | 92/92 | pass | pass | no_evidence | complete |
| `df_dias` | 16,560 | 180 | 0 | 92/92 | pass | pass | no_evidence | complete |
| `pop` | 4,140 | 15 | 0 | 92/92 | pass | pass | no_evidence | complete |

The local artifact directory is ignored by Git by design. The scheduled online
workflow archives the equivalent CSV, JSON, and RDS evidence as a workflow
artifact. A formal release should archive one strict run with the release.

## Remaining limitations

1. The Power BI DSR format and public endpoint are reverse-engineered and have
   no official stability or availability contract.
2. A strict result proves the declared expected grid at one timestamp. It does
   not prove that the public source itself contains every scientifically desired
   measure or that future downloads will be identical.
3. `response_completeness_status` remains separate from grid completeness;
   absence of truncation evidence is not proof that the server never omitted an
   undocumented dimension.
4. Partitioned complete download is only possible where validated server-side
   fields exist. The package fails honestly where it cannot prove partitioning.
5. Critical schema locks cover analysis-critical columns, not every dashboard
   presentation field. Extra noncritical fields may change without invalidating
   critical compatibility.
6. The official IBGE and SES-RJ fixtures are snapshots and require periodic,
   reviewed refreshes.
7. PM2.5 may be modelled or estimated. The package does not validate exposure
   measurement error, causal inference, GAM, DLNM, relative risk, machine
   learning, prediction, or epidemiologic conclusions.
8. The current lint workflow reports but does not fail on its 288 diagnostics.
   Most are inherited style or static cross-file false positives, but this debt
   should be handled in a dedicated non-behavioral cleanup before a stable
   release rather than mixed into integrity fixes.
9. Remote multi-platform CI is authoritative for Linux and macOS behavior and
   must be green on the published branch before merge.
10. No GitHub release was created by this audit. Release artifacts therefore
    cannot yet be attached to a release, by design.

## Breaking changes

- `vigiar_baixar()` no longer defaults to RJ; its default is `uf = NULL`.
- Strict completeness now errors on parser uncertainty, truncation evidence,
  missing expected domains, schema incompatibility, or incomplete RJ coverage.
- User-facing narrative messages are technical English.
- `regiao_saude` is the canonical field; `macrorregiao_saude` is a compatibility
  alias.
- Canonical checksum version 2 changes stored checksum identity.
- Benchmark strategy `year_asc_desc` is deprecated in favor of
  `two_ended_sample`.

## Migration guide

1. Replace implicit-RJ calls with `vigiar_baixar_rj()` or pass `uf = "RJ"`
   explicitly to the generic downloader.
2. Supply `anos_esperados`, `meses_esperados`, or period endpoints for formal
   strict audits.
3. Read structured status fields instead of parsing console messages.
4. Regenerate old checksums and snapshots with canonicalization version 2.
5. Prefer `regiao_saude`; retain the old alias only for compatibility.
6. Replace any completeness claim based only on `nrow() > 0` or overall 92/92
   presence with `vigiar_rj_completude_tabela()` or
   `vigiar_auditar_rj_online()`.
7. Archive the generated strict CSV/JSON/RDS bundle with each formal release.

## Security review

- Read-only workflows use least-privilege `contents: read` permissions.
- Pages write and OIDC permissions are restricted to the deploy job.
- Logs redact authorization values, cookies, resource keys, tokens, and URL
  query strings; regression tests check secret redaction.
- Online artifacts contain schema/checksum/coverage evidence, not session
  cookies or authentication headers.
- The dashboard resource key in the client is a public Power BI embed key, not a
  private repository secret; it is still excluded from exported logs.
- Actions use stable major releases. Pinning every action to an immutable commit
  SHA remains a supply-chain hardening option for a future policy change.

## Reproducibility review

- Canonical data identity is versioned and full-precision.
- Ordered identity remains available when row/column order is scientifically
  relevant.
- Snapshots store data, schema, metadata, package, R, platform, timestamp, and
  canonicalization evidence.
- Schema locks distinguish equality, compatibility, and critical compatibility.
- Cache keys and reads include provenance and integrity checks.
- Online audits write human-readable CSV plus structured JSON and RDS artifacts.
- The manual `data-raw/check-rj-download-completeness.R` script binds outputs to
  an optional release identifier and refuses strict temporal audits without an
  explicit domain.

## Release recommendation

**READY FOR RELEASE CANDIDATE**, conditional on the published branch passing
the current GitHub Actions matrix.

The package is suitable for a controlled release-candidate evaluation because
the known false passes were removed, strict RJ completeness is programmatic,
the live 2010-2024 audit passed for the four required tables, parser and schema
evidence are enforced, local R CMD check is clean, and reproducibility artifacts
are generated.

The package should not yet be labeled **READY FOR STABLE RELEASE**. A stable
release should additionally require a green remote matrix on the exact release
commit, an archived strict audit artifact attached to that release, at least one
subsequent scheduled canary confirming source stability, and a deliberate
resolution or policy decision for the remaining static-lint diagnostics.
