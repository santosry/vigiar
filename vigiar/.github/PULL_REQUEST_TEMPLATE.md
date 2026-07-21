## Checklist for Pull Requests

Before submitting a PR, please verify:

- [ ] **Tests**: New functionality has tests. Run `devtools::test()`.
- [ ] **Documentation**: New functions have Rd documentation.
- [ ] **NEWS.md**: Updated with changes under the appropriate version.
- [ ] **R CMD check**: `devtools::check()` passes with 0 ERRORs and 0 WARNINGs.
- [ ] **Offline tests**: All offline tests pass without internet.
- [ ] **Code style**: Follows existing conventions (snake_case, internal prefix `.vigiar_`).
- [ ] **No new dependencies**: If a new package is needed, it was discussed in an issue first.
- [ ] **ASCII only**: R code uses ASCII characters only.

## Description of Changes

<!-- Describe what this PR does -->

## Type of Change

- [ ] Bug fix
- [ ] New feature
- [ ] Documentation
- [ ] Test coverage
- [ ] Refactoring (no functional change)
- [ ] CI/CD / infrastructure
