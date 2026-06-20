---
name: Schema change detection
about: Report a change in the VIGIAR Power BI data schema
title: "[SCHEMA] "
labels: schema, breaking
assignees: ''
---

**What changed?**
Describe which table or column changed.

**Expected schema**
What the dictionary/package expected.

**Actual schema**
What was observed. Run `vigiar_validar_dicionario()` and paste output.

**Affected tables**
List the tables with schema changes.

**Impact**
Does this break any `process_*()` functions? Which ones?

**Environment**
- `R.version.string`
- `packageVersion("vigiar")`
- Date of observation
