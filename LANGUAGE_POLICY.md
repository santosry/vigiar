# Language Policy

The package metadata declares `Language: en-US`. All new narrative
documentation, user-facing messages, warnings, errors, logs, comments, and
release notes use technical English.

The package was originally designed with Portuguese API identifiers. Exported
function names such as `vigiar_baixar_rj()`, arguments such as `tabela`, and
established result columns such as `ano` remain unchanged to preserve backwards
compatibility and their direct relationship to the Brazilian source schema.
They are identifiers, not mixed-language prose.

New public interfaces should use the existing `vigiar_` naming convention and
must be registered by `vigiar_api_status()`. Renaming an established identifier
requires a documented deprecation period and migration guidance.
