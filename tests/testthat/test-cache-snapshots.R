# vigiar: reproducibility snapshots and local cache

# ── Snapshots ────────────────────────────────────────────────────────────────

test_that("vigiar_snapshot captures provenance and verifies its checksum", {
  .vigiar_local_reset()
  dados <- tibble::tibble(a = 1:3, b = letters[1:3])

  snap <- suppressMessages(vigiar_snapshot(dados, tabela = "teste"))

  expect_s3_class(snap, "vigiar_snapshot")
  expect_equal(snap$tabela, "teste")
  expect_equal(snap$n_rows, 3L)
  expect_equal(snap$n_cols, 2L)
  expect_equal(snap$colunas, c("a", "b"))
  expect_true(nzchar(snap$checksum_sha256))
  expect_true(suppressMessages(vigiar_verificar_snapshot(snap)))

  snap$dados$a[1] <- 999L
  expect_false(suppressMessages(vigiar_verificar_snapshot(snap)))
})

test_that("vigiar_snapshot validates its inputs", {
  .vigiar_local_reset()
  expect_error(vigiar_snapshot(), "Forneca 'dados' ou 'tabela'")
  expect_error(vigiar_snapshot(tabela = "df_anual"), "Nenhuma sessao ativa")
})

test_that("print.vigiar_snapshot reports metadata", {
  .vigiar_local_reset()
  snap <- suppressMessages(vigiar_snapshot(tibble::tibble(a = 1), tabela = "t"))
  expect_message(print(snap), "Snapshot")
})

test_that("snapshots round-trip through disk", {
  .vigiar_local_reset()
  snap <- suppressMessages(vigiar_snapshot(tibble::tibble(a = 1:2), tabela = "t"))
  path <- file.path(tempdir(), "snap-test")
  unlink(paste0(path, ".rds"))

  saved <- suppressMessages(vigiar_salvar_snapshot(snap, path))
  expect_true(file.exists(saved))

  loaded <- suppressMessages(vigiar_carregar_snapshot(saved))
  expect_s3_class(loaded, "vigiar_snapshot")
  expect_equal(loaded$checksum_sha256, snap$checksum_sha256)

  expect_error(
    vigiar_salvar_snapshot(snap, path),
    "ja existe"
  )
  expect_error(
    vigiar_carregar_snapshot(file.path(tempdir(), "nao-existe.rds")),
    "nao encontrado"
  )
})

test_that("vigiar_comparar_snapshots reports differences", {
  .vigiar_local_reset()
  s1 <- suppressMessages(vigiar_snapshot(tibble::tibble(a = 1:2), tabela = "t"))
  s2 <- suppressMessages(vigiar_snapshot(tibble::tibble(a = 1:3, b = 1), tabela = "t"))

  diffs <- suppressMessages(vigiar_comparar_snapshots(s1, s2))
  expect_equal(diffs$n_rows, 1L)
  expect_true(isTRUE(diffs$n_cols))
  expect_true("b" %in% diffs$cols_added)
  expect_true(isTRUE(diffs$checksum_changed))

  iguais <- suppressMessages(vigiar_comparar_snapshots(s1, s1))
  expect_false(isTRUE(iguais$checksum_changed))
})

# ── Cache directory ──────────────────────────────────────────────────────────

test_that("vigiar_cache_dir sets, returns and creates the directory", {
  .vigiar_local_reset()
  d <- file.path(tempdir(), "vigiar-cache-dir")
  unlink(d, recursive = TRUE)

  expect_message(vigiar_cache_dir(d), "Cache VIGIAR")
  expect_true(dir.exists(d))
  expect_equal(vigiar_cache_dir(), d)
})

test_that("vigiar_cache_info reports an empty cache", {
  .vigiar_local_reset()
  d <- file.path(tempdir(), "vigiar-cache-empty")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  .vigiar_env$cache_dir <- d

  info <- suppressMessages(vigiar_cache_info())
  expect_s3_class(info, "tbl_df")
  expect_equal(nrow(info), 0L)
  expect_named(info, c("tabela", "linhas", "colunas", "idade", "checksum"))
})

test_that("vigiar_baixar_com_cache stores, reuses and refreshes data", {
  .vigiar_local_reset()
  .vigiar_env$sessao <- .vigiar_fake_sessao()
  d <- file.path(tempdir(), "vigiar-cache-download")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  .vigiar_env$cache_dir <- d

  calls <- 0L
  local_mocked_bindings(
    vigiar_baixar = function(tabela, ...) {
      calls <<- calls + 1L
      tibble::tibble(x = 1:3)
    },
    .package = "vigiar"
  )

  r1 <- suppressMessages(vigiar_baixar_com_cache("df_anual"))
  expect_equal(calls, 1L)
  expect_equal(nrow(r1), 3L)

  r2 <- suppressMessages(vigiar_baixar_com_cache("df_anual"))
  expect_equal(calls, 1L) # served from cache
  expect_equal(r1, r2)

  suppressMessages(vigiar_baixar_com_cache("df_anual", refresh = TRUE))
  expect_equal(calls, 2L)

  info <- suppressMessages(vigiar_cache_info())
  expect_equal(nrow(info), 1L)
  expect_equal(info$tabela, "df_anual")

  removed <- suppressMessages(vigiar_limpar_cache("df_anual"))
  expect_equal(removed, 1L)
})

test_that("vigiar_baixar_com_cache errors without a session", {
  .vigiar_local_reset()
  expect_error(vigiar_baixar_com_cache("df_anual"), "Nenhuma sessao ativa")
})

test_that("vigiar_limpar_cache is a no-op without a configured cache", {
  .vigiar_local_reset()
  expect_message(res <- vigiar_limpar_cache(), "Cache nao configurado")
  expect_null(res)
})

# ── Schema lock ──────────────────────────────────────────────────────────────

test_that("schema lock round-trips and detects changes", {
  .vigiar_local_reset()
  .vigiar_env$esquema <- .vigiar_fake_esquema()
  d <- file.path(tempdir(), "vigiar-lock")
  unlink(d, recursive = TRUE)
  dir.create(d, recursive = TRUE)
  lock_path <- file.path(d, "lock.json")

  lock <- suppressMessages(vigiar_esquema_lock(lock_path))
  expect_s3_class(lock, "vigiar_schema_lock")
  expect_true(file.exists(lock_path))

  loaded <- vigiar_esquema_carregar_lock(lock_path)
  expect_s3_class(loaded, "vigiar_schema_lock")
  expect_equal(loaded$tabelas, names(.vigiar_fake_esquema()))

  diffs <- suppressMessages(vigiar_esquema_verificar(lock_path))
  expect_length(diffs, 0)

  # Mutate the live schema (new table + new column) and re-verify.
  .vigiar_env$esquema$nova_tabela <- list(x = list(nome = "x", tipo = "numeric"))
  .vigiar_env$esquema$df_anual$extra <- list(nome = "extra", tipo = "numeric")

  diffs2 <- suppressMessages(vigiar_esquema_verificar(lock_path))
  expect_true("nova_tabela" %in% diffs2$new_tables)
  expect_true("df_anual" %in% names(diffs2$col_changes))

  # Also accepts a lock object directly.
  diffs3 <- suppressMessages(vigiar_esquema_verificar(loaded))
  expect_true("nova_tabela" %in% diffs3$new_tables)
})

test_that("schema lock helpers validate inputs", {
  .vigiar_local_reset()
  expect_error(vigiar_esquema_lock(), "Nenhuma sessao ativa")
  expect_error(vigiar_esquema_verificar(), "Nenhuma sessao ativa")
  expect_error(
    vigiar_esquema_carregar_lock(file.path(tempdir(), "nope.json")),
    "nao encontrado"
  )
})
