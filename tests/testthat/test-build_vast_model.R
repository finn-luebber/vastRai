# ============================================================================
# Tests for build_vast_model() — full model construction from specs
# ============================================================================

build_vast_model <- vastRai:::build_vast_model


# --- Basic model building ---

test_that("build_vast_model returns model and code", {
  skip_if_not_installed("vastR")
  spec <- minimal_model_spec()
  result <- build_vast_model(spec)
  expect_type(result, "list")
  expect_true("model" %in% names(result))
  expect_true("code" %in% names(result))
  expect_type(result$code, "character")
})

test_that("build_vast_model produces a valid vast_model object", {
  skip_if_not_installed("vastR")
  spec <- minimal_model_spec()
  result <- build_vast_model(spec)
  expect_s3_class(result$model, "vast_model")
})

test_that("build_vast_model code contains header comments", {
  skip_if_not_installed("vastR")
  spec <- minimal_model_spec("My Title")
  result <- build_vast_model(spec)
  expect_match(result$code, "# VAST Model: My Title")
  expect_match(result$code, "# Naming mode: separated")
})

test_that("build_vast_model handles naming mode from spec", {
  skip_if_not_installed("vastR")
  spec <- minimal_model_spec()
  spec$naming_mode <- "fimm"
  result <- build_vast_model(spec)
  expect_match(result$code, 'naming_mode = "fimm"')
})


# --- Extended model (all node types, groups, IS/OUGHT) ---

test_that("build_vast_model builds extended spec with all node types", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_s3_class(result$model, "vast_model")
  # Code should reference all node types
  expect_match(result$code, "vast_concept")
  expect_match(result$code, "vast_data")
  expect_match(result$code, "vast_diamond")
  expect_match(result$code, "vast_noise_source")
})

test_that("build_vast_model generates IS/OUGHT statements", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_match(result$code, "# IS statements")
  expect_match(result$code, "vast_is")
  expect_match(result$code, "# OUGHT statements")
  expect_match(result$code, "vast_ought")
})

test_that("build_vast_model generates perspective nodes", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_match(result$code, 'holder = "Alice"')
  expect_match(result$code, 'holder = "Bob"')
})

test_that("build_vast_model generates group code", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_match(result$code, "vast_group")
  expect_match(result$code, '"HOC1"')
  expect_match(result$code, '"Weather Effects"')
})

test_that("build_vast_model code includes name comments on concept nodes", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  # Concepts should have their name labels as comments
  expect_match(result$code, '# "Temperature"')
  expect_match(result$code, '# "Ice Cream Sales"')
})

test_that("build_vast_model code includes edge type comments", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_match(result$code, "# causation:")
  expect_match(result$code, "# prediction:")
})

test_that("build_vast_model handles edge index", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_match(result$code, 'index = "1"')
})

test_that("build_vast_model handles noise edges", {
  skip_if_not_installed("vastR")
  spec <- extended_model_spec()
  result <- build_vast_model(spec)
  expect_match(result$code, "vast_noise")
})


# --- IS/OUGHT on groups (Change 3 integration test) ---

test_that("build_vast_model generates lhead for IS on group", {
  skip_if_not_installed("vastR")
  spec <- group_is_ought_spec()
  result <- build_vast_model(spec)
  # IS on HOC1 should use lhead pointing to the group
  expect_match(result$code, 'lhead = "HOC1"')
  # Should target first member "A"
  expect_match(result$code, '"A"')
  # Perspective Tina should still be generated
  expect_match(result$code, 'holder = "Tina"')
})


# --- Compound edges (lhead/ltail on standard and structural edges) ---

test_that("build_vast_model preserves compound edge lhead/ltail", {
  skip_if_not_installed("vastR")
  spec <- compound_edge_spec()
  result <- build_vast_model(spec)
  # Causation edge with lhead
  expect_match(result$code, 'lhead = "HOC1"')
  # Causation edge with ltail
  expect_match(result$code, 'ltail = "HOC1"')
})


# --- Empty/edge-case specs ---

test_that("build_vast_model handles spec with no edges", {
  skip_if_not_installed("vastR")
  spec <- list(
    title = "No edges",
    naming_mode = "fimm",
    nodes = list(list(id = "X", type = "concept", label = "X")),
    edges = list(),
    groups = list(),
    is_statements = list(),
    ought_statements = list()
  )
  result <- build_vast_model(spec)
  expect_s3_class(result$model, "vast_model")
})

test_that("build_vast_model handles spec with no groups", {
  skip_if_not_installed("vastR")
  spec <- minimal_model_spec()
  spec$groups <- list()
  result <- build_vast_model(spec)
  expect_false(grepl("vast_group", result$code))
})

test_that("build_vast_model handles spec with no IS/OUGHT", {
  skip_if_not_installed("vastR")
  spec <- minimal_model_spec()
  spec$is_statements <- list()
  spec$ought_statements <- list()
  result <- build_vast_model(spec)
  expect_false(grepl("vast_is", result$code))
  expect_false(grepl("vast_ought", result$code))
})

test_that("build_vast_model still returns code even with bad node refs", {
  skip_if_not_installed("vastR")
  # Create a spec that references a nonexistent node in an edge
  spec <- list(
    title = "Bad refs",
    naming_mode = "fimm",
    nodes = list(list(id = "X", type = "concept", label = "X")),
    edges = list(list(from = "X", to = "NONEXISTENT", type = "causation")),
    groups = list(),
    is_statements = list(),
    ought_statements = list()
  )
  # May warn depending on vastR's validation, but should not hard-error
  result <- suppressWarnings(build_vast_model(spec))
  # Code string should always be generated regardless of model eval success
  expect_type(result$code, "character")
  expect_true(nchar(result$code) > 0)
})
