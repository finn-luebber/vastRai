# ============================================================================
# Tests for diff_models() and compare_models()
# ============================================================================

diff_models <- vastRai:::diff_models
format_diff_text <- vastRai:::format_diff_text
format_strength_display <- vastRai:::format_strength_display


# --- format_strength_display() ---

test_that("format_strength_display formats numeric strength", {
  expect_equal(format_strength_display(0.5), "0.5")
})

test_that("format_strength_display formats NULL as default", {
  expect_equal(format_strength_display(NULL), "(default)")
})


# --- diff_models() ---

test_that("diff_models returns a vast_model_diff object", {
  spec_a <- minimal_model_spec("A")
  spec_b <- minimal_model_spec("B")
  result <- diff_models(spec_a, spec_b)
  expect_s3_class(result, "vast_model_diff")
})

test_that("diff_models identifies identical models as fully shared", {
  spec <- minimal_model_spec()
  result <- diff_models(spec, spec)
  expect_length(result$only_a_nodes, 0)
  expect_length(result$only_b_nodes, 0)
  expect_true(nrow(result$only_a) == 0)
  expect_true(nrow(result$only_b) == 0)
  expect_true(all(!result$shared_edges$differs))
})

test_that("diff_models detects unique nodes", {
  spec_a <- minimal_model_spec()
  spec_b <- minimal_model_spec()
  # Add an extra concept to spec_b
  spec_b$nodes <- c(spec_b$nodes, list(list(id = "Z", type = "concept", label = "Z")))

  result <- diff_models(spec_a, spec_b)
  expect_true("Z" %in% result$only_b_nodes)
  expect_length(result$only_a_nodes, 0)
})

test_that("diff_models detects different edge types", {
  spec_a <- minimal_model_spec()
  spec_b <- minimal_model_spec()
  # Change the causation to prediction in spec_b
  spec_b$edges[[1]]$type <- "prediction"

  result <- diff_models(spec_a, spec_b)
  differing <- result$shared_edges[result$shared_edges$differs, ]
  expect_true(nrow(differing) > 0)
  expect_equal(differing$type_a[1], "causation")
  expect_equal(differing$type_b[1], "prediction")
})

test_that("diff_models detects different edge strengths", {
  spec_a <- minimal_model_spec()
  spec_b <- minimal_model_spec()
  spec_b$edges[[1]]$strength <- 0.9

  result <- diff_models(spec_a, spec_b)
  differing <- result$shared_edges[result$shared_edges$differs, ]
  expect_true(nrow(differing) > 0)
})

test_that("diff_models detects unique edges", {
  spec_a <- minimal_model_spec()
  spec_b <- minimal_model_spec()
  # Add extra edge to spec_a
  spec_a$edges <- c(spec_a$edges, list(
    list(from = "Y", to = "X", type = "reasoning")
  ))
  result <- diff_models(spec_a, spec_b)
  expect_true(nrow(result$only_a) > 0)
})

test_that("diff_models uses custom model names", {
  spec <- minimal_model_spec()
  result <- diff_models(spec, spec, name_a = "Model Alpha", name_b = "Model Beta")
  expect_equal(result$name_a, "Model Alpha")
  expect_equal(result$name_b, "Model Beta")
})


# --- format_diff_text() ---

test_that("format_diff_text returns character vector", {
  spec <- minimal_model_spec()
  diff <- diff_models(spec, spec)
  result <- format_diff_text(diff)
  expect_type(result, "character")
})

test_that("format_diff_text includes relevant sections for differing models", {
  spec_a <- minimal_model_spec()
  spec_b <- minimal_model_spec()
  spec_b$edges[[1]]$type <- "prediction"
  spec_b$nodes <- c(spec_b$nodes, list(list(id = "Z", type = "concept", label = "Z")))

  diff <- diff_models(spec_a, spec_b)
  text <- paste(format_diff_text(diff), collapse = "\n")
  expect_match(text, "Node Differences")
  expect_match(text, "differ between models")
})
