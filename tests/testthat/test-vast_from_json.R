# ============================================================================
# Tests for vast_from_json() — the primary entry point
# ============================================================================


# --- vast_from_json() ---

test_that("vast_from_json builds models from JSON string", {
  skip_if_not_installed("vastR")
  json_str <- minimal_llm_json_string()
  result <- vast_from_json(json_str)
  expect_s3_class(result, "vast_translation")
  expect_s3_class(result$closest_match, "vast_model")
  expect_s3_class(result$distant_compatible, "vast_model")
})

test_that("vast_from_json builds models from JSON file", {
  skip_if_not_installed("vastR")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  writeLines(minimal_llm_json_string(), tmp)
  result <- vast_from_json(tmp)
  expect_s3_class(result, "vast_translation")
})

test_that("vast_from_json which='closest' only builds closest match", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string(), which = "closest")
  expect_s3_class(result$closest_match, "vast_model")
  expect_null(result$distant_compatible)
})

test_that("vast_from_json which='distant' only builds distant compatible", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string(), which = "distant")
  expect_null(result$closest_match)
  expect_s3_class(result$distant_compatible, "vast_model")
})

test_that("vast_from_json preserves source_text", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_equal(result$source_text, "Smoking causes lung cancer.")
})

test_that("vast_from_json preserves translation notes", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_length(result$translation_notes$auxiliary_assumptions, 1)
  expect_length(result$translation_notes$underspecification_points, 1)
  expect_type(result$translation_notes$model_divergence_summary, "character")
})

test_that("vast_from_json preserves rigor assessment", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_true("T1_phenomenon" %in% names(result$rigor_assessment))
  expect_equal(result$rigor_assessment$T1_phenomenon$rating, "clearly_specified")
})

test_that("vast_from_json preserves T3 sub-fields", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  t3 <- result$rigor_assessment$T3_mechanism
  expect_true(!is.null(t3$completeness))
  expect_true(!is.null(t3$explanatory_quality))
  expect_true(!is.null(t3$sufficiency))
})

test_that("vast_from_json generates R code for both models", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_type(result$r_code$closest_match, "character")
  expect_type(result$r_code$distant_compatible, "character")
  expect_match(result$r_code$closest_match, "vast_model")
})

test_that("vast_from_json computes model diff when both models built", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_s3_class(result$model_diff, "vast_model_diff")
})

test_that("vast_from_json stores parsed specs for round-tripping", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_true(!is.null(result$.parsed_specs))
  expect_true(!is.null(result$.parsed_specs$closest_match))
  expect_true(!is.null(result$.parsed_specs$distant_compatible))
})

test_that("vast_from_json errors on invalid which argument", {
  expect_error(
    vast_from_json(minimal_llm_json_string(), which = "invalid"),
    "should be one of"
  )
})

test_that("vast_from_json handles JSON with preamble text", {
  skip_if_not_installed("vastR")
  input <- paste0("Here is my analysis:\n\n", minimal_llm_json_string())
  result <- vast_from_json(input)
  expect_s3_class(result, "vast_translation")
})


# --- vast_from_json() with extended model ---

test_that("vast_from_json builds extended model with all features", {
  skip_if_not_installed("vastR")
  json_data <- minimal_llm_json()
  json_data$models$closest_match <- extended_model_spec()
  json_str <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = TRUE)

  result <- vast_from_json(json_str, which = "closest")
  expect_s3_class(result$closest_match, "vast_model")
})


# --- vast_from_json() backward compatibility with old T3 format ---

test_that("vast_from_json handles T3 without sub-fields (old format)", {
  skip_if_not_installed("vastR")
  json_data <- minimal_llm_json()
  # Simulate old format: T3 without sub-fields
  json_data$rigor_assessment$T3_mechanism <- list(
    rating = "unspecified",
    justification = "No mechanism given."
  )
  json_str <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = TRUE)
  result <- vast_from_json(json_str)
  # Should still work — sub-fields just won't be present
  expect_equal(result$rigor_assessment$T3_mechanism$rating, "unspecified")
  expect_null(result$rigor_assessment$T3_mechanism$completeness)
})


# --- Print methods ---

test_that("print.vast_translation runs without error", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_no_error(print(result))
})

test_that("print_translation_notes runs without error", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_no_error(print_translation_notes(result))
})

test_that("print_translation_notes errors on wrong class", {
  expect_error(print_translation_notes(list()), "vast_translation")
})

test_that("print_rigor_assessment runs without error", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  expect_no_error(print_rigor_assessment(result))
})

test_that("print_rigor_assessment displays T3 sub-fields", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  # cli output goes to stderr/connection; capture both streams
  output <- capture.output(print_rigor_assessment(result), type = "message")
  output_text <- paste(output, collapse = "\n")
  expect_match(output_text, "Completeness")
  expect_match(output_text, "Explanatory quality")
  expect_match(output_text, "Sufficiency")
})

test_that("print_rigor_assessment handles missing T3 sub-fields gracefully", {
  skip_if_not_installed("vastR")
  json_data <- minimal_llm_json()
  json_data$rigor_assessment$T3_mechanism <- list(
    rating = "unspecified",
    justification = "No mechanism."
  )
  json_str <- jsonlite::toJSON(json_data, auto_unbox = TRUE, pretty = TRUE)
  result <- vast_from_json(json_str)
  # Should not error even without sub-fields
  expect_no_error(print_rigor_assessment(result))
})

test_that("print_rigor_assessment errors on wrong class", {
  expect_error(print_rigor_assessment(list()), "vast_translation")
})


# --- compare_models() ---

test_that("compare_models returns a diff object", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())
  diff <- compare_models(result)
  expect_s3_class(diff, "vast_model_diff")
})

test_that("compare_models errors on wrong class", {
  expect_error(compare_models(list()), "vast_translation")
})
