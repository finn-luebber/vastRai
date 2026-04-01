# ============================================================================
# Tests for narrative_to_prompt() and vast_to_prompt()
# ============================================================================


# --- narrative_to_prompt() ---

test_that("narrative_to_prompt returns a prompt string", {
  result <- narrative_to_prompt(
    theory = "Smoking causes lung cancer.",
    copy_to_clipboard = FALSE
  )
  expect_type(result, "character")
  expect_true(nchar(result) > 100)
})

test_that("narrative_to_prompt includes the theory text in the prompt", {
  result <- narrative_to_prompt(
    theory = "Chronic stress leads to immune suppression.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "Chronic stress leads to immune suppression")
})

test_that("narrative_to_prompt includes context when provided", {
  result <- narrative_to_prompt(
    theory = "Smoking causes lung cancer.",
    context = "This is from a public health paper.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "public health paper")
})

test_that("narrative_to_prompt errors on empty theory", {
  expect_error(
    narrative_to_prompt(theory = "", copy_to_clipboard = FALSE),
    "non-empty"
  )
})

test_that("narrative_to_prompt errors on non-string input", {
  expect_error(
    narrative_to_prompt(theory = 42, copy_to_clipboard = FALSE),
    "character string"
  )
  expect_error(
    narrative_to_prompt(theory = c("a", "b"), copy_to_clipboard = FALSE),
    "character string"
  )
})

test_that("narrative_to_prompt errors on non-string context", {
  expect_error(
    narrative_to_prompt(theory = "test", context = 42, copy_to_clipboard = FALSE),
    "character string or NULL"
  )
})

test_that("narrative_to_prompt writes to file when requested", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  narrative_to_prompt(
    theory = "Smoking causes cancer.",
    copy_to_clipboard = FALSE,
    output_file = tmp
  )
  expect_true(file.exists(tmp))
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(nchar(content) > 100)
})

test_that("narrative_to_prompt includes VAST framework instructions", {
  result <- narrative_to_prompt(
    theory = "Theory text here.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "VAST")
  expect_match(result, "closest")
  expect_match(result, "distant")
})

# --- Prompt content: VAST paper fidelity ---

test_that("prompt includes relationship strength coefficient definition", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  # Should contain the precise definition from the paper
  expect_match(result, "proportion of Y")
  expect_match(result, "full-range increase")
})

test_that("prompt includes naming direction constraint", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "FROM concept TO name only")
})

test_that("prompt includes time-indexing for feedback loops", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  # Should recommend time-indexed concepts, not bidirectional arrows
  expect_match(result, "time-indexed")
  expect_match(result, "T1")
  expect_match(result, "T2")
})

test_that("prompt includes analyst role explanation", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "analyst")
  expect_match(result, "responsible for")
})

test_that("prompt includes objects/scope concept", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "different sets of objects")
})

test_that("prompt includes jingle/jangle fallacy warning", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "jingle")
  expect_match(result, "jangle")
})

# --- Prompt content: Rigor assessment (Indicators paper) ---

test_that("prompt includes all four T-items", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "T1.*Phenomenon")
  expect_match(result, "T2.*Concept Definitions")
  expect_match(result, "T3.*Mechanism")
  expect_match(result, "T4.*Scope")
})

test_that("prompt includes T3 sub-dimensions", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "Completeness")
  expect_match(result, "Explanatory quality")
  expect_match(result, "Sufficiency")
})

test_that("prompt includes orienting questions from Indicators paper", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "Orienting question")
})

test_that("prompt includes three-level rating scale with definitions", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "clearly_specified")
  expect_match(result, "partially_specified")
  expect_match(result, "unspecified")
})

test_that("prompt notes the extension from binary to three-level scale", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "binary checkboxes")
  expect_match(result, "extension")
})

test_that("prompt instructs rigor assessment on narrative, not model", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "evaluates the NARRATIVE TEXT, not your model")
})

# --- Prompt content: Model construction guidance ---

test_that("prompt stresses faithfulness to text", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "faithful to the text")
})

test_that("prompt allows distant model to be larger or smaller", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "larger.*smaller|smaller.*larger")
})

test_that("prompt explains two-model divergence rationale", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "interpretive degrees of freedom")
})

test_that("prompt flags ambiguous verbs", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "depends on.*AMBIGUOUS")
})

# --- Prompt content: JSON schema ---

test_that("prompt includes T3 sub-fields in JSON schema", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, '"completeness"')
  expect_match(result, '"explanatory_quality"')
  expect_match(result, '"sufficiency"')
})

test_that("prompt allows IS/OUGHT on group IDs", {
  result <- narrative_to_prompt(
    theory = "Test.",
    copy_to_clipboard = FALSE
  )
  expect_match(result, "concept or group id")
})


# --- vast_to_prompt() ---

test_that("vast_to_prompt errors on non-vast_model input", {
  expect_error(vast_to_prompt(list()), "vast_model")
})

test_that("vast_to_prompt returns a prompt string", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Test", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "Smoking"),
      vastR::vast_concept("Y", "Cancer")
    ) |>
    vastR::add_edges(vastR::vast_causation("X", "Y"))

  result <- vast_to_prompt(model, copy_to_clipboard = FALSE)
  expect_type(result, "character")
  expect_true(nchar(result) > 100)
})

test_that("vast_to_prompt includes model description in prompt", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Smoking Model", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "Smoking"),
      vastR::vast_concept("Y", "Cancer")
    ) |>
    vastR::add_edges(vastR::vast_causation("X", "Y"))

  result <- vast_to_prompt(model, copy_to_clipboard = FALSE)
  expect_match(result, "Smoking Model")
  expect_match(result, "Relationships:")
})

test_that("vast_to_prompt includes fidelity assessment when requested", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Test", naming_mode = "fimm") |>
    vastR::add_nodes(vastR::vast_concept("X", "X"))

  result <- vast_to_prompt(model, assess_fidelity = TRUE, copy_to_clipboard = FALSE)
  expect_match(result, "Fidelity Notes")
})

test_that("vast_to_prompt includes context when provided", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Test", naming_mode = "fimm") |>
    vastR::add_nodes(vastR::vast_concept("X", "X"))

  result <- vast_to_prompt(model, context = "Write for a lay audience.",
                           copy_to_clipboard = FALSE)
  expect_match(result, "lay audience")
})

test_that("vast_to_prompt writes to file when requested", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Test", naming_mode = "fimm") |>
    vastR::add_nodes(vastR::vast_concept("X", "X"))

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  vast_to_prompt(model, copy_to_clipboard = FALSE, output_file = tmp)
  expect_true(file.exists(tmp))
})
