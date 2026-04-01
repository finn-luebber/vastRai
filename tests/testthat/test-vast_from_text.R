# ============================================================================
# Tests for vast_from_text() — convenience wrapper
# ============================================================================

test_that("vast_from_text returns a prompt string", {
  result <- vast_from_text(
    theory = "Smoking causes lung cancer.",
    context = NULL,
    output_file = NULL
  )
  expect_type(result, "character")
  expect_true(nchar(result) > 100)
})

test_that("vast_from_text includes the theory text in the prompt", {
  result <- vast_from_text(theory = "Chronic stress leads to immune suppression.")
  expect_match(result, "Chronic stress")
})

test_that("vast_from_text writes to file when output_file is given", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  vast_from_text(theory = "Test theory.", output_file = tmp)
  expect_true(file.exists(tmp))
})
