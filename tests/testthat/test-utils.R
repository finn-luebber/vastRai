# ============================================================================
# Tests for utility functions in R/utils.R
# ============================================================================

# Access internal functions
escape_quotes <- vastRai:::escape_quotes
format_strength <- vastRai:::format_strength
format_value <- vastRai:::format_value
sanitize_filename <- vastRai:::sanitize_filename
`%||%` <- vastRai:::`%||%`


# --- %||% operator ---

test_that("%||% returns left side when non-NULL", {
  expect_equal("a" %||% "b", "a")
  expect_equal(0 %||% 1, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
})

test_that("%||% returns right side when left is NULL", {
  expect_equal(NULL %||% "b", "b")
  expect_equal(NULL %||% 42, 42)
})


# --- escape_quotes() ---

test_that("escape_quotes escapes double quotes", {
  expect_equal(escape_quotes('say "hello"'), 'say \\"hello\\"')
})

test_that("escape_quotes leaves strings without quotes unchanged", {
  expect_equal(escape_quotes("no quotes here"), "no quotes here")
})

test_that("escape_quotes handles empty string", {
  expect_equal(escape_quotes(""), "")
})


# --- format_strength() ---

test_that("format_strength formats numeric values", {
  expect_equal(format_strength(0.5), "0.5")
  expect_equal(format_strength(1), "1")
  expect_equal(format_strength(-0.3), "-0.3")
})

test_that("format_strength formats verbal values with quotes", {
  expect_equal(format_strength("strong"), '"strong"')
  expect_equal(format_strength("weak"), '"weak"')
})


# --- format_value() ---

test_that("format_value formats numeric values", {
  expect_equal(format_value(0.8), "0.8")
  expect_equal(format_value(0), "0")
})

test_that("format_value formats character values with quotes", {
  expect_equal(format_value("high"), '"high"')
})


# --- sanitize_filename() ---

test_that("sanitize_filename replaces spaces with underscores", {
  expect_equal(sanitize_filename("my report"), "my_report")
})

test_that("sanitize_filename removes special characters", {
  expect_equal(sanitize_filename("file:name/test"), "filenametest")
})

test_that("sanitize_filename collapses multiple underscores", {
  expect_equal(sanitize_filename("a   b"), "a_b")
})

test_that("sanitize_filename truncates to max_length", {
  result <- sanitize_filename("a_very_long_filename_that_exceeds_the_limit", max_length = 10)
  expect_true(nchar(result) <= 10)
})

test_that("sanitize_filename returns default for empty input", {
  expect_equal(sanitize_filename("!!!"), "vast_report")
})

test_that("sanitize_filename handles normal input unchanged", {
  expect_equal(sanitize_filename("smoking_model"), "smoking_model")
})
