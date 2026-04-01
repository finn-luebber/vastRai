# ============================================================================
# Tests for parse_vast_json() and extract_json_object()
# ============================================================================

# Access internal functions
extract_json_object <- vastRai:::extract_json_object
parse_vast_json <- vastRai:::parse_vast_json


# --- extract_json_object() ---

test_that("extract_json_object finds a simple JSON object", {
  json <- '{"key": "value"}'
  expect_equal(extract_json_object(json), json)
})

test_that("extract_json_object handles nested braces", {
  json <- '{"a": {"b": {"c": 1}}, "d": 2}'
  expect_equal(extract_json_object(json), json)
})

test_that("extract_json_object ignores braces inside strings", {
  json <- '{"text": "this { has } braces", "num": 1}'
  expect_equal(extract_json_object(json), json)
})

test_that("extract_json_object strips preamble text", {
  input <- 'Here is the JSON output:\n\n{"key": "value"}'
  expect_equal(extract_json_object(input), '{"key": "value"}')
})

test_that("extract_json_object strips trailing text", {
  input <- '{"key": "value"}\n\nHope this helps!'
  expect_equal(extract_json_object(input), '{"key": "value"}')
})

test_that("extract_json_object strips markdown code fences", {
  input <- 'Sure, here you go:\n\n```json\n{"key": "value"}\n```\n\nLet me know!'
  expect_equal(extract_json_object(input), '{"key": "value"}')
})

test_that("extract_json_object handles escaped quotes in strings", {
  json <- '{"text": "she said \\"hello\\""}'
  expect_equal(extract_json_object(json), json)
})

test_that("extract_json_object returns NULL for no braces", {
  expect_null(extract_json_object("no json here"))
})

test_that("extract_json_object returns NULL for unbalanced braces", {
  expect_null(extract_json_object('{"key": "value"'))
})

test_that("extract_json_object handles complex LLM preamble", {
  input <- paste0(
    "I've analyzed the paragraph and created two VAST models. ",
    "Here is the JSON output:\n\n",
    minimal_llm_json_string()
  )
  result <- extract_json_object(input)
  expect_type(result, "character")
  # Should be parseable
  parsed <- jsonlite::fromJSON(result, simplifyVector = FALSE)
  expect_true("models" %in% names(parsed))
})

test_that("extract_json_object handles braces in string values within nested JSON", {
  json <- '{"formula": "Y = f{X}", "nested": {"also": "a{b}c"}}'
  # The braces in strings should be ignored
  expect_equal(extract_json_object(json), json)
})


# --- parse_vast_json() ---

test_that("parse_vast_json parses a valid JSON string", {
  json_str <- minimal_llm_json_string()
  result <- parse_vast_json(json_str)
  expect_true("models" %in% names(result))
  expect_true("translation_notes" %in% names(result))
})

test_that("parse_vast_json parses from a file", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  writeLines(minimal_llm_json_string(), tmp)
  result <- parse_vast_json(tmp)
  expect_true("models" %in% names(result))
})

test_that("parse_vast_json handles LLM preamble before JSON", {
  input <- paste0(
    "Here is my analysis of the paragraph.\n\n",
    minimal_llm_json_string()
  )
  result <- parse_vast_json(input)
  expect_true("models" %in% names(result))
})

test_that("parse_vast_json handles markdown-fenced JSON", {
  input <- paste0(
    "```json\n",
    minimal_llm_json_string(),
    "\n```"
  )
  result <- parse_vast_json(input)
  expect_true("models" %in% names(result))
})

test_that("parse_vast_json handles preamble + code fences + trailing text", {
  input <- paste0(
    "Sure, here is the VAST translation:\n\n```json\n",
    minimal_llm_json_string(),
    "\n```\n\nLet me know if you have questions!"
  )
  result <- parse_vast_json(input)
  expect_true("models" %in% names(result))
})

test_that("parse_vast_json errors on non-string input", {
  expect_error(parse_vast_json(42), "single character string")
  expect_error(parse_vast_json(c("a", "b")), "single character string")
})

test_that("parse_vast_json errors when no JSON object found", {
  expect_error(parse_vast_json("no json here at all"), "Could not find a valid JSON object")
})

test_that("parse_vast_json errors on valid JSON missing required fields", {
  json_str <- '{"something": "else"}'
  expect_error(parse_vast_json(json_str), "missing required top-level field")
})

test_that("parse_vast_json errors on malformed JSON", {
  input <- '{"models": {, broken}'
  # The brace matcher may fail to find a balanced object, or jsonlite may fail to parse
  expect_error(parse_vast_json(input), "Could not find|failed to parse")
})

test_that("parse_vast_json handles file with preamble text before JSON", {
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  content <- paste0("LLM preamble text\n\n", minimal_llm_json_string())
  writeLines(content, tmp)
  result <- parse_vast_json(tmp)
  expect_true("models" %in% names(result))
})
