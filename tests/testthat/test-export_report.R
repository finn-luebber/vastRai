# ============================================================================
# Tests for export_report()
# ============================================================================


test_that("export_report errors on wrong class", {
  expect_error(export_report(list()), "vast_translation")
})

test_that("export_report creates output directory and report file", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())

  tmp_dir <- file.path(tempdir(), paste0("vast_test_", format(Sys.time(), "%H%M%S")))
  on.exit(unlink(tmp_dir, recursive = TRUE))

  export_report(result, dir = tmp_dir, render_diagrams = FALSE)
  expect_true(dir.exists(tmp_dir))
  expect_true(file.exists(file.path(tmp_dir, "report.txt")))
})

test_that("export_report writes report with expected sections", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())

  tmp_dir <- file.path(tempdir(), paste0("vast_test2_", format(Sys.time(), "%H%M%S")))
  on.exit(unlink(tmp_dir, recursive = TRUE))

  export_report(result, dir = tmp_dir, render_diagrams = FALSE)

  report <- paste(readLines(file.path(tmp_dir, "report.txt")), collapse = "\n")
  expect_match(report, "VAST Translation Report")
  expect_match(report, "SOURCE TEXT")
  expect_match(report, "THEORETICAL RIGOR")
  expect_match(report, "TRANSLATION NOTES")
  expect_match(report, "GENERATED R CODE")
})

test_that("export_report respects overwrite = FALSE", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())

  tmp_dir <- file.path(tempdir(), paste0("vast_test3_", format(Sys.time(), "%H%M%S")))
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_error(
    export_report(result, dir = tmp_dir, render_diagrams = FALSE, overwrite = FALSE),
    "already exists"
  )
})

test_that("export_report allows overwrite = TRUE", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())

  tmp_dir <- file.path(tempdir(), paste0("vast_test4_", format(Sys.time(), "%H%M%S")))
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Should not error
  export_report(result, dir = tmp_dir, render_diagrams = FALSE, overwrite = TRUE)
  expect_true(file.exists(file.path(tmp_dir, "report.txt")))
})

test_that("export_report generates default directory name from model title", {
  skip_if_not_installed("vastR")
  result <- vast_from_json(minimal_llm_json_string())

  # Remove any pre-existing directory
  expected_dir <- "Closest_Match"
  if (dir.exists(expected_dir)) unlink(expected_dir, recursive = TRUE)
  on.exit(unlink(expected_dir, recursive = TRUE))

  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)

  export_report(result, render_diagrams = FALSE)
  # Should have created a directory based on the model title
  # The exact name depends on sanitize_filename
  sanitized <- vastRai:::sanitize_filename("Closest Match")
  expect_true(dir.exists(sanitized))
})
