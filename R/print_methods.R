#' Print Translation Notes
#'
#' Formats and displays the auxiliary assumptions, underspecification points,
#' and model divergence summary from a VAST translation result.
#'
#' @param x A `vast_translation` object returned by [vast_from_json()].
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print_translation_notes <- function(x, ...) {
  if (!inherits(x, "vast_translation")) {
    cli::cli_abort("{.arg x} must be a {.cls vast_translation} object from {.fn vast_from_json}.")
  }

  notes <- x$translation_notes

  cli::cli_h1("Translation Notes")

  # Auxiliary assumptions
  cli::cli_h2("Auxiliary Assumptions")
  if (length(notes$auxiliary_assumptions) == 0) {
    cli::cli_alert_info("No auxiliary assumptions documented.")
  } else {
    for (assumption in notes$auxiliary_assumptions) {
      cli::cli_h3("{assumption$id}: {assumption$description}")
      cli::cli_text("Reason: {assumption$reason}")
      if (!is.null(assumption$text_evidence)) {
        cli::cli_text("Text evidence: {.emph {assumption$text_evidence}}")
      }
      cli::cli_text("Closest match chose: {assumption$closest_match_choice}")
      cli::cli_text("Distant model chose: {assumption$distant_compatible_choice}")
      if (length(assumption$other_options) > 0) {
        cli::cli_text("Other options: {paste(assumption$other_options, collapse = '; ')}")
      }
      cli::cli_text("")
    }
  }

  # Underspecification points
  cli::cli_h2("Underspecification Points")
  if (length(notes$underspecification_points) == 0) {
    cli::cli_alert_info("No underspecification points documented.")
  } else {
    for (point in notes$underspecification_points) {
      cli::cli_h3("{point$id}: {point$description}")
      cli::cli_text("Impact: {point$impact}")
      if (!is.null(point$text_location)) {
        cli::cli_text("Location: {.emph {point$text_location}}")
      }
      cli::cli_text("")
    }
  }

  # Model divergence summary
  if (!is.null(notes$model_divergence_summary)) {
    cli::cli_h2("Model Divergence Summary")
    cli::cli_text(notes$model_divergence_summary)
  }

  invisible(x)
}


#' Print Theoretical Rigor Assessment
#'
#' Formats and displays the T-item (theory specification) assessment from
#' a VAST translation result, based on the Indicators for the Assessment of
#' Theoretical Rigor (Musfeld et al., v3.0).
#'
#' @param x A `vast_translation` object returned by [vast_from_json()].
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print_rigor_assessment <- function(x, ...) {
  if (!inherits(x, "vast_translation")) {
    cli::cli_abort("{.arg x} must be a {.cls vast_translation} object from {.fn vast_from_json}.")
  }

  rigor <- x$rigor_assessment

  cli::cli_h1("Theoretical Rigor Assessment")
  cli::cli_text(
    "Based on T-items from Musfeld et al., Indicators for the Assessment of Theoretical Rigor (v3.0)"
  )
  cli::cli_text("")

  # Helper to display a T-item
  display_item <- function(item, label) {
    if (is.null(item)) return()
    symbol <- switch(item$rating,
      "clearly_specified" = cli::col_green(cli::symbol$tick),
      "partially_specified" = cli::col_yellow("~"),
      "unspecified" = cli::col_red(cli::symbol$cross),
      "?"
    )
    cli::cli_text("{symbol} {.strong {label}}: {item$rating}")
    cli::cli_text("  {item$justification}")
    if (!is.null(item$undefined_concepts) && length(item$undefined_concepts) > 0) {
      cli::cli_text("  Undefined concepts: {paste(item$undefined_concepts, collapse = ', ')}")
    }
    cli::cli_text("")
  }

  display_item(rigor$T1_phenomenon, "T1 - Phenomenon described")
  display_item(rigor$T2_concept_definitions, "T2 - Concepts defined")
  display_item(rigor$T3_mechanism, "T3 - Mechanism specified")

  # T3 sub-dimensions (new in revised prompt)
  if (!is.null(rigor$T3_mechanism)) {
    if (!is.null(rigor$T3_mechanism$completeness)) {
      cli::cli_text("  {.emph Completeness}: {rigor$T3_mechanism$completeness}")
    }
    if (!is.null(rigor$T3_mechanism$explanatory_quality)) {
      cli::cli_text("  {.emph Explanatory quality}: {rigor$T3_mechanism$explanatory_quality}")
    }
    if (!is.null(rigor$T3_mechanism$sufficiency)) {
      cli::cli_text("  {.emph Sufficiency}: {rigor$T3_mechanism$sufficiency}")
    }
    cli::cli_text("")
  }

  display_item(rigor$T4_scope, "T4 - Scope specified")

  if (!is.null(rigor$overall_assessment)) {
    cli::cli_h2("Overall Assessment")
    cli::cli_text(rigor$overall_assessment)
  }

  invisible(x)
}


#' Print method for vast_translation objects
#'
#' @param x A `vast_translation` object.
#' @param ... Additional arguments (currently unused).
#'
#' @export
print.vast_translation <- function(x, ...) {
  cli::cli_h1("VAST Translation Result")

  # Source text (truncated)
  if (!is.null(x$source_text)) {
    display_text <- if (nchar(x$source_text) > 120) {
      paste0(substr(x$source_text, 1, 117), "...")
    } else {
      x$source_text
    }
    cli::cli_text("Source: {.emph {display_text}}")
    cli::cli_text("")
  }

  # Model summaries
  if (!is.null(x$closest_match)) {
    cli::cli_alert_success("Closest match model: available")
  }
  if (!is.null(x$distant_compatible)) {
    cli::cli_alert_success("Distant compatible model: available")
  }

  # Notes summary
  n_assumptions <- length(x$translation_notes$auxiliary_assumptions)
  n_underspec <- length(x$translation_notes$underspecification_points)
  cli::cli_alert_info(
    "{n_assumptions} auxiliary assumptions, {n_underspec} underspecification points"
  )

  cli::cli_text("")
  cli::cli_text("Use {.fn print_translation_notes} to see translation details.")
  cli::cli_text("Use {.fn print_rigor_assessment} to see the rigor assessment.")
  cli::cli_text("Use {.fn compare_models} to see a structural comparison.")
  cli::cli_text("Use {.fn export_report} to save a report with diagrams.")
  cli::cli_text("Use {.code cat(result$r_code$closest_match)} to see the generated R code.")
  cli::cli_text("Use {.fn vastR::vast_render} to render the models.")

  invisible(x)
}
