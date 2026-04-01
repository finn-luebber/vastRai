# ============================================================================
# vastRai - Lightweight report export (text + PNG diagrams)
# ============================================================================

#' Export a VAST Translation Report
#'
#' Generates a lightweight report consisting of a plain text file and
#' (optionally) PNG renderings of both models. The report includes the
#' source text, rigor assessment, model comparison, translation notes,
#' and generated R code.
#'
#' @param x A `vast_translation` object returned by [vast_from_json()].
#' @param dir Character. Output directory. Defaults to a sanitized name
#'   derived from the closest-match model title. The directory is created
#'   if it does not exist.
#' @param render_diagrams Logical. If `TRUE` (default), renders both models
#'   as PNG files using [vastR::vast_export_png()]. Requires the
#'   `DiagrammeRsvg` and `rsvg` packages.
#' @param overwrite Logical. If `FALSE` (default) and the output directory
#'   already exists, the function aborts with an error. Set to `TRUE` to
#'   overwrite existing files.
#'
#' @return Invisibly returns the path to the output directory.
#'
#' @examples
#' \dontrun{
#' result <- vast_from_json("llm_response.json")
#' export_report(result)
#' export_report(result, dir = "my_analysis", overwrite = TRUE)
#' }
#'
#' @export
export_report <- function(x,
                          dir = NULL,
                          render_diagrams = TRUE,
                          overwrite = FALSE) {

  if (!inherits(x, "vast_translation")) {
    cli::cli_abort("{.arg x} must be a {.cls vast_translation} object from {.fn vast_from_json}.")
  }

  # --- Determine output directory ---
  if (is.null(dir)) {
    # Derive from model title
    title <- NULL
    if (!is.null(x$.parsed_specs$closest_match$title)) {
      title <- x$.parsed_specs$closest_match$title
    }
    dir <- sanitize_filename(title %||% "vast_report")
  }

  # Check overwrite safety
  if (dir.exists(dir) && !overwrite) {
    cli::cli_abort(c(
      "Output directory {.file {dir}} already exists.",
      "i" = "Use {.code overwrite = TRUE} to overwrite, or choose a different {.arg dir}."
    ))
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  # --- Build report text ---
  report_lines <- character()

  # Header
  report_lines <- c(report_lines,
    "=" |> strrep(72),
    "VAST Translation Report",
    "=" |> strrep(72),
    "",
    sprintf("Generated: %s", Sys.time()),
    ""
  )

  # Source text
  if (!is.null(x$source_text)) {
    report_lines <- c(report_lines,
      "-" |> strrep(72),
      "SOURCE TEXT",
      "-" |> strrep(72),
      "",
      x$source_text,
      ""
    )
  }

  # Rigor assessment
  if (!is.null(x$rigor_assessment)) {
    report_lines <- c(report_lines,
      "-" |> strrep(72),
      "THEORETICAL RIGOR ASSESSMENT",
      "-" |> strrep(72),
      sprintf("(Based on T-items from Musfeld et al., Indicators for the"),
      sprintf(" Assessment of Theoretical Rigor, v3.0)"),
      ""
    )

    rigor <- x$rigor_assessment
    format_item <- function(item, label) {
      if (is.null(item)) return(character())
      symbol <- switch(item$rating,
        "clearly_specified" = "[+]",
        "partially_specified" = "[~]",
        "unspecified" = "[-]",
        "[?]"
      )
      lines <- sprintf("%s %s: %s", symbol, label, item$rating)
      lines <- c(lines, sprintf("    %s", item$justification))
      if (!is.null(item$undefined_concepts) && length(item$undefined_concepts) > 0) {
        lines <- c(lines, sprintf("    Undefined: %s",
                                  paste(item$undefined_concepts, collapse = ", ")))
      }
      c(lines, "")
    }

    report_lines <- c(report_lines,
      format_item(rigor$T1_phenomenon, "T1 - Phenomenon described"),
      format_item(rigor$T2_concept_definitions, "T2 - Concepts defined"),
      format_item(rigor$T3_mechanism, "T3 - Mechanism specified")
    )

    # T3 sub-dimensions (new in revised prompt)
    if (!is.null(rigor$T3_mechanism)) {
      if (!is.null(rigor$T3_mechanism$completeness)) {
        report_lines <- c(report_lines,
          sprintf("    Completeness: %s", rigor$T3_mechanism$completeness))
      }
      if (!is.null(rigor$T3_mechanism$explanatory_quality)) {
        report_lines <- c(report_lines,
          sprintf("    Explanatory quality: %s", rigor$T3_mechanism$explanatory_quality))
      }
      if (!is.null(rigor$T3_mechanism$sufficiency)) {
        report_lines <- c(report_lines,
          sprintf("    Sufficiency: %s", rigor$T3_mechanism$sufficiency))
      }
      report_lines <- c(report_lines, "")
    }

    report_lines <- c(report_lines,
      format_item(rigor$T4_scope, "T4 - Scope specified")
    )

    if (!is.null(rigor$overall_assessment)) {
      report_lines <- c(report_lines,
        "Overall:",
        sprintf("  %s", rigor$overall_assessment),
        ""
      )
    }
  }

  # Model comparison
  if (!is.null(x$model_diff)) {
    report_lines <- c(report_lines,
      "-" |> strrep(72),
      "MODEL COMPARISON",
      "-" |> strrep(72),
      ""
    )
    report_lines <- c(report_lines, format_diff_text(x$model_diff))
  }

  # Translation notes
  if (!is.null(x$translation_notes)) {
    report_lines <- c(report_lines,
      "-" |> strrep(72),
      "TRANSLATION NOTES",
      "-" |> strrep(72),
      ""
    )
    notes <- x$translation_notes

    if (length(notes$auxiliary_assumptions) > 0) {
      report_lines <- c(report_lines, "Auxiliary Assumptions:", "")
      for (a in notes$auxiliary_assumptions) {
        report_lines <- c(report_lines,
          sprintf("  %s: %s", a$id, a$description),
          sprintf("    Reason: %s", a$reason)
        )
        if (!is.null(a$text_evidence)) {
          report_lines <- c(report_lines,
            sprintf('    Text: "%s"', a$text_evidence))
        }
        report_lines <- c(report_lines,
          sprintf("    Closest match: %s", a$closest_match_choice),
          sprintf("    Distant model: %s", a$distant_compatible_choice)
        )
        if (length(a$other_options) > 0) {
          report_lines <- c(report_lines,
            sprintf("    Other options: %s", paste(a$other_options, collapse = "; ")))
        }
        report_lines <- c(report_lines, "")
      }
    }

    if (length(notes$underspecification_points) > 0) {
      report_lines <- c(report_lines, "Underspecification Points:", "")
      for (u in notes$underspecification_points) {
        report_lines <- c(report_lines,
          sprintf("  %s: %s", u$id, u$description),
          sprintf("    Impact: %s", u$impact)
        )
        if (!is.null(u$text_location)) {
          report_lines <- c(report_lines,
            sprintf("    Location: %s", u$text_location))
        }
        report_lines <- c(report_lines, "")
      }
    }

    if (!is.null(notes$model_divergence_summary)) {
      report_lines <- c(report_lines,
        "Model Divergence Summary:",
        sprintf("  %s", notes$model_divergence_summary),
        ""
      )
    }
  }

  # R code for both models
  if (length(x$r_code) > 0) {
    report_lines <- c(report_lines,
      "-" |> strrep(72),
      "GENERATED R CODE",
      "-" |> strrep(72),
      ""
    )

    if (!is.null(x$r_code$closest_match)) {
      report_lines <- c(report_lines,
        "### Closest Match ###", "",
        x$r_code$closest_match, ""
      )
    }

    if (!is.null(x$r_code$distant_compatible)) {
      report_lines <- c(report_lines,
        "### Distant Compatible ###", "",
        x$r_code$distant_compatible, ""
      )
    }
  }

  # Diagram file references
  if (render_diagrams) {
    report_lines <- c(report_lines,
      "-" |> strrep(72),
      "DIAGRAMS",
      "-" |> strrep(72),
      "",
      "See accompanying files:",
      "  - model_closest_match.png",
      "  - model_distant_compatible.png",
      ""
    )
  }

  report_lines <- c(report_lines, "=" |> strrep(72))

  # --- Write report ---
  report_path <- file.path(dir, "report.txt")
  writeLines(report_lines, report_path)
  cli::cli_alert_success("Report written to {.file {report_path}}")

  # --- Render diagrams ---
  if (render_diagrams) {
    can_render <- requireNamespace("DiagrammeRsvg", quietly = TRUE) &&
                  requireNamespace("rsvg", quietly = TRUE)

    if (!can_render) {
      cli::cli_alert_warning(
        "Cannot render diagrams: install {.pkg DiagrammeRsvg} and {.pkg rsvg}."
      )
    } else {
      if (!is.null(x$closest_match)) {
        png_path <- file.path(dir, "model_closest_match.png")
        tryCatch({
          vastR::vast_export_png(x$closest_match, file = png_path)
          cli::cli_alert_success("Diagram: {.file {png_path}}")
        }, error = function(e) {
          cli::cli_alert_warning("Failed to render closest match: {e$message}")
        })
      }

      if (!is.null(x$distant_compatible)) {
        png_path <- file.path(dir, "model_distant_compatible.png")
        tryCatch({
          vastR::vast_export_png(x$distant_compatible, file = png_path)
          cli::cli_alert_success("Diagram: {.file {png_path}}")
        }, error = function(e) {
          cli::cli_alert_warning("Failed to render distant model: {e$message}")
        })
      }
    }
  }

  cli::cli_alert_success("Report exported to {.file {dir}/}")
  invisible(dir)
}


#' Format a model diff as plain text lines
#' @noRd
format_diff_text <- function(diff) {
  lines <- character()

  # Node differences
  if (length(diff$only_a_nodes) > 0 || length(diff$only_b_nodes) > 0) {
    lines <- c(lines, "Node Differences:")
    if (length(diff$only_a_nodes) > 0) {
      lines <- c(lines, sprintf("  Only in %s: %s",
                                diff$name_a, paste(diff$only_a_nodes, collapse = ", ")))
    }
    if (length(diff$only_b_nodes) > 0) {
      lines <- c(lines, sprintf("  Only in %s: %s",
                                diff$name_b, paste(diff$only_b_nodes, collapse = ", ")))
    }
    lines <- c(lines, sprintf("  Shared: %d concept nodes", length(diff$shared_nodes)), "")
  }

  # Shared edges
  matching <- diff$shared_edges[!diff$shared_edges$differs, , drop = FALSE]
  differing <- diff$shared_edges[diff$shared_edges$differs, , drop = FALSE]

  if (nrow(matching) > 0) {
    lines <- c(lines, "Shared Relationships (identical in both models):")
    for (i in seq_len(nrow(matching))) {
      row <- matching[i, ]
      lines <- c(lines, sprintf('  %s --[%s %s]--> %s',
                                row$from_name, row$type_a, row$strength_a, row$to_name))
    }
    lines <- c(lines, "")
  }

  if (nrow(differing) > 0) {
    lines <- c(lines, "Shared Relationships (differ between models):")
    for (i in seq_len(nrow(differing))) {
      row <- differing[i, ]
      lines <- c(lines, sprintf("  %s -> %s:", row$from_name, row$to_name))
      lines <- c(lines, sprintf("    %s: %s %s", diff$name_a, row$type_a, row$strength_a))
      lines <- c(lines, sprintf("    %s: %s %s", diff$name_b, row$type_b, row$strength_b))
    }
    lines <- c(lines, "")
  }

  # Unique edges
  if (nrow(diff$only_a) > 0) {
    lines <- c(lines, sprintf("Only in %s:", diff$name_a))
    for (i in seq_len(nrow(diff$only_a))) {
      row <- diff$only_a[i, ]
      lines <- c(lines, sprintf('  %s --[%s %s]--> %s',
                                row$from_name, row$type, row$strength, row$to_name))
    }
    lines <- c(lines, "")
  }

  if (nrow(diff$only_b) > 0) {
    lines <- c(lines, sprintf("Only in %s:", diff$name_b))
    for (i in seq_len(nrow(diff$only_b))) {
      row <- diff$only_b[i, ]
      lines <- c(lines, sprintf('  %s --[%s %s]--> %s',
                                row$from_name, row$type, row$strength, row$to_name))
    }
    lines <- c(lines, "")
  }

  lines
}
