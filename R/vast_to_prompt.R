# ============================================================================
# vastRai - Model serialization and model-to-narrative prompt generation
# ============================================================================

#' Serialize a vastR model object to a structured list
#'
#' Converts a vastR model into a list representation matching the JSON schema
#' used by the rest of vastRai. This enables round-tripping: a model built
#' manually in vastR can be serialized and then used with [vast_to_prompt()]
#' or the Shiny app.
#'
#' @param model A `vast_model` object from vastR.
#' @return A list with components: title, naming_mode, nodes, edges, groups.
#'   This list can be converted to JSON via `jsonlite::toJSON()`.
#'
#' @noRd
serialize_vast_model <- function(model) {
  if (!inherits(model, "vast_model")) {
    cli::cli_abort("{.arg model} must be a {.cls vast_model} object.")
  }

  # --- Serialize nodes ---
  nodes <- lapply(names(model$nodes), function(nid) {
    node <- model$nodes[[nid]]
    serialized <- list(
      id = node$id,
      type = node$type,
      label = node$label
    )

    # Add type-specific fields
    if (node$type == "is" || node$type == "ought") {
      # Extract value from label (format: "IS\nvalue" or "OUGHT\nvalue")
      parts <- strsplit(node$label, "\n")[[1]]
      if (length(parts) > 1) {
        val <- parts[2]
        serialized$value <- tryCatch(as.numeric(val), warning = function(w) val)
      }
    } else if (node$type == "perspective") {
      # Label format: "holder\nvalue" or just "holder"
      parts <- strsplit(node$label, "\n")[[1]]
      serialized$holder <- parts[1]
      if (length(parts) > 1) {
        val <- parts[2]
        serialized$value <- tryCatch(as.numeric(val), warning = function(w) val)
      }
    } else if (node$type == "diamond") {
      serialized$diamond_content <- node$label
    }

    serialized
  })

  # --- Serialize edges ---
  edges <- lapply(seq_along(model$edges), function(i) {
    edge <- model$edges[[i]]

    # Map internal single-letter type codes back to full names
    # Must cover all types defined in vastR including noise and structural
    type_map <- c(
      n = "naming", i = "implication", c = "causation",
      t = "transformation", p = "prediction", r = "reasoning",
      u = "unknown", noise = "noise"
    )
    raw_type <- edge$type %||% ""
    if (nchar(raw_type) > 0 && raw_type %in% names(type_map)) {
      full_type <- type_map[[raw_type]]
    } else {
      full_type <- if (raw_type == "") "structural" else raw_type
    }

    serialized <- list(
      id = paste0(raw_type, i),
      from = edge$from,
      to = edge$to,
      type = full_type
    )

    # Preserve strength
    if (!is.null(edge$strength)) {
      serialized$strength <- edge$strength
    }

    # Extract index from label. vastR stores the index baked into the label
    # field (e.g., label = "c1" means type "c" with index "1"). Extract by
    # stripping the type-letter prefix from the label.
    if (!is.null(edge$label) && nchar(raw_type) > 0) {
      label_str <- edge$label
      # The label format is "{type_letter}{index} {strength}" or just "{type_letter}{index}"
      # Strip any strength portion first (everything after a space)
      label_core <- sub("\\s.*$", "", label_str)
      # Strip the type letter prefix
      if (startsWith(label_core, raw_type)) {
        idx <- substring(label_core, nchar(raw_type) + 1)
        if (nchar(idx) > 0) {
          serialized$index <- idx
        }
      }
    }

    # Preserve compound edge attributes for round-tripping
    if (!is.null(edge$lhead)) {
      serialized$lhead <- edge$lhead
    }
    if (!is.null(edge$ltail)) {
      serialized$ltail <- edge$ltail
    }

    serialized
  })

  # --- Serialize groups ---
  groups <- lapply(names(model$groups), function(gid) {
    grp <- model$groups[[gid]]
    serialized <- list(
      id = grp$id,
      label = grp$label,
      members = grp$node_ids
    )
    if (length(grp$child_group_ids) > 0) {
      serialized$child_group_ids <- grp$child_group_ids
    }
    serialized
  })

  list(
    title = model$title,
    naming_mode = model$naming_mode,
    nodes = nodes,
    edges = edges,
    groups = groups
  )
}


#' Generate a readable text description of a VAST model
#'
#' Produces a structured text representation of the model suitable for
#' inclusion in an LLM prompt. This is NOT a narrative paragraph — it's
#' a systematic listing of concepts, names, and relationships.
#'
#' @param model A `vast_model` object from vastR.
#' @return A character string describing the model.
#'
#' @noRd
describe_vast_model <- function(model) {
  spec <- serialize_vast_model(model)

  lines <- character()
  lines <- c(lines, sprintf("VAST Model: %s", spec$title))
  lines <- c(lines, sprintf("Naming mode: %s", spec$naming_mode))
  lines <- c(lines, "")

  # Build name lookup
  name_lookup <- build_name_lookup(spec)

  # List concepts
  concepts <- Filter(function(n) n$type == "concept", spec$nodes)
  if (length(concepts) > 0) {
    lines <- c(lines, "Concepts:")
    for (node in concepts) {
      name <- name_lookup[[node$id]]
      if (!is.null(name)) {
        lines <- c(lines, sprintf('  %s: "%s"', node$id, name))
      } else {
        lines <- c(lines, sprintf("  %s", node$id))
      }
    }
    lines <- c(lines, "")
  }

  # List data nodes
  data_nodes <- Filter(function(n) n$type == "data", spec$nodes)
  if (length(data_nodes) > 0) {
    lines <- c(lines, "Empirical data:")
    for (node in data_nodes) {
      name <- name_lookup[[node$id]]
      label <- if (!is.null(name)) name else node$label
      lines <- c(lines, sprintf('  %s: "%s"', node$id, label))
    }
    lines <- c(lines, "")
  }

  # List relationships (non-naming)
  type_names <- c(
    implication = "implies", causation = "causes",
    transformation = "transforms to", prediction = "predicts",
    reasoning = "is a reason to believe", unknown = "is related to (unknown type)",
    noise = "is influenced by noise from"
  )

  substantive <- Filter(function(e) e$type != "naming" && e$type != "structural",
                        spec$edges)
  if (length(substantive) > 0) {
    lines <- c(lines, "Relationships:")
    for (edge in substantive) {
      from_name <- name_lookup[[edge$from]] %||% edge$from
      to_name <- name_lookup[[edge$to]] %||% edge$to
      verb <- type_names[[edge$type]] %||% edge$type
      strength_str <- ""
      if (!is.null(edge$strength)) {
        strength_str <- sprintf(" (strength: %s)", edge$strength)
      }
      lines <- c(lines, sprintf('  "%s" %s "%s"%s',
                                from_name, verb, to_name, strength_str))
    }
    lines <- c(lines, "")
  }

  # Groups
  if (length(spec$groups) > 0) {
    lines <- c(lines, "Higher-order concepts:")
    for (grp in spec$groups) {
      member_names <- vapply(grp$members, function(m) {
        name_lookup[[m]] %||% m
      }, character(1))
      lines <- c(lines, sprintf('  %s ("%s"): contains %s',
                                grp$id, grp$label,
                                paste(member_names, collapse = ", ")))
    }
    lines <- c(lines, "")
  }

  paste(lines, collapse = "\n")
}


#' Generate a Prompt for Translating a VAST Model to Narrative Text
#'
#' Takes a vastR model object and generates a prompt that, when pasted into
#' an LLM, will produce a natural-language paragraph describing the theory
#' captured by the model.
#'
#' @param model A `vast_model` object from vastR.
#' @param context Character or NULL. Optional context about the domain or
#'   desired style of the narrative (e.g., "Write for a psychology audience",
#'   "This is about hippocampal function").
#' @param assess_fidelity Logical. If `TRUE`, the prompt also asks the LLM
#'   to note which model elements are difficult to express in natural
#'   language (e.g., precise strength values, type distinctions).
#'   Default is `FALSE`.
#' @param copy_to_clipboard Logical. If `TRUE` (default when `clipr` is
#'   available), copies the prompt to the clipboard.
#' @param output_file Character or NULL. If provided, writes the prompt to file.
#'
#' @return Invisibly returns the prompt string.
#'
#' @examples
#' \dontrun{
#' library(vastR)
#' model <- vast_model(title = "Smoking Example", naming_mode = "fimm") |>
#'   add_nodes(
#'     vast_concept("Smoking", "Smoking"),
#'     vast_concept("LungCancer", "Lung Cancer")
#'   ) |>
#'   add_edges(vast_causation("Smoking", "LungCancer", strength = 0.5))
#'
#' vast_to_prompt(model)
#' # Paste into LLM to get a narrative paragraph
#' }
#'
#' @export
vast_to_prompt <- function(model,
                           context = NULL,
                           assess_fidelity = FALSE,
                           copy_to_clipboard = TRUE,
                           output_file = NULL) {

  if (!inherits(model, "vast_model")) {
    cli::cli_abort("{.arg model} must be a {.cls vast_model} object from vastR.")
  }

  model_description <- describe_vast_model(model)

  prompt_parts <- character()

  prompt_parts <- c(prompt_parts,
'You are an expert scientific writer. You will receive a structured description of
a VAST (Visual Argument Structure Tool) model that captures a theoretical argument.
Your task is to write a clear, natural-language paragraph that faithfully expresses
the theory captured by the model.

Guidelines:
- Write in the style of a theory paragraph in a scientific paper.
- Express all concepts and relationships from the model.
- Use natural language connectives that match the relationship types:
  - causation: "causes", "leads to", "triggers"
  - implication: "implies", "entails", "is a type of"
  - prediction: "predicts", "is associated with"
  - reasoning: "therefore", "because", "this suggests"
  - transformation: "is derived from", "converts to"
- If strength values are given, express them qualitatively (e.g., 0.8 = "strongly",
  0.3 = "weakly").
- If there are IS/OUGHT statements or perspectives, include them naturally.
- Do not add information that is not in the model.
- Write a single coherent paragraph (or two if the model is complex).'
  )

  if (assess_fidelity) {
    prompt_parts <- c(prompt_parts,
'
After the paragraph, add a section titled "Fidelity Notes" listing:
- Any model elements that were difficult to express in natural language
- Any distinctions (e.g., causation vs. prediction) that natural language tends to blur
- Any strength values or special markers that could not be precisely conveyed')
  }

  prompt_parts <- c(prompt_parts,
    "",
    "## VAST MODEL DESCRIPTION",
    "",
    model_description
  )

  if (!is.null(context)) {
    prompt_parts <- c(prompt_parts,
      "",
      "## ADDITIONAL CONTEXT",
      "",
      context
    )
  }

  full_prompt <- paste(prompt_parts, collapse = "\n")

  # Output handling
  cli::cli_h2("VAST-to-Narrative Prompt Generated")
  cli::cli_alert_info("Model: {model$title}")
  cli::cli_alert_info("Prompt length: {nchar(full_prompt)} characters (~{round(nchar(full_prompt)/4)} tokens)")

  if (copy_to_clipboard) {
    if (requireNamespace("clipr", quietly = TRUE)) {
      tryCatch({
        clipr::write_clip(full_prompt)
        cli::cli_alert_success("Prompt copied to clipboard.")
      }, error = function(e) {
        cli::cli_alert_warning("Could not copy to clipboard: {e$message}")
      })
    } else {
      cli::cli_alert_warning(
        "Install the {.pkg clipr} package to enable clipboard copying."
      )
    }
  }

  if (!is.null(output_file)) {
    writeLines(full_prompt, output_file)
    cli::cli_alert_success("Prompt written to {.file {output_file}}")
  }

  invisible(full_prompt)
}
