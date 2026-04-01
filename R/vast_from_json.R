#' Build VAST Models from LLM JSON Output
#'
#' Parses the JSON output from an LLM (generated via a [narrative_to_prompt()]
#' prompt) and constructs one or two vastR model objects, along with
#' translation notes and a rigor assessment.
#'
#' @param json_input Either a file path to a JSON file, or a JSON string.
#' @param which Character. Which model(s) to build: `"both"` (default),
#'   `"closest"`, or `"distant"`.
#' @param render Logical. If `TRUE`, renders the model(s) immediately via
#'   `vastR::vast_render()`. Default is `FALSE`.
#'
#' @return A list of class `vast_translation` with components:
#' \describe{
#'   \item{closest_match}{A vastR model object (if requested).}
#'   \item{distant_compatible}{A vastR model object (if requested).}
#'   \item{translation_notes}{A list containing auxiliary assumptions,
#'     underspecification points, and the model divergence summary.}
#'   \item{rigor_assessment}{A list containing the T-item ratings and
#'     overall assessment.}
#'   \item{source_text}{The original narrative paragraph.}
#'   \item{r_code}{A list with `closest_match` and/or `distant_compatible`
#'     entries, each containing the R code string that reproduces the model.}
#' }
#'
#' @details
#' The function maps JSON node/edge specifications to the exact vastR
#' constructors:
#'
#' | JSON type | vastR function |
#' |-----------|---------------|
#' | concept | [vastR::vast_concept()] |
#' | name | [vastR::vast_name()] |
#' | data | [vastR::vast_data()] |
#' | is | [vastR::vast_is()] |
#' | ought | [vastR::vast_ought()] |
#' | perspective | [vastR::vast_perspective()] |
#' | diamond | [vastR::vast_diamond()] |
#' | noise_source | [vastR::vast_noise_source()] |
#'
#' For edges:
#'
#' | JSON type | vastR function |
#' |-----------|---------------|
#' | naming | [vastR::vast_naming()] |
#' | implication | [vastR::vast_implication()] |
#' | causation | [vastR::vast_causation()] |
#' | transformation | [vastR::vast_transformation()] |
#' | prediction | [vastR::vast_prediction()] |
#' | reasoning | [vastR::vast_reasoning()] |
#' | unknown | [vastR::vast_unknown()] |
#' | noise | [vastR::vast_noise()] |
#' | structural | [vastR::vast_relation()] with `type = ""` |
#'
#' @examples
#' \dontrun{
#' result <- vast_from_json("llm_response.json")
#' vastR::vast_render(result$closest_match)
#' cat(result$r_code$closest_match)
#' print_translation_notes(result)
#' }
#'
#' @seealso [narrative_to_prompt()], [print_translation_notes()],
#'   [print_rigor_assessment()]
#'
#' @export
vast_from_json <- function(json_input, which = "both", render = FALSE) {

  which <- match.arg(which, c("both", "closest", "distant"))

  parsed <- parse_vast_json(json_input)

  result <- list(
    source_text = parsed$source_text,
    closest_match = NULL,
    distant_compatible = NULL,
    translation_notes = parsed$translation_notes,
    rigor_assessment = parsed$rigor_assessment,
    r_code = list(),
    .parsed_specs = parsed$models,  # internal: for diffing and serialization
    model_diff = NULL
  )

  if (which %in% c("both", "closest")) {
    build <- build_vast_model(parsed$models$closest_match)
    result$closest_match <- build$model
    result$r_code$closest_match <- build$code
  }

  if (which %in% c("both", "distant")) {
    build <- build_vast_model(parsed$models$distant_compatible)
    result$distant_compatible <- build$model
    result$r_code$distant_compatible <- build$code
  }

  # Compute model diff if both specs are available
  if (which == "both" &&
      !is.null(parsed$models$closest_match) &&
      !is.null(parsed$models$distant_compatible)) {
    result$model_diff <- diff_models(
      parsed$models$closest_match,
      parsed$models$distant_compatible
    )
  }

  # Summary output
  cli::cli_h2("VAST Models Built from JSON")

  if (!is.null(result$closest_match)) {
    n_nodes <- length(parsed$models$closest_match$nodes)
    n_edges <- length(parsed$models$closest_match$edges)
    cli::cli_alert_success(
      "Closest match: {n_nodes} nodes, {n_edges} edges"
    )
  }

  if (!is.null(result$distant_compatible)) {
    n_nodes <- length(parsed$models$distant_compatible$nodes)
    n_edges <- length(parsed$models$distant_compatible$edges)
    cli::cli_alert_success(
      "Distant compatible: {n_nodes} nodes, {n_edges} edges"
    )
  }

  n_assumptions <- length(parsed$translation_notes$auxiliary_assumptions)
  n_underspec <- length(parsed$translation_notes$underspecification_points)
  cli::cli_alert_info(
    "Translation notes: {n_assumptions} auxiliary assumptions, {n_underspec} underspecification points"
  )

  if (render) {
    if (!is.null(result$closest_match)) {
      cli::cli_h3("Closest Match Model")
      vastR::vast_render(result$closest_match)
    }
    if (!is.null(result$distant_compatible)) {
      cli::cli_h3("Distant Compatible Model")
      vastR::vast_render(result$distant_compatible)
    }
  }

  class(result) <- "vast_translation"
  invisible(result)
}


# ============================================================================
# Internal: JSON parsing
# ============================================================================

#' Parse JSON input (file or string) into an R list
#'
#' Handles common LLM output quirks: preamble text before the JSON,
#' markdown code fences, and trailing commentary after the closing brace.
#' Strategy: find the first `{`, then match its closing `}` accounting
#' for nesting, and extract that substring for parsing.
#'
#' @noRd
parse_vast_json <- function(json_input) {
  if (!is.character(json_input) || length(json_input) != 1) {
    cli::cli_abort("{.arg json_input} must be a single character string (file path or JSON).")
  }

  if (file.exists(json_input)) {
    raw_string <- paste(readLines(json_input, warn = FALSE), collapse = "\n")
  } else {
    raw_string <- json_input
  }

  # Extract the outermost JSON object by brace matching
  json_string <- extract_json_object(raw_string)

  if (is.null(json_string)) {
    cli::cli_abort(c(
      "Could not find a valid JSON object in the input.",
      "i" = "The input must contain a top-level JSON object (opening and closing braces).",
      "i" = "Preamble text and markdown fences are handled automatically."
    ))
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(json_string, simplifyVector = FALSE),
    error = function(e) {
      # Escape braces in the error message to prevent cli glue interpolation
      safe_msg <- gsub("\\{", "{{", gsub("\\}", "}}", e$message))
      cli::cli_abort(c(
        "Found a JSON object but failed to parse it.",
        "x" = safe_msg,
        "i" = "Common issues: trailing commas, unescaped quotes, invalid escapes."
      ))
    }
  )

  required_fields <- c("models", "translation_notes")
  missing <- setdiff(required_fields, names(parsed))
  if (length(missing) > 0) {
    cli::cli_abort(
      "JSON is missing required top-level field(s): {.val {missing}}"
    )
  }

  parsed
}


#' Extract the outermost JSON object from a string
#'
#' Finds the first `{`, then walks the string character by character
#' tracking brace nesting depth (while ignoring braces inside JSON
#' string literals). Returns the substring from the opening `{` to
#' its matching `}`, or NULL if no valid object is found.
#'
#' @param x A character string potentially containing a JSON object.
#' @return The extracted JSON string, or NULL.
#' @noRd
extract_json_object <- function(x) {
  chars <- strsplit(x, "")[[1]]
  n <- length(chars)

  # Find the first opening brace
  start <- match("{", chars)
  if (is.na(start)) return(NULL)

  depth <- 0
  in_string <- FALSE
  escape_next <- FALSE

  for (pos in start:n) {
    ch <- chars[pos]

    if (escape_next) {
      escape_next <- FALSE
      next
    }

    if (ch == "\\") {
      if (in_string) {
        escape_next <- TRUE
      }
      next
    }

    if (ch == '"') {
      in_string <- !in_string
      next
    }

    if (!in_string) {
      if (ch == "{") {
        depth <- depth + 1
      } else if (ch == "}") {
        depth <- depth - 1
        if (depth == 0) {
          return(substr(x, start, pos))
        }
      }
    }
  }

  # Unbalanced braces — return NULL
  NULL
}


# ============================================================================
# Internal: Model building
# ============================================================================

#' Build a vastR model from a parsed model specification
#' @noRd
build_vast_model <- function(spec) {

  title <- spec$title %||% "VAST Model"
  naming_mode <- spec$naming_mode %||% "separated"
  analyst <- spec$analyst %||% ""

  # --- Pre-pass: build lookup tables for comments ---
  # Map concept IDs to their name labels (for commenting)
  name_lookup <- build_name_lookup(spec)
  # Map edge from/to pairs to readable descriptions
  edge_type_labels <- c(
    naming = "naming", implication = "implication", causation = "causation",
    transformation = "transformation", prediction = "prediction",
    reasoning = "reasoning", unknown = "unknown", noise = "noise",
    structural = "structural link"
  )

  code_lines <- character()

  # --- Header ---
  code_lines <- c(code_lines,
    sprintf("# VAST Model: %s", title),
    sprintf("# Analyst: %s", analyst),
    sprintf("# Naming mode: %s", naming_mode),
    ""
  )

  if (nzchar(analyst)) {
    code_lines <- c(code_lines, sprintf(
      'model <- vast_model(title = "%s", naming_mode = "%s", analyst = "%s")',
      escape_quotes(title), naming_mode, escape_quotes(analyst)
    ))
  } else {
    code_lines <- c(code_lines, sprintf(
      'model <- vast_model(title = "%s", naming_mode = "%s")',
      escape_quotes(title), naming_mode
    ))
  }

  # --- Concept nodes (non-name, non-IS/OUGHT) ---
  concept_nodes <- Filter(function(n) n$type %||% "concept" == "concept", spec$nodes)
  other_nodes <- Filter(function(n) {
    t <- n$type %||% "concept"
    t %in% c("data", "diamond", "noise_source")
  }, spec$nodes)
  name_nodes <- Filter(function(n) n$type %||% "concept" == "name", spec$nodes)

  if (length(concept_nodes) > 0 || length(other_nodes) > 0 || length(name_nodes) > 0) {
    node_lines <- character()

    # Concepts first, with name comments
    if (length(concept_nodes) > 0) {
      node_lines <- c(node_lines, "    # Concepts")
      for (node in concept_nodes) {
        call <- build_node_call(node)
        name_label <- name_lookup[[node$id]]
        if (!is.null(name_label)) {
          call <- paste0(call, sprintf(',  # "%s"', name_label))
        } else {
          call <- paste0(call, ",")
        }
        node_lines <- c(node_lines, paste0("    ", call))
      }
    }

    # Data, diamond, noise_source nodes
    if (length(other_nodes) > 0) {
      node_lines <- c(node_lines, "    # Other nodes")
      for (node in other_nodes) {
        call <- build_node_call(node)
        comment <- switch(node$type %||% "",
          "data" = "  # empirical data",
          "diamond" = sprintf("  # connective: %s", node$diamond_content %||% node$label %||% ""),
          "noise_source" = "  # noise source",
          ""
        )
        call <- paste0(call, ",", comment)
        node_lines <- c(node_lines, paste0("    ", call))
      }
    }

    # Name nodes
    if (length(name_nodes) > 0) {
      node_lines <- c(node_lines, "    # Names")
      for (node in name_nodes) {
        call <- build_node_call(node)
        call <- paste0(call, ",")
        node_lines <- c(node_lines, paste0("    ", call))
      }
    }

    # Remove trailing comma from last node call
    last_idx <- length(node_lines)
    node_lines[last_idx] <- sub(",\\s*(#.*)?$", "\\1", node_lines[last_idx])

    code_lines <- c(code_lines, "", paste0(
      "model <- model |>\n  add_nodes(\n",
      paste(node_lines, collapse = "\n"),
      "\n  )"
    ))
  }

  # --- Edges ---
  naming_edges <- Filter(function(e) (e$type %||% "") == "naming", spec$edges)
  substantive_edges <- Filter(function(e) {
    t <- e$type %||% ""
    t != "naming" && t != ""
  }, spec$edges)

  if (length(substantive_edges) > 0 || length(naming_edges) > 0) {
    edge_lines <- character()

    # Substantive relationships first
    if (length(substantive_edges) > 0) {
      edge_lines <- c(edge_lines, "    # Relationships")
      for (edge in substantive_edges) {
        call <- build_edge_call(edge)
        if (is.null(call)) next
        # Build a human-readable comment
        from_name <- name_lookup[[edge$from]] %||% edge$from
        to_name <- name_lookup[[edge$to]] %||% edge$to
        type_label <- edge_type_labels[[edge$type %||% "unknown"]] %||% edge$type
        comment <- sprintf('  # %s: "%s" -> "%s"', type_label, from_name, to_name)
        call <- paste0(call, ",", comment)
        edge_lines <- c(edge_lines, paste0("    ", call))
      }
    }

    # Naming relationships
    if (length(naming_edges) > 0) {
      edge_lines <- c(edge_lines, "    # Naming relationships")
      for (edge in naming_edges) {
        call <- build_edge_call(edge)
        if (is.null(call)) next
        call <- paste0(call, ",")
        edge_lines <- c(edge_lines, paste0("    ", call))
      }
    }

    # Remove trailing comma from last edge call
    last_idx <- length(edge_lines)
    edge_lines[last_idx] <- sub(",\\s*(#.*)?$", "\\1", edge_lines[last_idx])

    code_lines <- c(code_lines, "", paste0(
      "model <- model |>\n  add_edges(\n",
      paste(edge_lines, collapse = "\n"),
      "\n  )"
    ))
  }

  # --- Groups ---
  if (length(spec$groups) > 0) {
    group_lines <- character()
    group_lines <- c(group_lines, "    # Higher-order concepts")
    for (group in spec$groups) {
      call <- build_group_call(group)
      call <- paste0(call, ",")
      group_lines <- c(group_lines, paste0("    ", call))
    }
    last_idx <- length(group_lines)
    group_lines[last_idx] <- sub(",\\s*(#.*)?$", "\\1", group_lines[last_idx])

    code_lines <- c(code_lines, "", paste0(
      "model <- model |>\n  add_groups(\n",
      paste(group_lines, collapse = "\n"),
      "\n  )"
    ))
  }

  # --- IS/OUGHT statements ---
  # Build group lookup for IS/OUGHT-on-groups support
  group_ids <- vapply(spec$groups %||% list(), function(g) g$id, character(1))
  group_members <- list()
  for (g in spec$groups %||% list()) {
    group_members[[g$id]] <- g$members %||% g$node_ids %||% character(0)
  }

  if (length(spec$is_statements) > 0) {
    code_lines <- c(code_lines, "", "# IS statements")
    for (stmt in spec$is_statements) {
      concept_name <- name_lookup[[stmt$concept_id]] %||% stmt$concept_id
      code_lines <- c(code_lines, sprintf('# IS for "%s"', concept_name))
      extra <- build_is_ought_code(stmt, "is",
                                   group_ids = group_ids,
                                   group_members = group_members)
      code_lines <- c(code_lines, extra)
    }
  }

  if (length(spec$ought_statements) > 0) {
    code_lines <- c(code_lines, "", "# OUGHT statements")
    for (stmt in spec$ought_statements) {
      concept_name <- name_lookup[[stmt$concept_id]] %||% stmt$concept_id
      code_lines <- c(code_lines, sprintf('# OUGHT for "%s"', concept_name))
      extra <- build_is_ought_code(stmt, "ought",
                                   group_ids = group_ids,
                                   group_members = group_members)
      code_lines <- c(code_lines, extra)
    }
  }

  full_code <- paste(code_lines, collapse = "\n")

  # Execute to build the actual model object
  # (comments are ignored by the parser)
  env <- new.env(parent = loadNamespace("vastR"))
  model <- tryCatch(
    eval(parse(text = full_code), envir = env),
    error = function(e) {
      cli::cli_warn(c(
        "Failed to build vastR model from JSON specification.",
        "x" = e$message,
        "i" = "The generated R code is still available in the result."
      ))
      NULL
    }
  )

  list(model = model, code = full_code)
}


#' Build a lookup table mapping concept IDs to their name labels
#'
#' Scans the node and edge lists to find naming relationships and
#' returns a named list: concept_id -> name_label.
#'
#' @param spec A parsed model specification (list with nodes and edges).
#' @return A named list mapping concept IDs to their name labels.
#' @noRd
build_name_lookup <- function(spec) {
  lookup <- list()

  # Build a quick ID -> node map
  node_map <- list()
  for (node in spec$nodes) {
    node_map[[node$id]] <- node
  }

  # Find naming edges and resolve them
  for (edge in spec$edges) {
    if ((edge$type %||% "") != "naming") next
    from_id <- edge$from
    to_id <- edge$to
    target_node <- node_map[[to_id]]
    if (!is.null(target_node) && (target_node$type %||% "") == "name") {
      # Strip any quote characters the LLM may have added
      label <- target_node$label %||% to_id
      label <- gsub('^\u201C|\u201D$|^"|"$', '', label)
      lookup[[from_id]] <- label
    }
  }

  lookup
}


# ============================================================================
# Internal: Node code generation
# ============================================================================

#' Build a single node function call string
#'
#' Maps JSON node types to vastR constructors with correct signatures:
#' - vast_concept(id, label = id, ...)
#' - vast_name(id, label, ...)           # label is auto-quoted with curly quotes
#' - vast_data(id, label = id, ...)
#' - vast_is(id, value = NULL, ...)
#' - vast_ought(id, value = NULL, ...)
#' - vast_perspective(id, holder, value = NULL, ...)
#' - vast_diamond(id, label = "AND", ...)
#' - vast_noise_source(id)
#'
#' @noRd
build_node_call <- function(node) {
  id <- node$id
  label <- node$label %||% id
  type <- node$type %||% "concept"

  switch(type,

    "concept" = sprintf(
      'vast_concept("%s", label = "%s")',
      id, escape_quotes(label)
    ),

    "name" = {
      # vast_name() auto-wraps label in curly quotes (\u201C...\u201D)
      # Strip any quotes the LLM may have added
      clean_label <- label
      clean_label <- gsub('^\u201C|\u201D$', '', clean_label)
      clean_label <- gsub('^"|"$', '', clean_label)
      clean_label <- gsub('^\\"|\\"$', '', clean_label)
      sprintf('vast_name("%s", label = "%s")', id, escape_quotes(clean_label))
    },

    "data" = sprintf(
      'vast_data("%s", label = "%s")',
      id, escape_quotes(label)
    ),

    "is" = {
      value <- node$value
      if (!is.null(value)) {
        sprintf('vast_is("%s", value = %s)', id, format_value(value))
      } else {
        sprintf('vast_is("%s")', id)
      }
    },

    "ought" = {
      value <- node$value
      if (!is.null(value)) {
        sprintf('vast_ought("%s", value = %s)', id, format_value(value))
      } else {
        sprintf('vast_ought("%s")', id)
      }
    },

    "perspective" = {
      # vast_perspective(id, holder, value = NULL, ...)
      holder <- node$holder %||% node$label %||% id
      value <- node$value
      if (!is.null(value)) {
        sprintf(
          'vast_perspective("%s", holder = "%s", value = %s)',
          id, escape_quotes(holder), format_value(value)
        )
      } else {
        sprintf(
          'vast_perspective("%s", holder = "%s")',
          id, escape_quotes(holder)
        )
      }
    },

    "diamond" = {
      # vast_diamond(id, label = "AND", ...)
      content <- node$diamond_content %||% node$label %||% "AND"
      sprintf('vast_diamond("%s", label = "%s")', id, escape_quotes(content))
    },

    "noise_source" = sprintf('vast_noise_source("%s")', id),

    # Fallback: treat as concept
    sprintf('vast_concept("%s", label = "%s")', id, escape_quotes(label))
  )
}


# ============================================================================
# Internal: Edge code generation
# ============================================================================

#' Build a single edge function call string
#'
#' Maps JSON edge types to vastR constructors with correct signatures:
#' - vast_naming(from, to, strength = NULL, index = NULL, lhead = NULL, ltail = NULL)
#' - vast_causation(from, to, strength, index, lhead, ltail)  [same pattern for all typed edges]
#' - vast_noise(noise_id, to, strength = NULL, color = "gray50")
#' - vast_relation(from, to, type = "", ...) for plain structural links
#'
#' @noRd
build_edge_call <- function(edge) {
  from <- edge$from
  to <- edge$to
  type <- edge$type %||% "unknown"

  fn_info <- switch(type,
    "naming"         = list(fn = "vast_naming",         style = "standard"),
    "implication"    = list(fn = "vast_implication",     style = "standard"),
    "causation"      = list(fn = "vast_causation",       style = "standard"),
    "transformation" = list(fn = "vast_transformation",  style = "standard"),
    "prediction"     = list(fn = "vast_prediction",      style = "standard"),
    "reasoning"      = list(fn = "vast_reasoning",       style = "standard"),
    "unknown"        = list(fn = "vast_unknown",         style = "standard"),
    "noise"          = list(fn = "vast_noise",           style = "noise"),
    "structural"     = list(fn = "vast_relation",        style = "structural"),
    # Fallback
    list(fn = "vast_unknown", style = "standard")
  )

  # --- Noise edges: vast_noise(noise_id, to, strength) ---
  if (fn_info$style == "noise") {
    if (is.null(from)) {
      cli::cli_warn("Noise edge to {.val {to}} has no noise_id (from). Skipping.")
      return(NULL)
    }
    args <- c(sprintf('"%s"', from), sprintf('"%s"', to))
    if (!is.null(edge$strength)) {
      args <- c(args, paste0("strength = ", format_strength(edge$strength)))
    }
    return(sprintf("%s(%s)", fn_info$fn, paste(args, collapse = ", ")))
  }

  # --- Structural edges: vast_relation(from, to, type = "") ---
  if (fn_info$style == "structural") {
    if (is.null(from) || is.null(to)) {
      cli::cli_warn("Structural edge missing from/to. Skipping.")
      return(NULL)
    }
    args <- c(sprintf('"%s"', from), sprintf('"%s"', to), 'type = ""')
    # Compound edge attributes (for IS/OUGHT pointing to groups, etc.)
    lhead <- edge$lhead %||% edge$compound$lhead
    ltail <- edge$ltail %||% edge$compound$ltail
    if (!is.null(lhead)) args <- c(args, sprintf('lhead = "%s"', lhead))
    if (!is.null(ltail)) args <- c(args, sprintf('ltail = "%s"', ltail))
    return(sprintf("vast_relation(%s)", paste(args, collapse = ", ")))
  }

  # --- Standard typed relationship edges ---
  if (is.null(from) || is.null(to)) {
    cli::cli_warn("Edge of type {.val {type}} missing from/to node. Skipping.")
    return(NULL)
  }

  args <- c(sprintf('"%s"', from), sprintf('"%s"', to))

  # strength parameter
  if (!is.null(edge$strength)) {
    args <- c(args, paste0("strength = ", format_strength(edge$strength)))
  }

  # index parameter (e.g., "1" renders as "c1" on the arrow)
  index <- edge$index
  if (!is.null(index)) {
    args <- c(args, sprintf('index = "%s"', index))
  }

  # compound edge attributes (lhead/ltail)
  lhead <- edge$lhead %||% edge$compound$lhead
  ltail <- edge$ltail %||% edge$compound$ltail
  if (!is.null(lhead)) args <- c(args, sprintf('lhead = "%s"', lhead))
  if (!is.null(ltail)) args <- c(args, sprintf('ltail = "%s"', ltail))

  sprintf("%s(%s)", fn_info$fn, paste(args, collapse = ", "))
}


# ============================================================================
# Internal: Group code generation
# ============================================================================

#' Build a group function call string
#'
#' Maps to vast_group(id, label, node_ids, ...) or
#' vast_nested_group(id, label, node_ids, child_group_ids, ...)
#'
#' @noRd
build_group_call <- function(group) {
  id <- group$id
  label <- group$label %||% id
  # JSON may use "members" or "node_ids"
  members <- group$members %||% group$node_ids %||% character(0)
  children <- group$child_group_ids %||% character(0)

  members_str <- paste(sprintf('"%s"', members), collapse = ", ")

  if (length(children) > 0) {
    children_str <- paste(sprintf('"%s"', children), collapse = ", ")
    sprintf(
      'vast_nested_group("%s", label = "%s", node_ids = c(%s), child_group_ids = c(%s))',
      id, escape_quotes(label), members_str, children_str
    )
  } else {
    sprintf(
      'vast_group("%s", label = "%s", node_ids = c(%s))',
      id, escape_quotes(label), members_str
    )
  }
}


# ============================================================================
# Internal: IS/OUGHT statement code generation
# ============================================================================

#' Build code for IS/OUGHT statements and their perspective connections
#'
#' Generates add_nodes() and add_edges() calls for:
#' - The IS/OUGHT pentagon node via vast_is() / vast_ought()
#' - A structural edge (vast_relation with type="") connecting it to the concept
#'   - If concept_id refers to a group, the edge targets a member node with
#'     lhead pointing to the group (compound edge)
#' - Any perspective nodes via vast_perspective() and their connections
#'
#' @param stmt A parsed IS/OUGHT statement from the JSON spec.
#' @param type Character, "is" or "ought".
#' @param group_ids Character vector of all known group IDs in the model.
#'   When concept_id matches a group ID, a compound edge with lhead is generated.
#' @param group_members Named list mapping group IDs to their member node ID vectors.
#'   Used to pick a target node for the compound edge.
#'
#' @noRd
build_is_ought_code <- function(stmt, type = "is",
                                group_ids = character(0),
                                group_members = list()) {
  concept_id <- stmt$concept_id
  value <- stmt$value
  node_id <- paste0(toupper(type), "_", concept_id)

  code_parts <- character()

  # IS/OUGHT node
  fn <- if (type == "is") "vast_is" else "vast_ought"
  if (!is.null(value)) {
    node_call <- sprintf('%s("%s", value = %s)', fn, node_id, format_value(value))
  } else {
    node_call <- sprintf('%s("%s")', fn, node_id)
  }

  # Structural edge: IS/OUGHT → concept (plain line, no type label)
  # If concept_id is a group, use lhead to point the arrow to the group border,
  # targeting an arbitrary member node inside the group as the physical endpoint.
  is_group <- concept_id %in% group_ids
  if (is_group) {
    members <- group_members[[concept_id]]
    if (length(members) > 0) {
      # Target the first member node; the arrow visually connects to the group border
      target_node <- members[[1]]
      edge_call <- sprintf(
        'vast_relation("%s", "%s", type = "", lhead = "%s")',
        node_id, target_node, concept_id
      )
    } else {
      # Fallback: no members known, treat as regular concept
      cli::cli_warn(
        "IS/OUGHT statement targets group {.val {concept_id}} but no members found. Using direct edge."
      )
      edge_call <- sprintf(
        'vast_relation("%s", "%s", type = "")',
        node_id, concept_id
      )
    }
  } else {
    edge_call <- sprintf(
      'vast_relation("%s", "%s", type = "")',
      node_id, concept_id
    )
  }

  code_parts <- c(code_parts, sprintf(
    "model <- model |>\n  add_nodes(%s) |>\n  add_edges(%s)",
    node_call, edge_call
  ))

  # Perspective nodes and their connections to this IS/OUGHT
  if (length(stmt$perspectives) > 0) {
    for (persp in stmt$perspectives) {
      holder <- persp$holder
      agreement <- persp$agreement
      # Unique ID: combine holder name, type, and concept
      persp_id <- paste0("P_", gsub("[^A-Za-z0-9]", "_", holder), "_", node_id)

      if (!is.null(agreement)) {
        persp_node <- sprintf(
          'vast_perspective("%s", holder = "%s", value = %s)',
          persp_id, escape_quotes(holder), agreement
        )
      } else {
        persp_node <- sprintf(
          'vast_perspective("%s", holder = "%s")',
          persp_id, escape_quotes(holder)
        )
      }

      # Connect perspective → IS/OUGHT with structural edge
      persp_edge <- sprintf(
        'vast_relation("%s", "%s", type = "")',
        persp_id, node_id
      )

      code_parts <- c(code_parts, sprintf(
        "model <- model |>\n  add_nodes(%s) |>\n  add_edges(%s)",
        persp_node, persp_edge
      ))
    }
  }

  code_parts
}
