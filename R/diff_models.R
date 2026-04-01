# ============================================================================
# vastRai - Model comparison / diffing
# ============================================================================

#' Compare two VAST model specifications
#'
#' Produces a structured diff between two parsed model specs, identifying
#' shared structure, differences in relationship types/strengths, and
#' elements unique to each model.
#'
#' @param spec_a A parsed model specification (list with nodes, edges, etc.).
#' @param spec_b A parsed model specification.
#' @param name_a Character. Label for the first model. Default "closest_match".
#' @param name_b Character. Label for the second model. Default "distant_compatible".
#'
#' @return A list of class `vast_model_diff` with components:
#' \describe{
#'   \item{shared_edges}{Data frame of edges present in both models
#'     (matched by from/to), with columns showing type and strength in each.}
#'   \item{only_a}{Data frame of edges only in model A.}
#'   \item{only_b}{Data frame of edges only in model B.}
#'   \item{shared_nodes}{Character vector of node IDs present in both.}
#'   \item{only_a_nodes}{Character vector of node IDs only in A.}
#'   \item{only_b_nodes}{Character vector of node IDs only in B.}
#'   \item{name_a}{Label for model A.}
#'   \item{name_b}{Label for model B.}
#' }
#'
#' @noRd
diff_models <- function(spec_a, spec_b,
                        name_a = "closest_match",
                        name_b = "distant_compatible") {

  lookup_a <- build_name_lookup(spec_a)
  lookup_b <- build_name_lookup(spec_b)

  # --- Node comparison ---
  # Only compare concept-level nodes (not name nodes, which are derived)
  concept_ids_a <- vapply(
    Filter(function(n) (n$type %||% "concept") %in% c("concept", "data", "diamond"),
           spec_a$nodes),
    function(n) n$id, character(1)
  )
  concept_ids_b <- vapply(
    Filter(function(n) (n$type %||% "concept") %in% c("concept", "data", "diamond"),
           spec_b$nodes),
    function(n) n$id, character(1)
  )

  shared_nodes <- intersect(concept_ids_a, concept_ids_b)
  only_a_nodes <- setdiff(concept_ids_a, concept_ids_b)
  only_b_nodes <- setdiff(concept_ids_b, concept_ids_a)

  # --- Edge comparison ---
  # Only compare substantive edges (not naming or structural)
  substantive_types <- c("implication", "causation", "transformation",
                         "prediction", "reasoning", "unknown", "noise")

  edges_a <- Filter(function(e) (e$type %||% "") %in% substantive_types, spec_a$edges)
  edges_b <- Filter(function(e) (e$type %||% "") %in% substantive_types, spec_b$edges)

  # Key edges by from->to pair
  edge_key <- function(e) paste0(e$from, "->", e$to)

  keyed_a <- list()
  for (e in edges_a) keyed_a[[edge_key(e)]] <- e
  keyed_b <- list()
  for (e in edges_b) keyed_b[[edge_key(e)]] <- e

  all_keys <- union(names(keyed_a), names(keyed_b))

  shared_rows <- list()
  only_a_rows <- list()
  only_b_rows <- list()

  for (key in all_keys) {
    ea <- keyed_a[[key]]
    eb <- keyed_b[[key]]

    if (!is.null(ea) && !is.null(eb)) {
      # Edge exists in both — compare
      from_name <- lookup_a[[ea$from]] %||% lookup_b[[ea$from]] %||% ea$from
      to_name <- lookup_a[[ea$to]] %||% lookup_b[[ea$to]] %||% ea$to

      differs <- !identical(ea$type, eb$type) ||
                 !identical(ea$strength, eb$strength)

      shared_rows[[length(shared_rows) + 1]] <- data.frame(
        from = ea$from,
        to = ea$to,
        from_name = from_name,
        to_name = to_name,
        type_a = ea$type %||% "",
        strength_a = format_strength_display(ea$strength),
        type_b = eb$type %||% "",
        strength_b = format_strength_display(eb$strength),
        differs = differs,
        stringsAsFactors = FALSE
      )

    } else if (!is.null(ea)) {
      from_name <- lookup_a[[ea$from]] %||% ea$from
      to_name <- lookup_a[[ea$to]] %||% ea$to
      only_a_rows[[length(only_a_rows) + 1]] <- data.frame(
        from = ea$from, to = ea$to,
        from_name = from_name, to_name = to_name,
        type = ea$type %||% "",
        strength = format_strength_display(ea$strength),
        stringsAsFactors = FALSE
      )

    } else {
      from_name <- lookup_b[[eb$from]] %||% eb$from
      to_name <- lookup_b[[eb$to]] %||% eb$to
      only_b_rows[[length(only_b_rows) + 1]] <- data.frame(
        from = eb$from, to = eb$to,
        from_name = from_name, to_name = to_name,
        type = eb$type %||% "",
        strength = format_strength_display(eb$strength),
        stringsAsFactors = FALSE
      )
    }
  }

  shared_edges <- if (length(shared_rows) > 0) do.call(rbind, shared_rows) else
    data.frame(from = character(), to = character(), from_name = character(),
               to_name = character(), type_a = character(), strength_a = character(),
               type_b = character(), strength_b = character(), differs = logical(),
               stringsAsFactors = FALSE)

  only_a_edges <- if (length(only_a_rows) > 0) do.call(rbind, only_a_rows) else
    data.frame(from = character(), to = character(), from_name = character(),
               to_name = character(), type = character(), strength = character(),
               stringsAsFactors = FALSE)

  only_b_edges <- if (length(only_b_rows) > 0) do.call(rbind, only_b_rows) else
    data.frame(from = character(), to = character(), from_name = character(),
               to_name = character(), type = character(), strength = character(),
               stringsAsFactors = FALSE)

  result <- list(
    shared_edges = shared_edges,
    only_a = only_a_edges,
    only_b = only_b_edges,
    shared_nodes = shared_nodes,
    only_a_nodes = only_a_nodes,
    only_b_nodes = only_b_nodes,
    name_a = name_a,
    name_b = name_b
  )
  class(result) <- "vast_model_diff"
  result
}


#' Format a strength value for display (human readable)
#' @noRd
format_strength_display <- function(strength) {
  if (is.null(strength)) return("(default)")
  as.character(strength)
}


#' Print a model comparison
#'
#' @param x A `vast_model_diff` object.
#' @param ... Ignored.
#' @export
print.vast_model_diff <- function(x, ...) {
  cli::cli_h1("Model Comparison: {x$name_a} vs. {x$name_b}")

  # Node differences
  if (length(x$only_a_nodes) > 0 || length(x$only_b_nodes) > 0) {
    cli::cli_h2("Node Differences")
    if (length(x$only_a_nodes) > 0) {
      cli::cli_alert_info("Only in {x$name_a}: {paste(x$only_a_nodes, collapse = ', ')}")
    }
    if (length(x$only_b_nodes) > 0) {
      cli::cli_alert_info("Only in {x$name_b}: {paste(x$only_b_nodes, collapse = ', ')}")
    }
    cli::cli_text("{length(x$shared_nodes)} shared concept nodes.")
  }

  # Shared edges with differences
  differing <- x$shared_edges[x$shared_edges$differs, , drop = FALSE]
  matching <- x$shared_edges[!x$shared_edges$differs, , drop = FALSE]

  if (nrow(matching) > 0) {
    cli::cli_h2("Shared Relationships (identical)")
    for (i in seq_len(nrow(matching))) {
      row <- matching[i, ]
      cli::cli_text(
        '  {row$from_name} --[{row$type_a} {row$strength_a}]--> {row$to_name}'
      )
    }
  }

  if (nrow(differing) > 0) {
    cli::cli_h2("Shared Relationships (differ)")
    for (i in seq_len(nrow(differing))) {
      row <- differing[i, ]
      cli::cli_text('  {row$from_name} -> {row$to_name}:')
      cli::cli_text('    {x$name_a}: {row$type_a} {row$strength_a}')
      cli::cli_text('    {x$name_b}: {row$type_b} {row$strength_b}')
    }
  }

  # Unique edges
  if (nrow(x$only_a) > 0) {
    cli::cli_h2("Only in {x$name_a}")
    for (i in seq_len(nrow(x$only_a))) {
      row <- x$only_a[i, ]
      cli::cli_text(
        '  {row$from_name} --[{row$type} {row$strength}]--> {row$to_name}'
      )
    }
  }

  if (nrow(x$only_b) > 0) {
    cli::cli_h2("Only in {x$name_b}")
    for (i in seq_len(nrow(x$only_b))) {
      row <- x$only_b[i, ]
      cli::cli_text(
        '  {row$from_name} --[{row$type} {row$strength}]--> {row$to_name}'
      )
    }
  }

  invisible(x)
}


#' Compare the two models in a VAST translation result
#'
#' Produces a structured comparison of the closest match and distant
#' compatible models, showing shared structure, differences, and
#' unique elements.
#'
#' @param x A `vast_translation` object returned by [vast_from_json()].
#'
#' @return A `vast_model_diff` object (also printed to console).
#'
#' @export
compare_models <- function(x) {
  if (!inherits(x, "vast_translation")) {
    cli::cli_abort("{.arg x} must be a {.cls vast_translation} object from {.fn vast_from_json}.")
  }

  if (!is.null(x$model_diff)) {
    # Already computed during vast_from_json
    print(x$model_diff)
    return(invisible(x$model_diff))
  }

  # Compute from stored specs
  if (is.null(x$.parsed_specs$closest_match) ||
      is.null(x$.parsed_specs$distant_compatible)) {
    cli::cli_abort(
      "Both model specs are required for comparison. Re-run {.fn vast_from_json} with {.code which = \"both\"}."
    )
  }

  result <- diff_models(
    x$.parsed_specs$closest_match,
    x$.parsed_specs$distant_compatible
  )
  print(result)
  invisible(result)
}
