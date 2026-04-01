#' Generate a VAST Translation Prompt from Theory Text
#'
#' Creates a structured prompt that can be pasted into a large language model
#' (e.g., Claude, ChatGPT) to translate theory text into VAST model
#' specifications and assess its theoretical rigor. The LLM will return JSON
#' that can be processed by [vast_from_json()].
#'
#' @param theory Character string. The theory text to translate. This may be
#'   a single paragraph, multiple paragraphs, or an extended excerpt from a
#'   paper. The LLM will distinguish between theoretical claims (which become
#'   VAST model elements) and other content (which informs the rigor
#'   assessment). To get the best results, include any of the following that
#'   are available in the paper:
#'
#'   \describe{
#'     \item{Theoretical claims}{The core of the translation. Concepts,
#'       relationships, mechanisms, and causal chains become nodes and edges
#'       in the VAST model.}
#'     \item{Phenomenon description}{What the theory aims to explain. Feeds
#'       the T1 (Phenomenon) rigor assessment. Often found in the introduction
#'       or the opening of the theory section.}
#'     \item{Concept definitions}{Verbal definitions of key terms. Feeds the
#'       T2 (Concept Definitions) assessment. May be in the same paragraph, a
#'       methods section, or a prior paper.}
#'     \item{Mechanistic detail}{How the concepts bring about the phenomenon,
#'       including intermediate steps and causal pathways. Feeds the T3
#'       (Mechanism) assessment, which evaluates completeness, explanatory
#'       quality, and sufficiency of the causal chain.}
#'     \item{Scope statements}{Boundary conditions, target populations,
#'       simplifying assumptions. Feeds the T4 (Scope) assessment. Often found
#'       in the discussion or limitations section. Many theory texts are
#'       entirely silent on scope.}
#'     \item{Empirical evidence}{Data or findings that serve as premises in
#'       the theoretical argument (modeled as data nodes). Evidence cited as
#'       background is used for rigor assessment only.}
#'   }
#'
#' @param context Character string or NULL. Optional additional context about the
#'   theory, domain, or specific instructions for the translation. For example,
#'   you might specify which concepts are empirically measured (data nodes), or
#'   clarify terminology. This does NOT become part of the VAST model; it guides
#'   the LLM's interpretation.
#' @param analyst Character string or NULL. Name of the analyst to record in
#'   the VAST models. If NULL (the default), the prompt instructs the LLM to
#'   identify itself (e.g., "Claude" or "ChatGPT"). If provided, this name is
#'   used in the analyst field of both models. Use this when you (the user)
#'   want to take responsibility for the structural choices from the start.
#' @param copy_to_clipboard Logical. If `TRUE` (the default when the `clipr`
#'   package is available), copies the prompt to the system clipboard.
#' @param output_file Character string or NULL. If provided, writes the prompt
#'   to this file path.
#'
#' @return Invisibly returns the full prompt as a character string. The prompt
#'   is also printed to the console (and optionally copied to clipboard or
#'   written to a file).
#'
#' @details
#' The generated prompt instructs the LLM to:
#'
#' 1. Produce **two VAST models** in JSON format:
#'    - A **closest match** model (minimum auxiliary assumptions)
#'    - A **distant but compatible** model (structurally different but still
#'      consistent with the text, revealing interpretive degrees of freedom)
#'
#' 2. Document all **auxiliary assumptions** — every interpretive choice not
#'    directly forced by the text.
#'
#' 3. Identify **underspecification points** in the narrative.
#'
#' 4. Assess **theoretical rigor** against T1-T4 from Musfeld et al.'s
#'    Indicators for the Assessment of Theoretical Rigor (v3.0), using an
#'    extended three-level scale (clearly_specified, partially_specified,
#'    unspecified).
#'
#' The rigor assessment evaluates the narrative text, not the model. A text
#' may be underspecified even when the model is fully connected — that
#' underspecification shows up as auxiliary assumptions. Users can iterate:
#' translate text to a VAST model, generate a narrative from the model with
#' [vast_to_prompt()], and assess again to see if specification has improved.
#'
#' The workflow is:
#' 1. Call `narrative_to_prompt("your theory text")` to generate the prompt.
#' 2. Paste the prompt into a capable LLM chatbot.
#' 3. Save the LLM's JSON response to a file.
#' 4. Call `vast_from_json("response.json")` to build vastR models.
#'
#' @examples
#' \dontrun{
#' # Translate a single theory paragraph
#' prompt <- narrative_to_prompt(
#'   theory = "Concepts are organizing structures that define how contents
#'     are related to each other. Their formation inherently depends on
#'     generalization over, and integration of experiences."
#' )
#'
#' # Include surrounding context for better rigor assessment
#' prompt <- narrative_to_prompt(
#'   theory = paste(
#'     "The hippocampus plays a key role in concept formation.",
#'     "Concepts are organizing structures that define how contents",
#'     "are related to each other. Their formation inherently depends on",
#'     "generalization over, and integration of experiences. Thus, a role",
#'     "of the hippocampus in generalization seemed considerable due to",
#'     "its roles in binding elements into spatial and episodic context",
#'     "as well as integration of information over episodes."
#'   ),
#'   context = "This is from a neuroscience paper about hippocampal function."
#' )
#'
#' # After getting the LLM response:
#' # result <- vast_from_json("llm_response.json")
#'
#' # Set yourself as analyst from the start:
#' prompt <- narrative_to_prompt(
#'   theory = "Smoking causes lung cancer.",
#'   analyst = "Finn"
#' )
#' }
#'
#' @seealso [vast_from_json()] for processing the LLM's JSON response,
#'   [vast_to_prompt()] for the reverse direction (model to narrative).
#'
#' @export
narrative_to_prompt <- function(theory,
                                context = NULL,
                                analyst = NULL,
                                copy_to_clipboard = TRUE,
                                output_file = NULL) {

  # Input validation
  if (!is.character(theory) || length(theory) != 1 || nchar(theory) == 0) {
    cli::cli_abort("{.arg theory} must be a non-empty character string.")
  }

  if (!is.null(context) && (!is.character(context) || length(context) != 1)) {
    cli::cli_abort("{.arg context} must be a single character string or NULL.")
  }

  if (!is.null(analyst) && (!is.character(analyst) || length(analyst) != 1)) {
    cli::cli_abort("{.arg analyst} must be a single character string or NULL.")
  }

  # Build prompt from template functions (defined in R/prompt_template.R)
  system_prompt <- vast_system_prompt(analyst = analyst)
  user_prompt <- vast_user_prompt(theory, context)

  # Combine into a single prompt
  # We structure it so the user can paste it as a single message
  full_prompt <- paste0(
    system_prompt,
    "\n\n---\n\n",
    user_prompt
  )

  # Output handling
  cli::cli_h2("VAST Translation Prompt Generated")
  cli::cli_alert_info("Theory text length: {nchar(theory)} characters")
  cli::cli_alert_info("Prompt length: {nchar(full_prompt)} characters (~{round(nchar(full_prompt)/4)} tokens)")

  if (!is.null(context)) {
    cli::cli_alert_info("Additional context included.")
  }

  # Copy to clipboard if requested and available
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

  # Write to file if requested
  if (!is.null(output_file)) {
    writeLines(full_prompt, output_file)
    cli::cli_alert_success("Prompt written to {.file {output_file}}")
  }

  cli::cli_h3("Next Steps")
  cli::cli_ol(c(
    "Paste the prompt into a capable LLM (e.g., Claude, ChatGPT).",
    "Copy the LLM's JSON response and save it to a file.",
    "Run {.code vast_from_json(\"response.json\")} to build your VAST models."
  ))

  invisible(full_prompt)
}
