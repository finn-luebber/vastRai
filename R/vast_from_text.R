#' Translate Theory Text to VAST Models (Full Workflow)
#'
#' This is the primary user-facing function. It generates a prompt, displays
#' workflow instructions, and — once the user provides JSON from the LLM —
#' builds the VAST models.
#'
#' @param theory Character string. The theory text to translate. See
#'   [narrative_to_prompt()] for guidance on what to include.
#' @param context Character string or NULL. Optional additional context.
#' @param analyst Character string or NULL. Name of the analyst to record in
#'   the VAST models. Defaults to NULL (LLM identifies itself). See
#'   [narrative_to_prompt()] for details.
#' @param output_file Character string or NULL. If provided, also writes the
#'   prompt to this file.
#'
#' @return Invisibly returns the prompt string. The function's main purpose is
#'   its side effects: printing the prompt and workflow instructions.
#'
#' @details
#' This function is a convenience wrapper around [narrative_to_prompt()]. The
#' complete workflow is:
#'
#' ```r
#' # Step 1: Generate the prompt
#' vast_from_text("Your theory text here...")
#'
#' # Step 2: Paste prompt into LLM, get JSON response, save to file
#'
#' # Step 3: Build models from JSON
#' result <- vast_from_json("response.json")
#'
#' # Step 4: Explore
#' vastR::vast_render(result$closest_match)
#' vastR::vast_render(result$distant_compatible)
#' print_translation_notes(result)
#' print_rigor_assessment(result)
#' cat(result$r_code$closest_match)
#' ```
#'
#' @examples
#' \dontrun{
#' vast_from_text(
#'   "Smoking causes tar buildup in the lungs, which in turn
#'    causes lung cancer."
#' )
#'
#' # With user as analyst from the start:
#' vast_from_text(
#'   "Smoking causes tar buildup in the lungs, which in turn
#'    causes lung cancer.",
#'   analyst = "Finn"
#' )
#' }
#'
#' @seealso [narrative_to_prompt()], [vast_from_json()]
#'
#' @export
vast_from_text <- function(theory, context = NULL, analyst = NULL, output_file = NULL) {
  narrative_to_prompt(
    theory = theory,
    context = context,
    analyst = analyst,
    copy_to_clipboard = TRUE,
    output_file = output_file
  )
}
