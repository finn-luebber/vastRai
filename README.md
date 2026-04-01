# vastRai <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

AI-assisted translation between narrative theory paragraphs and
[VAST](https://doi.org/10.15626/MP.2021.2911) models, powered by
[vastR](https://github.com/finn-luebber/vastR).

## Why this package?

Theories in psychology and other sciences are often stated as narrative
text. This makes them hard to evaluate: the same paragraph can support
multiple, structurally different formal interpretations. **vastRai**
makes this problem visible by:

1. Generating a structured prompt that instructs a large language model
   to translate your theory paragraph into VAST model specifications.
2. Producing **two models** from the same text: a *closest match* and a
   *distant but compatible* alternative — showing the full range of
   interpretive freedom the narrative permits.
3. Documenting every **auxiliary assumption** the translation required.
4. Assessing the paragraph's **theoretical rigor** against the T-items
   from [Musfeld et al.'s Indicators for the Assessment of Theoretical
   Rigor](https://doi.org/10.17605/OSF.IO/CAFU5).
5. Translating VAST models **back to narrative text** via LLM prompts.

The package is deliberately **LLM-agnostic**: it generates prompts you
paste into any capable chatbot (Claude, ChatGPT, etc.), keeping vastR
lightweight and free of API dependencies.

## Installation

```r
# Install vastR first (see vastR README for details)
# Then install vastRai:
install.packages("path/to/vastRai", repos = NULL, type = "source")
```

## Workflow: Narrative → VAST

```r
library(vastRai)
library(vastR)

# Step 1: Generate the prompt
vast_from_text(
 "Concepts are organizing structures that define how contents are
  related to each other and can be used to transfer meaning to novel
  input. Their formation thus inherently depends on generalization
  over, and integration of experiences."
)
# → Prompt is copied to your clipboard

# Step 2: Paste the prompt into a capable LLM (Claude, ChatGPT, etc.)
#         Save the JSON response to a file, e.g. "response.json"

# Step 3: Build VAST models from the JSON
result <- vast_from_json("response.json")

# Step 4: Explore
vast_render(result$closest_match)          # Render primary model
vast_render(result$distant_compatible)     # Render alternative model
print_translation_notes(result)            # See all auxiliary assumptions
print_rigor_assessment(result)             # See T-item assessment
compare_models(result)                     # Structural comparison table
cat(result$r_code$closest_match)           # See generated R code

# Step 5: Export a report (text file + PNG diagrams)
export_report(result)
```

## Workflow: VAST → Narrative

```r
# Take any vastR model and generate a prompt for narrative text
model <- vast_model(title = "Example", naming_mode = "fimm") |>
  add_nodes(
    vast_concept("Smoking", "Smoking"),
    vast_concept("LungCancer", "Lung Cancer")
  ) |>
  add_edges(vast_causation("Smoking", "LungCancer", strength = 0.5))

vast_to_prompt(model)
# → Prompt is copied to clipboard. Paste into LLM to get a paragraph.

# With fidelity assessment:
vast_to_prompt(model, assess_fidelity = TRUE)
```

## What you get

The `vast_from_json()` function returns a `vast_translation` object
containing:

| Component | Description |
|-----------|-------------|
| `closest_match` | vastR model: best interpretation with minimal assumptions |
| `distant_compatible` | vastR model: structurally different but text-compatible |
| `translation_notes` | Auxiliary assumptions, underspecification points, divergence summary |
| `rigor_assessment` | T1–T4 ratings from the Indicators for Theoretical Rigor |
| `r_code` | Generated R code strings for both models (commented, copy-edit-rerun) |
| `model_diff` | Structural comparison of the two models |

## Functions

| Function | Direction | Purpose |
|----------|-----------|---------|
| `vast_from_text()` | text → VAST | Generate prompt, copy to clipboard |
| `narrative_to_prompt()` | text → VAST | Generate prompt (more options) |
| `vast_from_json()` | JSON → VAST | Build models from LLM response |
| `vast_to_prompt()` | VAST → text | Generate prompt for narrative generation |
| `compare_models()` | — | Structural comparison of both models |
| `export_report()` | — | Save text report + PNG diagrams |
| `print_translation_notes()` | — | Display auxiliary assumptions |
| `print_rigor_assessment()` | — | Display T-item ratings |

## Design Decisions

- **Separated naming mode by default**: Concepts get abstract labels
  (C1, C2, ...) with separate name nodes, preserving the VAST
  distinction between signifier and referent.

- **Two models, not many**: The closest match and one maximally distant
  alternative communicate the theory's underspecification more
  powerfully than a list of decision points.

- **No API dependency**: The prompt-bridge approach means anyone with
  access to a chatbot can use the package.

- **Editable R code output**: Generated vastR code is commented and
  stored as a string — a starting point for manual refinement.

- **Lightweight reports**: Plain text + PNG files, no heavy dependencies.

## References

- Leising, D., Grenke, O., & Cramer, M. (2023). Visual Argument
  Structure Tool (VAST) Version 1.0. *Meta-Psychology*, 7.
  [doi:10.15626/MP.2021.2911](https://doi.org/10.15626/MP.2021.2911)

- Musfeld, P., Freyer, N., Lange, J., Schönbrodt, F., Van Dongen, N.,
  Peikert, A., Schubert, A.-L., Geppert, R., Borgstede, M., & Leising,
  D. (2025). Indicators for the Assessment of Theoretical Rigor (v3.0).

## License

MIT
