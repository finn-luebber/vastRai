# ============================================================================
# Shared test fixtures for vastRai tests
# ============================================================================

# --- Minimal valid LLM JSON output (the core test fixture) ---
# This is the smallest possible valid response matching the expected schema.

minimal_model_spec <- function(title = "Test Model") {
  list(
    title = title,
    naming_mode = "separated",
    analyst = "Test",
    nodes = list(
      list(id = "X", type = "concept", label = "X"),
      list(id = "Y", type = "concept", label = "Y"),
      list(id = "N_X", type = "name", label = "Smoking"),
      list(id = "N_Y", type = "name", label = "Lung Cancer")
    ),
    edges = list(
      list(id = "c1", from = "X", to = "Y", type = "causation", strength = 0.5),
      list(id = "n1", from = "X", to = "N_X", type = "naming"),
      list(id = "n2", from = "Y", to = "N_Y", type = "naming")
    ),
    groups = list(),
    is_statements = list(),
    ought_statements = list()
  )
}

minimal_llm_json <- function() {
  list(
    source_text = "Smoking causes lung cancer.",
    models = list(
      closest_match = minimal_model_spec("Closest Match"),
      distant_compatible = minimal_model_spec("Distant Compatible")
    ),
    translation_notes = list(
      auxiliary_assumptions = list(
        list(
          id = "A1",
          description = "Direct causation assumed",
          reason = "Text uses 'causes' directly",
          text_evidence = "Smoking causes lung cancer",
          closest_match_choice = "Direct causation",
          distant_compatible_choice = "Mediated causation",
          other_options = list("Predictive only")
        )
      ),
      underspecification_points = list(
        list(
          id = "U1",
          description = "Mechanism not specified",
          impact = "Could be direct or mediated",
          text_location = "causes"
        )
      ),
      model_divergence_summary = "Models differ in directness of causal path."
    ),
    rigor_assessment = list(
      T1_phenomenon = list(rating = "clearly_specified", justification = "Clear."),
      T2_concept_definitions = list(
        rating = "partially_specified",
        justification = "Concepts named but not formally defined.",
        undefined_concepts = list("mechanism")
      ),
      T3_mechanism = list(
        rating = "unspecified",
        justification = "No mechanism given.",
        completeness = "No causal chain present.",
        explanatory_quality = "No edges to assess.",
        sufficiency = "Cannot assess without a mechanism."
      ),
      T4_scope = list(rating = "unspecified", justification = "No scope specified."),
      overall_assessment = "A minimal causal claim."
    )
  )
}

# Convert to JSON string
minimal_llm_json_string <- function() {
  jsonlite::toJSON(minimal_llm_json(), auto_unbox = TRUE, pretty = TRUE)
}


# --- Extended fixture: model with groups, IS/OUGHT, perspectives ---

extended_model_spec <- function() {
  list(
    title = "Extended Test",
    naming_mode = "separated",
    analyst = "Test",
    nodes = list(
      list(id = "X", type = "concept", label = "X"),
      list(id = "Y", type = "concept", label = "Y"),
      list(id = "Z", type = "concept", label = "Z"),
      list(id = "N_X", type = "name", label = "Temperature"),
      list(id = "N_Y", type = "name", label = "Ice Cream Sales"),
      list(id = "D1", type = "data", label = "Survey Data"),
      list(id = "AND1", type = "diamond", label = "AND"),
      list(id = "NS1", type = "noise_source")
    ),
    edges = list(
      list(id = "c1", from = "X", to = "Y", type = "causation", strength = 0.7, index = "1"),
      list(id = "p1", from = "X", to = "Z", type = "prediction", strength = 0.3),
      list(id = "n1", from = "X", to = "N_X", type = "naming"),
      list(id = "n2", from = "Y", to = "N_Y", type = "naming"),
      list(id = "noise1", from = "NS1", to = "Y", type = "noise"),
      list(id = "s1", from = "AND1", to = "Y", type = "structural")
    ),
    groups = list(
      list(id = "HOC1", label = "Weather Effects", members = list("X", "Y"))
    ),
    is_statements = list(
      list(
        concept_id = "X",
        value = 0.8,
        perspectives = list(
          list(holder = "Alice", agreement = 0.9),
          list(holder = "Bob", agreement = 0.5)
        )
      )
    ),
    ought_statements = list(
      list(
        concept_id = "Y",
        value = 0.3,
        perspectives = list()
      )
    )
  )
}


# --- Fixture: IS/OUGHT on a group ---

group_is_ought_spec <- function() {
  list(
    title = "Group IS/OUGHT Test",
    naming_mode = "separated",
    analyst = "Test",
    nodes = list(
      list(id = "A", type = "concept", label = "A"),
      list(id = "B", type = "concept", label = "B"),
      list(id = "C", type = "concept", label = "C")
    ),
    edges = list(
      list(id = "c1", from = "A", to = "B", type = "causation")
    ),
    groups = list(
      list(id = "HOC1", label = "Mechanism", members = list("A", "B"))
    ),
    is_statements = list(
      list(
        concept_id = "HOC1",
        value = 1,
        perspectives = list(
          list(holder = "Tina", agreement = 0.5)
        )
      )
    ),
    ought_statements = list()
  )
}


# --- Fixture: compound edges with lhead/ltail ---

compound_edge_spec <- function() {
  list(
    title = "Compound Edge Test",
    naming_mode = "fimm",
    analyst = "Test",
    nodes = list(
      list(id = "X", type = "concept", label = "Cause"),
      list(id = "M1", type = "concept", label = "Mediator 1"),
      list(id = "M2", type = "concept", label = "Mediator 2"),
      list(id = "Y", type = "concept", label = "Effect")
    ),
    edges = list(
      list(id = "c1", from = "X", to = "M1", type = "causation",
           lhead = "HOC1"),
      list(id = "c2", from = "M2", to = "Y", type = "causation",
           ltail = "HOC1"),
      list(id = "s1", from = "X", to = "M1", type = "structural",
           lhead = "HOC1")
    ),
    groups = list(
      list(id = "HOC1", label = "Mechanism", members = list("M1", "M2"))
    ),
    is_statements = list(),
    ought_statements = list()
  )
}
