# ============================================================================
# Tests for serialize_vast_model() and describe_vast_model()
# ============================================================================

# Access internal functions
serialize_vast_model <- vastRai:::serialize_vast_model
describe_vast_model <- vastRai:::describe_vast_model


# --- serialize_vast_model() ---

test_that("serialize_vast_model errors on non-vast_model input", {
  expect_error(serialize_vast_model(list()), "vast_model")
})

test_that("serialize_vast_model returns correct top-level structure", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Test", naming_mode = "fimm") |>
    vastR::add_nodes(vastR::vast_concept("X", "X"))
  spec <- serialize_vast_model(model)
  expect_true(all(c("title", "naming_mode", "nodes", "edges", "groups") %in% names(spec)))
  expect_equal(spec$title, "Test")
  expect_equal(spec$naming_mode, "fimm")
})

test_that("serialize_vast_model serializes node types correctly", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Types", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "X"),
      vastR::vast_diamond("AND1", label = "AND")
    )
  spec <- serialize_vast_model(model)
  types <- vapply(spec$nodes, function(n) n$type, character(1))
  expect_true("concept" %in% types)
  expect_true("diamond" %in% types)
})

test_that("serialize_vast_model maps edge type codes to full names", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Edges", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "X"),
      vastR::vast_concept("Y", "Y")
    ) |>
    vastR::add_edges(
      vastR::vast_causation("X", "Y", strength = 0.5)
    )
  spec <- serialize_vast_model(model)
  edge_types <- vapply(spec$edges, function(e) e$type, character(1))
  expect_true("causation" %in% edge_types)
  # Should not contain single-letter codes
  expect_false("c" %in% edge_types)
})

test_that("serialize_vast_model preserves edge strength", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Strength", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "X"),
      vastR::vast_concept("Y", "Y")
    ) |>
    vastR::add_edges(
      vastR::vast_causation("X", "Y", strength = 0.7)
    )
  spec <- serialize_vast_model(model)
  causation_edge <- Filter(function(e) e$type == "causation", spec$edges)
  expect_length(causation_edge, 1)
  expect_equal(causation_edge[[1]]$strength, 0.7)
})

test_that("serialize_vast_model extracts edge index from label", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Index", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "X"),
      vastR::vast_concept("Y", "Y")
    ) |>
    vastR::add_edges(
      vastR::vast_causation("X", "Y", index = "1")
    )
  spec <- serialize_vast_model(model)
  causation_edge <- Filter(function(e) e$type == "causation", spec$edges)
  expect_length(causation_edge, 1)
  # vastR stores index baked into the label (e.g., "c1"); serialization extracts it
  expect_equal(causation_edge[[1]]$index, "1")
})

test_that("serialize_vast_model preserves lhead and ltail", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Compound", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "X"),
      vastR::vast_concept("M1", "M1"),
      vastR::vast_concept("M2", "M2")
    ) |>
    vastR::add_edges(
      vastR::vast_causation("X", "M1", lhead = "HOC1")
    ) |>
    vastR::add_groups(
      vastR::vast_group("HOC1", "Mechanism", node_ids = c("M1", "M2"))
    )
  spec <- serialize_vast_model(model)
  causation_edge <- Filter(function(e) e$type == "causation", spec$edges)
  expect_length(causation_edge, 1)
  # lhead/ltail are stored as separate fields on the vastR edge object
  expect_equal(causation_edge[[1]]$lhead, "HOC1")
})

test_that("serialize_vast_model serializes groups with child_group_ids", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Nested", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("A", "A"),
      vastR::vast_concept("B", "B"),
      vastR::vast_concept("C", "C")
    ) |>
    vastR::add_groups(
      vastR::vast_group("G1", "Inner", node_ids = c("A", "B")),
      vastR::vast_nested_group("G2", "Outer", node_ids = c("C"),
                               child_group_ids = c("G1"))
    )
  spec <- serialize_vast_model(model)
  nested <- Filter(function(g) g$id == "G2", spec$groups)
  expect_length(nested, 1)
  expect_true("G1" %in% unlist(nested[[1]]$child_group_ids))
})

test_that("serialize_vast_model handles structural edges (type = empty string)", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Structural", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "X"),
      vastR::vast_is("IS_X", value = 0.5)
    ) |>
    vastR::add_edges(
      vastR::vast_relation("IS_X", "X", type = "")
    )
  spec <- serialize_vast_model(model)
  structural <- Filter(function(e) e$type == "structural", spec$edges)
  expect_length(structural, 1)
})


# --- describe_vast_model() ---

test_that("describe_vast_model returns a character string", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Desc Test", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("X", "Smoking"),
      vastR::vast_concept("Y", "Cancer")
    ) |>
    vastR::add_edges(
      vastR::vast_causation("X", "Y")
    )
  result <- describe_vast_model(model)
  expect_type(result, "character")
  expect_match(result, "VAST Model: Desc Test")
  expect_match(result, "Relationships:")
})

test_that("describe_vast_model includes group info", {
  skip_if_not_installed("vastR")
  model <- vastR::vast_model(title = "Group Desc", naming_mode = "fimm") |>
    vastR::add_nodes(
      vastR::vast_concept("A", "A"),
      vastR::vast_concept("B", "B")
    ) |>
    vastR::add_groups(
      vastR::vast_group("G1", "Mechanism", node_ids = c("A", "B"))
    )
  result <- describe_vast_model(model)
  expect_match(result, "Higher-order concepts:")
  expect_match(result, "Mechanism")
})
