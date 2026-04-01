# ============================================================================
# Tests for internal code generation functions
# ============================================================================

# Access internal functions
build_node_call <- vastRai:::build_node_call
build_edge_call <- vastRai:::build_edge_call
build_group_call <- vastRai:::build_group_call
build_is_ought_code <- vastRai:::build_is_ought_code
build_name_lookup <- vastRai:::build_name_lookup


# --- build_node_call() ---

test_that("build_node_call generates concept node", {
  node <- list(id = "X", type = "concept", label = "Temperature")
  result <- build_node_call(node)
  expect_match(result, 'vast_concept\\("X"')
  expect_match(result, 'label = "Temperature"')
})

test_that("build_node_call generates name node and strips curly quotes", {
  node <- list(id = "N_X", type = "name", label = "\u201CSmoking\u201D")
  result <- build_node_call(node)
  expect_match(result, "vast_name")
  expect_match(result, '"Smoking"')
  # Should not contain curly quotes in the call (vastR adds them automatically)
  expect_false(grepl("\u201C", result))
})

test_that("build_node_call generates data node", {
  node <- list(id = "D1", type = "data", label = "Survey Results")
  result <- build_node_call(node)
  expect_match(result, "vast_data")
  expect_match(result, '"D1"')
})

test_that("build_node_call generates IS node with value", {
  node <- list(id = "IS_X", type = "is", value = 0.8)
  result <- build_node_call(node)
  expect_match(result, "vast_is")
  expect_match(result, "value = 0.8")
})

test_that("build_node_call generates IS node without value", {
  node <- list(id = "IS_X", type = "is")
  result <- build_node_call(node)
  expect_match(result, 'vast_is\\("IS_X"\\)')
})

test_that("build_node_call generates OUGHT node", {
  node <- list(id = "OUGHT_Y", type = "ought", value = 0)
  result <- build_node_call(node)
  expect_match(result, "vast_ought")
  expect_match(result, "value = 0")
})

test_that("build_node_call generates perspective node", {
  node <- list(id = "P_Alice", type = "perspective", holder = "Alice", value = 0.9)
  result <- build_node_call(node)
  expect_match(result, "vast_perspective")
  expect_match(result, 'holder = "Alice"')
  expect_match(result, "value = 0.9")
})

test_that("build_node_call generates diamond node", {
  node <- list(id = "AND1", type = "diamond", diamond_content = "AND")
  result <- build_node_call(node)
  expect_match(result, "vast_diamond")
  expect_match(result, '"AND"')
})

test_that("build_node_call generates noise source", {
  node <- list(id = "NS1", type = "noise_source")
  result <- build_node_call(node)
  expect_match(result, "vast_noise_source")
})

test_that("build_node_call falls back to concept for unknown type", {
  node <- list(id = "Q", type = "unknown_type", label = "Q")
  result <- build_node_call(node)
  expect_match(result, "vast_concept")
})


# --- build_edge_call() ---

test_that("build_edge_call generates all typed relationship edges", {
  types <- c("naming", "implication", "causation", "transformation",
             "prediction", "reasoning", "unknown")
  expected_fns <- c("vast_naming", "vast_implication", "vast_causation",
                    "vast_transformation", "vast_prediction", "vast_reasoning",
                    "vast_unknown")

  for (i in seq_along(types)) {
    edge <- list(from = "X", to = "Y", type = types[i])
    result <- build_edge_call(edge)
    expect_match(result, expected_fns[i],
                 info = paste("Type:", types[i]))
  }
})

test_that("build_edge_call includes strength parameter", {
  edge <- list(from = "X", to = "Y", type = "causation", strength = 0.7)
  result <- build_edge_call(edge)
  expect_match(result, "strength = 0.7")
})

test_that("build_edge_call includes verbal strength", {
  edge <- list(from = "X", to = "Y", type = "causation", strength = "strong")
  result <- build_edge_call(edge)
  expect_match(result, 'strength = "strong"')
})

test_that("build_edge_call includes index parameter", {
  edge <- list(from = "X", to = "Y", type = "causation", index = "1")
  result <- build_edge_call(edge)
  expect_match(result, 'index = "1"')
})

test_that("build_edge_call includes lhead/ltail for standard edges", {
  edge <- list(from = "X", to = "M1", type = "causation",
               lhead = "HOC1", ltail = "HOC2")
  result <- build_edge_call(edge)
  expect_match(result, 'lhead = "HOC1"')
  expect_match(result, 'ltail = "HOC2"')
})

test_that("build_edge_call reads lhead/ltail from compound sub-object", {
  edge <- list(from = "X", to = "M1", type = "causation",
               compound = list(lhead = "HOC1"))
  result <- build_edge_call(edge)
  expect_match(result, 'lhead = "HOC1"')
})

test_that("build_edge_call generates noise edge", {
  edge <- list(from = "NS1", to = "Y", type = "noise", strength = 0.5)
  result <- build_edge_call(edge)
  expect_match(result, "vast_noise")
  expect_match(result, '"NS1"')
  expect_match(result, "strength = 0.5")
})

test_that("build_edge_call warns on noise edge without from", {
  edge <- list(to = "Y", type = "noise")
  expect_warning(build_edge_call(edge), "noise_id")
})

test_that("build_edge_call generates structural edge", {
  edge <- list(from = "IS_X", to = "X", type = "structural")
  result <- build_edge_call(edge)
  expect_match(result, "vast_relation")
  expect_match(result, 'type = ""')
})

test_that("build_edge_call preserves lhead/ltail on structural edges", {
  edge <- list(from = "IS_HOC", to = "M1", type = "structural", lhead = "HOC1")
  result <- build_edge_call(edge)
  expect_match(result, "vast_relation")
  expect_match(result, 'lhead = "HOC1"')
})

test_that("build_edge_call warns on missing from/to", {
  edge <- list(type = "causation", to = "Y")
  expect_warning(build_edge_call(edge), "missing from/to")
})

test_that("build_edge_call falls back to vast_unknown for unrecognized type", {
  edge <- list(from = "X", to = "Y", type = "wormhole")
  result <- build_edge_call(edge)
  expect_match(result, "vast_unknown")
})


# --- build_group_call() ---

test_that("build_group_call generates a simple group", {
  group <- list(id = "HOC1", label = "Mechanism", members = list("X", "Y"))
  result <- build_group_call(group)
  expect_match(result, "vast_group")
  expect_match(result, '"HOC1"')
  expect_match(result, '"Mechanism"')
  expect_match(result, '"X", "Y"')
})

test_that("build_group_call generates a nested group", {
  group <- list(id = "HOC2", label = "Super Group",
                members = list("Z"), child_group_ids = list("HOC1"))
  result <- build_group_call(group)
  expect_match(result, "vast_nested_group")
  expect_match(result, 'child_group_ids = c\\("HOC1"\\)')
})

test_that("build_group_call uses node_ids field if members missing", {
  group <- list(id = "G1", label = "G1", node_ids = list("A", "B"))
  result <- build_group_call(group)
  expect_match(result, '"A", "B"')
})


# --- build_name_lookup() ---

test_that("build_name_lookup maps concepts to their names", {
  spec <- minimal_model_spec()
  lookup <- build_name_lookup(spec)
  expect_equal(lookup[["X"]], "Smoking")
  expect_equal(lookup[["Y"]], "Lung Cancer")
})

test_that("build_name_lookup strips curly quotes from name labels", {
  spec <- list(
    nodes = list(
      list(id = "X", type = "concept", label = "X"),
      list(id = "N_X", type = "name", label = "\u201CSmoking\u201D")
    ),
    edges = list(
      list(from = "X", to = "N_X", type = "naming")
    )
  )
  lookup <- build_name_lookup(spec)
  expect_equal(lookup[["X"]], "Smoking")
})

test_that("build_name_lookup returns empty list for spec without naming edges", {
  spec <- list(
    nodes = list(list(id = "X", type = "concept")),
    edges = list()
  )
  lookup <- build_name_lookup(spec)
  expect_length(lookup, 0)
})


# --- build_is_ought_code() ---

test_that("build_is_ought_code generates IS node and structural edge", {
  stmt <- list(concept_id = "X", value = 0.8, perspectives = list())
  result <- build_is_ought_code(stmt, "is")
  code <- paste(result, collapse = "\n")
  expect_match(code, 'vast_is\\("IS_X"')
  expect_match(code, "value = 0.8")
  expect_match(code, 'vast_relation\\("IS_X", "X"')
})

test_that("build_is_ought_code generates OUGHT node", {
  stmt <- list(concept_id = "Y", value = 0.3, perspectives = list())
  result <- build_is_ought_code(stmt, "ought")
  code <- paste(result, collapse = "\n")
  expect_match(code, 'vast_ought\\("OUGHT_Y"')
  expect_match(code, "value = 0.3")
})

test_that("build_is_ought_code generates perspective nodes and edges", {
  stmt <- list(
    concept_id = "X", value = 0.8,
    perspectives = list(
      list(holder = "Alice", agreement = 0.9),
      list(holder = "Bob", agreement = NULL)
    )
  )
  result <- build_is_ought_code(stmt, "is")
  code <- paste(result, collapse = "\n")
  # Alice with agreement
  expect_match(code, 'holder = "Alice"')
  expect_match(code, "value = 0.9")
  # Bob without agreement
  expect_match(code, 'holder = "Bob"')
  # Both connected to IS_X
  expect_true(sum(grepl("IS_X", result)) >= 3)  # IS node + at least 2 perspective edges
})


# --- build_is_ought_code() with groups (Change 3) ---

test_that("build_is_ought_code generates lhead for group IS statement", {
  stmt <- list(
    concept_id = "HOC1", value = 1,
    perspectives = list()
  )
  result <- build_is_ought_code(
    stmt, "is",
    group_ids = "HOC1",
    group_members = list(HOC1 = c("A", "B"))
  )
  code <- paste(result, collapse = "\n")
  # Should target first member "A" with lhead = "HOC1"
  expect_match(code, '"A"')
  expect_match(code, 'lhead = "HOC1"')
})

test_that("build_is_ought_code generates lhead for group OUGHT statement", {
  stmt <- list(
    concept_id = "HOC1", value = 0,
    perspectives = list()
  )
  result <- build_is_ought_code(
    stmt, "ought",
    group_ids = "HOC1",
    group_members = list(HOC1 = c("X", "Y"))
  )
  code <- paste(result, collapse = "\n")
  expect_match(code, 'vast_ought\\("OUGHT_HOC1"')
  expect_match(code, 'lhead = "HOC1"')
  expect_match(code, '"X"')  # targets first member
})

test_that("build_is_ought_code falls back when group has no members", {
  stmt <- list(
    concept_id = "HOC_EMPTY", value = 1,
    perspectives = list()
  )
  expect_warning(
    result <- build_is_ought_code(
      stmt, "is",
      group_ids = "HOC_EMPTY",
      group_members = list(HOC_EMPTY = character(0))
    ),
    "no members found"
  )
  code <- paste(result, collapse = "\n")
  # Falls back to direct edge
  expect_match(code, '"HOC_EMPTY"')
  expect_false(grepl("lhead", code))
})

test_that("build_is_ought_code treats non-group concept_id normally", {
  stmt <- list(concept_id = "X", value = 0.5, perspectives = list())
  result <- build_is_ought_code(
    stmt, "is",
    group_ids = "HOC1",   # X is NOT in group_ids
    group_members = list(HOC1 = c("A", "B"))
  )
  code <- paste(result, collapse = "\n")
  expect_match(code, 'vast_relation\\("IS_X", "X"')
  expect_false(grepl("lhead", code))
})

test_that("build_is_ought_code with group still generates perspectives", {
  stmt <- list(
    concept_id = "HOC1", value = 1,
    perspectives = list(
      list(holder = "Tina", agreement = 0.5)
    )
  )
  result <- build_is_ought_code(
    stmt, "is",
    group_ids = "HOC1",
    group_members = list(HOC1 = c("A", "B"))
  )
  code <- paste(result, collapse = "\n")
  expect_match(code, 'holder = "Tina"')
  expect_match(code, "value = 0.5")
  expect_match(code, 'lhead = "HOC1"')
})
