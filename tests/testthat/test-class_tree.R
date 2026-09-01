# Structural (non-evaluating) tests for the Node tree-edit helpers that back the
# drag-and-drop cohort builder.

make_tree <- function() {
  root <- Node$new(value_type = "and")
  f1 <- Node$new(value_type = "filter", rdv = "dia", column = "x",
                 query_type = "str_starts", inclusion = "ever", value = "E11")
  orn <- Node$new(value_type = "or")
  f2 <- Node$new(value_type = "filter", rdv = "dia", column = "y",
                 query_type = "str_starts", inclusion = "ever", value = "E12")
  root$insert_at(f1)
  root$insert_at(orn)
  orn$insert_at(f2)
  list(root = root, f1 = f1, f2 = f2, orn = orn)
}

test_that("every node gets a unique id", {
  t <- make_tree()
  ids <- vapply(list(t$root, t$f1, t$f2, t$orn), function(n) n$id, character(1))
  expect_length(unique(ids), 4)
})

test_that("find_by_id locates nodes anywhere in the subtree", {
  t <- make_tree()
  expect_identical(t$root$find_by_id(t$f2$id), t$f2)
  expect_identical(t$root$find_by_id(t$root$id), t$root)
  expect_null(t$root$find_by_id("nonexistent"))
})

test_that("contains_id guards against cycles", {
  t <- make_tree()
  expect_true(t$root$contains_id(t$orn$id))   # descendant
  expect_true(t$orn$contains_id(t$orn$id))    # self
  expect_false(t$orn$contains_id(t$root$id))  # ancestor is not contained
})

test_that("remove_by_id detaches a node and returns it", {
  t <- make_tree()
  removed <- t$root$remove_by_id(t$orn$id)
  expect_identical(removed, t$orn)
  expect_length(t$root$children, 1)
  expect_null(t$root$find_by_id(t$orn$id))
})

test_that("insert_at honours 1-based index and clamps", {
  t <- make_tree()
  moved <- t$root$remove_by_id(t$orn$id)
  t$root$insert_at(moved, 1)
  expect_identical(t$root$children[[1]]$id, t$orn$id)

  extra <- Node$new(value_type = "and")
  t$root$insert_at(extra, 999)  # clamped to the end
  expect_identical(t$root$children[[length(t$root$children)]]$id, extra$id)
})

test_that("is_evaluable reflects operator child constraints", {
  expect_true(Node$new(value_type = "base")$is_evaluable())
  expect_true(Node$new(value_type = "filter", rdv = "d", column = "c",
                       query_type = "str_starts", inclusion = "ever",
                       value = "x")$is_evaluable())

  expect_false(Node$new(value_type = "and")$is_evaluable())  # empty operator
  expect_false(Node$new(value_type = "not")$is_evaluable())  # NOT needs 1 child

  not1 <- Node$new(value_type = "not")
  not1$insert_at(Node$new(value_type = "base"))
  expect_true(not1$is_evaluable())

  not2 <- Node$new(value_type = "not")
  not2$insert_at(Node$new(value_type = "base"))
  not2$insert_at(Node$new(value_type = "base"))
  expect_false(not2$is_evaluable())  # NOT with >1 child
})
