context("Testing generate_partial_clique")

test_that("generate_partial_clique returns correct output type", {
  res <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9,
                                 seed=42)
  
  expect_true(is.list(res))
  expect_true(is.matrix(res$adj_mat))
  expect_true(all(dim(res$adj_mat) == c(10,10)))
})

test_that("generate_partial_clique crashes with incorrect input type", {
  expect_error(generate_partial_clique(n = "hello",
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9,
                                 seed=42))
  expect_error(generate_partial_clique(n = 10,
                                 clique_fraction = "hello",
                                 clique_edge_density = 0.9,
                                 seed=42))
  expect_error(generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = "hello",
                                 seed=42))
  expect_error(generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9,
                                 seed="hello"))
})

