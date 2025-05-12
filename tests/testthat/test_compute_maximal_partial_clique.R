context("Testing compute_maximal_partial_clique")

test_that("compute_maximal_partial_clique outputs something that is the correct type", {
  
  mat <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9,
                                 seed=42)
  result <- compute_maximal_partial_clique(adj_mat = mat$adj_mat, alpha = 0.6)
  
  expect_type(result, "list")
  expect_true(all(c("clique_idx", "edge_density") %in% names(result)))
  expect_type(result$clique_idx, "integer")
  expect_true(all(result$clique_idx %% 1 == 0))
})

test_that("compute_maximal_partial_clique outputs something that is within the correct range", {
  mat <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0.9,
                                 seed=42)
  result <- compute_maximal_partial_clique(adj_mat = mat$adj_mat, alpha = 0.6)
  
  expect_true(all(result$clique_idx >= 1 & result$clique_idx <= nrow(mat$adj_mat)))
  expect_true(result$edge_density >= 0 && result$edge_density <= 1)
})

test_that("compute_maximal_partial_clique handles corner case of disconnected graph", {
  mat <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.5,
                                 clique_edge_density = 0,
                                 seed=42)
  
  result <- compute_maximal_partial_clique(adj_mat = mat$adj_mat, alpha = 0.5)
  
  expect_true(result$clique_idx == 1 & result$edge_density == 1)
})

test_that("compute_maximal_partial_clique gets the correct answer when changing the graph without changing the expected clique", {
  mat <- generate_partial_clique(n = 10,
                                 clique_fraction = 0.7,
                                 clique_edge_density = 0.9,
                                 seed=42)
  result1 <- compute_maximal_partial_clique(adj_mat = mat$adj_mat, alpha = 0.6)
  
  mat$adj_mat[8,10] = 0
  mat$adj_mat[10,8] = 0
  
  result2 <- compute_maximal_partial_clique(adj_mat = mat$adj_mat, alpha = 0.6)
  
  expect_true(all(result1$clique_idx == result2$clique_idx))
})

test_that("compute_maximal_partial_clique gets the correct answer for carefully crafted problem with two cliques of different sizes", {
  n <- 5
  adj_matrix <- matrix(0, nrow = n, ncol = n)
  adj_matrix[1:3, 1:3] <- 1
  adj_matrix[4:5, 4:5] <- 1
  result <- compute_maximal_partial_clique(adj_mat = adj_matrix, alpha = 0.9)
  expect_true(all(result$clique_idx == c(1,2,3)))
})

