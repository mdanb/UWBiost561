context("Testing perform_simulation")

imp_numbers <- 1:15

test_that("Runs with various alpha levels (excluding bad implementation)", {
  result <- perform_simulation(imp_numbers=1:14, 
                               trials=3, 
                               n_vec=c(10), 
                               alpha_vec=c(0.5, 0.75, 0.95))
  expect_type(result, "list")
})

test_that("Returns invalid clique for all methods for n=1 (excluding bad one)", {
  result <- perform_simulation(imp_numbers=1:14, 
                               trials=1, 
                               n_vec=c(1), 
                               alpha_vec=c(0.9))
  implementations <- result$`n:1`$`alpha:0.9`$`Trial: 1`
  valid_flags <- sapply(implementations, function(impl) impl$valid)
  expect_true(all(valid_flags))
})

