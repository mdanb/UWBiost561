#' Perform simulation
#'
#' @param imp_numbers List. List of implementations to test
#' @param trials Integer. Number of Trials.
#' @param n_vec List. n values to test
#' @param alpha_vec List. alpha values to test
#'
#' @return A list with simulation results
#' @export
#' 
perform_simulation <- function(imp_numbers, trials, n_vec, alpha_vec) {
  # loop over n levels
  n_level_list <- lapply(n_vec, function(n) {
    print(paste("Value of n:", n))
    
    # loop over alpha levels
    level_trial_list <- lapply(alpha_vec, function(alpha) {
      print(paste("  Value of alpha:", alpha))
      
      # loop over the different trials for this level
      trial_list <- lapply(1:trials, function(trial){
        print(paste("    Working on trial:", trial))
        set.seed(trial) # to freeze the randomness of adj_mat
        
        # generate the data
        data <-  generate_partial_clique(n = n, 
                                        clique_fraction = 0.9, 
                                        clique_edge_density = 0.9)
        adj_mat <- data$adj_mat
        
        # loop over the methods for this trial
        result_list <- lapply(imp_numbers, function(imp_number){
          set.seed(trial) # to freeze the randomness of the method
          cat('*')
          result <- compute_maximal_partial_clique_master(
            adj_mat = adj_mat,
            alpha = alpha,
            number = imp_number,
            time_limit = 30
          )
          return(result)
        })
        names(result_list) <- paste("Implementation:", imp_numbers)
        cat("\n")
        
        return(result_list)
      })
      names(trial_list) <- paste("Trial:", 1:trials)
      print("  ====")
      
      return(trial_list)
    })
    names(level_trial_list) <- paste0("alpha:", alpha_vec)
    
    return(level_trial_list)
  })
  
  names(n_level_list) <- paste0("n:", n_vec)
  
  return(n_level_list)
}

