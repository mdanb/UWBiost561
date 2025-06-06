rm(list = ls())
load("vignettes/HW4_simulation.RData")
library(ggplot2)
library(cowplot)

# List to store individual ggplot objects
ggplot_list <- list()
plot_index <- 1  # unique numeric index to prevent name collision

for (n in n_vec) {
  for (alpha in alpha_vec) {
    n_key <- paste0("n:", n)
    alpha_key <- paste0("alpha:", alpha)
    
    if (!(n_key %in% names(n_level_list))) {
      warning(paste("Missing n-level key:", n_key))
      next
    }
    if (!(alpha_key %in% names(n_level_list[[n_key]]))) {
      warning(paste("Missing alpha-level key under", n_key, ":", alpha_key))
      next
    }
    
    trial_list <- n_level_list[[n_key]][[alpha_key]]
    
    # Make sure all trials return properly
    trial_mat <- sapply(trial_list, function(result_list) {
      clique_list <- lapply(result_list, function(x) x$clique_idx)
      bool_vec <- sapply(result_list, function(x) x$valid)
      valid_idx <- which(bool_vec)
      clique_list_valid <- clique_list[valid_idx]
      
      if (length(clique_list_valid) == 0) {
        winner_vec <- setNames(rep(FALSE, length(result_list)), names(result_list))
        return(winner_vec)
      }
      
      size_vec_valid <- sapply(clique_list_valid, length)
      max_size <- max(size_vec_valid)
      winning_methods <- names(clique_list_valid)[which(size_vec_valid == max_size)]
      
      winner_vec <- setNames(rep(FALSE, length(result_list)), names(result_list))
      winner_vec[winning_methods] <- TRUE
      
      return(winner_vec)
    }, simplify = "matrix")
    
    if (is.vector(trial_mat)) {
      trial_mat <- matrix(trial_mat, ncol = 1)
    }
    
    tabulate_vec <- rowSums(trial_mat)
    
    method_names <- as.character(1:15)
    df <- data.frame(method = method_names,
                     number_wins = tabulate_vec)
    
    gg <- ggplot(df, aes(x = method, y = number_wins)) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = method_names) +
      labs(x = "Method",
           y = "Number of Wins",
           title = paste("n =", n, ", alpha =", alpha)) +
      theme_minimal()
    
    ggplot_list[[plot_index]] <- gg
    plot_index <- plot_index + 1
  }
}

# Combine all plots into one image
plot_all <- cowplot::plot_grid(plotlist = ggplot_list, ncol = 3)

ggsave(plot_all, file = "~/UWBiost561/vignettes/hw4-demo_bayes_plot_all_n_alpha.png",
       height = ceiling(length(ggplot_list) / 3) * 3,
       width = 12,
       units = "in")

