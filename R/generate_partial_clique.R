
#' Generate a Partial Clique Adjacency Matrix
#'
#' @param n Integer. Total number of nodes in the graph.
#' @param clique_fraction Numeric between 0 and 1. Fraction of nodes in the partial clique.
#' @param clique_edge_density Numeric between 0 and 1. Edge density within the partial clique.
#' @param seed Optional. Integer seed for reproducibility.
#'
#' @return A list with at least the element 'adj_mat', a symmetric adjacency matrix with a partial clique.
#'
#' @examples
#' # Generate a partial clique with 10 nodes, 50% in the clique,
#' # and 80% edge density within the clique.
#' result <- generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.8, seed = 42)
#' 
#' # Inspect the adjacency matrix
#' print(result$adj_mat)
#'
#' @name generate_partial_clique
#' @export
generate_partial_clique <- function(n, clique_fraction, clique_edge_density, seed = NULL) {
  # Input validation
  if (!(is.numeric(n) && length(n) == 1 && n > 0 && floor(n) == n)) {
    stop("n must be a single positive integer.")
  }
  
  if (!(is.numeric(clique_fraction) && length(clique_fraction) == 1 &&
        clique_fraction >= 0 && clique_fraction <= 1)) {
    stop("clique_fraction must be a single numeric value between 0 and 1.")
  }
  
  if (!(is.numeric(clique_edge_density) && length(clique_edge_density) == 1 &&
        clique_edge_density >= 0 && clique_edge_density <= 1)) {
    stop("clique_edge_density must be a single numeric value between 0 and 1.")
  }
  
  if (!(is.null(seed) || (is.numeric(seed) && floor(seed) == seed))) {
    stop("seed must be integer if set.")
  }
  
  
  if (!is.null(seed)) set.seed(seed)
  
  # Calculate number of clique nodes and minimum number of edges in the partial clique
  m <- round(n * clique_fraction)
  total_clique_edges <- m * (m - 1) / 2
  required_edges <- round(clique_edge_density * total_clique_edges)
  
  # Initialize adjacency matrix
  adj_mat <- matrix(0, nrow = n, ncol = n)
  
  if (m > 1 && required_edges > 0) {
    # Randomly select nodes for the clique
    clique_nodes <- sort(sample(1:n, m, replace = FALSE))
    
    # Generate all possible edges within the clique
    clique_combinations <- combn(clique_nodes, 2, simplify = FALSE)
    
    # Randomly select edges for the partial clique
    selected_edges <- sample(clique_combinations, required_edges, replace = FALSE)
    
    # Fill in the adjacency matrix with selected edges
    for (edge in selected_edges) {
      i <- edge[1]
      j <- edge[2]
      adj_mat[i, j] <- 1
      adj_mat[j, i] <- 1
    }
  }
  
  # Set diagonal to 1s (self-loops)
  diag(adj_mat) <- 1
  
  # Ensure symmetry and no row/column names
  dimnames(adj_mat) <- NULL
  
  return(list(adj_mat = adj_mat))
}
