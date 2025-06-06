library(UWBiost561)
n_level_list = perform_simulation(imp_numbers = 1:15,
                                  trials=3,
                                  n_vec=c(10,15,20),
                                  alpha_vec=c(0.5,0.75,0.95))
# it's always useful to save the date and R session info
date_of_run <- Sys.time()
session_info <- devtools::session_info()

save(n_level_list, # save your results
     alpha_vec, # save which alphas you used (for convenience)
     date_of_run, session_info,
     file = "~/HW4_simulation.RData")
