get_sim_args <- function(params, all_args) {
    sim_args <- names(params)
    used_sim_args <- sim_args[sim_args %in% all_args]
}