
compute_sim_offset <- function(n_sims, prob_payoff_funs, sim_offset0) {

    sim_offset <- matrix(NA, nrow = n_sims, ncol = length(prob_payoff_funs), dimnames = list(NULL, prob_payoff_funs))
    for (fun in prob_payoff_funs){
        sims <- 1:n_sims
        sim_offset[, fun] <- (sims - 1) * sim_offset0[[fun]]
    }
    return(sim_offset)
}

