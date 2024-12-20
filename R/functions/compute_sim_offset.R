   # sim is the last dimension of the probability function vectors
    # so, we need to offset the probabilities for each sim for each probability and reward function

compute_sim_offset <- function(n_sims, prob_reward_funs, sim_offset0) {
    # sim_offset <- lapply(prob_reward_funs, function(x) {
    #     (sim - 1) * sim_offset0[[x]]
    # })
    # names(sim_offset) <- prob_reward_funs
    sim_offset <- matrix(NA, nrow = n_sims, ncol = length(prob_reward_funs), dimnames = list(NULL, prob_reward_funs))
    for (fun in prob_reward_funs){
        sims <- 1:n_sims
        sim_offset[, fun] <- (sims - 1) * sim_offset0[[fun]]
    }
    return(sim_offset)
}


