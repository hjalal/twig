   
    

compute_sim_offset <- function(n_sims, prob_reward_funs, sim_offset0) {
    
    
    
    
    sim_offset <- matrix(NA, nrow = n_sims, ncol = length(prob_reward_funs), dimnames = list(NULL, prob_reward_funs))
    for (fun in prob_reward_funs){
        sims <- 1:n_sims
        sim_offset[, fun] <- (sims - 1) * sim_offset0[[fun]]
    }
    return(sim_offset)
}


