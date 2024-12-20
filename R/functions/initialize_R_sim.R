initialize_R_sim <- function(   n_decisions, 
                                n_rewards, 
                                n_sims, 
                                decision_names,
                                reward_funs) {
    d_R_summary <- c("decision", "reward", "sim")
    dim_R_summary <- c(decision = n_decisions, reward = n_rewards, sim = n_sims)
    dim_names_R_summary <- list(decision = decision_names, reward = reward_funs, sim = 1:n_sims)
    #R0 <- array(NA, dim = c(n_decisions, n_rewards), dimnames = list(decision_id = decision_ids, reward_id = reward_funs))
    R_sim <- array(NA, dim = dim_R_summary, dimnames = dim_names_R_summary)
    return(R_sim)
}