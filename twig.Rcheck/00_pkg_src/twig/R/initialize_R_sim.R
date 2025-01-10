initialize_R_sim <- function(   n_decisions, 
                                n_payoffs, 
                                n_sims, 
                                decision_names,
                                payoff_funs) {
    d_R_summary <- c("decision", "payoff", "sim")
    dim_R_summary <- c(decision = n_decisions, payoff = n_payoffs, sim = n_sims)
    dim_names_R_summary <- list(decision = decision_names, payoff = payoff_funs, sim = 1:n_sims)

    R_sim <- array(NA, dim = dim_R_summary, dimnames = dim_names_R_summary)
    return(R_sim)
}