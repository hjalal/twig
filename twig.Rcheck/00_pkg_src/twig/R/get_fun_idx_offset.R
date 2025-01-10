
get_fun_idx_offset <- function( prob_payoff_funs, fun_args, eval_funs, sim_args, n_sims){
    idx_offset <- lapply(prob_payoff_funs, function(x) {
        if ( any(sim_args %in% fun_args[[x]])){
            return(length(eval_funs[[x]])/n_sims)
        } else {
            return(0)
        }
    })
    names(idx_offset) <- prob_payoff_funs
    return(idx_offset)
}
