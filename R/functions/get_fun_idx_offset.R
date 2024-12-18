# get the idx offset for each function to be multiplied by the number of simualtions
get_fun_idx_offset <- function( prob_reward_funs, fun_args, eval_funs, sim_args, n_sims){
    idx_offset <- lapply(prob_reward_funs, function(x) {
        if ( any(sim_args %in% fun_args[[x]])){
            return(length(eval_funs[[x]])/n_sims)
        } else {
            return(0)
        }
    })
    names(idx_offset) <- prob_reward_funs
    return(idx_offset)
}
