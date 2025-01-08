get_event_dep_rewards <- function(reward_funs, fun_args, event_args){
    is_reward_event_dep <- rep(FALSE, length(reward_funs))
    for (i in seq_along(reward_funs)){
        reward <- reward_funs[i]
        reward_args <- fun_args[[reward]]
        is_reward_event_dep[i] <- any(event_args %in% reward_args)
    }
    return(is_reward_event_dep)
}