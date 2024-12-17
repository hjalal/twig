# 9. R0: create a single array for all event-dep rewards by path k --------------
# rewards are by evnets, but have to be made dependent on path k

reward_funs
reward_fun_args <- fun_args[reward_funs]
reward_fun_values <- eval_funs[reward_funs]
reward_fun_arg_sizes <- arg_value_sizes[reward_fun_args]

# preparation ----------------
# get ids 
# get reward names that are event dependent
is_reward_event_dep <- rep(FALSE, length(reward_funs))
for (i in seq_along(reward_funs)){
    reward <- reward_funs[i]
    reward_args <- fun_args[[reward]]
    is_reward_event_dep[i] <- any(event_args %in% reward_args)
}
is_reward_event_dep

event_dep_rewards <- reward_funs[is_reward_event_dep]
event_indep_rewards <- reward_funs[!is_reward_event_dep]

# for event dependnet rewards weight them by their paths -----------------

# goal is to get a single matrix for each reward function:
# dims = (S, C, D), k=path_id, event_dep_reward_id




# combine with the event independent rewards to get a single array for all rewards


# get the reward values for each event








# simulation dependent rewards ----------------
sim 


