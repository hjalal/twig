# steps to evaluate a function
prob_funs <- get_prob_funs(twig_obj)
reward_funs <- get_reward_funs(twig_obj)
p0_funs <- get_p0_funs(twig_obj)

twig_funs <- c(prob_funs, reward_funs, p0_funs)

# used prob and reward arguments in the twig
fun_args <- get_function_arguments(twig_funs)

# unique arguments in twig functions
all_args <- unique(unlist(fun_args))

# used core arguments D, S, C, E(s), O
core_args <- get_core_args(twig_obj, all_args)

# use psa arguments from the parameters column names
sim_args <- get_sim_args(params, all_args)

# get used argument values 
arg_values <- get_arg_values(twig_obj, core_args, sim_args, n_cycles)

# get argument value sizes
arg_value_sizes <- get_arg_value_sizes(arg_values, core_args, sim_args)

# evaluate all functions in the twig and generate a vector for each function
eval_funs <- evaluate_function(twig_funs, fun_args, core_args, sim_args, arg_values, params)
print(eval_funs)

#TODO: 
# output data by default the first PSA into excel file 
