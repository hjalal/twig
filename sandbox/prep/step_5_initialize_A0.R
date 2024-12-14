# path array initialization
# and the dimension of the F array and its dimnames
F0 <- matrix(0, nrow = size_core_arg_values, ncol = n_prob_funs)
dim_F <- c(core_arg_value_sizes, prob_funs = n_prob_funs)
dimnames_F <- arg_values[core_args]
dimnames_F$prob_funs <- prob_funs