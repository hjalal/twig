
# Get the IDX of indices of the arguments in the sorted arguments
core_arg_value_sizes <- arg_value_sizes[core_args]
size_core_arg_values <- prod(core_arg_value_sizes)
n_prob_funs <- length(prob_funs)
IDX <- matrix(NA, nrow = size_core_arg_values, ncol = length(prob_funs))

for (i in seq_along(prob_funs)){
    fun <- prob_funs[i]
    
    # get function's core arguments
    sel_core_args <- core_args[core_args %in% fun_args[[fun]]]

    # get other core argumetns not in function's core arguments
    sel_expanded_args <- core_args[!core_args %in% sel_core_args]

    # get the size of the unsorted core arguments
    size0 <- arg_value_sizes[sel_core_args]

    # a vector of original indices of prob function
    idx0 <- 1:prod(size0)

    # get the dimensions of the unsorted core arguments
    dim_unsorted <- c(sel_core_args, sel_expanded_args)

    # Unsorted expanded idx0 to the size of the core arguments
    IDX0 <- array(idx0, dim = arg_value_sizes[dim_unsorted])

    # Create the permutation vector to reorder dim0 to match core_args
    perm <- match(core_args, dim_unsorted)

    # Sort the IDX dims so they match the core arguments order
    idx <- aperm(IDX0, perm)
    dim(idx) <- size_core_arg_values
    IDX[,i] <- idx
}
IDX


