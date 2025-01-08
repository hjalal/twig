create_fun_array <- function(funs, fun_args, arg_value_sizes, core_args, size_arg_values){
    IDX <- matrix(NA, nrow = size_arg_values, ncol = length(funs),
    dimnames = list(NULL, funs))
    for (i in seq_along(funs)){
        fun <- funs[i]
        
        
        sel_core_args <- core_args[core_args %in% fun_args[[fun]]]

        
        sel_expanded_args <- core_args[!core_args %in% sel_core_args]

        
        size0 <- arg_value_sizes[sel_core_args]

        
        idx0 <- 1:prod(size0)

        
        dim_unsorted <- c(sel_core_args, sel_expanded_args)

        
        IDX0 <- array(idx0, dim = arg_value_sizes[dim_unsorted])

        
        perm <- match(core_args, dim_unsorted)

        
        idx <- aperm(IDX0, perm)
        dim(idx) <- size_arg_values
        IDX[,i] <- idx
    }
    return(IDX)
}
