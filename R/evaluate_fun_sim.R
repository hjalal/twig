
evaluate_fun_sim <- function(   X0, 
                                X_IDX,
                                funs, 
                                eval_funs){

    X <- X0
    for (i in seq_along(funs)){
        fun <- funs[i]

        X[, i] <- eval_funs[[fun]][X_IDX[, i]] 
    }
    return(X)
}