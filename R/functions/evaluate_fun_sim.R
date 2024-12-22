# for each prob or event dep reward function, we need to harmonize the entries 
# so that they have the same structure of S,C,D,E(s)
# this function also samples the values from teh function vectors by
# offsetting the indices by the simulation offset values
# because sim is the last dimensions of each function vector.

evaluate_fun_sim <- function(   X0, 
                                X_IDX,
                                funs, 
                                eval_funs){
    # initalize the output matrix to have the same dimensions as the input matrix
    X <- X0
    for (i in seq_along(funs)){
        fun <- funs[i]
        # harmonize the probabilities based on their harmonized indices per simulation
        # and add the simulation offset to the indices
        X[, i] <- eval_funs[[fun]][X_IDX[, i]] # + sim_offset[fun]]
    }
    return(X)
}