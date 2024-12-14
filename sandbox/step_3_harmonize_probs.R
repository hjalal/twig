
# for each prob function, we need to harmonize the probabilities
F <- F0
for (i in 1:n_prob_funs){
    fun <- prob_funs[i]
    # harmonize the probabilities based on their harmonized indices
    F[,i] <- eval_funs[[fun]][IDX[,i]]
}

# dim(F) <- dim_F
# dimnames(F) <- dimnames_F
F
