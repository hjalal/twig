# E[D,S,C,E(s),j=event_id]. so similar to the F array, 
# but with the event_id instead of the prob_funs, which 
# involves computing the complement of the probabilities.


# adjustments of the event array for each sim
#dim(F) <- c(prod(core_arg_value_sizes), n_prob_funs)

E <- E0

E[, non_compl_id] <- F[, event_prob_link[non_compl_id]]
for (i in 1:length(hash_id)){
    E[, hash_id[i]] <- 1 - rowSums(E[, compl_id[[i]], drop = FALSE])
}
E

# output E for debugging if needed
# dim_E <- c(core_arg_value_sizes, event_id = n_events)
# dimnames_E <- arg_values[core_args]
# dimnames_E$event_id <- event_ids

#E0 <- array(NA, dim = dim_E, dimnames = dimnames_E)