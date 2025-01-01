get_O <- function(n_decisions, n_dest, A, dest_paths, decision_names, unique_dest_names){
    # browser()
    O <- matrix(NA, nrow = n_decisions, ncol = n_dest, 
                dimnames = list(decision_names, unique_dest_names))
                outcome <- unique_dest_names[1]
    for (outcome in unique_dest_names){
        outcome_paths <- dest_paths[[outcome]]
        if (length(outcome_paths) == 1){
        O[,outcome] <- A[,outcome_paths]
        } else if (length(outcome_paths) > 1){
        O[,outcome] <- rowSums(A[,outcome_paths])
        } else {
        stop("No paths found for outcome ", outcome)
        }
    }
    return(O)
}
