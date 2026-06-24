check_params <- function(params, verbose, parallel) {

    is_scalar <- function(x) {
        is.atomic(x) && length(x) == 1
    }

    if (is.data.frame(params)) {
        if (nrow(params) == 0) {
            stop("params data frame has no rows. Provide at least one parameter set.",
                 call. = FALSE)
        }
        non_numeric <- names(params)[!vapply(params, is.numeric, logical(1))]
        if (length(non_numeric) > 0) {
            stop("All params columns must be numeric. Non-numeric column(s): ",
                 paste(non_numeric, collapse = ", "),
                 ". Convert factor/character columns with as.numeric(as.character(x)).",
                 call. = FALSE)
        }
        if (verbose) {
            n_sims <- 1
            params <- params[1, ]
            parallel <- FALSE
            message("Since verbose is enabled, only the first simulation (row) of the parameters data frame was used to avoid returning large objects and running out of memory.")
        } else {
            n_sims <- nrow(params)
        }
    } else if (is.list(params)) {
        n_sims <- 1

        if (!all(sapply(params, is_scalar))) {
            stop("All parameters in the list must be scalars.")
        }
        params <- as.data.frame(params)
        parallel <- FALSE
    } else {
        stop("params must be a data frame or a list of parameters with one value per parameter")
    }

    results <- list()
    results$parallel <- parallel
    results$n_sims <- n_sims
    results$params <- params
    return(results)
}
