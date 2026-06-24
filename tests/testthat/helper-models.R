# Define named functions in the global environment (where run_twig() resolves
# user-defined functions) for the duration of the calling test, then remove them
# automatically when the test finishes.
local_twig_funs <- function(..., .env = parent.frame()) {
  funs <- list(...)
  nms <- names(funs)
  for (n in nms) assign(n, funs[[n]], envir = globalenv())
  withr::defer(suppressWarnings(rm(list = nms, envir = globalenv())), envir = .env)
  invisible(nms)
}

# A small deterministic 2-state Markov model used across tests. With p = 0 the
# whole cohort stays Alive, so over N cycles: cost = 10 * N and utility = N.
build_markov_model <- function() {
  twig() +
    decisions(names = c(A, B)) +
    states(names = c(Alive, Dead), init_probs = c(1, 0)) +
    event(name = death_event,
          options = c(yes, none),
          probs = c(pDie, leftover),
          transitions = c(Dead, stay)) +
    payoffs(names = c(cost, utility))
}

# Skip a parallel test only when the parallel workers cannot load an installed
# copy of twig at all (for example under devtools::load_all(), where twig is not
# installed). When twig IS loadable on the workers, the test proceeds and a real
# parallel failure surfaces as a test failure rather than a skip.
skip_if_no_parallel_twig <- function() {
  loadable <- tryCatch({
    cl <- parallel::makeCluster(1)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    isTRUE(parallel::clusterEvalQ(cl, requireNamespace("twig", quietly = TRUE))[[1]])
  }, error = function(e) FALSE)
  if (!isTRUE(loadable)) {
    testthat::skip("twig is not loadable on parallel workers (not an installed package)")
  }
}
