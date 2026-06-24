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
