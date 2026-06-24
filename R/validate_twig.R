#' Validate a twig model and its functions before running
#'
#' A pre-flight check that complements the internal twig-structure validation by
#' verifying that the probability, payoff, and initial-probability
#' *functions* referenced by a twig are defined and that their arguments are
#' consistent with the model's core arguments and the supplied parameters.
#'
#' Unlike the checks performed inside [run_twig()], `validate_twig()` collects
#' *all* problems it finds and reports them together, which makes it easier to
#' fix several issues at once before launching a (potentially long) simulation.
#'
#' @param twig_obj A twig object created with [twig()].
#' @param params An optional named list or data frame of parameters, as passed
#'   to [run_twig()]. When supplied, function arguments are checked against the
#'   parameter names; when `NULL`, only function existence and core arguments are
#'   checked.
#' @param envir The environment in which the user-defined functions are searched
#'   for. Defaults to the calling environment.
#' @return Invisibly `TRUE` when validation passes. Otherwise an error is raised
#'   listing every problem found.
#' @seealso [run_twig()]
#' @export
#' @examples
#' mytwig <- twig() +
#'   decisions(names = c(A, B)) +
#'   states(names = c(Alive, Dead), init_probs = c(1, 0)) +
#'   event(name = death_event,
#'         options = c(yes, none),
#'         probs = c(pDie, leftover),
#'         transitions = c(Dead, stay)) +
#'   payoffs(names = c(utility))
#'
#' pDie <- function(state, prob_die) prob_die * (state == "Alive")
#' utility <- function(state) 1 * (state == "Alive")
#'
#' validate_twig(mytwig, params = list(prob_die = 0.1))
validate_twig <- function(twig_obj, params = NULL, envir = parent.frame()) {

  problems <- character(0)
  add <- function(msg) problems[[length(problems) + 1L]] <<- msg

  is_number <- function(x) !is.na(suppressWarnings(as.numeric(x)))
  reserved  <- c("leftover", "complement", "stay", "none")

  # ---- collect the function names referenced by the twig --------------------
  event_layers <- retrieve_layer_by_type(twig_obj, type = "event")
  event_names  <- unique(unlist(lapply(event_layers, function(x) x$event)))
  prob_refs    <- unlist(lapply(event_layers, function(x) x$probs))
  prob_refs    <- unique(prob_refs[!prob_refs %in% reserved & !vapply(prob_refs, is_number, logical(1))])

  payoff_layer <- retrieve_layer_by_type(twig_obj, type = "payoffs")
  payoff_refs  <- payoff_layer$payoffs
  payoff_refs  <- unique(payoff_refs[!vapply(payoff_refs, is_number, logical(1))])

  state_layer <- retrieve_layer_by_type(twig_obj, type = "states")
  p0_refs <- character(0)
  if (length(state_layer) > 0) {
    p0_refs <- state_layer$init_probs
    p0_refs <- unique(p0_refs[!p0_refs %in% reserved & !vapply(p0_refs, is_number, logical(1))])
  }

  referenced <- unique(c(prob_refs, payoff_refs, p0_refs))

  # ---- duplicate payoff names ----------------------------------------------
  dup_payoffs <- unique(payoff_layer$payoffs[duplicated(payoff_layer$payoffs)])
  if (length(dup_payoffs) > 0) {
    add(paste0("Duplicate payoff name(s): ", paste(dup_payoffs, collapse = ", "),
               ". Each payoff must have a unique name."))
  }

  # ---- existence of referenced functions ------------------------------------
  defined <- vapply(referenced, function(f) exists(f, mode = "function", envir = envir),
                    logical(1))
  missing_funs <- referenced[!defined]
  if (length(missing_funs) > 0) {
    add(paste0("Referenced function(s) not found: ", paste(missing_funs, collapse = ", "),
               ". Define them (vectorized) before calling run_twig()."))
  }

  # ---- argument consistency for the functions that do exist -----------------
  # Use the same core arguments run_twig() accepts for this model type, so
  # validate_twig() does not pass argument combinations the run would reject.
  core_args  <- if (inherits(twig_obj, "markov_twig")) {
    c("decision", "state", "cycle", "cycle_in_state")
  } else {
    c("decision", "outcome")
  }
  param_args <- if (is.null(params)) character(0) else names(params)
  allowed    <- c(core_args, event_names, param_args)

  for (f in referenced[defined]) {
    fa <- names(formals(get(f, envir = envir)))
    if (is.null(fa)) next                       # no-argument function: always valid
    extra <- setdiff(fa, allowed)
    if (length(extra) > 0) {
      hint <- if (is.null(params)) {
        " (pass `params` to validate_twig() to check parameter names too)"
      } else {
        " - not a core argument, prior event, or a name in `params`"
      }
      add(paste0("Function '", f, "' has unexpected argument(s): ",
                 paste(extra, collapse = ", "), hint, "."))
    }
  }

  # ---- parameters that are never used (informational) -----------------------
  if (!is.null(params) && length(param_args) > 0) {
    used <- unique(unlist(lapply(referenced[defined], function(f) {
      names(formals(get(f, envir = envir)))
    })))
    unused <- setdiff(param_args, used)
    if (length(unused) > 0) {
      message("Note: parameter(s) not used by any function: ",
              paste(unused, collapse = ", "), ".")
    }
  }

  if (length(problems) > 0) {
    stop("validate_twig() found ", length(problems), " problem(s):\n",
         paste0("  - ", unlist(problems), collapse = "\n"), call. = FALSE)
  }

  message("validate_twig: all referenced functions are defined and consistent.")
  invisible(TRUE)
}
