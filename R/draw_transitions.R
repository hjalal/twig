#' Draw the Markov state-transition diagram
#'
#' Visualizes the state-transition structure of a Markov twig as a directed
#' graph, with edges labeled by their transition probabilities. Because the
#' probabilities depend on the decision (and possibly the cycle), the diagram is
#' drawn for a single decision and cycle.
#'
#' The transition probabilities are taken from a verbose [run_twig()] result, or
#' computed by running the model when a twig object and parameters are supplied.
#' The Graphviz DOT description is generated in pure R; rendering
#' (`output = "graph"`) requires the suggested package \pkg{DiagrammeR}.
#'
#' @param x One of: a verbose [run_twig()] result (a list with a
#'   `markov_trans_probs` element), a `markov_trans_probs` array, or a twig
#'   object. When a twig object is supplied, `params` is required and the model
#'   is run (single simulation) to obtain the probabilities.
#' @param params A named list or data frame of parameters, required only when
#'   `x` is a twig object.
#' @param n_cycles Number of cycles to use when `x` is a twig object that must be
#'   run. Default 1.
#' @param decision The decision to display, as a name or index. Defaults to the
#'   first decision.
#' @param cycle The cycle to display when the transition probabilities are
#'   cycle-dependent. Default 1.
#' @param output Either `"graph"` (default, a \pkg{DiagrammeR} htmlwidget) or
#'   `"dot"` (the Graphviz DOT string).
#' @param digits Number of digits for the probability edge labels. Default 3.
#' @param threshold Transitions with probability at or below this value are
#'   omitted. Default 0 (only nonzero transitions are drawn).
#' @return A \pkg{DiagrammeR} htmlwidget when `output = "graph"`, otherwise a
#'   Graphviz DOT string.
#' @seealso [draw_decision_tree()], [run_twig()]
#' @export
#' @examples
#' # A 2-state transition array (state x dest x decision)
#' tp <- array(
#'   c(0.9, 0, 0.1, 1,      # decision A
#'     0.8, 0, 0.2, 1),     # decision B
#'   dim = c(2, 2, 2),
#'   dimnames = list(state = c("Alive", "Dead"),
#'                   dest = c("Alive", "Dead"),
#'                   decision = c("A", "B")))
#' cat(draw_transitions(tp, output = "dot"))
#'
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   draw_transitions(tp, decision = "A")
#' }
draw_transitions <- function(x, params = NULL, n_cycles = 1, decision = NULL,
                             cycle = 1, output = c("graph", "dot"),
                             digits = 3, threshold = 0) {
  output <- match.arg(output)
  P <- get_trans_probs_matrix(x, params, n_cycles, decision, cycle)
  dot <- trans_matrix_to_dot(P, digits = digits, threshold = threshold)
  render_dot(dot, output)
}

# Resolve the requested decision/cycle slice into a square origin-by-dest matrix.
get_trans_probs_matrix <- function(x, params, n_cycles, decision, cycle) {

  if (is.array(x)) {
    tp <- x
  } else if (is.list(x) && !is.null(x$markov_trans_probs)) {
    tp <- x$markov_trans_probs
  } else if (inherits(x, "twig_class")) {
    if (is.null(params)) {
      stop("`params` is required when `x` is a twig object.", call. = FALSE)
    }
    result <- run_twig(x, params = params, n_cycles = n_cycles, verbose = TRUE,
                       parallel = FALSE, progress_bar = FALSE)
    tp <- result$markov_trans_probs
  } else {
    stop("`x` must be a twig object, a verbose run_twig() result, or a ",
         "markov_trans_probs array.", call. = FALSE)
  }

  n_dim <- length(dim(tp))
  if (n_dim != 3 && n_dim != 4) {
    stop("Transition probabilities must be a 3D (state x dest x decision) or ",
         "4D (state x dest x cycle x decision) array.", call. = FALSE)
  }

  decision_names <- dimnames(tp)[[n_dim]]
  d_idx <- resolve_index(decision, decision_names, "decision")

  if (n_dim == 4) {
    cycle_names <- dimnames(tp)[[3]]
    c_idx <- resolve_index(cycle, cycle_names, "cycle", allow_numeric_value = TRUE)
    P <- tp[, , c_idx, d_idx]
  } else {
    P <- tp[, , d_idx]
  }
  P
}

# Map a name or index to a position within a dimension.
resolve_index <- function(value, names_vec, what, allow_numeric_value = FALSE) {
  if (is.null(value)) return(1L)
  if (is.character(value)) {
    idx <- match(value, names_vec)
    if (is.na(idx)) {
      stop("Unknown ", what, " '", value, "'. Available: ",
           paste(names_vec, collapse = ", "), ".", call. = FALSE)
    }
    return(idx)
  }
  if (allow_numeric_value && !is.null(names_vec)) {
    pos <- match(as.character(value), names_vec)
    if (!is.na(pos)) return(pos)
  }
  as.integer(value)
}

# Build the Graphviz DOT for a transition matrix (internal, dependency-free).
trans_matrix_to_dot <- function(P, digits = 3, threshold = 0) {
  origins <- rownames(P)
  dests   <- colnames(P)
  if (is.null(origins)) origins <- as.character(seq_len(nrow(P)))
  if (is.null(dests))   dests   <- as.character(seq_len(ncol(P)))

  states <- union(origins, dests)
  nodes <- vapply(states, function(s) {
    sprintf('  "%s" [shape=ellipse, style=filled, fillcolor="#e8f0fe"];', s)
  }, character(1))

  edges <- character(0)
  for (i in seq_along(origins)) {
    for (j in seq_along(dests)) {
      w <- P[i, j]
      if (!is.na(w) && w > threshold) {
        edges <- c(edges, sprintf('  "%s" -> "%s" [label="%s"];',
                                  origins[i], dests[j], format(round(w, digits), nsmall = 0)))
      }
    }
  }

  paste0(
    "digraph transitions {\n",
    "  rankdir=LR;\n",
    "  node [fontname=Helvetica];\n",
    "  edge [fontname=Helvetica, fontsize=10];\n",
    paste(nodes, collapse = "\n"), "\n",
    paste(edges, collapse = "\n"), "\n",
    "}\n"
  )
}
