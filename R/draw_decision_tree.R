#' Draw the decision tree of a twig
#'
#' Produces a visualization of the event (chance-node) structure of a twig as a
#' decision tree: a square decision node branches into each strategy, and each
#' strategy expands the shared sequence of chance nodes (events) down to the
#' terminal outcomes.
#'
#' The Graphviz DOT description is generated in pure R (no extra dependency). To
#' *render* the diagram (`output = "graph"`), the suggested package
#' \pkg{DiagrammeR} must be installed; otherwise request `output = "dot"` to get
#' the DOT string, which can be rendered by any Graphviz engine.
#'
#' @param twig_obj A twig object created with [twig()].
#' @param output Either `"graph"` (default) to return a rendered \pkg{DiagrammeR}
#'   htmlwidget, or `"dot"` to return the Graphviz DOT string.
#' @param prob_labels Logical; if `TRUE` (default) edges are labeled with the
#'   option name and its probability-function name.
#' @return A \pkg{DiagrammeR} htmlwidget when `output = "graph"`, otherwise a
#'   length-1 character string of Graphviz DOT (invisibly).
#' @seealso [draw_transitions()], [run_twig()]
#' @export
#' @examples
#' mytwig <- twig() +
#'   decisions(names = c(Treat, DoNotTreat)) +
#'   event(name = progress,
#'         options = c(yes, none),
#'         probs = c(pProgress, leftover),
#'         transitions = c(sick, healthy)) +
#'   payoffs(names = c(cost))
#'
#' # Get the Graphviz DOT (no extra package needed):
#' cat(draw_decision_tree(mytwig, output = "dot"))
#'
#' # Render it (requires the suggested DiagrammeR package):
#' if (requireNamespace("DiagrammeR", quietly = TRUE)) {
#'   draw_decision_tree(mytwig)
#' }
draw_decision_tree <- function(twig_obj, output = c("graph", "dot"),
                               prob_labels = TRUE) {
  output <- match.arg(output)
  dot <- twig_to_dot_decision_tree(twig_obj, prob_labels = prob_labels)
  render_dot(dot, output)
}

# Build the Graphviz DOT for the decision tree (internal, dependency-free).
twig_to_dot_decision_tree <- function(twig_obj, prob_labels = TRUE) {

  decision_layer <- retrieve_layer_by_type(twig_obj, type = "decisions")
  decisions <- decision_layer$decisions

  events_df <- get_events_df(twig_obj)
  event_names <- unique(events_df$event)
  initial_event <- unique(events_df$event[!events_df$event %in% events_df$transitions])
  if (length(initial_event) != 1) {
    stop("Expected a single initial event but found: ",
         paste(initial_event, collapse = ", "), ".")
  }

  acc <- new.env(parent = emptyenv())
  acc$nodes <- character(0)
  acc$edges <- character(0)
  acc$counter <- 0L

  edge_label <- function(option, prob) {
    if (prob_labels && !prob %in% c("leftover", "complement")) {
      paste0(option, " (", prob, ")")
    } else {
      option
    }
  }

  emit_event <- function(event, prefix) {
    node_id <- paste0(prefix, "__", event)
    acc$nodes <- c(acc$nodes,
                   sprintf('  "%s" [label="%s", shape=ellipse, style=filled, fillcolor="#e8f0fe"];',
                           node_id, event))
    rows <- events_df[events_df$event == event, , drop = FALSE]
    for (i in seq_len(nrow(rows))) {
      option <- rows$options[i]
      prob   <- rows$probs[i]
      trans  <- rows$transitions[i]
      lab    <- edge_label(option, prob)
      if (trans %in% event_names) {
        child_id <- emit_event(trans, prefix)
      } else {
        acc$counter <- acc$counter + 1L
        child_id <- paste0(prefix, "__term", acc$counter)
        acc$nodes <- c(acc$nodes,
                       sprintf('  "%s" [label="%s", shape=box, style=filled, fillcolor="#fce8e6"];',
                               child_id, trans))
      }
      acc$edges <- c(acc$edges,
                     sprintf('  "%s" -> "%s" [label="%s"];', node_id, child_id, lab))
    }
    node_id
  }

  acc$nodes <- c(acc$nodes, '  "__root" [label="Decision", shape=box, style=filled, fillcolor="#fff2cc"];')
  for (d in decisions) {
    child <- emit_event(initial_event, prefix = d)
    acc$edges <- c(acc$edges, sprintf('  "__root" -> "%s" [label="%s"];', child, d))
  }

  paste0(
    "digraph decision_tree {\n",
    "  rankdir=LR;\n",
    "  node [fontname=Helvetica];\n",
    "  edge [fontname=Helvetica, fontsize=10];\n",
    paste(acc$nodes, collapse = "\n"), "\n",
    paste(acc$edges, collapse = "\n"), "\n",
    "}\n"
  )
}
