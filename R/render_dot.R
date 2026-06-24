# Escape characters that are special inside a double-quoted Graphviz string
# (backslash first, then double quote) so user-supplied node names or labels
# cannot produce invalid DOT.
dot_escape <- function(s) {
  s <- gsub("\\\\", "\\\\\\\\", s)
  gsub('"', '\\\\"', s)
}

# Render a Graphviz DOT string, or return it as text (internal helper).
#
# output = "dot"   -> returns the DOT string.
# output = "graph" -> returns a DiagrammeR htmlwidget, requiring the suggested
#                     DiagrammeR package.
render_dot <- function(dot, output = c("graph", "dot")) {
  output <- match.arg(output)
  if (output == "dot") {
    return(dot)
  }
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("Rendering the diagram requires the 'DiagrammeR' package.\n",
         "Install it with install.packages(\"DiagrammeR\"), or call this ",
         "function with output = \"dot\" to obtain the Graphviz source.",
         call. = FALSE)
  }
  DiagrammeR::grViz(dot)
}
