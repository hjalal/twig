#' Negation of %in% operator
#'
#' This function checks if elements of a vector are not in another vector.
#'
#' @param x A vector of values to be checked.
#' @param table A vector of values to be compared against.
#' @return A logical vector indicating if the elements of `x` are not in `table`.
#' @export
#' @examples
#' x <- c("A", "B", "C")
#' table <- c("B", "C", "D")
#' x %out% table
`%out%` <- function(x, table) {
  !(x %in% table)
}