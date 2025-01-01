# Define the %out% function
`%out%` <- function(x, table) {
  !(x %in% table)
}