#' @export
print.reactibble <- function(x, ...) {
  # very weird but we need this to print the tilde in col header
  print(strip_reactibble_class(x))
  invisible(x)
}
