
#' Convert to a reactibble object
#' @param x A data frame, list, matrix, or other object that could reasonably be coerced to a tibble.
#'
#' @export
is_reactibble <- function(x) {
  inherits(x, "reactibble")
}


#' Convert to a reactibble object
#' @param x A data frame, list, matrix, or other object that could reasonably be coerced to a tibble.
#'
#' @export
as_reactibble <- function(x) {
  if(is_reactibble(x)) {
    class(x) <- c("reactibble", "tbl_df", "tbl", "data.frame")
  }
  x <- tibble::as_tibble(x)
  class(x) <- union("reactibble", class(x))
  x
}

#' Build a reactibble object
#' @param ... A set of name-value pairs, use `~` to define a reactive column
#'
#' @export
reactibble <- function(...) {
  dots  <- process_reactive_dots(...)
  x <- dplyr::tibble(!!!dots)
  x <- process_reactive_output(x, dots)
  as_reactibble(x)
}

strip_reactibble_class <- function(x) {
  class(x) <- setdiff(attr(x, "class"), "reactibble")
  x
}

strip_reactive_col <- function(x) {
  class(x) <- setdiff(attr(x, "class"), "reactive_col")
  attr(x, "reactibble_expr") <- NULL
  x
}

#' convert reactive columns to static columns
#'
#' If no column names are provided, the full reactibble is materialized. The
#' class "reactibble" is preserved.
#'
#' @param  x a reactibble object
#' @param ... bare column names
#' @export
materialize <- function(x, ...) {
  x <- strip_reactibble_class(x)
  if (! ...length()) {
    x[] <- lapply(x, strip_reactive_col)
  } else {
    cols <- sapply(eval(substitute(alist(...))), deparse1)
    x[cols] <- lapply(x[cols], strip_reactive_col)
  }
  as_reactibble(x)
}

`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}
