# set_formula_en
#
# quosure2formula <- function(q) {
#
# }


#' Convert to a reactibble object
#' @param x A data frame, list, matrix, or other object that could reasonably be coerced to a tibble.
#'
#' @export
is_reactibble <- function(x) {
  inherits(x, "reactibble")
}

is_reactive_col <- function(x) {
  inherits(x, "reactive_col")
}


#' Convert to a reactibble object
#'
#' @param x forwarded to tibble::as_tibble
#' @param ... forwarded to tibble::as_tibble
#' @param .rows forwarded to tibble::as_tibble
#' @param .name_repair forwarded to tibble::as_tibble
#' @param rownames forwarded to tibble::as_tibble
#' @export
as_reactibble <- function(
  x, ..., .rows = NULL,
  .name_repair = c("check_unique", "unique", "universal", "minimal"),
  rownames = pkgconfig::get_config("tibble::rownames", NULL )) {
  if(is_reactibble(x)) {
    class(x) <- c("reactibble", "tbl_df", "tbl", "data.frame")
    return(x)
  }
  x <- tibble::as_tibble(
    x, ..., .rows = .rows, .name_repair = .name_repair, rownames = rownames)
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

as_reactive_col <- function(x, expr) {
  if(is.list(x))
    # to work around tibble issue
    class(x) <- union(c("reactive_col", "list"), attr(x, "class"))
  else
    class(x) <- union("reactive_col", attr(x, "class"))
  attr(x,"reactibble_col_def") <- expr
  x
}

strip_reactive_col <- function(x) {
  # we need this wrapper because vec_data smodifies some non vcrts objects like data frames
  if(is_reactive_col(x)) vec_data(x) else x
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
