
#' @export
`[[<-.reactibble` <- function(x, name, value) {
  x <- strip_reactibble_class(x)
  x[[name]] <- value
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  as_reactibble(x)
}

#' @export
`$<-.reactibble` <- function(x, name, value) {
  x[[name]] <- value
  x
}

#' @export
`[<-.reactibble` <- function(x, ..., value) {
  cl <- class(x)
  x <- strip_reactibble_class(x)
  x[...] <- value
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  class(x) <- cl
  x
}

#' @export
transform.reactibble <- function (`_data`, ...) {
  cl <- class(`_data`)
  x <- strip_reactibble_class(`_data`)
  x <- transform(x, ...)
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  class(x) <- cl
  x
}

#' @export
within.reactibble <- function (data, expr, ...) {
  cl <- class(data)
  x <- strip_reactibble_class(data)
  x <- eval.parent(substitute(within(x, expr, ...)))
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  class(x) <- cl
  x
}


#' @export
`[.reactibble` <- function(x, ...){
  cl <- class(x)
  x <- strip_reactibble_class(x)
  x <- x[...]
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  class(x) <- cl
  x
}

#' @export
#' @importFrom stats setNames
`names<-.reactibble` <- function(x, value) {
  cl <- class(x)
  x <- strip_reactibble_class(x)
  # renaming arg for `substitute`
  args <- setNames(lapply(value, as.symbol), names(x))
  for (i in seq_along(x)) {
    if(inherits(x[[i]], "reactive_col")) {
      attr(x[[i]], "reactibble_expr") <-
        do.call(substitute, c(list(attr(x[[i]], "reactibble_expr"), args)))
    }
  }
  names(x) <- value
  class(x) <- cl
  x
}

# #' @export
# #' @method as.data.frame reactibble
# as.data.frame.reactibble <- function(
#   x, row.names = NULL, optional = FALSE, ...) {
#   x <- strip_reactibble_class(x)
#   x[] <- lapply(x, strip_reactive_col)
#   NextMethod()
# }
#
# #' Convert to tibble
# #'
# #' @param x react tibble object
# #' @param ... forwarded to tibble::as_tibble
# #' @param .rows forwarded to tibble::as_tibble
# #' @param .name_repair Treatment of problematic column names
# #' @param rownames rownames How to treat existing row names of a data frame or matrix
# #' @export
# as_tibble.reactibble <- function(
#   x, ..., .rows = NULL,
#   .name_repair = c("check_unique",  "unique", "universal", "minimal"),
#   rownames = pkgconfig::get_config("tibble::rownames", NULL)) {
#   x <- strip_reactibble_class(x)
#   x[] <- lapply(x, strip_reactive_col)
#   NextMethod()
# }
#
#
# #' @export
# #' @method as.data.table reactibble
# as.data.table.reactibble <- function(x, keep.rownames = FALSE, ...) {
#   x <- strip_reactibble_class(x)
#   x[] <- lapply(x, strip_reactive_col)
#   NextMethod()
# }
#
# # to avoid error with devtools::load_all()
# as.data.table <- NULL
#

#' @export
`[[.reactibble` <- function(x, ...) {
  strip_reactive_col(.subset2(x, ...))
}

#' @export
`$.reactibble` <- function(x, ...) {
  strip_reactive_col(.subset2(x, ...))
}
