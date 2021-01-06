
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
  x <- strip_reactibble_class(x)
  x[...] <- value
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  as_reactibble(x)
}

#' @export
transform.reactibble <- function (`_data`, ...) {
  x <- strip_reactibble_class(`_data`)
  x <- transform(x, ...)
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  as_reactibble(x)
}

#' @export
within.reactibble <- function (data, expr, ...) {
  x <- strip_reactibble_class(data)
  x <- eval.parent(substitute(within(x, expr, ...)))
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  as_reactibble(x)
}
