
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
  warning("`transform` converts a 'reactibble' object to a static data frame, ",
          "use `mutate.reactibble` to preserve reactive columns")
  as.data.frame(materialize(`_data`))
}


#' @export
within.reactibble <- function (data, expr, ...) {
  warning(
    "Using `within` on a 'reactibble' object is discouraged and ",
    "potentially unsafe, use `mutate.reactibble` instead")
  cl <- class(data)
  x <- strip_reactibble_class(data)
  x <- eval.parent(substitute(within(x, expr, ...), environment()))
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  class(x) <- cl
  x
}

#' @export
with.reactibble <- function (data, expr, ...) {
  # this just makes sure the output is not a reactive column
  warning(
    "Using `with` on a 'reactibble' object is discouraged and potentially ",
    "unsafe, use `mutate.reactibble` instead")
  data <- eval.parent(substitute(with.default(data, expr, ...), environment()))
  strip_reactive_col(data)
}


#' @export
`[.reactibble` <- function(x, ...){
  x <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
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
      attr(x[[i]], "reactible_col_def") <-
        do.call(substitute, c(list(attr(x[[i]], "reactible_col_def"), args)))
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

# This is necessary for binding functions
# binding is only allowed between reactive columns of the same formula and
# NAs (which are implicitly added when rowbinding)

#' @export
c.reactive_col <- function(...) {
  dots <- list(...)
  expr <- attr(..1, "reactible_col_def")
  all_na <- all(sapply(dots[-1], is.na))
  if(all_na) {
    res <- NA
    attr(res, "reactible_col_def") <- expr
    return(res)
  }
  all_rc <- all(sapply(dots[-1], inherits, "reactive_col"))
  if(!all_rc)
    stop("Tried to bind a `reactive_col` with an incompatible data type")
  unique_expr <- length(unique(lapply(dots, attr, "reactible_col_def"))) == 1
  if(!unique_expr)
    stop("Tried to bind `reactive_col` objects with incompatible column definitions")
  res <- NextMethod()
  attr(res, "reactible_col_def") <- expr
  res
}

# This is necessary so dplyr::bind_rows reconstruct the reactibble and refreshes
# it right

#' @export
dplyr_reconstruct.reactibble <- function (data, template)
{
  data   <- NextMethod()
  data[] <- Map(function(x,y) {
    attributes(x) <- y
    x
  }, data, lapply(template, attributes))
  data
}

#' @export
rbind.reactibble <- function(..., deparse.level = 1) {
  data <- rbind.data.frame(..., deparse.level = 1)
  # the main method does checks already so we do our checks
  dots <- list(...)
  rcs <- sapply(dots[[1]], inherits, "reactive_col")
  nms <- names(which(rcs))
  exprs1 <- sapply(.subset(dots[[1]], nms), attr, "reactible_col_def")
  for(input in dots[-1]) {
    exprs <- sapply(.subset(input, nms), attr, "reactible_col_def")
    if(!identical(exprs, exprs1))
      stop("Tried to bind a `reactive_col` to an incompatible object.")
  }
  refresh(data)
}
