
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
      attr(x[[i]], "reactibble_col_def") <-
        do.call(substitute, c(list(attr(x[[i]], "reactibble_col_def"), args)))
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

# This is necessary so dplyr::bind_rows reconstruct the reactibble and refreshes
# it right

#' @export
dplyr_reconstruct.reactibble <- function (data, template) {
  # insert here a warning and suggest to use `rt_bind_rows` for more efficiency and robustness

  # hack to retrieve attributes from all tables, might break if dplyr's code changes
  dots <- get("dots", parent.frame(2))
  reactive_col_attrs <- unlist(lapply(dots, function(x) {
    lapply(x, attr, "reactibble_col_def")
  }), FALSE)

  reactive_col_attrs <- reactive_col_attrs[!duplicated(names(reactive_col_attrs))]
  nms <- names(reactive_col_attrs)
  data[] <- Map(function(x, y) {
    attr(x, "reactibble_col_def") <- y
    x
    }, data, reactive_col_attrs)
  class(data) <- class(template)
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
rbind.reactibble <- function(..., deparse.level = 1) {
  data <- rbind.data.frame(..., deparse.level = 1)
  # the main method does checks already so we do our checks
  dots <- list(...)
  rcs <- sapply(dots[[1]], inherits, "reactive_col")
  nms <- names(which(rcs))
  exprs1 <- sapply(.subset(dots[[1]], nms), attr, "reactibble_col_def")
  for(input in dots[-1]) {
    exprs <- sapply(.subset(input, nms), attr, "reactibble_col_def")
    if(!identical(exprs, exprs1))
      stop("Tried to bind a `reactive_col` to an incompatible object.")
  }
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
slice.reactibble <- function(.data, ..., .preserve = FALSE) {
  cl <- class(.data)
  attrs <- lapply(.data, attr, "reactibble_col_def")
  .data <- dplyr::slice(tibble::as_tibble(.data), ..., .preserve = TRUE)
  .data[] <- Map(function(x, y) {
    attr(x, "reactibble_col_def") <- y
    x
  }, .data, attrs)
  class(.data) <- cl
  if(getOption("reactibble.autorefresh")) {
    .data <- refresh(.data)
  }
  .data
}
