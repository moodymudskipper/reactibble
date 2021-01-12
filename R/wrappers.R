
#' @export
`[[<-.reactibble` <- function(x, name, value) {
  x <- strip_reactibble_class(x)
  x[[name]] <- value
  x <- refresh_if_relevant(x)
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
  x <- refresh_if_relevant(x)
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
  x <- refresh_if_relevant(x)
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
  refresh_if_relevant(x)
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
#' @param template template
#' @param data data
#' @rdname dplyr_methods
dplyr_reconstruct.reactibble <- function (data, template) {
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
  refresh_if_relevant(data)
}

#' @export
rbind.reactibble <- function(..., deparse.level = 1) {
  warning(
    "Using `rbind()` on a 'reactibble' object is discouraged and ",
    "potentially unsafe, use `rt_bind_rows` instead")
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
  refresh_if_relevant(data)
}

#' @export
cbind.reactibble <- function(..., deparse.level = 1) {
  warning(
    "Using `cbind()` on a 'reactibble' object is discouraged and ",
    "potentially unsafe, use `rt_bind_cols` instead")
  data <- cbind.data.frame(..., deparse.level = 1)
  data <- as_reactibble(data)
  refresh_if_relevant(data)
}


#' @export
#' @inheritParams dplyr::slice
#' @rdname dplyr_methods
slice.reactibble <- function(.data, ..., .preserve = FALSE) {
  cl <- class(.data)
  attrs <- lapply(.data, attr, "reactibble_col_def")
  .data <- dplyr::slice(tibble::as_tibble(.data), ..., .preserve = TRUE)
  .data[] <- Map(function(x, y) {
    attr(x, "reactibble_col_def") <- y
    x
  }, .data, attrs)
  class(.data) <- cl
  refresh_if_relevant(.data)
}

#' Efficiently bind multiple data frames by row and column
#'
#'  Counterpart of `dplyr::bind_rows` that works efficiently on *"reactibble"*
#'  objects. While `bind_rows()` can be used on "reactibbles" (at time of writing),
#'  it is brittle and inefficient, as it triggers more refreshes than necessary.
#'
#' @inheritParams dplyr::bind_rows
#'
#' @export
rt_bind_rows <- function(..., .id = NULL) {
  dots <- lapply(list(...), tibble::as_tibble)
  data <- dplyr::bind_rows(!!!dots, .id = .id)
  data <- as_reactibble(data)
  refresh_if_relevant(data)
}

#' Efficiently bind multiple data frames by row and column
#'
#'  Counterpart of `dplyr::bind_cols` that works efficiently on *"reactibble"*
#'  objects. `bind_cols()` will fail "reactibbles" so this new function was
#'  required..
#'
#' @inheritParams dplyr::bind_cols
#'
#' @export
rt_bind_cols <- function(..., .id = NULL) {
  dots <- lapply(list(...), tibble::as_tibble)
  data <- dplyr::bind_cols(!!!dots, .id = .id)
  data <- as_reactibble(data)
  refresh_if_relevant(data)
}

#' Add rows to a reactibble
#'
#'  Counterpart of `tibble::add_row` that works efficiently on *"reactibble"*
#'  objects. Beware of using `add_row()` instead as it would return an out of sync `reactibble`
#'
#' @inheritParams tibble::add_row
#'
#' @export
rt_add_row <- function(.data, ..., .before = NULL, .after = NULL) {
  .data <- tibble::as_tibble(.data)
  .data <- tibble::add_row(.data, ..., .before = NULL, .after = NULL)
  .data <- as_reactibble(.data)
  refresh_if_relevant(.data)
}
