process_reactive_dots <- function(...) {
  dots <- rlang::enquos(...)
  lapply(dots, function(x) {
    expr <- rlang::quo_get_expr(x)
    expr_is_reactive <- is.call(expr) && identical(expr[[1]], quote(`~`))
    if(expr_is_reactive) {
      env <- attr(x, ".Environment")
      column_definition <- rlang::as_quosure(expr[[2]], attr(x, ".Environment"))
      x <- NA
      attr(x, "reactibble_expr") <- column_definition
    }
    x
  })
}

process_reactive_output <- function(x, dots) {
  dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  nms <- names(dots)
  x[nms] <- lapply(nms, function(nm) {
    expr <- attr(dots[[nm]], "reactibble_expr")
    col <- .subset2(x, nm)
    if (is.null(expr)) {
      col <- strip_reactive_col(col)
    } else {
      col <- as_reactive_col(col, expr)
    }
    col
  })

  if(getOption("reactibble.autorefresh")) {
    x <- refresh(x)
  }
  x
}

#' modify a reactibble object
#'
#' These work exactly like dplyr's mutate and transmute, except that one can
#' define reactive columns using `~`.
#' @export
#' @inheritParams dplyr::mutate
#' @rdname mutate.reactibble
mutate.reactibble <- function(.data, ...) {
  cl <- class(.data)
  dots  <- process_reactive_dots(...)
  .data <- dplyr::mutate(strip_reactibble_class(.data), !!!dots)
  .data <- process_reactive_output(.data, dots)
  class(.data) <- cl
  .data
}

#' @export
#' @rdname mutate.reactibble
transmute.reactibble <- function(.data, ...) {
  cl <- class(.data)
  dots  <- process_reactive_dots(...)
  .data <- dplyr::transmute(strip_reactibble_class(.data), !!!dots)
  .data <- process_reactive_output(.data, dots)
  class(.data) <- cl
  .data
}
