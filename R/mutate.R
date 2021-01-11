process_reactive_dots <- function(...) {
  dots <- rlang::enquos(...)
  lapply(dots, function(x) {
    expr <- rlang::quo_get_expr(x)
    expr_is_reactive <- is.call(expr) && identical(expr[[1]], quote(`~`))
    if(expr_is_reactive) {
      x <- reactive_col(NA, list(
        expr = expr[[2]],
        env = attr(x, ".Environment")
        ))
    }
    x
  })
}

process_reactive_output <- function(x, dots) {
  # keep last definition of all modified/created vars
  dots <- dots[!duplicated(names(dots), fromLast = TRUE)]
  nms <- names(dots)
  # for those remove class "reactive_col" if it wasn't defined as a reactive col
  # (so copying a reactive_col without using "~" creates a static copy)
  # if it was defined as such, add the class
  x[nms] <- lapply(nms, function(nm) {
    expr <- attr(dots[[nm]], "reactibble_col_def")
    col <- .subset2(x, nm)
    if (is.null(expr)) {
      if (is_reactive_col(col)) col <- vec_data(col)
    } else {
      col <- reactive_col(col, expr)
    }
    col
  })

  refresh_if_relevant(x)
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
