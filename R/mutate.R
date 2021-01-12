process_reactive_dots <- function(...) {
  dots <- rlang::enquos(...)
  lapply(dots, function(x) {
    expr <- rlang::quo_get_expr(x)
    expr_is_reactive <- is.call(expr) && identical(expr[[1]], quote(`~`))
    if(expr_is_reactive) {
      env <- attr(x, ".Environment")
      # check if we find a call to M
      expr_uses_M <- "M" %in% setdiff(all.names(expr), all.vars(expr))
      if(expr_uses_M) {
        env <- setup_memoise_env(expr, env)
      }
      x <- reactive_col(NA, list(
        expr = expr[[2]],
        env = env
        ))
    }
    x
  })
}

setup_memoise_env <- function(expr, env) {
  # for notes
  ..memoised_funs.. <- NULL
  M_inputs <- list()
  recurse <- function(expr) {
    if(!is.call(expr))
      return(invisible(NULL))
    if(identical(expr[[1]], quote(M)))
      M_inputs <<- c(M_inputs, expr[[2]])
    else
      lapply(expr, recurse)
    invisible(NULL)
  }
  lapply(expr[[2]], recurse)
  # wrap in memoise
  M_inputs_m <- lapply(M_inputs, function(x) as.call(c(quote(memoise::memoise), x)))
  memoised_funs <- lapply(M_inputs_m, eval, env)
  names(memoised_funs) <- sapply(M_inputs, deparse1)
  # reinitiate our environment as a chiled of the quosure env
  env <- new.env(parent = env)
  # store memoised functions and M
  env$..memoised_funs.. <- memoised_funs
  env$M <- function(x) ..memoised_funs..[[deparse1(substitute(x))]]
  environment(env$M) <- env
  env
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
