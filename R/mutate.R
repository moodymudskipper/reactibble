#' modify a reactibble object
#'
#' These work exactly like dplyr's mutate and transmute, except that one can
#' define reactive columns using `~`.
#' @export
#' @inheritParams dplyr::mutate
#' @rdname mutate.reactibble
mutate.reactibble <- function(.data, ...) {
  dots <- rlang::enquos(...)
  # I don't know all the right rlang utilities so I just take my big knife sorry
  has_tilde_lgl <- sapply(dots, function(x) {
    expr <- rlang::quo_get_expr(x)
    is.call(expr) && identical(expr[[1]], quote(`~`))
  })
  inds <- which(has_tilde_lgl)
  nms <- names(dots)[inds]
  exprs <- vector("list", length(inds))
  for(i in seq_along(inds)) {
    env <- attr(dots[[inds[i]]], ".Environment")
    exprs[[i]] <- rlang::quo_get_expr(dots[[inds[i]]])[[2]]
    dots[[inds[i]]] <- rlang::as_quosure(exprs[[i]], env = env)
  }
  res <- dplyr::mutate(strip_reactibble_class(.data), !!!dots)
  for(i in seq_along(inds)) {
    class(res[[nms[i]]]) <- union("reactive_col", class(res[[nms[i]]]))
    attr(res[[nms[i]]],"expr") <- exprs[[i]]
  }
  if(getOption("reactibble.autorefresh")) {
    res <- refresh(res)
  }
  as_reactibble(res)
}

#' @export
#' @rdname mutate.reactibble
transmute.reactibble <- function(.data, ...) {
  dots <- rlang::enquos(...)
  # I don't know all the right rlang utilities so I just take my big knife sorry
  has_tilde_lgl <- sapply(dots, function(x) {
    expr <- rlang::quo_get_expr(x)
    is.call(expr) && identical(expr[[1]], quote(`~`))
  })
  inds <- which(has_tilde_lgl)
  nms <- names(dots)[inds]
  exprs <- vector("list", length(inds))
  for(i in seq_along(inds)) {
    env <- attr(dots[[inds[i]]], ".Environment")
    exprs[[i]] <- rlang::quo_get_expr(dots[[inds[i]]])[[2]]
    dots[[inds[i]]] <- rlang::as_quosure(exprs[[i]], env = env)
  }
  res <- dplyr::transmute(strip_reactibble_class(.data), !!!dots)
  for(i in seq_along(inds)) {
    class(res[[nms[i]]]) <- union("reactive_col", class(res[[nms[i]]]))
    attr(res[[nms[i]]],"expr") <- exprs[[i]]
  }
  if(getOption("reactibble.autorefresh")) {
    res <- refresh(res)
  }
  as_reactibble(res)
}
