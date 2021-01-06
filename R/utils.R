
#' Convert to a reactibble object
#' @param x A data frame, list, matrix, or other object that could reasonably be coerced to a tibble.
#'
#' @export
is_reactibble <- function(x) {
  inherits(x, "reactibble")
}


#' Convert to a reactibble object
#' @param x A data frame, list, matrix, or other object that could reasonably be coerced to a tibble.
#'
#' @export
as_reactibble <- function(x) {
  if(is_reactibble(x)) return(x)
  x <- tibble::as_tibble(x)
  class(x) <- union("reactibble", class(x))
  x
}

#' Build a reactibble object
#' @param ... A set of name-value pairs, use `~` to define a reactive column
#'
#' @export
reactibble <- function(...) {
  # this should allow definition of reactive columns right away
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
  res <- dplyr::tibble(!!!dots)
  for(i in seq_along(inds)) {
    attr(res[[nms[i]]],"expr") <- exprs[[i]]
    # creating a class just for display is messy, should be handled by using
    # pillar and tibble in the print method
    class(res[[nms[i]]]) <-
      c(paste0("~",pillar::type_sum(res[[nms[i]]])),
        "reactive_col", class(res[[nms[i]]]))
  }
  if(getOption("reactibble.autorefresh")) {
    res <- refresh(res)
  }
  as_reactibble(res)
}

strip_reactibble_class <- function(x) {
  class(x) <- setdiff(class(x), "reactibble")
  x
}

strip_reactive_col <- function(x) {
  cl<- class(x)
  cl <- setdiff(cl, "reactive_col")
  # this is a hack, come back here once we have a proper printing method
  cl <- cl[!startsWith(cl, "~")]
  class(x) <- cl
  attr(x, "expr") <- NULL
  x
}

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
