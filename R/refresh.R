#' Refresh dynamic columns manually
#'
#' @param x object
#' @export
refresh <- function(x) {
  UseMethod("refresh", x)
}

#' @export
refresh.data.frame <- function(x) {
  call <- sys.call()
  pf <- parent.frame()
  unrefreshed <- sapply(x, inherits, "reactive_col")
  unrefreshed_vars <- lapply(x[unrefreshed], function(x) {
    all.vars(attr(x,"reactibble_expr"))
  })
  while(any(unrefreshed)) {
    unrefreshed_bkp <- unrefreshed
    for(var in names(unrefreshed_vars)) {
      dependencies <-unrefreshed_vars[[var]]
      if(!any(na.omit(unrefreshed[dependencies]))){
        cl <- class(x[[var]])
        expr <- attr(x[[var]],"reactibble_expr")
        x[[var]] <- tryCatch(eval(expr, x, pf), error = function(e) {
          missing_vars <- setdiff(all.vars(expr), names(x))
          msg <- paste0(
            e$message,
            "\nDid you drop a necessary variable or provide an incorrect expression?")
          e$message <- msg
          e$call <- call
          stop(e)
        })
        class(x[[var]]) <- union("reactive_col", attr(x[[var]], "class"))
        attr(x[[var]],"reactibble_expr") <- expr
        unrefreshed[var] <- FALSE
        unrefreshed_vars[var] <- NULL
      }
    }
    if(identical(unrefreshed, unrefreshed_bkp)) {
      stop("The definition of reactive columns is circular")
    }
  }
  x
}

#' @export
refresh.reactibble <- function(x) {
  cl <- class(x)
  x <- refresh(strip_reactibble_class(x))
  class(x) <- cl
  x
}

