refresh <- function(x) {
  #message("refreshing")
  call <- sys.call()
  pf <- parent.frame()
  for(i in seq_along(x)) {
    if(inherits(x[[i]], "reactive_col")) {
      cl <- class(x[[i]])
      expr <- attr(x[[i]],"reactibble_expr")
      x[[i]] <- tryCatch(eval(expr, x, pf), error = function(e) {
        missing_vars <- setdiff(all.vars(expr), names(x))
        msg <- sprintf(
          "Attempt to drop variables required by `%s`: %s",
          names(x)[[i]], toString(paste0("`", missing_vars, "`")))
        e$message <- msg
        e$call <- call
        stop(e)
      })
      class(x[[i]]) <- union("reactive_col", class(x[[i]]))
      attr(x[[i]],"reactibble_expr") <- expr
    }
  }
  x
}
