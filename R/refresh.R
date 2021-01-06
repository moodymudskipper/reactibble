refresh <- function(x) {
  call <- sys.call()
  pf <- parent.frame()
  for(i in seq_along(x)) {
    if(inherits(x[[i]], "reactive_col")) {
      cl <- class(x[[i]])
      expr <- attr(x[[i]],"expr")
      x[[i]] <- tryCatch(eval(expr, x, pf), error = function(e) {
        missing_vars <- setdiff(all.vars(expr), names(x))
        msg <- sprintf(
          "Attempt to drop variables required by `%s`: `%s`",
          names(x)[[i]], toString(missing_vars))
        e$message <- msg
        e$call <- call
        stop(e)
      })
      attr(x[[i]],"expr") <- expr
      class(x[[i]]) <-
        unique(c(paste0("~", pillar::type_sum(x[[i]])),
          "reactive_col", class(x[[i]])))
    }
  }
  x
}
