refresh <- function(x) {
  pf <- parent.frame()
  for(i in seq_along(x)) {
    if(inherits(x[[i]], "reactive_col")) {
      cl <- class(x[[i]])
      expr <- attr(x[[i]],"expr")
      x[[i]] <- eval(expr, x, pf)
      attr(x[[i]],"expr") <- expr
      class(x[[i]]) <-
        unique(c(paste0("~", pillar::type_sum(x[[i]])),
          "reactive_col", class(x[[i]])))
    }
  }
  x
}
