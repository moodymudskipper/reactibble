#' Refresh dynamic columns manually
#'
#' @param x object
#' @export
refresh <- function(x) {
  UseMethod("refresh", x)
}

#' @importFrom stats na.omit
#' @export
refresh.data.frame <- function(x) {
  if(!nrow(x)) return(x)
  # to avoid weird issue with bind_rows
  if(anyDuplicated(names(x))) return(x)


  call <- sys.call()
  pf <- parent.frame()
  unrefreshed <- sapply(x, inherits, "reactive_col")
  unrefreshed_vars <- lapply(x[unrefreshed], function(x) {
    all.vars(attr(x,"reactibble_col_def")$expr)
  })
  while(any(unrefreshed)) {
    unrefreshed_bkp <- unrefreshed
    for(var in names(unrefreshed_vars)) {
      dependencies <-unrefreshed_vars[[var]]
      if(!any(na.omit(unrefreshed[dependencies]))){
        col_def <- attr(x[[var]],"reactibble_col_def")
        x[[var]] <- tryCatch(
          eval(col_def$expr, x, col_def$env),
          error = function(e) {
            missing_vars <- setdiff(all.vars(col_def), names(x))
            msg <- paste0(
              e$message,
              "\nDid you drop a necessary variable or provide an incorrect expression?")
            e$message <- msg
            e$call <- call
            stop(e)
          })
        x[[var]] <- reactive_col(x[[var]], col_def)
        unrefreshed[var] <- FALSE
        unrefreshed_vars[var] <- NULL
      }
    }
    if(identical(unrefreshed, unrefreshed_bkp)) {
      stop("The definition of reactive columns is circular")
    }
  }
  if(getOption("reactibble.verbose.refresh"))
    message("refreshed 'reactibble' object")
  x
}

refresh_if_relevant <- function(data) {
if(getOption("reactibble.autorefresh")) refresh(data) else data
}

#' @export
refresh.reactibble <- function(x) {
  cl <- class(x)
  x <- refresh(strip_reactibble_class(x))
  class(x) <- cl
  x
}

