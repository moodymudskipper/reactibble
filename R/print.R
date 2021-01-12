#' @export
print.reactibble <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  txt <- format(as_reactibble0(x), ..., n = n, width = width, n_extra = n_extra)
  cli::cat_line(txt)
  invisible(x)
}

# we define a class reactibble0 so we can print without calling [.reactibble,
# which is costly as it triggers a refresh

as_reactibble0 <- function(x) {
  class(x) <- c("reactibble0", class(x))
  x
}

# #' @export
# as.data.frame.reactibble0 <- function(x) {
#   class(x) <- "data.frame"
#   x
# }

#' @export
`[.reactibble0` <-  function(x, ...) {
  class(x) <- setdiff(class(x), c("reactibble0", "reactibble"))
  x <- x[...]
  class(x) <- c("reactibble0", "reactibble", class(x))
  x
}

#' tibble methods
#' @inheritParams tibble::tbl_sum
#' @export
#' @name tibble_methods
tbl_sum.reactibble <- function (x){
  f <- getOption("reactibble.highlight") %||% c
  setNames(paste(nrow(x), "x", ncol(x)), paste("A", f("reactibble")))
}

#' pillar methods
#' @inheritParams pillar::pillar_shaft
#' @export
#' @name pillar_methods
pillar_shaft.reactive_col <- function(x, ...) {
  printing_tibble <-
    list(quote(print.tbl(x))) %in% as.list(sys.calls())
  if(printing_tibble && getOption("reactibble.autorefresh")) {
    placeholder <- "unsynced!!!"
    pillar::new_pillar_shaft(
      rep_len(placeholder, length(x)),
      class = "pillar_shaft_unsynced",
      align = "left", na_indent = 5, width = nchar(placeholder))
  } else {
  # create the pillar from the original class
  shaft <- pillar::pillar_shaft(strip_reactive_col(x), ...)
  # add a class so it can be forwarded to the right format method
  class(shaft) <- c("pillar_shaft_reactive_col", class(shaft))
  shaft
  }
}

#' @export
format.pillar_shaft_reactive_col<- function(x, ...) {
  f <- getOption("reactibble.highlight") %||% c
  # apply format method for original class
  fmt<- NextMethod()
  # add color to output, preserving the attributes by using []<-
  if(!is.null(getOption("reactibble.highlight"))) {
    # deal with lists, which are already colored by crayon
    fmt[] <- gsub("\033\\[90m(.*?)\033\\[39m", "\\1", fmt)
    fmt[] <- f(fmt)
  }
  fmt
}

#' @export
format.pillar_shaft_unsynced <- function(x, ...) {
  format(crayon::red(x))
}
