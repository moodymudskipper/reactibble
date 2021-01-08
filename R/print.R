# #' @export
# print.reactibble <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
#   txt <- format(x, ..., n = n, width = width, n_extra = n_extra)
#   cli::cat_line(txt)
#   invisible(x)
# }

#' @inheritParams tibble::tbl_sum
#' @export
#' @rdname methods
tbl_sum.reactibble <- function (x){
  c(`A reactibble` = paste(nrow(x), "x", ncol(x)))
}

#' methods
#'
#' @inheritParams vctrs::vec_ptype_abbr
#' @export
#' @rdname methods
vec_ptype_abbr.reactive_col <- function(x) {
  paste0("~", pillar::type_sum(strip_reactive_col(x)))
}

#' @inheritParams pillar::pillar_shaft
#' @export
#' @rdname methods
pillar_shaft.reactive_col <- function(x, ...) {
  # create the pillar from the original class
  shaft <- pillar::pillar_shaft(strip_reactive_col(x), ...)
  # add a class so it can be forwarded to the right format method
  class(shaft) <- c("pillar_shaft_reactive_col", class(shaft))
  shaft
}

#' @inheritParams base::format
#' @export
#' @rdname methods
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
