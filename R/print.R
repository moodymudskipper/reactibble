#' @export
print.reactibble <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  # tibble:::format.tbl calls trunc_mat which calls as.data.frame,
  # but our as.data.frame method removes reactive columns, so we add a class
  x_classed <- x
  class(x_classed) <- c("printable_reactibble", class(x))
  txt <- format(x_classed, ..., n = n, width = width, n_extra = n_extra)
  txt <- gsub("\033\\[3m\033\\[38;5;246m<~(.*?)>\033\\[39m\033\\[23m",
       "\033[31m<~\\1>\033[39m", txt)
  cli::cat_line(txt)
  invisible(x)
}

#' @export
as.data.frame.printable_reactibble <- as.data.frame.data.frame

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
  paste0("~", pillar::type_sum(unclass(x)[[1]]))
}

#' @inheritParams pillar::pillar_shaft
#' @export
#' @rdname methods
pillar_shaft.reactive_col <- function(x, ...) {
  # to use the format of the original class
  pillar::pillar_shaft(unclass(x)[[1]], ...)
}
