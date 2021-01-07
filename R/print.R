#' @export
print.reactibble <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  txt <- format(x, ..., n = n, width = width, n_extra = n_extra)
  # txt <- gsub("\033\\[3m\033\\[38;5;246m<~(.*?)>\033\\[39m\033\\[23m",
  #      "\033[31m<~\\1>\033[39m", txt)
  txt <- gsub("\033\\[3m\033\\[90m<~(.*?)>\033\\[39m\033\\[23m",
              "\033[31m<~\\1>\033[39m", txt)
  cli::cat_line(txt)
  invisible(x)
}

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
  # to use the format of the original class
  pillar::pillar_shaft(strip_reactive_col(x), ...)
}
