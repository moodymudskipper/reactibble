
#' @export
merge.reactibble <- function (x, y, ...) {
  warning("`merge` when called on 'reactibble' objects return a static data frame, ",
          "use dplyr join functions to preserve reactive columns")
  merge(
    as.data.frame(materialize(x)),
    as.data.frame(materialize(y)),
    ...)
}

check_join_by_arg <- function(x, y, by) {
  nms.x <- names(x)
  nms.y <- names(y)
  if(is.null(by)) {
    by.x <- by.y <- intersect(nms.x, nms.y)
  } else if(is.null(names(by))) {
    by.x <- by.y <- by
  } else {
    by.x <- unname(by)
    by.y <- names(by)
  }

  wrong_by.x <- intersect(by.x, nms.x[sapply(x, inherits, "reactive_col")])
  if(length(wrong_by.x)) {
    stop(sprintf("In `x`, attempt to join by reactive column(s): %s",
                 toString(paste0("`", wrong_by.x, "`"))))
  }
  wrong_by.y <- intersect(by.y, nms.y[sapply(x, inherits, "reactive_col")])
  if(length(wrong_by.y)) {
    stop(sprintf("In `y`, attempt to join by reactive column(s): %s",
                 toString(paste0("`", wrong_by.y, "`"))))
  }
}

#' @export
left_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, suffix = c(".x",  ".y"), ..., keep = FALSE) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
right_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, suffix = c(".x",  ".y"), ..., keep = FALSE) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
inner_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, suffix = c(".x",  ".y"), ..., keep = FALSE) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
full_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, suffix = c(".x",  ".y"), ..., keep = FALSE) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
anti_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, ...) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
semi_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, ...) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

#' @export
nest_join.reactibble <- function (
  x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  check_join_by_arg(x, y, by)
  data <- NextMethod()
  if(getOption("reactibble.autorefresh")) {
    data <- refresh(data)
  }
  data
}

