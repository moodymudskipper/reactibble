
#' @import vctrs
NULL


new_reactive_col <- function(
  x = logical(),
  reactibble_col_def = list(expr = quote(expr=), env = .GlobalEnv)) {
  new_vctr(x, reactibble_col_def = reactibble_col_def, class = "reactive_col")
}

# for reactive cols we don't need to cast the input
reactive_col <- function(
  x = logical(),
  reactibble_col_def = list(expr = quote(expr=), env = .GlobalEnv)) {
  new_reactive_col(x, reactibble_col_def)
}

# our format method fetches the data and forwards it to its format method

#' @export
format.reactive_col <- function(x, ...) {
  out <- format(vec_data(x))
  out[is.na(x)] <- NA
  out
}

#' @export
vec_ptype_abbr.reactive_col <- function(x, ...) {
  # the vec_ptype_abbr depends on the input
  paste0("~", vec_ptype_abbr(vec_data(x)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# vec_cast methods

#' @export
vec_ptype2.reactive_col <- function(x, y, ...) new_reactive_col()

#' @export
vec_ptype2.double.reactive_col <- function(x, y, ...) double()

#' @export
vec_cast.reactive_col.double <- function(x, to, ...) x

#' @export
vec_cast.double.reactive_col <- function(x, to, ...) vec_data(x)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# vec_math methods

#' @export
vec_math.reactive_col <- function(.fn, .x, ...) {
  vec_math_base(.fn, vec_data(.x), ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# proxy methods

# I give up
Ops.reactive_col <- function(e1, e2 = NULL) {
  if(is_reactive_col(e1)) e1 <- vec_data(e1)
  if(is_reactive_col(e2)) e2 <- vec_data(e2)
  FUN <- get(.Generic, envir = parent.frame(), mode = "function")
  FUN(e1,e2)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# arith methods

#' @export
vec_arith.reactive_col <- function(op, x, y, ...) {
  if(is_reactive_col(y))
    vec_arith_base(op, vec_data(x), vec_data(y))
  else
    vec_arith_base(op, vec_data(x), y)
}


#' @export
vec_arith.difftime.reactive_col <- function(op, x, y, ...) {
  vec_arith_base(op, vec_data(x), vec_data(y))
}

#' @export
vec_arith.logical.reactive_col <- function(op, x, y, ...) {
  vec_arith_base(op, vec_data(x), vec_data(y))
}

#' @export
vec_arith.numeric.reactive_col <- function(op, x, y, ...) {
  vec_arith_base(op, vec_data(x), vec_data(y))
}

#' @export
vec_arith.POSIXct.reactive_col <- function(op, x, y, ...) {
  vec_arith_base(op, vec_data(x), vec_data(y))
}

#' @export
vec_arith.POSIXlt.reactive_col <- function(op, x, y, ...) {
  vec_arith_base(op, vec_data(x), vec_data(y))
}

#' @export
vec_arith.Date.reactive_col <- function(op, x, y, ...) {
  vec_arith_base(op, vec_data(x), vec_data(y))
}
