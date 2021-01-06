.onLoad <- function(...) {
  registerS3method("mutate", "reactibble", mutate.reactibble, asNamespace("dplyr"))
  registerS3method("transmute", "reactibble", transmute.reactibble, asNamespace("dplyr"))
  registerS3method("as_tibble", "reactibble", as_tibble.reactibble, asNamespace("tibble"))
  if(requireNamespace("data.table")) {
  registerS3method(
    "as.data.table", "reactibble",
    as.data.table.reactibble, asNamespace("data.table"))
  }
  op <- options()
  op.reactibble <- list(
    reactibble.autorefresh = TRUE
  )
  toset <- !(names(op.reactibble) %in% names(op))
  if(any(toset)) options(op.reactibble[toset])
  invisible()
}
