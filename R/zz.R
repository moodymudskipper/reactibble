.onLoad <- function(...) {
  registerS3method("mutate", "reactibble", mutate.reactibble, asNamespace("dplyr"))
  op <- options()
  op.reactibble <- list(
    reactibble.autorefresh = TRUE
  )
  toset <- !(names(op.reactibble) %in% names(op))
  if(any(toset)) options(op.reactibble[toset])
  invisible()
}
