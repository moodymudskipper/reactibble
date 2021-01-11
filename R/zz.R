.onLoad <- function(...) {
  registerS3method("mutate", "reactibble", mutate.reactibble, asNamespace("dplyr"))
  registerS3method("transmute", "reactibble", transmute.reactibble, asNamespace("dplyr"))
  registerS3method("left_join", "reactibble", left_join.reactibble, asNamespace("dplyr"))
  registerS3method("right_join", "reactibble", right_join.reactibble, asNamespace("dplyr"))
  registerS3method("inner_join", "reactibble", inner_join.reactibble, asNamespace("dplyr"))
  registerS3method("full_join", "reactibble", full_join.reactibble, asNamespace("dplyr"))
  registerS3method("anti_join", "reactibble", anti_join.reactibble, asNamespace("dplyr"))
  registerS3method("semi_join", "reactibble", semi_join.reactibble, asNamespace("dplyr"))
  registerS3method("nest_join", "reactibble", nest_join.reactibble, asNamespace("dplyr"))
  registerS3method("dplyr_reconstruct", "reactibble", dplyr_reconstruct.reactibble, asNamespace("dplyr"))
  #registerS3method("as_tibble", "reactibble", as_tibble.reactibble, asNamespace("tibble"))
  registerS3method("vec_ptype_abbr", "reactive_col", vec_ptype_abbr.reactive_col, asNamespace("vctrs"))
  registerS3method("pillar_shaft", "reactive_col", pillar_shaft.reactive_col, asNamespace("pillar"))
  registerS3method("tbl_sum", "reactibble", tbl_sum.reactibble, asNamespace("tibble"))


  #registerS3method("vec_arith", "default", vec_arith.default, asNamespace("vctrs"))


  # setHook(packageEvent("data.table", "onLoad"), function(...) {
  #         registerS3method(
  #           "as.data.table", "reactibble",
  #           as.data.table.reactibble, asNamespace("data.table"))})

  op <- options()
  op.reactibble <- list(
    reactibble.autorefresh = TRUE,
    reactibble.highlight = crayon::cyan, # set to NULL to disable
    reactibble.verbose.refresh = FALSE
  )
  toset <- !(names(op.reactibble) %in% names(op))
  if(any(toset)) options(op.reactibble[toset])
  invisible()
}
