test_that("reactibbles have the intended structure", {
  env <- environment()
  rt <- reactibble(c = ~2*b, b = ~2*a, a=1)
  expect_equal(class(rt), c("reactibble", "tbl_df", "tbl", "data.frame"))
  expect_equal(
    lapply(rt, class),
    list(c = c("reactive_col", "vctrs_vctr"),
         b = c("reactive_col", "vctrs_vctr"),
         a = "numeric"))
  expect_equal(
    attr(.subset2(rt,1), "reactibble_col_def"),
    list(expr = quote(2 * b), env = env))

})

test_that("reactibbles print ok", {
  rt <- reactibble(a=1, b = ~2*a)
  expect_equal(
    capture.output(rt),
    c("# A reactibble: 1 x 2",
      "      a      b",
      "  <dbl> <~dbl>",
      "1     1      2")
  )
})

test_that("`[[` subsetting doesn't return a 'reactive_col'", {
  env <- environment()
  rt <- reactibble(a=1, b = ~2*a)
  expect_equal(rt[["a"]], 1)
  expect_equal(rt[["b"]], 2)
  expect_equal(rt$"a", 1)
  expect_equal(rt$"b", 2)
  expect_equal(dplyr::pull(rt,"a"), 1)
  expect_equal(dplyr::pull(rt,"b"), 2)

  # exception if .subset2 is used
  expect_equal(
    .subset2(rt,"b"),
    reactive_col(2, list(expr = quote(2*a), env = env)))
})

test_that("`[` subsetting works, and fails explicitly when needed", {
  rt <- reactibble(c = ~2*b, b = ~2*a, a=1:2)

  # works
  expect_equal(
    rt[2:3],
    reactibble(b = ~2*a, a=1:2))
  expect_equal(
    rt[,2:3],
    reactibble(b = ~2*a, a=1:2))
  expect_equal(
    rt[1,],
    reactibble(c = ~2*b, b = ~2*a, a=1))

  # same with subset
  expect_equal(
    subset(rt, TRUE, c("b", "a")),
    reactibble(b = ~2*a, a=1:2))
  expect_equal(
    subset(rt, c(TRUE, FALSE)),
    reactibble(c = ~2*b, b = ~2*a, a=1))

  # same with dplyr
  expect_equal(
    dplyr::select(rt, b, a),
    reactibble(b = ~2*a, a=1:2))
  expect_equal(
    dplyr::slice(rt, 1),
    reactibble(c = ~2*b, b = ~2*a, a=1))

  # fails as we remove columns needed by others
  expect_error(rt[-3])
  expect_error(rt[-2])
  expect_error(dlyr::select(rt, -a))
  expect_error(dlyr::select(rt, -b))
  expect_error(subset(rt, TRUE, -3))
  expect_error(subset(rt, TRUE, -2))
})



test_that("reactibble() computes and refreshes fine", {
  rt <- reactibble(c = ~2*b, b = ~2*a, a=1)
  expect_equal(rt[["a"]], 1)
  expect_equal(rt[["b"]], 2)
  expect_equal(rt[["c"]], 4)
})


test_that("disabling/enabling autorefresh works", {
  options(reactibble.autorefresh = FALSE)
  rt <- reactibble(c = ~2*b, b = ~2*a, a=1)
  # not refreshed, so contains NA
  expect_equal(rt[["a"]], 1)
  expect_equal(rt[["b"]], NA)
  expect_equal(rt[["c"]], NA)

  # refresh, so it's in sync
  rt <- refresh(rt)
  expect_equal(rt[["a"]], 1)
  expect_equal(rt[["b"]], 2)
  expect_equal(rt[["c"]], 4)

  # change `a`, the rest is out of sync
  rt[["a"]] <- 2
  expect_equal(rt[["a"]], 2)
  expect_equal(rt[["b"]], 2)
  expect_equal(rt[["c"]], 4)

  # refresh, so it's in sync
  rt <- refresh(rt)
  expect_equal(rt[["a"]], 2)
  expect_equal(rt[["b"]], 4)
  expect_equal(rt[["c"]], 8)

  options(reactibble.autorefresh = TRUE)

  # now chage will autorefresh
  rt[["a"]] <- 1
  expect_equal(rt[["a"]], 1)
  expect_equal(rt[["b"]], 2)
  expect_equal(rt[["c"]], 4)
})


test_that("mutate() and transmute() work", {
  rt  <-  reactibble(a=1, b= 2)
  rt2 <-  reactibble(a=1, b= 2, c=~2*a)

  expect_equal(rt2, dplyr::mutate(rt, c=~2*a))
  expect_equal(rt2[-2], dplyr::transmute(rt, a = b/2, c = ~2*a))

  # copying without using `~` makes a static copy
  expect_equal(reactibble(a=1, b= 2, c=~2*a, d = 2*a),
               dplyr::mutate(rt2, d = c))
})


test_that("Group generic operations work", {
  rt  <-  reactibble(a = c(3, -5), b = ~ a /2)
  # Arith
  expect_error(regexp = NA, dplyr::mutate(rt, c = b * 2))

  # Compare
  expect_error(regexp = NA, dplyr::mutate(rt, c = b > 0))

  # Logic
  expect_error(regexp = NA, dplyr::mutate(rt, c = b == 1.5))

  # Math
  expect_error(regexp = NA, dplyr::mutate(rt, c = abs(b)))

  # Math2
  expect_error(regexp = NA, dplyr::mutate(rt, c = round(b)))

  # Summary
  expect_error(regexp = NA, dplyr::mutate(rt, c = max(b)))

  # Complex
  expect_error(regexp = NA, dplyr::mutate(rt, c = Re(b)))
})
