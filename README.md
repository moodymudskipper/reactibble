
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reactibble

IN PROGRESS

Reactive columns for data frames\!

Original twitter thread here :
<https://twitter.com/antoine_fabri/status/1346421382981283840>

New thread:
<https://twitter.com/antoine_fabri/status/1346839934783643652>

## Installation

Install with:

``` r
remotes::install_github("moodymudskipper/reactibble")
```

## Example

`reactibble()` works like `tibble()` except we can define reactive
columns by using `~`:

``` r
library(reactibble)
rt <- reactibble(
  height = c(150, 172, 164),
  weight = c(7, 82, 68),
  bmi = ~ weight / height^2
)
rt
#> # A reactibble: 3 x 3
#>   height weight      bmi
#>    <dbl>  <dbl>   <~dbl>
#> 1    150      7 0.000311
#> 2    172     82 0.000311
#> 3    164     68 0.000311
```

Oops, height was in the wrong unit, we correct it, bmi is corrected as
well:

``` r
rt <- transform(rt, height = height/100)
rt
#> # A reactibble: 3 x 3
#>   height weight    bmi
#>    <dbl>  <dbl> <~dbl>
#> 1   1.5       7   3.11
#> 2   1.72     82   3.11
#> 3   1.64     68   3.11
```

It is robust to other ways to change values

``` r
rt[[1]] <- runif(3, 1.5, 2)
rt
#> # A reactibble: 3 x 3
#>   height weight    bmi
#>    <dbl>  <dbl> <~dbl>
#> 1   1.63      7   2.63
#> 2   1.69     82   2.63
#> 3   1.79     68   2.63

rt[1] <- runif(3, 1.5, 2)
rt
#> # A reactibble: 3 x 3
#>   height weight    bmi
#>    <dbl>  <dbl> <~dbl>
#> 1   1.95      7   1.83
#> 2   1.60     82   1.83
#> 3   1.95     68   1.83

rt$height <- runif(3, 1.5, 2)
rt
#> # A reactibble: 3 x 3
#>   height weight    bmi
#>    <dbl>  <dbl> <~dbl>
#> 1   1.97      7   1.80
#> 2   1.83     82   1.80
#> 3   1.81     68   1.80

rt <- within(rt, height <- runif(3, 1.5, 2))
rt
#> # A reactibble: 3 x 3
#>   height weight    bmi
#>    <dbl>  <dbl> <~dbl>
#> 1   1.53      7   2.99
#> 2   1.60     82   2.99
#> 3   1.59     68   2.99
```

We can add other reactive columns by using `mutate` with `~`, we’ll need
to attach *{dplyr}* :

``` r
library(dplyr, warn.conflicts = FALSE)
mutate(
  rt, 
  height_cm = ~ 100 * height, 
  height = runif(3, 1.5, 2)
  )
#> # A reactibble: 3 x 4
#>   height weight    bmi height_cm
#>    <dbl>  <dbl> <~dbl>    <~dbl>
#> 1   1.84      7   2.06      184.
#> 2   1.69     82   2.06      184.
#> 3   1.88     68   2.06      184.
```

## Notes

  - If for some reason a reactibble is out of sync, call `rt <-
    refresh(rt)` (and post an issue :)).
  - If you want to disable autorefresh, maybe because your columns are
    expensive to recompute, set `options(reactibble.autorefresh =
    FALSE)`.
  - At the moment (and maybe forever), only `mutate` can create new
    reactive columns, so you can’t do things like `rt$var <- ~ expr`.
  - Dropping a column used by a formula triggers an explicit error
  - Renaming a column updates the formulas so they use the new name
