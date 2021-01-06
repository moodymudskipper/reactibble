
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reactibble

IN PROGRESS

Reactive columns for data frames\!

Follow the twitter thread here :
<https://twitter.com/antoine_fabri/status/1346421382981283840>

## Installation

Install with:

``` r
remotes::("moodymudskipper/reactibble")
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
#> # A tibble: 3 x 3
#>   height weight bmi         
#>    <dbl>  <dbl> <~dbl>      
#> 1    150      7 0.0003111111
#> 2    172     82 0.0027717685
#> 3    164     68 0.0025282570
```

Oops, height was in the wrong unit, we correct it, bmi is corrected as
well:

``` r
rt <- transform(rt, height = height/100)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.5       7  3.111111
#> 2   1.72     82 27.717685
#> 3   1.64     68 25.282570
```

It is robust to other ways to change values

``` r
rt[[1]] <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.96      7  1.818747
#> 2   1.77     82 26.282112
#> 3   1.98     68 17.305198

rt[1] <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.78      7  2.208189
#> 2   1.87     82 23.531816
#> 3   1.87     68 19.520299

rt$height <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.69      7  2.458935
#> 2   1.92     82 22.155712
#> 3   1.88     68 19.301707

rt <- within(rt, height <- runif(3, 1.5, 2))
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.52      7  3.047258
#> 2   1.51     82 35.922420
#> 3   1.90     68 18.905873
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
#> # A tibble: 3 x 4
#>   height weight bmi       height_cm
#>    <dbl>  <dbl> <~dbl>    <~dbl>   
#> 1   1.65      7  2.576127 164.8410 
#> 2   1.77     82 26.282194 176.6347 
#> 3   1.81     68 20.852501 180.5824
```

## Notes

  - If for some reason a reactibble is out of sync, call `refresh(rt)`
    (and post an issue :)).
  - If you want to disable autorefresh, maybe because your columns are
    expensive to recompute, set `options(reactibble.autorefresh =
    FALSE)`.
  - At the moment (and maybe forever), only `mutate` can create new
    reactive columns, so you can’t do things like `rt$var <- ~ expr`.
  - At the moment if you rename columns used by reactive columns, you
    will break your reactibble. This will hopefully be solved.
