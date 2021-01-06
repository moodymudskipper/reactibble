
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
#> 1   1.59      7  2.758495
#> 2   1.69     82 28.853924
#> 3   1.68     68 24.124361

rt[1] <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.91      7  1.920707
#> 2   1.86     82 23.634968
#> 3   1.81     68 20.814870

rt$height <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.59      7  2.766004
#> 2   1.57     82 33.297369
#> 3   1.96     68 17.634890

rt <- within(rt, height <- runif(3, 1.5, 2))
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.98      7  1.777496
#> 2   1.95     82 21.555442
#> 3   1.92     68 18.490269
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
#> 1   1.87      7  2.006236 186.7919 
#> 2   1.57     82 33.148080 157.2815 
#> 3   1.74     68 22.482970 173.9112
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
