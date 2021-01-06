
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
#> 1   1.85      7  2.054159
#> 2   1.71     82 27.910637
#> 3   1.98     68 17.296518

rt[1] <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.68      7  2.487698
#> 2   1.70     82 28.444086
#> 3   1.95     68 17.954131

rt$height <- runif(3, 1.5, 2)
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.90      7  1.945893
#> 2   1.84     82 24.268744
#> 3   1.61     68 26.212256

rt <- within(rt, height <- runif(3, 1.5, 2))
rt
#> # A tibble: 3 x 3
#>   height weight bmi      
#>    <dbl>  <dbl> <~dbl>   
#> 1   1.58      7  2.816554
#> 2   1.59     82 32.373374
#> 3   1.66     68 24.582466
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
#>   height weight bmi      height_cm
#>    <dbl>  <dbl> <~dbl>   <~dbl>   
#> 1   1.52      7  3.01123 152.4674 
#> 2   1.75     82 26.73476 175.1333 
#> 3   1.73     68 22.68869 173.1210
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
