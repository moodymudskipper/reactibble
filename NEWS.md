# reactibble 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package
* All common ways to mutate columns are supported
* Dropping/selecting columns triggers a refresh and fails explicitly if needed columns were dropped
* Renaming columns changes the formulas so they use the new names
* Implemented is_reactibble, and materialize
* Implemented a proper printing method
* When using `[[` or `$` on a reactibble, we fetch the static value
* All columns created by mutate and transmute are forced to be static when `~` is
  not used, this means copying a column without using `~` creates a static copy
* Refreshing occurs in the correct order
* Dynamic columns now print in cyan, or in a custom color
* Reactibble object can contain list columns
* `transform.reactibble` triggers a warnings and returns a static data.frame
* `within.reactibble` warns that it might be unsafe
* `with.reactibble` is defined, only to make sure we don't return a "reactive_col" object
* Implemented methods for {dplyr} join functions and base::merge
* Optimized print so it doesn't refresh
* When printing a tibble and the "reactibble.autorefresh" option is `TRUE` mark
  columns as "unsynced" colored in red.
* Optimize process_reactive_dots so reactive columns are coomputed only in the end
* Use quosures instead of expressions as column definitions
* Implemented temporary solution to rbind and bind_rows using vctrs
* Defined a method for `dplyr::slice`
* Added tests
* Implemented `rt_bind_rows`, `rt_bind_cols` and `rt_add_row` as robust and efficient counterparts
 of `dplyr::bind_rows`, `dplyr::bind_cols` and `tibble::add_row`, which we can unfortunately not use
 on reactibbles reliably.
* Implemented easy way to memoise, by using M() in the reactive col definition
