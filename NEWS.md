# reactibble 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package
* all common ways to mutate columns are supported
* dropping/selecting columns triggers a refresh and fails explicitly if needed columns were dropped
* renaming columns change the formulas so they use the new names
* implemented is_reactibble, and materialize
* implemented a proper printing method
* when using `[[` or `$` on a reactibble, we fetch the static value
* all columns created by mutate and transmute are forced to be static when `~` is
  not used, this means copying a column without using `~` creates a static copy
* Refreshing occurs in the correct order
* Dynamic columns now print in cyan, or in a custom color
* reactibble object can contain list columns
* `transform.reactibble` triggers a warnings and returns a static data.frame
* `within.reactibble` warns that it might be unsafe
* `with.reactibble` is defined, only to make sure we don't return a "reactive_col" object
* implement methods for join functions
* Optimized print so it doesn't refresh
* When printing a tibble and the "reactibble.autorefresh" option is `TRUE` mark
  columns as "unsynced" colored in red.
  
  
