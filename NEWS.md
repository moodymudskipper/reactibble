# reactibble 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package
* all common ways to mutate columns are supported
* dropping/selecting columns triggers a refresh and fails explicitly if needed columns were dropped
* renaming columns change the formulas so they use the new names
* implemented is_reactibble, and materialize
* implemented a proper printing method
* when using `[[` or `$` on a reactibble, we fetch the static value
