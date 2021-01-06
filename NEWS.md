# reactibble 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
* all common ways to mutate columns are supported
* dropping/selecting columns triggers a refresh and fails explicitly if needed columns were dropped
* renaming columns change the formulas so they use the new names
* implemented methods for as.data.frame, as_tibble and as.data.table, 
  which convert the reactibble to a static object and clean up the classes and
  attributes of reactive columns
