# pinr 0.1.0.9000


# pinr 0.1.0

* New `pin()` function for creating new PINs from date and order of birth.

* New `pin_rescue()` function: attempts to fix common problems in PINs.

* New `pin_ordinal()` function: extract the ordinal number from a PIN.

* `pseudonymize()` now warns if some pseudonymized columns contain only `NA`
  values. Use `quiet = TRUE` to suppress these warnings.


# pinr 0.0.0.9007

* `is_probably_pin()` gains a `data.frame` method. Use to check if a dataframe
probably contains at least one PIN column (@sakasute).

* New `is_valid_pin()` function: check if elements of a vector are valid PINs.

* `pin_century()` is now exported: use it to get the century of birth based
on the separator character present in a PIN.

* `is_probably_pin()` is now a generic function, allowing custom methods
for different classes.

* When `replace = FALSE`, `pseudonymize()`'d columns are now insterted directly
after the corresponding PIN column, rather than appended to the end of the data.


# pinr 0.0.0.9006

* Added a `NEWS.md` file to track changes to the package.

## New features

* Added new function `pin_extract()` for extracting the date of birth and
sex from a PIN column in a data frame into new columns.

## Breaking changes

* The `drop_pin` argument in `pseudonymize()` has been renamed to `replace`.
This more descriptive name is also more consistent with names of arguments with
similar functionality in other packages.

* Removed the `try_fix` option in `pin_dob()`. In the future, this
feature will be implemented more comprehensively in a dedicated function (#12).

## Bug fixes

* When removing pin columns, now nothing gets suffixed and manual renames are
applied. When retaining pin columns, pseudonymized columns that are not manually
renamed gain the  `pid_suffix` (#1).
