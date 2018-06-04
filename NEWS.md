# pinr 0.0.0.9006

* Added a `NEWS.md` file to track changes to the package.

## New features

* Added new function `pin_extract()` for extracting the date of birth and
sex from a PIN column in a data frame into new columns.

## Breaking changes

* The `drop_pin` argument in `pseudonymize()` has been renamed to `remove`. This
is more consistent with names of arguments with similar functionality in other
packages (such as `separate` and `unite` in **tidyr**).

* Removed the `try_fix` option in `pin_dob()`. In the future, this
feature will be implemented more comprehensively in a dedicated function (#12).

## Bug fixes

* When removing pin columns, now nothing gets suffixed and manual renames are
applied. When retaining pin columns, pseudonymized columns that are not manually
renamed gain the  `pid_suffix` (#1).
