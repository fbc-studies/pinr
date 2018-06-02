# pinr 0.0.0.9006

* Added a `NEWS.md` file to track changes to the package.

## Breaking changes

* The `drop_pin` argument in `pseudonymize()` has been renamed to `remove`. This
is more consistent with names of arguments with similar functionality in other
packages (such as `separate` and `unite` in **tidyr**).

* When removing pin columns, now nothing gets suffixed and manual renames are
applied. When retaining pin columns, pseudonymized columns that are not manually renamed gain the  `pid_suffix`.
