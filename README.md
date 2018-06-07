
<!-- README.md is generated from README.Rmd. Please edit that file -->
pinr
====

[![Travis build status](https://travis-ci.org/mikmart/pinr.svg?branch=master)](https://travis-ci.org/mikmart/pinr) [![Coverage status](https://codecov.io/gh/mikmart/pinr/branch/master/graph/badge.svg)](https://codecov.io/github/mikmart/pinr?branch=master)

The goal of **pinr** is to simplify working with data containing Finnish personal identity codes (PINs). You can:

-   Check the validity of or extract information from PINs.
-   Use a heuristic to identify data columns that potentially contain PINs.
-   `pseudonymize()` existing data with a key file without manual joins.

Installation
------------

Currently you can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mikmart/pinr")
```

Usage
-----

The primary (pipe-friendly) utility function automates pseudonymizing columns containing PINs in your data:

``` r
library(pinr)

df <- data.frame(pin = c("311280-888Y", "311280-888Y", "131052-308T"))
key <- data.frame(pin = c("311280-888Y", "131052-308T"), pid = c(1, 2))

pseudonymize(df, key, pid = pin)
#>   pid
#> 1   1
#> 2   1
#> 3   2
```

The result is equivalent to looking up the `pid` from the key data frame, but **pinr** also takes care of managing the columns and naming in your data.

``` r
key$pid[match(df$pin, key$pin)]
#> [1] 1 1 2
```

Rather than manually specifying columns containing PINs, you can also use a heuristic implemented in the `is_probably_pin()` function to `guess` which columns need to be pseudonymized:

``` r
pseudonymize(df, key, guess = TRUE, replace = FALSE)
#>           pin pin_pid
#> 1 311280-888Y       1
#> 2 311280-888Y       1
#> 3 131052-308T       2
```

**pinr** also includes helpers for extracting data contained in the Finnish PINs, such as the date of birth and sex:

``` r
pins <- c("311280-888Y", "131052-308T")

pin_dob(pins)
#> [1] "1980-12-31" "1952-10-13"

pin_sex(pins)
#> [1] Female Female
#> Levels: Male Female
```

There is also a `pin_extract()` wrapper for these extraction functions that makes it easy to extract these data into new columns in a data frame context:

``` r
pin_extract(df, pin)
#>           pin        dob    sex
#> 1 311280-888Y 1980-12-31 Female
#> 2 311280-888Y 1980-12-31 Female
#> 3 131052-308T 1952-10-13 Female
```

All of the **pinr** functions that work with data frames are pipe-friendly, lending themselves to readable workflows such as this:

``` r
library(magrittr) # for the pipe operator

df %>% 
  pin_extract(pin) %>% 
  pseudonymize(key, pid = pin)
#>   pid        dob    sex
#> 1   1 1980-12-31 Female
#> 2   1 1980-12-31 Female
#> 3   2 1952-10-13 Female
```
