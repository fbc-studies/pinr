
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis build status](https://travis-ci.org/mikmart/pinr.svg?branch=master)](https://travis-ci.org/mikmart/pinr)

pinr
====

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
