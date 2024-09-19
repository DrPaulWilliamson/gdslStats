
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdslStats

<!-- badges: start -->
<!-- badges: end -->

The goal of gdslStats is to provide a set of tools to simplify the
process of conducting the types of analyses normally covered in an
introductory statistics course - data description, tabulation,
correlation and OLS and logistic regression. Highlights of the package
include the ability to create publication-standard of 1-, 2- and 3-way
versions of cross-tabulations.

## Installation

You can install the development version of gdslStats from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("DrPaulWilliamson/gdslStats")
```

## Rounding

The `round` function provided as part of `gdslStats` is designed to to
replace the `round` function supplied as part of R’s `base` package. The
`gdslStats` version of this function provides better support for the
rounding of data objects that include columns (tables, matrices,
data.frames and tibbles). Specifically, columns stored as factors or
strings are ignored, rather than causing an error, and the option is
provided to apply different roundings to different columns. In addition,
for tables and tibbles, the way the data object is displayed is adjusted
to match the rounding applied. Finally, more informative error messages
are provided if asked to round the wrong kind of data.

``` r
library(gdslStats)
new_round( data_in = 1.1234 )
#> [1] 1.12

df <- data.frame( var = c("A", "B", "C"), v1 = rep(1.1234, 3), v2 = rep(1.1234, 3) )

new_round( df, dp = 1 )
#>   var  v1  v2
#> 1   A 1.1 1.1
#> 2   B 1.1 1.1
#> 3   C 1.1 1.1

new_round( df, dp = c(NA, 1, 2) )
#>   var  v1   v2
#> 1   A 1.1 1.12
#> 2   B 1.1 1.12
#> 3   C 1.1 1.12

#' # Example of table with as_string = FALSE to follow

new_round( tibble::as_tibble( df), dp = c(NA, 2, 1) )
#> # A tibble: 3 × 3
#>   var          v1        v2
#>   <chr> <num:.2!> <num:.1!>
#> 1 A          1.12       1.1
#> 2 B          1.12       1.1
#> 3 C          1.12       1.1
```
