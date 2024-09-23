
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
#> 
#> Attaching package: 'gdslStats'
#> The following object is masked from 'package:base':
#> 
#>     round
round( data_in = 1.1234 )
#> [1] 1.12

df <- data.frame( var = c("A", "B", "C"), v1 = rep(1.1234, 3), v2 = rep(1.1234, 3) )

round( df, dp = 1 )
#>   var  v1  v2
#> 1   A 1.1 1.1
#> 2   B 1.1 1.1
#> 3   C 1.1 1.1

round( df, dp = c(NA, 1, 2) )
#>   var  v1   v2
#> 1   A 1.1 1.12
#> 2   B 1.1 1.12
#> 3   C 1.1 1.12

#' # Example of table with as_string = FALSE to follow

round( tibble::as_tibble( df), dp = c(NA, 2, 1) )
#> # A tibble: 3 × 3
#>   var          v1        v2
#>   <chr> <num:.2!> <num:.1!>
#> 1 A          1.12       1.1
#> 2 B          1.12       1.1
#> 3 C          1.12       1.1
```

## (Cross-)tabulation

The `tab` function provides a wrapper to various base r tabulation
functions designed to simplify the process of tabulating survey or admin
style datasets. The function supports the creation of one-, two- and
three-way tabulations using counts or rates (joint, row or column
proportions or percentages). The resulting table can optionally include
row and/or column totals, whilst tables involving row or column rates
can also include the base count (frequency) underpinning the rate. For
three-way tables a further option is to include a final sub-table
aggregating results across the third dimension.

The output from the `tab` function is a data object of class `tab`,
which is compatible with functions requiring `table` and `xtable` class
inputs, but also compatible with the `publish_tab` fuction which can be
used to convert any `tab` data object into a publication-standard table
in one line of code.

``` r
#' # Load toy dataset supplied with package
#' #load(survey)
#'
#' # Tabulation examples
#'
#' # Frequency table (counts), with and without missing values
#' survey |> tab( ~WorkStatus, na.rm = TRUE )
#' survey |> tab( ~WorkStatus, na.rm = FALSE )
#'
#' # Frequency table (proportions and cumulative proportions)
#' survey |> tab( ~ Age, measure = "col_prop" )
#' survey |> tab( ~ Age, measure = "col_prop", cum_sum = TRUE )
#'
#' # Two-way cross-tabulation (column percentages; row percentages with row totals)
#' survey |> tab( Sex ~ Age, measure = "col_pct" )
#' survey |> tab( Sex ~ Age, measure = "row_pct", totals = "row_col" )
#'
#' # Two-way cross-tabulation (row percentages with row totals and base count)
#' survey |> tab( Sex ~ Age, measure = "row_pct", totals = "col",
#'                base_count = TRUE )
#'
#' # Three-way cross-tabulation including counts aggregated across third dimension
#' survey |> tab( Health ~ Age + Sex, measure = "count", total_3d = TRUE )
#'
#' # All-singing all-dancing three-way table
#' survey |> tab( Health ~ Age + Sex, measure = "row_pct", totals = "row",
#'                base_count = TRUE, total_3d = TRUE )
#'
#' # Publication-standard version of a table
#' # (note: any rounding must take place prior to call to `publish_tab` )
#' survey |>
#'   tab( Age ~ Sex, measure = "row_pct", 
#'        totals = "row", base_count = TRUE ) |>
#'   round( dp = 0 ) |>
#'   publish_tab( )
```
