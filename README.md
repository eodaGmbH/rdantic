
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdantic

<!-- badges: start -->
<!-- badges: end -->

The goal of rdantic is to add type safety to your R code.

## Installation

You can install the development version of rdantic from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eodaGmbH/rdantic")
```

## Example

``` r
library(rdantic)

numbers <- base_model(
  a = is.numeric,
  b = is.numeric
)

numbers(a = 2, b = 4)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 4
```

``` r

try(
  numbers(a = 2, b = "Hi")
)
#> Error : Value of 'b' ("Hi") failed test: .Primitive("is.numeric")
```

``` r

# Use type checking inside your functions

add_two_numbers <- function(a, b) {
  params <- numbers(environment())
  params$a + params$b
}

add_two_numbers(2, 4)
#> [1] 6
```

``` r

try(
  add_two_numbers(2, "4")
)
#> Error : Value of 'b' ("4") failed test: .Primitive("is.numeric")
```

``` r

numbers <- base_model(
  a = is.numeric,
  b = function(x) is.numeric(x) & x != 0
)

devide_two_numbers <- function(a, b) {
  params <- numbers(environment())
  params$a / params$b
}

devide_two_numbers(4, 2)
#> [1] 2
```

``` r

try(
  devide_two_numbers(4, 0)
)
#> Error : Value of 'b' ("0") failed test: function (x) is.numeric(x) & x != 0
```

``` r

# Add validators

numbers_with_validator <- base_model(
  a = is.numeric,
  b = is.numeric,
  .validators = list(
    b = function(x) ifelse(x == 0, 1, x)
  )
)

devide_two_numbers_set_0_to_1 <- function(a, b) {
  params <- numbers_with_validator(environment())
  params$a / params$b
}

devide_two_numbers_set_0_to_1(4, 0)
#> [1] 4
```
