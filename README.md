
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rdantic

<!-- badges: start -->

[![R-CMD-check](https://github.com/eodaGmbH/rdantic/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eodaGmbH/rdantic/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rdantic is to add type safety to your R code.

## Installation

You can install the development version of rdantic from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("eodaGmbH/rdantic")
```

## Examples

### Models

``` r
library(rdantic)
library(rlang)

numbers <- base_model(
  a = is_integer,
  b = is_integer
)

numbers(a = 2L, b = 4L)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 4
```

``` r

try(numbers(a = 2L, b = 4.5))
#> Error in raise_type_check_error(k, value, type_check) : 
#>   Type check failed.
#> ℹ field: b
#> ✖ value: 4.5
#> ✖ test: function (x, n = NULL) { .Call(ffi_is_integer, x, n) }
```

``` r

my_model <- base_model(
  convert_me_to_camel_case = is_scalar_character,
  a = is_optional(is_integer),
  b = is_integer,
  txt = is_scalar_character
)

(m <- my_model(convert_me_to_camel_case = "okay", b = 10L, txt = "Hi"))
#> $convert_me_to_camel_case
#> [1] "okay"
#> 
#> $b
#> [1] 10
#> 
#> $txt
#> [1] "Hi"
#> 
#> $a
#> NULL
```

``` r

m |> model_dump(exclude_null = TRUE, camels = TRUE)
#> $convertMeToCamelCase
#> [1] "okay"
#> 
#> $b
#> [1] 10
#> 
#> $txt
#> [1] "Hi"
```

``` r

m |> model_dump(include = c("a", "b"))
#> $b
#> [1] 10
#> 
#> $a
#> NULL
```

### Type safety inside functions

``` r
add_two_numbers <- function(a, b) {
  validate_args(
    a = is_scalar_double,
    b = is_scalar_double
  )
  a + b
}

add_two_numbers(2, 4)
#> [1] 6
```

``` r

try(add_two_numbers(2, c(2, 4)))
#> Error in raise_type_check_error(k, value, type_check) : 
#>   Type check failed.
#> ℹ field: b
#> ✖ value: c(2, 4)
#> ✖ test: function (x) { .Call(ffi_is_double, x, 1L, NULL) }
```

``` r

devide_two_numbers <- function(a, b) {
  validate_args(
    a = is_double,
    b = ~ is_double(.x) & .x != 0
  )
  a / b
}

devide_two_numbers(4, 2)
#> [1] 2
```

``` r

try(devide_two_numbers(4, 0))
#> Error in raise_type_check_error(k, value, type_check) : 
#>   Type check failed.
#> ℹ field: b
#> ✖ value: 0
#> ✖ test: structure(function (..., .x = ..1, .y = ..2, . = ..1) is_double(.x) &
#>   .x != 0, class = c("rlang_lambda_function", "function" ))
```

``` r

# Add validators

devide_two_numbers <- function(a, b) {
  validate_args(
    a = is_double,
    b = is_double,
    .validators_after = list(
      b = ~ ifelse(.x == 0, 1, .x)
    )
  )
  a / b
}

devide_two_numbers(4, 0)
#> [1] 4
```

### Get settings from env vars

``` r
Sys.setenv(POSTGRES_USERNAME = "postgres")
Sys.setenv(POSTGRES_PASSWORD = "superSecret!")
Sys.setenv(POSTGRES_PORT = 15432)

postgres_settings <- base_settings(
  username = as.character,
  password = as.character,
  port = as.integer,
  .prefix = "POSTGRES"
)

postgres_settings()
#> $username
#> [1] "postgres"
#> 
#> $password
#> [1] "superSecret!"
#> 
#> $port
#> [1] 15432
```

``` r

Sys.setenv(POSTGRES_PORT = "")

try(postgres_settings())
#> Error in raise_type_check_error(env_var_name, .obj[[k]], as_type) : 
#>   Type check failed.
#> ℹ field: POSTGRES_PORT
#> ✖ value: NA_integer_
#> ✖ test: as.integer
```
