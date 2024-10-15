f <- function(a = is.integer, b = is.integer) {
  validate_fn(f)
  a + b
}

f(2L, 5L)

try(f(2, 5))
