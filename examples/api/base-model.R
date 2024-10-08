validators <- list(
  b = function(x) ifelse(x == 0, 1, x)
)

my_model <- base_model(
  a = is.numeric,
  b = is.numeric,
  .validators = validators
)

f <- function(a, b) a / b

do.call(f, my_model(a = 1, b = 0)) # 1
do.call(f, list(a = 1, b = 0)) # Inf

# Use model inside function
f <- function(a, b) {
  params <- my_model(environment())
  params$a / params$b
}

f(1, 0) # 1
