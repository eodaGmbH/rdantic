# Models
my_model <- base_model(
  a = is.integer,
  b = is.integer,
  txt = is.character
)

my_model(a = 1L, b = 2L, txt = "My awesome model")
try(my_model(a = 1, b = 2L, txt = "My awesome model"))

# Validate function args
f <- function(a, b) {
  validate_args(a = is.numeric, b = is.numeric)
  a + b
}

f(4, 5)
try(f(4, "5"))
