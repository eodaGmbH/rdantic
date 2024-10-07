my_model <- base_model(
  a = is.numeric,
  b = is.numeric,
  txt = is.character,
  .validators = list(
    b = function(x) as.numeric(x) * 20
  )
)

x <- list(
  a = 10,
  b = "20",
  txt = "yes"
)

my_model(x)

f <- function(a, b, txt) {
  args <- my_model(environment(), .force_list = FALSE)
  return(args)
}

f(10, "20", "yes")

# ---
another_model <- base_model(
  a = function(x) is.null(x) | is.numeric(x),
  b = is.numeric,
  txt = is.character
)

another_model(
  list(a = NULL, b = 10, txt = "Hello"),
  .drop_null = TRUE
)
