devtools::load_all()

#check_args <- function(...) {
#  fields <- list(...)
#  if (length(fields) == 0) {
#    fn <- rlang::caller_fn()
#    fmls <- rlang::fn_fmls(fn)
#    fields <- purrr::map(as.list(fmls), eval)
#  }

#  e <- rlang::caller_env()
#  for (name in names(e)) {
#    value <- e[[name]]
#    if (is.list(value)) {
#      e[[name]] <- value$default
#    }
#  }

#  base_model2(fields)(.x = e)
#}

f <- function(a, b = 80L) {
  check_args(a = is.integer, b = is.integer)
  a + b
}

f(2L)

f2 <- function(aa = is.numeric, bb = model_field(is.integer, 10L)) {
  check_args()
  aa + bb
}

f2(5)

# ---
my_model2 <- base_model2(
  cyl = is.double,
  mpg = is.integer,
  .validators_before = list(
    mpg = as.integer
  )
)
my_model2(.x = tibble::as_tibble(mtcars))
