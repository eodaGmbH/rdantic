devtools::load_all()

check_args <- function(...) {
  if (length(list(...)) > 0) {
    return(base_model2(...)(.x = rlang::caller_env()))
  }

  fn <- rlang::caller_fn()
  fmls <- rlang::fn_fmls(fn)
  fields <- purrr::map(as.list(fmls), eval)
  base_model2(fields)(.x = rlang::caller_env())
}

f <- function(a, b) {
  check_args(a = is.integer, b = is.integer)
  a + b
}

f(2L, 4)

f2 <- function(aa = is.numeric, bb = is.integer) {
  check_args()
  aa + bb
}

f2(10, 20)

# ---
my_model2 <- base_model2(cyl = is.double, mpg = is.integer)
my_model2(.x = mtcars)
