is_any <- function(x) TRUE

# ---
#' Mark a field as optional
#' @param fn type check function
#' @export
is_optional <- function(fn) {
  fn_name <- deparse(substitute(fn))
  eval(parse(text = paste0("function(x) ", fn_name, "(x) | is.null(x)")))
}

is_optional2 <- function(.f) set_attributes(.f, optional = TRUE)

#' Check/assert a model inside a model
#' @param model_obj model to check
#' @export
is_model <- function(model_obj) {
  function(x) is.list(model_obj(x))
}
