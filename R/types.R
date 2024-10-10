is_any <- function(x) TRUE

# ---
#' Mark a field as optional
#' @param .f type check function
#' @export
is_optional <- function(.f) {
  f_name <- deparse(substitute(.f))
  eval(parse(text = paste0("function(x) ", f_name, "(x) | is.null(x)")))
}

is_optional2 <- function(.f) set_attributes(.f, optional = TRUE)

#' Check another model inside a model
#' @param .model model to check
#' @export
is_another_model <- function(.model) {
  function(x) is.list(.model(x))
}

# ---
#' Validate args inside function
#' @inherit base_model params return
#' @export
validate_args <- function(..., .validators_before = NULL, .validators_after = NULL) {
  base_model(
    ...,
    .validators_before = .validators_before,
    .validators_after = .validators_after
  )(rlang::caller_env())
}
