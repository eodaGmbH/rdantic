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

#' Check sub model
#' @param sub_model sub model
#' @export
is_sub_model <- function(sub_model) {
  function(x) is.list(sub_model(x))
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
