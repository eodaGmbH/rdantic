is_any <- function(x) TRUE

# ---
is_optional <- function(.f) {
  f_name <- deparse(substitute(.f))
  eval(parse(text = paste0("function(x) ", f_name, "(x) | is.null(x)")))
}

is_optional2 <- function(.f) set_attributes(.f, optional = TRUE)

# ---
#' @export
validate_args <- function(..., .validators = NULL) {
  base_model(..., .validators = .validators)(rlang::caller_env())
}
