is_any <- function(x) TRUE

# ---
is_optional <- function(.f) {
  function(x) .f(x) | is.null(x)
}

is_optional2 <- function(.f) set_attributes(.f, optional = TRUE)
