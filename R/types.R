is_any <- function(x) TRUE

# ---
is_optional <- function(.f) {
  function(x) .f(x) | is.null(x)
}
