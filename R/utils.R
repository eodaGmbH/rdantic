to_camel_case <- function(x) {
  gsub("_(\\w?)", "\\U\\1", x, perl = TRUE)
}

# TODO: Make it recursive
keys_to_camel_case <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  stats::setNames(x, to_camel_case(names(x)))
}

# ---

set_attributes <- function(x, ...) {
  attributes(x) <- c(attributes(x), list(...))
  return(x)
}
