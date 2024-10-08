set_attributes <- function(x, ...) {
  attributes(x) <- c(attributes(x), list(...))
  return(x)
}
