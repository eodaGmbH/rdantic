to_camel_case <- function(x) {
  gsub("_(\\w?)", "\\U\\1", x, perl = TRUE)
}

keys_to_camel_case_DEPRECATED <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  stats::setNames(x, to_camel_case(names(x)))
}

keys_to_camel_case <- function(x, .recursive = TRUE) {
  if (is.null(x)) {
    return(x)
  }

  l <- list()
  for (k in names(x)) {
    value <- x[[k]]
    k <- to_camel_case(k)
    if (is.list(value) & isTRUE(.recursive)) {
      l[[k]] <- keys_to_camel_case(value)
    } else {
      l[[k]] <- value
    }
  }

  return(l)
}

# ---

set_attributes <- function(x, ...) {
  attributes(x) <- c(attributes(x), list(...))
  return(x)
}

# ---
camels_to_snakes <- function(strings) {
  gsub(" ", "_", tolower(gsub("(.)([A-Z])", "\\1 \\2", strings)))
}

# TODO: Make it recursive
keys_to_snake_case_DEPRECATED <- function(x) {
  stats::setNames(x, camels_to_snakes(names(x)))
}

keys_to_snake_case <- function(x, .recursive = TRUE) {
  l <- list()
  for (k in names(x)) {
    value <- x[[k]]
    k <- camels_to_snakes(k)
    if (is.list(value) & isTRUE(.recursive)) {
      l[[k]] <- keys_to_snake_case(value)
    } else {
      l[[k]] <- value
    }
  }

  return(l)
}
