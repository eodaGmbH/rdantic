check_types <- function(types, validators = NULL) {
  function(.obj = list(), ..., .drop_null = TRUE) {
    if (is.environment(.obj)) .obj <- as.list(.obj)

    .obj <- utils::modifyList(.obj, list(...))

    if (!is.null(validators)) {
      for (k in names(validators)) {
        .obj[[k]] <- validators[[k]](.obj[[k]])
      }
    }

    for (k in names(.obj)) {
      type_check <- types[[k]]
      value <- .obj[[k]]
      if (!is.null(value)) {
        if (!type_check(value)) {
          stop("Value of '", k, "' (\"", value, "\") failed test: ", deparse(substitute(type_check)), call. = FALSE)
        }
      }
    }
    if (isTRUE(.drop_null)) return(purrr::compact(.obj))
    return(.obj)
  }
}

#' Create a model
#' @param ... model parameters and their type testers
#' @param .validators list of validators
#' @returns model function
#' @example examples/api/base-model.R
#' @export
base_model <- function(..., .validators = NULL) {
  types <- list(...)
  return(check_types(types, .validators))
}
