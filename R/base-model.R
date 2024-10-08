check_types <- function(types, validators = NULL) {
  function(.obj = list(), ..., .drop_null = TRUE, .force_list = FALSE) {
    # Convert environment into list
    if (isTRUE(.force_list)) .obj <- as.list(.obj)

    if (is.list(.obj)) {
      .obj <- utils::modifyList(.obj, list(...), keep.null = TRUE)
    }

    if (length(.obj) == 0) .obj <- rlang::caller_env()

    # TODO: Rename to 'validators_before'
    if (!is.null(validators)) {
      for (k in names(validators)) {
        .obj[[k]] <- validators[[k]](.obj[[k]])
      }
    }

    for (k in names(.obj)) {
      type_check <- rlang::as_function(types[[k]])
      value <- .obj[[k]]
      # testthat::expect_true(type_check(value)) |>
      #  testthat::show_failure()
      if (!type_check(value)) {
        stop("Value of '", k, "' (\"", value, "\") failed test: ", deparse(substitute(type_check)), call. = FALSE)
      }
    }

    if (is.environment(.obj)) invisible(.obj)

    if (is.list(.obj) & isTRUE(.drop_null)) {
      return(purrr::compact(.obj))
    }

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

model_dump <- function(.m, drop_null = TRUE, ...) {

}
