validate_model_values <- function(.obj, validators) {
for (k in names(validators)) {
  .obj[[k]] <- validators[[k]](.obj[[k]])
}
  return(.obj)
}

check_types <- function(types, validators_before = NULL, validators_after = NULL) {
  function(.obj = list(), ..., .drop_null = FALSE, .force_list = FALSE) {
    # Convert environment into list
    if (isTRUE(.force_list)) .obj <- as.list(.obj)

    if (is.list(.obj)) {
      .obj <- utils::modifyList(.obj, list(...), keep.null = TRUE)
    }

    if (length(.obj) == 0) .obj <- rlang::caller_env()

    if (!is.null(validators_before)) {
      .obj <- validate_model_values(.obj, validators_before)
    }

    # for (k in names(.obj)) {
    for (k in names(types)) {
      if (!k %in% names(.obj)) .obj[k] <- list(NULL)
      type_check <- rlang::as_function(types[[k]])
      value <- .obj[[k]]
      # testthat::expect_true(type_check(value)) |>
      #  testthat::show_failure()
      if (!type_check(value)) {
        stop("Value of '", k, "' (\"", value, "\") failed test: ", deparse(substitute(type_check)), call. = FALSE)
      }
    }

    if (!is.null(validators_after)) {
      .obj <- validate_model_values(.obj, validators_after)
    }

    if (is.environment(.obj)) invisible(.obj)

    if (is.list(.obj) & isTRUE(.drop_null)) {
      return(purrr::compact(.obj))
    }

    return(.obj)
  }
}

#' Create a model
#' @param ... model parameters and their type-check functions
#' @param .validators_before list of validators that run before types are checked
#' @param .validators_after list of validators that run after types are checked
#' @returns model function
#' @example examples/api/base-model.R
#' @export
base_model <- function(..., .validators_before = NULL, .validators_after = NULL) {
  types <- list(...)
  return(check_types(types, .validators_before, .validators_after))
}

#' Modify a model object
#' @param .obj model object
#' @param exclude A set of fields to exclude from the output.
#' @param include A set of fields to include in the output.
#' @param exclude_null Whether to drop items with the value `NULL`.
#' @param exclude_na Whether to drop items with the value `NA`.
#' @param camels Whether to convert all keys to camel case.
#' @returns list
#' @export
model_dump <- function(.obj,
                       exclude = NULL,
                       include = NULL,
                       exclude_null = FALSE,
                       exclude_na = FALSE,
                       camels = FALSE) {
  if (!is.null(exclude)) .obj <- purrr::discard_at(.obj, exclude)
  if (!is.null(include)) .obj <- purrr::keep_at(.obj, include)
  if (exclude_null) .obj <- purrr::discard(.obj, is.null)
  if (exclude_na) .obj <- purrr::discard(.obj, is.na)
  if (camels) .obj <- keys_to_camel_case(.obj)
  return(.obj)
}


# model_dump_json <- function(.obj, ...) {
#  model_dump(.obj, ...) |>
#    jsonlite::toJSON(auto_unbox = TRUE)
# }
