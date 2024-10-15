raise_type_check_error <- function(key, value, f_type_check) {
  value_text <- rlang::quo_text(value)
  f_text <- ifelse(
    rlang::is_primitive(f_type_check),
    rlang::prim_name(f_type_check),
    rlang::quo_text(f_type_check)
  )
  cli::cli_abort(
    c(
      "Type check failed.",
      i = "field: {key}",
      x = "value: {value_text}",
      x = "test: {f_text}"
    )
  )
}

validate_model_values <- function(.obj, validators) {
  for (k in names(validators)) {
    .obj[[k]] <- rlang::as_function(validators[[k]])(.obj[[k]])
  }

  return(.obj)
}

# TODO: Rename types to fields
# TODO: Can/Should we rename '.obj' to 'obj'?
base_model <- function(fields = list(), ..., .validators_before = NULL, .validators_after = NULL) {
  fields = utils::modifyList(fields, list(...), keep.null = TRUE)
  function(.obj = list(), ..., .drop_null = FALSE, .force_list = FALSE) {

    # Convert environment into list
    if (isTRUE(.force_list)) .obj <- as.list(.obj)

    if (is.list(.obj)) {
      .obj <- utils::modifyList(.obj, list(...), keep.null = TRUE)
    }

    # TODO: Remove
    if (length(.obj) == 0) .obj <- rlang::caller_env()

    if (!is.null(.validators_before)) {
      .obj <- validate_model_values(.obj, .validators_before)
    }

    for (name in names(fields)) {
      # TODO: Do we really want this?
      #if (!is.environment(.obj) & !k %in% names(.obj)) {
      #  .obj[k] <- list(NULL)
      #}

      check_type <- rlang::as_function(fields[[name]])
      value <- .obj[[name]]
      if (isFALSE(check_type(value))) {
        raise_type_check_error(name, value, check_type)
      }
    }

    if (!is.null(.validators_after)) {
      .obj <- validate_model_values(.obj, .validators_after)
    }

    if (is.environment(.obj)) return(invisible(.obj))

    # Only return defined fields
    .obj <- purrr::keep_at(.obj, names(fields))

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
base_model_DEPRECATED <- function(..., .validators_before = NULL, .validators_after = NULL) {
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

# ---
#' Validate function arguments
#' @inherit base_model params return
#' @export
validate_args <- function(..., .validators_before = NULL, .validators_after = NULL) {
  base_model(
    ...,
    .validators_before = .validators_before,
    .validators_after = .validators_after
  )(rlang::caller_env())
}

# ---
#' Validate function parameters
#' @param fn function containing the arguments to be checked
#' @export
validate_fn <- function(fn) {
  fmls<- rlang::fn_fmls(fn)
  fields <- purrr::map(as.list(fmls), eval)
  purrr::exec(base_model, !!!fields)(rlang::caller_env())
}

# model_dump_json <- function(.obj, ...) {
#  model_dump(.obj, ...) |>
#    jsonlite::toJSON(auto_unbox = TRUE)
# }


#' Derive a [base_model()] from a template list.
#' Checks for type, mode and class of the object.
#'
#' @param template A list of key/value pairs with the value used for type derivation.
#'
#' @returns Returns a [base_model()] object.
#' @export
#'
#' @example examples/derive-model.R
derive_model <- function(template){
  fields <- purrr::map(template, function(v) {
    body <- substitute({
      typeof(x) == type_ & class(x) == class_ & mode(x) == mode_
    }, list(type_ = typeof(v), class_ = class(v), mode_ = mode(v)))
    rlang::new_function(alist(x = ), body = body)
  })
  do.call(base_model, fields)
}
