raise_type_check_error <- function(key, value, f_type_check) {
  value_text <- rlang::quo_text(value)
  f_text <- ifelse(
    rlang::is_primitive(f_type_check),
    rlang::prim_name(f_type_check),
    rlang::quo_text(f_type_check)
  )
  # cli::cli_abort(
  cli::format_error(
    c(
      "Type check failed.",
      i = "field: {key}",
      x = "value: {value_text}",
      x = "test: {f_text}"
    )
  )
}

validate_model_values <- function(obj, validators) {
  for (k in names(validators)) {
    obj[[k]] <- rlang::as_function(validators[[k]])(obj[[k]])
  }

  return(obj)
}

#' Create a model
#'
#' @param fields A named list where values are functions used for the type checks.
#' @param ... Model parameters and their functions used for the type checks.
#'  Usually only `fields` or `...` are supplied.
#' @param .validators_before A list of validators that run before types are checked.
#' @param .validators_after A list of validators that run after types are checked.
#' @param .model_config **not used at the moment**
#' @returns base model function/
#' @example examples/api/base-model.R
#' @export
base_model <- function(fields = list(), ...,
                       .validators_before = NULL,
                       .validators_after = NULL,
                       .model_config = list()) {
  fields = utils::modifyList(fields, list(...), keep.null = TRUE)
  function(obj = list(), ...) {
    if (is.list(obj)) {
      obj <- utils::modifyList(obj, list(...), keep.null = TRUE)
    }

    # TODO: Remove
    if (length(obj) == 0) obj <- rlang::caller_env()

    if (!is.null(.validators_before)) {
      obj <- validate_model_values(obj, .validators_before)
    }

    for (name in names(fields)) {
      # TODO: Do we really want this?
      #if (!is.environment(obj) & !k %in% names(obj)) {
      #  obj[k] <- list(NULL)
      #}

      check_type <- rlang::as_function(fields[[name]])
      value <- obj[[name]]
      if (isFALSE(check_type(value))) {
        stop(raise_type_check_error(name, value, check_type))
      }
    }

    if (!is.null(.validators_after)) {
      obj <- validate_model_values(obj, .validators_after)
    }

    if (is.environment(obj)) return(invisible(obj))

    # Only return defined fields
    obj <- purrr::keep_at(obj, names(fields))

    return(obj)
  }
}


base_model_DEPRECATED <- function(..., .validators_before = NULL, .validators_after = NULL) {
  types <- list(...)
  return(check_types(types, .validators_before, .validators_after))
}

#' Modify a [base_model()] object
#' @param obj model object
#' @param exclude A set of fields to exclude from the output.
#' @param include A set of fields to include in the output.
#' @param exclude_null Whether to drop items with the value `NULL`.
#' @param exclude_na Whether to drop items with the value `NA`.
#' @param camels Whether to convert all keys to camel case.
#' @returns list
#' @export
model_dump <- function(obj,
                       exclude = NULL,
                       include = NULL,
                       exclude_null = FALSE,
                       exclude_na = FALSE,
                       camels = FALSE) {
  if (!is.null(exclude)) obj <- purrr::discard_at(obj, exclude)
  if (!is.null(include)) obj <- purrr::keep_at(obj, include)
  if (exclude_null) obj <- purrr::discard(obj, is.null)
  if (exclude_na) obj <- purrr::discard(obj, is.na)
  if (camels) obj <- keys_to_camel_case(obj)
  return(obj)
}

# ---
#' Validate function arguments
#'
#' @inherit base_model params return
#' @example examples/api/validate-args.R
#' @export
validate_args <- function(..., .validators_before = NULL, .validators_after = NULL) {
  base_model(
    ...,
    .validators_before = .validators_before,
    .validators_after = .validators_after
  )(rlang::caller_env())
}

# ---
#' Validate function arguments
#'
#' @param fn A function containing the arguments to be checked.
#' @returns The function's `environment`.
#' @example examples/api/validate-fn.R
#' @export
validate_fn <- function(fn) {
  fmls<- rlang::fn_fmls(fn)
  fields <- purrr::map(as.list(fmls), eval)
  base_model(fields)(rlang::caller_env())
}

# ---
#' Derive a [base_model()] from a template list.
#'
#' Checks for type, mode and class of the object.
#'
#' @param template A list of key/value pairs with the value used for type derivation.
#' @returns Returns a [base_model()] object.
#' @example examples/derive-model.R
#' @export
derive_model <- function(template){
  fields <- purrr::map(template, function(v) {
    body <- substitute({
      typeof(x) == type_ & class(x) == class_ & mode(x) == mode_
    }, list(type_ = typeof(v), class_ = class(v), mode_ = mode(v)))
    rlang::new_function(alist(x = ), body = body)
  })
  return(base_model(fields))
}
