# ---
model_field <- function(fn, default = NA, ...) {
  l <- as.list(environment())
  return(c(l, list(...)))
}


# ---
base_model2 <- function(...) {
  fields <- purrr::map(list(...), ~ {
    if (inherits(.x, "function")) {
      return(model_field(fn = .x))
    }

    return(.x)
  })

  model_args <- purrr::map(fields, ~ .x$default)

  # Create model function
  model_fn <- rlang::new_function(c(model_args, alist(... = , .x = NULL)), quote({
    if (is_not_null(.x)) {
      obj <- .x
    } else {
      obj <- as.list(environment())
    }

    for (name in names(fields)) {
      check_type_fn <- rlang::as_function(fields[[name]]$fn)
      obj_value <- obj[[name]]
      if (isFALSE(check_type_fn(obj_value))) {
        cli::cli_abort(
          c(
            i = "{name} = {rlang::quo_text(obj_value)}",
            i = "typeof({name}): {typeof(obj_value)}",
            i = "length({name}): {length(obj_value)}",
            x = rlang::quo_text(check_type_fn)
          ),
          .frame = rlang::caller_env()
        )
      }
    }

    if (is.environment(obj)) {
      return(invisible(obj))
    }

    obj <- purrr::keep_at(obj, names(fields))

    return(obj)
  }))

  return(set_attributes(model_fn, fields = fields))
}
