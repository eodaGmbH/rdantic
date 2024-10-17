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

  # Create model function
  model_fn <- rlang::new_function(purrr::map(fields, ~ .x$default), quote({
    obj <- as.list(environment())
    for (name in names(fields)) {
      check_type_fn <- rlang::as_function(fields[[name]]$fn)
      obj_value <- obj[[name]]
      if (isFALSE(check_type_fn(obj_value))) {
        cli::cli_abort(
          c(
            i = "{name} = {obj_value}",
            x = rlang::quo_text(check_type_fn)
          ), .frame = rlang::caller_env()
        )
      }
    }

    return(obj)
  }))

  return(set_attributes(model_fn, fields = fields))
}
