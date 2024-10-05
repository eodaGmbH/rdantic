get_settings <- function(types, prefix = "") {
  function() {
    .obj <- list()
    for(k in names(types)) {
      env_var_name <- toupper(glue::glue("{prefix}_{k}"))
      as_type <- types[[k]]
      .obj[[k]] <- as_type(Sys.getenv(env_var_name))
    }
    return(.obj)
  }
}

#' Create settings
#' @param ... env vars and their type convertors
#' @param .prefix prefix of env vars
#' @return settings function
#' @example examples/api/base-settings.R
#' @export
base_settings <- function(..., .prefix = "") {
  types <- list(...)
  return(get_settings(types, .prefix))
}
