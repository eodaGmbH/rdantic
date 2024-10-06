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
  types <- unlist(list(...))
  return(check_types(types, .validators))
}


template <- list(a_character = "hello",
                 a_model = lm(mpg ~ cyl, data = mtcars),
                 a_dataframe = mtcars,
                 a_number = 12,
                 an_integer = 12L)


derive_model <- function(template){
  if(!is.list(template)){
    stop ("Please provide a list of key/value pairs with the value used for type derivation.)")
  }
  validators <- lapply(template, function(value){
    type <- typeof(value)
    class <- class(value)
    mode <- mode(value)
    storage.mode <- storage.mode(value)
    validator <- function(x){
      (typeof(x) == eval(type) &&
         class(x) == class &&
         mode(x) == mode)
    }
    return(validator)
  })
  base_model(setNames(validators,names(template)))

}

my_model <- derive_model(template)
my_model(list(
  an_integer = 12L,
  a_character = "test",
  a_model = lm(Sepal.Width ~ Petal.Width, data = iris),
  a_number = 7
))
