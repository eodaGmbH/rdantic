test_that("validate lists", {
  # Prepare
  my_model <- base_model(
    a = is.numeric,
    b = is.integer,
    txt = is.character
  )

  # Act
  l <- my_model(a = 10.5, b = 20L, txt = "Hi")

  # Assert
  expect_equal(l, list(a = 10.5, b = 20L, txt = "Hi"))
  expect_error(my_model(
    a = 10,
    b = 10,
    txt = "Hi"
  ))
})

test_that("validators before", {
  # Prepare
  my_model <- base_model(
    a = is.integer,
    b = is.integer,
    .validators_before = list(
      a = as.integer,
      b = as.integer
    )
  )

  # Act
  m <- my_model(
    a = 1L,
    b = 2
  )

  # Assert
  expect_equal(m, list(a = 1L, b = 2L))
})

test_that("validate function arguments", {
  # Prepare
  f <- function(a, b) {
    validate_args(a = is.integer, b = is.integer)
    a + b
  }

  # Act
  res <- f(2L, 4L)

  # Assert
  expect_equal(res, 6L)
  expect_error(f(2, 5L))
})

test_that("validate typed functions arguments", {
  # Prepare
  f_with_typed_args <- function(a = is.integer, b = is.integer) {
    validate_fn()
    a + b
  }

  # Act
  res <- f_with_typed_args(2L, 5L)

  # Assert
  expect_equal(res, 7L)
  expect_error(f(2L, 5))
})

test_that("validate data frame", {
  # Prepare
  df_to_fail <- data.frame(
    mpg = as.integer(mtcars$mpg),
    cyl = mtcars$cyl
  )

  my_model <- base_model(
    mpg = is.double,
    cyl = is.integer,
    .validators_before = list(
      cyl = as.integer
    )
  )

  # Act
  df <- my_model(mtcars)

  # Assert
  expect_equal(names(df), c("mpg", "cyl"))
  expect_s3_class(df, "data.frame")
  expect_type(df$cyl, "integer")
  expect_error(my_model(df_to_fail))
})
