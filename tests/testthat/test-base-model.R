test_that("lists", {
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
  # expect_equal(names(l), c("a", "b", "txt"))
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

test_that("validate func args", {
  # Prepare
  f <- function(a, b) {
    validate_args(a = is.integer, b = is.integer)
    a + b
  }

  # Act
  res <- f(2L, 4L)

  # Assert
  expect_equal(res, 6L)
})

test_that("validate func", {
  # Prepare
  f_with_typed_args <- function(a = is.integer, b = is.integer) {
    validate_fn(f_with_typed_args)
    a + b
  }

  # Act
  res <- f_with_typed_args(2L, 5L)

  # Assert
  expect_equal(res, 7L)
})
