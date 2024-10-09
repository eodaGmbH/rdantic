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
