test_that("Check derive model", {
  template <- list(
    a_character = "hello",
    a_model = lm(mpg ~ cyl, data = mtcars),
    a_dataframe = mtcars,
    a_number = 12,
    an_integer = 12L
  )


  my_model <- derive_model(template)

  expect_equal(
    list(
      a_character = "test",
      a_model = lm(cyl ~ mpg, data = mtcars),
      a_dataframe = iris,
      a_number = 234.1,
      an_integer = 1L
    ),
    my_model(
      a_character = "test",
      a_model = lm(cyl ~ mpg, data = mtcars),
      a_dataframe = iris,
      a_number = 234.1,
      an_integer = 1L
    )
  )

  expect_error(
    my_model(
      a_character = 2,
      a_model = lm(cyl ~ mpg, data = mtcars),
      a_dataframe = iris,
      a_number = 234.1,
      an_integer = 1L
    )
  )

  expect_error(
    my_model(
      a_character = "test",
      a_model = 1 ~ 2,
      a_dataframe = iris,
      a_number = 234.1,
      an_integer = 1L
    )
  )

  expect_error(
    my_model(
      a_character = "test",
      a_model = lm(cyl ~ mpg, data = mtcars),
      a_dataframe = list(1, 2, 3),
      a_number = 234.1,
      an_integer = 1L
    )
  )

  expect_error(
    my_model(
      a_character = "test",
      a_model = lm(cyl ~ mpg, data = mtcars),
      a_dataframe = iris,
      a_number = 234.1,
      an_integer = 1
    )
  )
})
