test_that("snakes to camels", {
  # Prepare
  l <- list(
    snake_case = "yes",
    camelCase = "yes",
    isSnakeCase = "no",
    is_camel_case = list(more_snakes = "yeah")
  )

  # Act
  l_camels <- keys_to_camel_case(l)

  # Assert
  expect_equal(names(l_camels), c("snakeCase", "camelCase", "isSnakeCase", "isCamelCase"))
  expect_equal(l_camels$isCamelCase$moreSnakes, "yeah")
})

test_that("camels to snakes", {
  # Prepare
  from_json <- list(
    convertMeToSnakes = "okay",
    this_is_snake_case = "yes"
  )

  # Act
  res <- keys_to_snake_case(from_json)

  # Assert
  expect_equal(res, list(convert_me_to_snakes = "okay", this_is_snake_case = "yes"))
})
