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
