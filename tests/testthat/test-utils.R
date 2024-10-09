test_that("snakes to camels", {
  # Prepare
  l <- list(
    snake_case = "yes",
    camelCase = "yes",
    isSnakeCase = "no",
    is_camel_case = "no"
  )

  # Act
  l_camels <- keys_to_camel_case(l)

  # Assert
  expect_equal(names(l_camels), c("snakeCase", "camelCase", "isSnakeCase", "isCamelCase"))
})
