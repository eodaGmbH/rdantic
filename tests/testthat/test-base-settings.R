test_that("base settings", {
  # Prepare
  Sys.setenv(POSTGRES_USERNAME = "postgres")
  Sys.setenv(POSTGRES_PASSWORD = "superSecret!")
  Sys.setenv(POSTGRES_PORT = 15432)

  postgres_settings <- base_settings(
    username = as.character,
    password = as.character,
    port = as.integer,
    .prefix = "POSTGRES"
  )

  # Act
  pg_settings <- postgres_settings()

  # Assert
  expect_equal(pg_settings, list(username = "postgres", password = "superSecret!", port = 15432))
})
