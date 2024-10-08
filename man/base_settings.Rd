% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base-settings.R
\name{base_settings}
\alias{base_settings}
\title{Create settings}
\usage{
base_settings(..., .prefix = "")
}
\arguments{
\item{...}{env vars and their type convertors}

\item{.prefix}{prefix of env vars}
}
\value{
settings function
}
\description{
Create settings
}
\examples{
Sys.setenv(POSTGRES_USERNAME = "postgres")
Sys.setenv(POSTGRES_PASSWORD = "superSecret!")
Sys.setenv(POSTGRES_PORT = 15432)

postgres_settings <- base_settings(
  username = as.character,
  password = as.character,
  port = as.integer,
  .prefix = "POSTGRES"
)

postgres_settings()
}
