% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{login}
\alias{login}
\title{Login to the Brickset API.}
\usage{
login(
  username = getOption("brickset_username"),
  password = getOption("brickset_password"),
  key = getOption("brickset_key")
)
}
\arguments{
\item{username}{the Brickset username.}

\item{password}{the Brickset password.}

\item{key}{the Brickset API key.}
}
\value{
the user hash used for other API calls.
}
\description{
Brickset API documentation is available here:
https://brickset.com/article/52664/api-version-3-documentation/
}
