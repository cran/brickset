% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getThemes.R
\name{getThemes}
\alias{getThemes}
\title{Returns a table of themes with number of sets and years active.}
\usage{
getThemes(key = getOption("brickset_key"), ...)
}
\arguments{
\item{key}{the Brickset API key.}

\item{...}{other parameters passed to \code{\link{getUserHash}} including
the Brickset username and password if they are not available from
\code{getOption('brickset_username')} and \code{getOption('brickset_password')}.}
}
\value{
a data.frame with the themes.
}
\description{
Brickset API documentation is available here:
https://brickset.com/article/52664/api-version-3-documentation/
}
\examples{
\dontrun{
options(brickset_key = 'BRICKSET_KEY',
        brickset_username = 'BRICKSET_UERNAME',
        brickset_password = 'BRICKSET_PASSWORD')
getThemes()
}
}
