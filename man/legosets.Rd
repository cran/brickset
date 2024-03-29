% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brickset-package.R
\docType{data}
\name{legosets}
\alias{legosets}
\title{Lego sets from 1970 through 2022.}
\format{
A data.frame.
\describe{
\item{setID}{integer; 19409 unique values}
\item{number}{character; 17997 unique values}
\item{numberVariant}{integer; 25 unique values}
\item{name}{character; 16206 unique values}
\item{year}{integer; 54 unique values}
\item{theme}{character; 158 unique values}
\item{themeGroup}{character; 17 unique values}
\item{subtheme}{character; 957 unique values}
\item{category}{character; 7 unique values}
\item{released}{logical; 2 unique values}
\item{pieces}{integer; 1461 unique values}
\item{minifigs}{integer; 34 unique values}
\item{bricksetURL}{character; 19409 unique values}
\item{rating}{numeric; 30 unique values}
\item{reviewCount}{integer; 63 unique values}
\item{packagingType}{character; 19 unique values}
\item{availability}{character; 10 unique values}
\item{agerange_min}{integer; 17 unique values}
\item{thumbnailURL}{character; 18355 unique values}
\item{imageURL}{character; 18355 unique values}
\item{US_retailPrice}{numeric; 154 unique values}
\item{US_dateFirstAvailable}{Date; 979 unique values}
\item{US_dateLastAvailable}{Date; 2197 unique values}
\item{UK_retailPrice}{numeric; 226 unique values}
\item{UK_dateFirstAvailable}{Date; 927 unique values}
\item{UK_dateLastAvailable}{Date; 2068 unique values}
\item{CA_retailPrice}{numeric; 177 unique values}
\item{CA_dateFirstAvailable}{Date; 745 unique values}
\item{CA_dateLastAvailable}{Date; 1880 unique values}
\item{DE_retailPrice}{numeric; 173 unique values}
\item{DE_dateFirstAvailable}{Date; 514 unique values}
\item{DE_dateLastAvailable}{Date; 1252 unique values}
\item{height}{numeric; 248 unique values}
\item{width}{numeric; 290 unique values}
\item{depth}{numeric; 282 unique values}
\item{weight}{numeric; 1107 unique values}
}
}
\source{
\url{https://brickset.com}
}
\description{
This dataset was built using the \code{\link{getSets}} function. For working
with the LEGO sets data frame this pre-built data is preferred as it will
minimize the API requests. Note that the only disadvantage is that the
\code{rating} and \code{reviewCount} may be out-of-date and inaccurate. The
remaining variables are relatively static.
}
\keyword{lego}
