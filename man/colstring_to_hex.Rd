% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pop_evo.R
\name{colstring_to_hex}
\alias{colstring_to_hex}
\title{Convert a built-in R colour string to a hex code string}
\usage{
colstring_to_hex(...)
}
\arguments{
\item{...}{any built-in R colour strings such as \code{colours()}}
}
\value{
a character vector of equal length as the input, of corresponding hex colour codes
}
\description{
Convert a built-in R colour string to a hex code string
}
\examples{
colstring_to_hex("red","yellow2","springgreen4")
}
