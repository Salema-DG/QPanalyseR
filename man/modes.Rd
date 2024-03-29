% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modes.R
\name{modes}
\alias{modes}
\title{Calculate the Mode}
\usage{
modes(x, untie = "none")
}
\arguments{
\item{x}{A numeric/integer vector to calculate the mode for.}

\item{untie}{optional. In case there is a tie, the function unties to the "max" or the "min".}
}
\value{
A vector with the modes.
}
\description{
Calculate the mode of a vector.
The function returns multiple modes, if they exist.
The user can also, in case of a tie, keep the biggest or the smallest.
}
\examples{
vec <- c(1, 2, 3, 3, 4, 4, 5)

vec |> modes()
vec |> modes(untie = "max")
vec |> modes(untie = "min")

}
