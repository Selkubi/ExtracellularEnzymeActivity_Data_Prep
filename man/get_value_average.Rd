% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_value_average.R
\name{get_value_average}
\alias{get_value_average}
\title{Calculate Average Contrasts Result}
\usage{
get_value_average(x, list)
}
\arguments{
\item{x}{A character string representing the name of the argument to extract from the emmGrid objects.}

\item{data}{A list of emmGrid objects from which to extract the contrasts and p-values.}
}
\value{
A data frame containing the contrasts, positions, and the average values until the 2nd significant figure.
}
\description{
This function calculates the average contrasts result from a list of emmGrid objects.
}
\examples{
# Example usage:
# data <- list(emmGrid1, emmGrid2, ...)
# result <- average_contrasts_result_calculation("p.value", data)
# print(result)

}
