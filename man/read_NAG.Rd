% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_enzyme_data.R
\name{read_NAG}
\alias{read_NAG}
\title{Read N-Acetylglucosaminidase (NAG) Enzyme Activity Data}
\usage{
read_NAG(df)
}
\arguments{
\item{df}{A string representing the path to the Excel file containing the NAG enzyme activity data.}

\item{range}{anchored(anchor = "Z63", dim = c(2L, 5L), input = NULL, col_names = NULL, byrow = FALSE)}
}
\value{
A data frame containing positive NAG enzyme activity measurements.
}
\description{
This function reads N-Acetylglucosaminidase (NAG) enzyme activity measurements from an Excel file.
}
\examples{
# Example usage:
# read_NAG("enzyme_data.xlsx")
}
