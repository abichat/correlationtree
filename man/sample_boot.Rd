% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R
\name{sample_boot}
\alias{sample_boot}
\title{Variable bootstrap}
\usage{
sample_boot(table, matrix = FALSE, col = 1)
}
\arguments{
\item{table}{a dataframe or matrix}

\item{matrix}{logical}

\item{col}{integer. The column to keep}
}
\value{
A table.
}
\description{
Variable bootstrap
}
\examples{
sample_boot(swiss)
}
