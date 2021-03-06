% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mat2list.R
\name{mat2list}
\alias{mat2list}
\title{Matrix to list}
\usage{
mat2list(mat)
}
\arguments{
\item{mat}{matrix.}
}
\value{
\code{mat2list} returns a named list whose each entry is a
row of the original matrix.
}
\description{
Convert a named matrix of abundance to a named list.
}
\examples{
m <- matrix(1:12, ncol = 3)
rownames(m) <- letters[1:4]
mat2list(m)
}
