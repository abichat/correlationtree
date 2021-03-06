% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shuffle_tiplabels.R
\name{shuffle_tiplabels}
\alias{shuffle_tiplabels}
\title{Shuffle tip labels}
\usage{
shuffle_tiplabels(tree)
}
\arguments{
\item{tree}{an object of class \code{"phylo"}.}
}
\value{
The same tree with permuted tip labels.
}
\description{
Shuffle tip labels of a \code{"phylo"} object
}
\examples{
(tree <- ape::rtree(8))
shuffle_tiplabels(tree)
}
