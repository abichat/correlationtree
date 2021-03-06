% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lineage_length.R
\name{lineage_length}
\alias{lineage_length}
\title{Lineage length}
\usage{
lineage_length(tree, label)
}
\arguments{
\item{tree}{a "phylo" object.}

\item{label}{tip label.}
}
\value{
A double.
}
\description{
Compute the distance from the root for a leaf.
}
\examples{
tree <- ape::read.tree(text = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
lineage_length(tree, "A")
lineage_length(tree, "D")
}
