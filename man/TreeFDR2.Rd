% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treefdr.R
\name{TreeFDR2}
\alias{TreeFDR2}
\title{False Discovery Rate (FDR) Control Integrating Prior Tree Structure for Microbiome Data}
\usage{
TreeFDR2(
  X,
  Y,
  tree,
  test.func,
  perm.func,
  eff.sign = TRUE,
  B = 20,
  q.cutoff = 0.5,
  alpha = 1,
  ...
)
}
\arguments{
\item{X}{a data matrix, rows are the features and columns are the samples.}

\item{Y}{a vector of the phenotypic values, where association tests are being assessed.}

\item{tree}{tree an object of \code{phylo} class}

\item{test.func}{a function that performs the actual tests. It takes X, Y
and ... as the inputs, and returns a list with two slots p.value and e.sign,
which are vectors of p-values and signs of the effects.}

\item{perm.func}{a function that performs the permutation tests. It takes X,
Y and ... as the inputs, and returns a list with two slots X and Y, which
contain the permuted data.}

\item{eff.sign}{a logical value indicating whether the direction of the
effects should be considered. If it is true (default), negative and positive
effects provide conflicting information.}

\item{B}{the number of permutations. The default is 20. If computation time
is not a big concern, B=100 is suggested to achieve excellent reproducibility
between different runs.}

\item{q.cutoff}{the quantile cutoff to determine the feature sets to
estimate the number of false positives under the null. This cutoff is to
protect the signal part of the distributions. The default is 0.5.}

\item{alpha}{the exponent applied to the distance matrix. Large values have
more smoothing effects for closely related species. The default is 1. If the
underlying structure assumption is considered to be very strong, robustness
can be improved by decreasing the value to 0.5.}

\item{...}{further arguments such as covariates to be passed to test.func}
}
\value{
A list composed of \code{p.adj}, \code{p.unadj}, \code{z.adj},
\code{z.unadj}, \code{k} and \code{rho}.
}
\description{
Modification of \code{StructFDR::TreeFDR} function to remove safety nets.
}
