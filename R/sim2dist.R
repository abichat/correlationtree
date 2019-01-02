#' Convert a similarity matrix into a distance matrix
#' @description Convert a similarity matrix into a distance matrix.
#' @param sim a similarity matrix of class \code{"matrix"} or \code{"dist"}
#' @param transformation character. See details.
#' @return \code{sim2dist} returns an object of the same class as the input \code{sim}.
#' @export
#' @importFrom stats runif
#' @examples
#' n <- 4
#' mat <- matrix(runif(n^2,-1, 1), ncol = n)
#' sim <- (mat + t(mat)) / 2
#' diag <- n * (seq_len(n) - 1) + seq_len(n)
#' sim[diag] <- 1
#' sim2dist(sim)
#' sim2dist(as.dist(sim), transformation = "log")
sim2dist <- function(sim, transformation = c("reverse", "abs", "log")){
  transformation <- match.arg(transformation)
  if(transformation == "reverse") return(1-sim)
  if(transformation == "abs") return(1-abs(sim))
  if(transformation == "log") return(-log(sim^2))
}
