#' Convert a similarity matrix into a distance matrix
#' @description Convert a similarity matrix into a distance matrix.
#' @param sim a similarity matrix of class \code{"matrix"} or \code{"dist"}
#' @param transformation function or character.
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
#' sim2dist(as.dist(sim), transformation = "1-absx")
sim2dist <- function(sim, transformation = "1-x"){
  if(is.character(transformation)){
    if(transformation == "1-x") return(1-sim)
    if(transformation == "1-absx") return(1-abs(sim))
  } else {
    return(transformation(sim))
  }
}
