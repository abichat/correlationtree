#' Shuffle tip labels
#' @description Shuffle tip labels of a \code{"phylo"} object
#' @param tree an object of class \code{"phylo"}
#' @return The same tree with permuted tip labels.
#' @export
#' @examples
#' (tree <- ape::rtree(8))
#' shuffle_tiplabels(tree)
shuffle_tiplabels <- function(tree){
  tree$tip.label <- sample(tree$tip.label)
  return(tree)
}
