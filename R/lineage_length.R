#' Lineage length
#' @description Compute the distance from the root for a leaf.
#' @param tree a "phylo" object.
#' @param label tip label.
#' @return A double.
#' @export
#' @importFrom ape read.tree
#' @examples
#' tree <- ape::read.tree(text = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
#' lineage_length(tree, "A")
#' lineage_length(tree, "D")
lineage_length <- function(tree, label){
  N <- length(tree$tip.label)

  tip <- which(tree$tip.label == label)
  e <- which(tree$edge[, 2] == tip)
  parent <- tree$edge[e, 1]
  sum <- tree$edge.length[e]

  while (parent > N+1) {
    e <- which(tree$edge[, 2] == parent)
    parent <- tree$edge[e, 1]
    sum <- sum + tree$edge.length[e]
  }
  return(sum)
}

#' Mean lineage length
#' @description Compute the mean distance from the root to leafs.
#' @param tree a "phylo" object.
#' @param ultrametric logical. Is \code{tree} ultrametric? (see details)
#' @return A double.
#' @export
#' @importFrom ape read.tree
#' @importFrom purrr map_dbl
#' @details A tree is ultrametric when the distances from the root to
#' every branch tip are equal.
#' @examples
#' tree <- ape::read.tree(text = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
#' lineage_length(tree, "A")
mean_lineage_length <- function(tree, ultrametric = FALSE) {
  if (ultrametric) {
    L <- lineage_length(tree, tree$tip.label[1])
    return(L)
  } else {
    L <- vapply(tree$tip.label, function(x) lineage_length(tree, x),
                FUN.VALUE = double(1))
    return(mean(L))
  }
}
