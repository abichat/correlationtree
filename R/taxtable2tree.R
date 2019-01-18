#' Taxonomic table to taxonomic tree
#' @description Compute a taxonomic tree of class phylo from taxonomic table.
#' @param df dataframe.
#' @param collapse logical. Should node with one child be vanished?
#' @param lineage_length double. Mean lineage length.
#' @param root character. Name of the root.
#'
#' @return \code{taxtable2tree()} returns a "phylo" object.
#' @export
#' @importFrom ape collapse.singles
#' @importFrom ape write.tree
#' @importFrom ape read.tree
#' @importFrom stats na.omit
#' @examples
taxtable2tree <- function(df, collapse = TRUE, lineage_length = 1, root = ""){

  ## Convert to data.frame with factor columns

  df <- as.data.frame(apply(df, 2, as.factor))

  ## Remove NA columns

  na_col <- apply(df, 2, function(x) all(is.na(x)))
  df <- df[, !na_col]

  ## Remove rows that contains NA

  df <- na.omit(df)

  ## Number of levels

  nlvl <- ncol(df)

  ## Create a root if necessary

  if(length(unique(df[, 1])) >= 2){

    df[, nlvl+1] <- as.factor(root)
    df <- df[, c(nlvl+1, 1:nlvl)]

    nlvl <- nlvl + 1
  }

  ## Labels & convert factors to unique integer

  # tiplab <- as.character(df[[ncol(df)]])
  tiplab <- levels(df[[ncol(df)]])
  df[[ncol(df)]] <- as.numeric(df[[ncol(df)]])

  count <- length(tiplab)
  nodelab <- character()

  for(i in 1:(nlvl-1)){

    nodelab <- c(nodelab, levels(df[[i]]))

    df[[i]] <- as.numeric(df[[i]]) + count

    count <- max(df[[i]])
  }

  alllab <- c(tiplab, nodelab)


  ## Edgelist

  el <- as.matrix(df[, 1:2])

  if(nlvl > 2){
    for(i in 2:(nlvl-1)){
      el <- rbind(el, as.matrix(df[, i:(i+1)]))
    }
  }

  el <- unique(unname(el))


  ## Tree

  tree <- list(edge = el, tip.label = tiplab,
               Nnode = length(nodelab), node.label = nodelab,
               edge.length = rep(lineage_length/(nlvl-1), nrow(el)))
  class(tree) <- "phylo"

  tree <- read.tree(text = write.tree(tree))

  ## Collapse

  if(collapse){
    tree <- collapse.singles(tree)
  }

  # Add a root

  tree$root.edge <- 0

  # Return

  tree
}
