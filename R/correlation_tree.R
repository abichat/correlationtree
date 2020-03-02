#' Correlation tree
#' @description Compute the correlation tree from an abundance table.
#' @param table a dataframe or a matrix.
#' @param col integer giving the column to be passed as row names.
#' @param matrix logical. Is table a matrix? Default to \code{FALSE}.
#' @param remove logical. If \code{TRUE} (default), shared zeros between
#' species are removed for the computation of the pairwise correlation
#' coefficients.
#' @param fill logical. If \code{TRUE}, \code{NA} are set to 0. Default
#' to \code{FALSE}.
#' @inheritParams sim2dist
#' @param ... arguments to be passed to \code{\link[stats]{cor}} such
#' as \code{method} (see details).
#' @return \code{correlation_tree} returns a "phylo" object.
#' @details For \code{method}: one of "pearson" (default), "kendall",
#' or "spearman".
#' @export
#' @importFrom stats hclust
#' @importFrom ape as.phylo
#' @examples
#' df <- data.frame(Clade = letters[1:4],
#'                  v = c(3, 0, 0, 3),
#'                  w = c(4, 5, 7, 0),
#'                  x = c(8, 5, 5, 0),
#'                  y = c(1, 8, 0, 0),
#'                  z = c(3, 5, 2, 2))
#' plot(correlation_tree(df, method = "spearman"))
correlation_tree <- function(table, col = 1, matrix = FALSE,
                             remove = TRUE, fill = FALSE,
                             transformation = "1-x", ...){
  if(matrix) col <- 0
  L <- mat2list(df2mat(table, col))
  sim <- cross_cor(L, remove, ...)

  if (fill) {
    sim[is.na(sim)] <- 0
  }

  hc <- hclust(sim2dist(sim, transformation))
  return(as.phylo(hc))
}
