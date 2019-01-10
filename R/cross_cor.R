#' Pairwise correlations
#' @description Compute pairwise correlations within a list of vector.
#' @param L the list of all vector to be compared.
#' @param remove logical. If \code{TRUE} (default), shared zeros are removed.
#' @param .names character.
#' @param ... arguments to be passed to \code{\link[stats]{cor}} such as \code{method} (see details).
#' @return A similarity matrix of class \code{dist} with all correlation between vectors.
#' @export
#' @importFrom purrr map
#' @importFrom stats as.dist cor
#' @details For \code{method}: one of "pearson" (default), "kendall", or "spearman".
#' @examples
#' df <- data.frame(Clade = letters[1:4],
#'                  v = c(3, 0, 0, 3),
#'                  w = c(4, 5, 7, 0),
#'                  x = c(8, 5, 5, 0),
#'                  y = c(1, 8, 0, 0),
#'                  z = c(3, 5, 2, 2))
#' L <- mat2list(df2mat(df))
#' cross_cor(L, remove = TRUE, method = "spearman")
cross_cor <- function(L, remove = TRUE, .names = names(L), ...){

  if(is.null(.names)) .names <- paste0("vec", seq_along(L))

  if(remove){
    cors <- map(L,
                function(x, y) {vapply(y,
                                       function(y, x) cor_wo_shared_zero(x, y,
                                                                         ...),
                                       x = x, double(1))},
                y = L)
  } else {
    cors <- map(L,
                function(x, y) {vapply(y,
                                       function(y, x) cor(x, y, ...),
                                       x = x, double(1))},
                y = L)
  }
  names(cors) <- .names
  return(as.dist(as.data.frame(cors)))
}
