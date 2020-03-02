#' Correlation without shared zeros
#' @description Remove positions where \code{x=y=0} and then compute
#' correlation.
#' @param x a numeric vector with positive or null entries.
#' @param y a numeric vector with positive or null entries.
#' @param ... arguments to be passed to \code{\link[stats]{cor}} such as
#' \code{method} (see details).
#' @return The estimated correlation coefficient between \code{x} and \code{y}
#' after removing shared zeros.
#' @export
#' @importFrom stats cor
#' @details For \code{method}: one of "pearson" (default), "kendall", or
#' "spearman".
#' @examples
#' x <- c(0, 7, 5, 0, 2)
#' y <- c(8, 0, 0, 0, 2)
#' cor_wo_shared_zero(x, y, method = "spearman")
#' cor(x[-4], y[-4], method = "spearman")
cor_wo_shared_zero <- function(x, y, ...){
  cor(x[x != 0 | y != 0], y[x != 0 | y != 0], ...)
}
