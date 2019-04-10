

#' \code{n*(n-1)/2} unique combinations
#'
#' @param list list
#'
#' @return a data.frame
#' @importFrom dplyr mutate filter select row_number
#' @importFrom rlang .data
pairs_grid <- function(list) {
  n <- length(list)
  ind <- which(lower.tri(matrix(nrow = n, ncol = n)))

  expand.grid(list, list) %>%
    mutate(rn = row_number()) %>%
    filter(.data$rn %in% ind) %>%
    select(-.data$rn)
}

#' Transform a \code{n*(n-1)/2} vector into a distance.
#'
#' @param vec vector
#'
#' @return \code{dist} object
#' @importFrom stats as.dist
#'
#' @examples
vec2dist <- function(vec) {
  m <- length(vec)
  n <- (1 + sqrt(1 + 8 * m)) / 2
  stopifnot(round(n) == n)

  mat <- matrix(nrow = n, ncol = n)
  mat[lower.tri(mat)] <- vec
  as.dist(mat)
}

#' Billera-Holmes-Vogtmann distance
#'
#' Wrapper around \code{distory::dist.multiPhylo()}.
#'
#' Could be used with
#'
#' @param multiphylo a multiphylo object
#'
#' @return
#' @importFrom distory dist.multiPhylo
#' @importFrom furrr future_map2 future_map_dbl
#' @importFrom dplyr mutate pull
#' @importFrom rlang .data
#' @export
#'
#' @examples
future_dist_BHV <- function(multiphylo) {
  multiphylo %>%
    pairs_grid() %>%
    mutate(trees = future_map2(.data$Var1, .data$Var2, c),
           dist = future_map_dbl(.data$trees, dist.multiPhylo)) %>%
    pull(.data$dist) %>%
    vec2dist()
}
