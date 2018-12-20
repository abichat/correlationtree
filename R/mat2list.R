#' Matrix to list
#' @description Convert a matrix of abundance to a list.
#' @param mat matrix
#' @return a list
#' @export
#' @examples
#' m <- matrix(1:12, ncol = 3)
#' rownames(m) <- letters[1:4]
#' mat2list(m)
mat2list <- function(mat){
  as.list(as.data.frame(t(mat)))
}
