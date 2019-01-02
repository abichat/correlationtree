#' Matrix to list
#' @description Convert a named matrix of abundance to a named list.
#' @param mat matrix.
#' @return \code{mat2list} returns a named list whose each entry is a row of the original matrix.
#' @export
#' @examples
#' m <- matrix(1:12, ncol = 3)
#' rownames(m) <- letters[1:4]
#' mat2list(m)
mat2list <- function(mat){
  as.list(as.data.frame(t(mat)))
}
