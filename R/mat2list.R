#' Matrix to list
#'
#' @param mat matrix
#'
#' @return a list
#' @export
#'
#' @examples
mat2list <- function(mat){
  as.list(as.data.frame(t(mat)))
}
