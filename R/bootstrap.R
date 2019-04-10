#' Variable bootstrap
#' @description
#' @param table a dataframe or matrix
#' @param matrix logical
#' @param col integer. The column to keep
#'
#' @return A table.
#' @export
#'
#' @examples
#' sample_boot(swiss)
sample_boot <- function(table, matrix = FALSE, col = 1){

  if(col == 0 | matrix){
    newcol <- sample(ncol(table), replace = TRUE)
  } else {
    newcol <- c(col, sample(seq_along(table)[-col], replace = TRUE))
  }

  return(table[, newcol])
}

