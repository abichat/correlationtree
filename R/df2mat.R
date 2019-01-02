#' Dataframe to matrix
#' @description Convert an abundance dataframe to a named matrix.
#' @param df a data frame.
#' @param col integer giving the column to be passed as row names.
#' @return \code{df2mat} returns a named matrix.
#' @details Use \code{col = 0} if there are already row names (or use \code{as.matrix(df)} directly).
#' @export
#' @examples
#' df <- data.frame(Clade = letters[1:4], S1 = 1:4, S2 = 7:8)
#' df2mat(df)
df2mat <- function(df, col = 1){
  if (col == 0) {
    return(as.matrix(df))
  } else {
    mat <- as.matrix(df[, -col])
    rownames(mat) <- df[[col]]
    return(mat)
  }
}
