#' Filter at sepecific level
#' @description Filter a dataset with CMD formatting
#' @param dataset dataframe.
#' @param level characher. One of \code{c("kingdom", "phylum", "class",
#' "order", "family", "genus", "species", "strain")}.
#'
#' @return
#' @export
#' @importFrom dplyr filter
#' @importFrom stringr str_sub
#' @importFrom stringr str_detect
#' @examples
filter_cmd_level <- function(dataset,
                         level = c("kingdom", "phylum", "class", "order",
                                   "family", "genus", "species", "strain")) {

  level <- match.arg(level)

  letter <- ifelse(level == "strain", "t", str_sub(level, end = 1))

  filter(dataset, str_detect(Clade, paste0(letter, "__[a-zA-Z0-9_]*$")))
}




