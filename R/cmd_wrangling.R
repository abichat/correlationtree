#' Filter at specific level
#' @description Filter a dataset with CMD formatting
#' @param dataset dataframe.
#' @param level character. One of \code{c("kingdom", "phylum", "class",
#' "order", "family", "genus", "species", "strain")}.
#' @return dataframe with only desired level entries.
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


#' Create taxonomic table
#' @description Create a taxonomic table from CMD format for clades.
#' @param x character. The vector of clades with CMD format.
#' @return \code{compute_cmd_taxtable()} returns a data.frame.
#' @export
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom purrr map
#' @importFrom purrr transpose
#' @examples
compute_cmd_taxtable <- function(x){
  levels <- c("kingdom", "phylum", "class", "order", "family",
             "genus", "species", "strain")

  N <- str_count(x, "\\|")
  stopifnot(all(N == N[1]))

  list <- str_split(str_remove_all(unique(x), "\\|"), ".__")
  list <- map(list, ~ .[-1])

  list <- map(transpose(list, .names = levels[1:(N[1] + 1)]), unlist)
  return(as.data.frame(list))
}


#' Clean clade
#' @description Returns only the last level name.
#' @param x character. The vector of clades with CMD format.
#' @return \code{clean_cmd_clade()} returns a character with
#' same length as \code{x}.
#' @export
#' @importFrom stringr str_remove
#' @importFrom stringr str_count
#' @examples
clean_cmd_clade <- function(x){

  stopifnot(all(str_count(x, "\\|") == str_count(x[1], "\\|")))

  return(str_remove(x, ".*__"))
}



