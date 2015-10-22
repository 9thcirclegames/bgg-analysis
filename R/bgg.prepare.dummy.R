#' Prepare the dataset for the mining stuff
#'
#' This function will create the dummy variables from category and mechanic
#'
#' @param bgg.dataset the BoardGames dataset object
#'
#' @importFrom splitstackshape cSplit_e
#' @importFrom dplyr select
#'
#' @export
bgg.prepare.dummy <- function(bgg.dataset){

  bgg.dataset.dummy <- cSplit_e(bgg.dataset, "attributes.boardgamecategory", type="character", fill=0, drop=TRUE)
  bgg.dataset.dummy <- cSplit_e(bgg.dataset.dummy, "attributes.boardgamemechanic", type="character", fill=0, drop=TRUE)

  colnames(bgg.dataset.dummy) <- gsub(" ", "", colnames(bgg.dataset.dummy))
  colnames(bgg.dataset.dummy) <- gsub("/", "-", colnames(bgg.dataset.dummy))
  colnames(bgg.dataset.dummy) <- gsub("_", ".", colnames(bgg.dataset.dummy))

  return(bgg.dataset.dummy)

}

