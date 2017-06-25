#' Decode dummy variables
#'
#' This function will create the dummy variables for some attributes (game category and game mechanic atm).
#' It doesn't drop the original columns.
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
  bgg.dataset.dummy <- cSplit_e(bgg.dataset.dummy, "attributes.boardgamefamily", type="character", fill=0, drop=TRUE)
  bgg.dataset.dummy <- cSplit_e(bgg.dataset.dummy, "attributes.boardgameimplementation", type="character", fill=0, drop=TRUE)

  colnames(bgg.dataset.dummy) <- gsub(" ", "", colnames(bgg.dataset.dummy))
  colnames(bgg.dataset.dummy) <- gsub("/", "-", colnames(bgg.dataset.dummy))
  colnames(bgg.dataset.dummy) <- gsub("_", ".", colnames(bgg.dataset.dummy))

  return(bgg.dataset.dummy)

}

