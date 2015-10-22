#' Prepare the dataset for the mining stuff
#' 
#' This function will prepare the dataset removing unuseful character attributes,
#' removing videogames (and videogames-only attributes) and finally creating the dummyvars
#' for boardgame category and mechanics.
#' It also remove the 'details.name' attribute (you can always merge with the BoardGame dataset to retrive it)
#' and the 'stats.median' column, as it seems to be broken in BGG XMLAPI2.
#' 
#' @param bgg.dataset the BoardGames dataset object
#' 
#' @importFrom splitstackshape cSplit_e
#' @importFrom dplyr select
#' 
#' @export
bgg.prepare.data <- function(bgg.dataset){
  
  # I really don't want videogames in my DB
  videogames.id <- unique(c(
    which(!is.na(bgg.dataset$stats.family.amiga.pos)),
    which(!is.na(bgg.dataset$stats.family.arcade.pos)),
    which(!is.na(bgg.dataset$stats.family.atarist.pos)),
    which(!is.na(bgg.dataset$stats.family.commodore64.pos))
  )
  )
  
  bgg.dataset <- bgg.dataset[-videogames.id,]
  
  bgg.dataset <- select(bgg.dataset, -one_of("details.image", "details.thumbnail", "details.description"))
  bgg.dataset <- select(bgg.dataset, -contains("stats.family.amiga"),
               -contains("stats.family.arcade"),
               -contains("stats.family.atarist"),
               -contains("stats.family.commodore64")
  )
  
  # This column doesn't work atm, so I'm leaving it out
  bgg.dataset <- select(bgg.dataset, -one_of("stats.median"))
  
  bgg.dataset.dummy <- cSplit_e(bgg.dataset, "attributes.boardgamecategory", type="character", fill=0)
  bgg.dataset.dummy <- cSplit_e(bgg.dataset.dummy, "attributes.boardgamemechanic", type="character", fill=0)
  
  colnames(bgg.dataset.dummy) <- gsub(" ", "", colnames(bgg.dataset.dummy))
  colnames(bgg.dataset.dummy) <- gsub("/", "-", colnames(bgg.dataset.dummy))
  colnames(bgg.dataset.dummy) <- gsub("_", ".", colnames(bgg.dataset.dummy))
  
  return(bgg.dataset.dummy)
  
}

