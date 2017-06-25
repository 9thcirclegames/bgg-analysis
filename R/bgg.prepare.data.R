#' Prepare the dataset for the mining stuff
#'
#' This function prepares the dataset removing unuseful character attributes,
#' removing videogames (and videogames-only attributes).\cr\
#' These are the performed operations:
#' \itemize{
#' \item It removes the \code{details.name} attribute, as it's a redundand ambiguous identifier,
#' and the \code{stats.median} column, as it seems to be broken in BGG XMLAPI2.
#' \item Some exploratory analysis bring up that lot of games have yearpublished=0: this is probably a missing
#' set to zero by mistake, so we put to NA (we are aware that we have some very ancient games in the dataset, but we
#' consider them as outliers).
#' \item Finally, we removed games with \code{yearpublished < 1959} (.05 percentile and really unuseful).\cr
#' We are aware that there are some games with \code{yearpublished > 2015}, but we decided to keep those
#' yet unreleased games as they could be useful as test set.
#'}
#' @param bgg.dataset the BoardGames dataset object to prepare
#'
#' @importFrom splitstackshape cSplit_e
#' @importFrom dplyr select "%>%" one_of contains
#' @importFrom plyr round_any
#'
#' @export
bgg.prepare.data <- function(bgg.dataset = BoardGames){

  # I really don't want videogames in my DB
  videogames.id <- (function(x){
    if(NROW(which(colnames(BoardGames)=="stats.family.amiga.pos"))>0){
      return(
        unique(c(
          which(!is.na(bgg.dataset$stats.family.amiga.pos)),
          which(!is.na(bgg.dataset$stats.family.arcade.pos)),
          which(!is.na(bgg.dataset$stats.family.atarist.pos)),
          which(!is.na(bgg.dataset$stats.family.commodore64.pos)))
        )
      ) }
    else return(NULL)
  })(bgg.dataset)
  
  bgg.dataset <- bgg.dataset[-videogames.id,]

  bgg.dataset <- select(bgg.dataset, -one_of("details.image", "details.thumbnail", "details.description", "stats.median"))
  bgg.dataset <- select(bgg.dataset, -contains("stats.family.amiga"),
               -contains("stats.family.arcade"),
               -contains("stats.family.atarist"),
               -contains("stats.family.commodore64"),
               -contains("stats.subtype.videogame")
  )

  # Zero value in some columna are missing, so I convert to NA
  bgg.dataset[which(as.numeric(bgg.dataset$details.yearpublished)==0),]$details.yearpublished <- NA
  bgg.dataset[which(as.numeric(bgg.dataset$stats.average)==0),]$stats.average <- NA

  # I'm going to filter out games that were released before 1959 or not yet released
  # We're going to loose only 5% of the dataset
  summary(as.numeric(bgg.dataset$details.yearpublished))
  quantile(as.numeric(bgg.dataset$details.yearpublished), seq(0, 1, 0.05), na.rm = TRUE)

  bgg.dataset <- bgg.dataset %>% filter(
    (as.numeric(details.yearpublished) >= 1959) |
      is.na(details.yearpublished)
  )

  # Some useful discretizations
  bgg.dataset$stats.weight.factor <- as.factor(round_any(bgg.dataset$stats.averageweight, 1))
  bgg.dataset$details.playingtime <- round_any(bgg.dataset$details.playingtime, 15)

  bgg.dataset$details.playingtime.factor <- as.factor(ifelse(bgg.dataset$details.playingtime >= 240, "240+", as.character(bgg.dataset$details.playingtime)))
  bgg.dataset$details.minplayers.factor <- as.factor(ifelse(bgg.dataset$details.minplayers >= 8, "8+", as.character(bgg.dataset$details.minplayers)))
  bgg.dataset$details.maxplayers.factor <- as.factor(ifelse(bgg.dataset$details.maxplayers >= 15, "15+", as.character(bgg.dataset$details.maxplayers)))
  bgg.dataset$details.minage.factor <- as.factor(ifelse(bgg.dataset$details.minage >= 21, "21+", as.character(bgg.dataset$details.minage)))


  return(bgg.dataset)

}

