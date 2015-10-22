#' Prepare the dataset for the mining stuff
#'
#' This function will prepare the dataset removing unuseful character attributes,
#' removing videogames (and videogames-only attributes).
#' It also remove the 'details.name' attribute and the 'stats.median' column,
#' as it seems to be broken in BGG XMLAPI2.
#' Some exploratory analysis bring us that lot of games have yearpublished=0:
#' this is definitively a missing we put to NA.
#' Then, we removed games with yearpublished < 1959 (in the .05 percentile and really unuseful)
#' and > 2015 (games not released yet)
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

  bgg.dataset <- select(bgg.dataset, -one_of("details.image", "details.thumbnail", "details.description", "stats.median"))
  bgg.dataset <- select(bgg.dataset, -contains("stats.family.amiga"),
               -contains("stats.family.arcade"),
               -contains("stats.family.atarist"),
               -contains("stats.family.commodore64"),
               -contains("stats.subtype.videogame")
  )

  # Zero value in yearpublished column is a missing, so I convert to NA
  bgg.dataset[which(as.numeric(bgg.dataset$details.yearpublished)==0),]$details.yearpublished <- NA

  # Same for minage, where we're going to put to NA if it's > than 25, too
  bgg.dataset[which(bgg.dataset$details.minage==0 | bgg.dataset$details.minage > 25),]$details.minage <- NA

  # I'm going to filter out games that were released before 1959 or not yet released
  # We're going to lose only 5% of the dataset
  summary(as.numeric(bgg.dataset$details.yearpublished))
  quantile(as.numeric(bgg.dataset$details.yearpublished), seq(0, 1, 0.05), na.rm = TRUE)

  bgg.dataset <- bgg.dataset %>% filter(
    ((as.numeric(details.yearpublished) >= 1959) &
       (as.numeric(details.yearpublished) <= 2015)) |
      is.na(details.yearpublished)
  )

  return(bgg.dataset)

}

