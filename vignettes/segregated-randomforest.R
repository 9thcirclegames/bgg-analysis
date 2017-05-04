################
# REQS         #
################
lapply(c("dplyr", "ggplot2", "caret", "dummies", "arulesViz", "RColorBrewer", "plyr"), function(pkg){
  if(! require(pkg, character.only = TRUE)) install.packages(pkg, depend = TRUE)
  library(pkg, character.only = TRUE)
})

if(! require(dendextend)) {
  devtools::install_github('talgalili/dendextend')
  devtools::install_github('talgalili/dendextendRcpp')
}
require(dendextend)

require(bggAnalysis)

data("BoardGames")

brewer.palette.top <- "Spectral"

#########################################
# Prepare Data Sets                     #
#########################################
bgg.useful.predictions <- BoardGames %>%
  filter(is.na(details.yearpublished) | details.yearpublished <= 2017) %>%
  filter(game.type == "boardgame") %>%
  bgg.prepare.data() %>%
  filter(stats.usersrated >= 50)

rownames(bgg.useful.predictions) <- make.names(bgg.useful.predictions$details.name, unique=TRUE)

bgg.useful.predictions <- bgg.prepare.dummy(bgg.useful.predictions)

bgg.useful.predictions$stats.average.factor <- discretize(bgg.useful.predictions$stats.average, method="frequency", categories = 6, ordered = TRUE)

bgg.useful.predictions <- cbind(
  bgg.useful.predictions
  ,dummy("details.minplayers.factor", bgg.useful.predictions, sep="=")
  ,dummy("details.maxplayers.factor", bgg.useful.predictions, sep="=")
  ,dummy("details.playingtime.factor", bgg.useful.predictions, sep="=")
  ,dummy("details.minage.factor", bgg.useful.predictions, sep="=")
  ,dummy("stats.weight.factor", bgg.useful.predictions, sep="=")
  ,dummy("stats.average.factor", bgg.useful.predictions, sep="=")
  ,dummy("polls.language.dependence", bgg.useful.predictions, sep="=")
)

bgg.good.variance <- bgg.useful.predictions[, -nearZeroVar(bgg.useful.predictions, saveMetrics= FALSE)]
