require("bggAnalysis")
require("stringi")
require("XML")
require("RCurl")

source("./data-raw/bgg.cache.R")
#source("./data-raw/bgg.cache.XML.R")
source("./data-raw/bgg.get.R")
source("./data-raw/parse.attributes.R")
source("./data-raw/parse.main.details.R")
source("./data-raw/parse.meta.details.R")
source("./data-raw/parse.polls.R")
source("./data-raw/parse.statistics.R")

parsers = list(game=parse.meta.details,
               details=parse.main.details,
               attributes=parse.attributes,
               stats=parse.statistics,
               polls=parse.polls)

#################################
# Get the lists of games to parse
# -------------------------------
#
# Since the bgg API doesn't have an endopoint to get the list of games
# parse the sitemap to get a vactors of all the IDs
loc <- stri_replace_all_charclass(
  xmlToDataFrame(
    xmlParse(getURL("https://www.boardgamegeek.com/sitemapindex", ssl.verifypeer=FALSE)
    )
  )$loc,
  "\\p{WHITE_SPACE}", "")

loc.games <- loc[grepl("sitemap_geekitems_boardgame_page_", loc)]

games.sitemap <- do.call(
  rbind.fill, lapply(loc.games, function(x) {

    xmlToDataFrame(xmlParse(getURL(x,
                                   ssl.verifypeer=FALSE,
                                   .opts=curlOptions(followlocation = TRUE)
                                     
                                   )
                            )
    )
  })
)

games.id <- as.character(do.call(rbind, lapply(strsplit(as.character(games.sitemap$loc), "/"), function(x) { x[5] })))

# Removing invalid entries
games.id <- games.id[which(games.id != "189330")]

bgg.complete <- bgg.get(games.id, parsers = parsers, .progress = create_progress_bar("text"))

# Drop poll votes
BoardGames <- select(bgg.complete, -matches("(totalvotes|numvotes)$"))
