require(XML)
require(RCurl)
require(plyr)
require(beepr)
require(stringi)
require(readr)

source("./include/bgg.get.R")

loc <- stri_replace_all_charclass(
  xmlToDataFrame(
    xmlParse(getURL("https://www.boardgamegeek.com/sitemapindex", ssl.verifypeer=FALSE)
    )
  )$loc,
  "\\p{WHITE_SPACE}", "")

loc.games <- loc[grepl("sitemap_geekitems_boardgame_page_", loc)]

games.sitemap2 <- do.call(
  rbind.fill, lapply(loc.games, function(x) {

    xmlToDataFrame(xmlParse(getURL(x, ssl.verifypeer=FALSE)))
  })
)

games.id <- as.character(do.call(rbind, lapply(strsplit(as.character(games.sitemap$loc), "/"), function(x) { x[5] })))

bgg.complete <- bgg.get(games.id, .progress = create_progress_bar("text"))


