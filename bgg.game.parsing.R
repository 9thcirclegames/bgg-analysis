require(dplyr)

source("./include/bgg.get.R")

source("./include/parse.meta.details.R")
source("./include/parse.main.details.R")
source("./include/parse.attributes.R")
source("./include/parse.statistics.R")
source("./include/parse.polls.R")

loc <- stri_replace_all_charclass(
  xmlToDataFrame(
    xmlParse(getURL("https://www.boardgamegeek.com/sitemapindex", ssl.verifypeer=FALSE)
    )
  )$loc,
  "\\p{WHITE_SPACE}", "")

loc.games <- loc[grepl("sitemap_geekitems_boardgame_page_", loc)]

games.sitemap <- do.call(
  rbind.fill, lapply(loc.games, function(x) {
    
    xmlToDataFrame(xmlParse(getURL(x, ssl.verifypeer=FALSE)))
  })
)

games.id <- as.character(do.call(rbind, lapply(strsplit(as.character(games.sitemap$loc), "/"), function(x) { x[5] })))

bgg.complete <- bgg.get(games.id, parsers = list(game=parse.meta.details,
                                                 details=parse.main.details,
                                                 attributes=parse.attributes,
                                                 stats=parse.statistics,
                                                 polls=parse.polls),
                        .progress = create_progress_bar("text"))

# Drop poll votes
bgg.clean <- select(bgg.complete, -matches("(totalvotes|numvotes)$"))

write.csv2(bgg.clean, "./data/bgg.dataset.csv", row.names = FALSE, fileEncoding = "utf8")
