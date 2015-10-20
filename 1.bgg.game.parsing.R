source("./include/bgg.get.R")

source("./include/parse.meta.details.alternate.R")
source("./include/parse.main.details.alternate.R")
source("./include/parse.attributes.alternate.R")
source("./include/parse.statistics.alternate.R")
source("./include/parse.polls.alternate.R")

require(dplyr)
parsers = list(game=parse.meta.details.alternate,
               details=parse.main.details.alternate,
               attributes=parse.attributes.alternate,
               stats=parse.statistics.alternate,
               polls=parse.polls.alternate)

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

bgg.complete <- bgg.get(games.id, parsers = parsers, .progress = create_progress_bar("text"))

# Drop poll votes
bgg.dataset <- select(bgg.dataset, -matches("(totalvotes|numvotes)$"))
bgg.dataset <- select(bgg.dataset, -one_of("details.image", "details.description", "details.thumbnail"))

write.csv2(bgg.dataset, "./data/bgg.dataset.csv", row.names = FALSE, fileEncoding = "utf8")
