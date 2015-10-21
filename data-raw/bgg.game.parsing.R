require("bggAnalysis")

parsers = list(game=parse.meta.details,
               details=parse.main.details,
               attributes=parse.attributes,
               stats=parse.statistics,
               polls=parse.polls)

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
BoardGames <- select(bgg.complete, -matches("(totalvotes|numvotes)$"))
