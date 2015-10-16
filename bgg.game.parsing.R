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
    
    file.hd <- NULL
    
    sitemap.filename <- unlist(strsplit(x, "/"))[[4]]
    sitemap.local <- paste0("./data/sitemap/", sitemap.filename, ".xml", sep="")
    

    if(file.exists(sitemap.local)){
      file.hd <- read_file(sitemap.local)
    } else {
      file.hd <- getURL(x, ssl.verifypeer=FALSE)
      fileConn<-file(sitemap.local)
      writeLines(file.hd, fileConn)
      close(fileConn)
    }

    xmlToDataFrame(xmlParse(file.hd))
  })
)

games.id <- as.character(do.call(rbind, lapply(strsplit(as.character(games.sitemap$loc), "/"), function(x) { x[5] })))

bgg.complete <- bgg.get(games.id, .progress = create_progress_bar("text"))


