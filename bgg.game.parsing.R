source("./include/bgg.get.R")

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

bgg.complete <- bgg.get(games.id, .progress = create_progress_bar("text"))


#### clean noise and irrilevant data
bgg.complete <- bgg.complete[ , -which(names(bgg.complete) %in% colswithallmiss)]

bgg.complete <- bgg.complete[ , -grep("poll.results.suggested_numplayers.([2-9][0-9]|1[3-9])", colnames(bgg.complete))]
bgg.complete <- bgg.complete[ , -grep("poll.results.suggested_numplayers.[0-9]+\\.", colnames(bgg.complete))]
bgg.complete <- bgg.complete[ , -which(names(bgg.complete) %in% c("details.image", "details.thumbnail", "attributes.t.fg.2....", "stats.median.value"))]
bgg.complete <- bgg.complete[ , -grep("poll.results.suggested_playerage.[0-9]+\\.numvotes", colnames(bgg.complete))]

# ix = grep("^poll.results.suggested_numplayers.(\\d+).*", names(bgg.complete))
# numbers = as.numeric(gsub("^poll.results.suggested_numplayers.(\\d+).*", "\\1", names(bgg.complete)[ix]))
# 
# m = matrix(numbers, ncol=length(ix), nrow=nrow(bgg.complete), byrow=T)
# bgg.complete[ix][!(m>=bgg.complete$details.minplayers)] <-NA

# Column reordering
#
# (function(x) x + 3)(10)
bgg.order <- (function(x) {
  cols <- c("^([^.]+)$", "^(details)", "^(attributes)", "^(stats)", "^(poll)")
  s <- names(x)
  n <- seq_along(cols)
  for(i in n) s <- sub(cols[i], paste0(n[i], "\\1"), s)
  new_vec <- substr(s[order(s)], 2, nchar(s[order(s)]))
  new_vec
})(bgg.complete)

bgg.clean <- bgg.complete[bgg.order]

# Converting to proper types
# by hand because i'm lazy...

idx <- grep("poll.results.(suggested_numplayers.[0-9]+|language_dependence)$", colnames(bgg.clean))

lapply(idx, function(x){
  bgg.clean[,x] <- as.factor(bgg.clean[,x])
})
bgg.clean$details.maxplayers <- as.numeric(bgg.clean$details.maxplayers)
bgg.clean$details.minplayers <- as.numeric(bgg.clean$details.minplayers)
bgg.clean$details.maxplaytime <- as.numeric(bgg.clean$details.maxplaytime)
bgg.clean$details.minage <- as.numeric(bgg.clean$details.minage)
bgg.clean$details.maxplaytime <- as.numeric(bgg.clean$details.maxplaytime)
bgg.clean$details.minplaytime <- as.numeric(bgg.clean$details.minplaytime)
bgg.clean$details.playingtime <- as.numeric(bgg.clean$details.playingtime)

bgg.clean$details.yearpublished <- as.factor(bgg.clean$details.yearpublished)

bgg.clean 

write.csv2(bgg.complete, "./data/bgg.dataset.csv", row.names = FALSE, fileEncoding = "utf8")
