parse.statistics <- function(xml){
  
  collection <- xmlParse(xml, asText=TRUE)
  
  items <- xmlToList(xmlRoot(collection))
  items[[length(items)]] <- NULL

  game.stats <- do.call(rbind.fill, lapply(items, function(item){
    item.stats <- as.matrix(unlist(item$statistics$ratings))
    return(as.data.frame(t(item.stats[which(!grepl("^ranks", row.names(item.stats))),]), stringsAsFactors=FALSE)) 
  }))
  
  indx <- sapply(game.stats, is.character)
  game.stats[indx] <- lapply(game.stats[indx], function(x) as.numeric(x))
  
  names(game.stats) <- paste("stats", names(game.stats), sep=".")
  
  return(game.stats)
  
}