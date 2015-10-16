parse.main.details <- function(collection){
  
  fields <- c("image",
              "thumbnail",
              "description",
              "yearpublished",
              "minplayers",
              "maxplayers",
              "playingtime",
              "minplaytime",
              "maxplaytime",
              "minage")
  
  els = getNodeSet(xmlRoot(collection), "//items/item")
  header <- do.call(rbind.fill, list(
    lapply(els, function(x){
      data.frame(id=xmlAttrs(x)['id'], type=xmlAttrs(x)['type'], stringsAsFactors=FALSE)
    }) , stringsAsFactors=FALSE)
  )
  
  
  items <- xmlToList(xmlRoot(collection))
  items[[length(items)]] <- NULL
  game.details <- detailsDataToDF(items, fields=fields)
  
  primary.name <- unlist(lapply(items, function(x) {
    n <- names(x)
    all.names <- as.data.frame(t(as.matrix(sapply(lapply(x[which(n=="name")], function(y) {y}), c))), stringsAsFactors = FALSE)
    return(all.names[which(all.names$type == "primary"),]$value)
  })
  )
  
  names(game.details) <- paste("details", names(game.details), sep=".")
  
  return(data.frame(header, name=primary.name, game.details))
  
}