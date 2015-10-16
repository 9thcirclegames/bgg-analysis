parse.attributes <- function(collection, sep = ","){

  items <- xmlToList(xmlRoot(collection))
  items[[length(items)]] <- NULL
  
  links <- lapply(items, function(x) {
    n <- names(x)
    l <- lapply(x[which(n=="link")], function(y) {y})
  })
  
  
  # TODO: Resolve
  #   Warning message:
  #     In (function (..., deparse.level = 1)  :
  #           number of columns of result is not a multiple of vector length (arg 1)
  links.goes <- lapply(links, function(game){ do.call(rbind, game) })

  game.attributes <- do.call(rbind.fill, lapply(links.goes, function(g) {
    row.names(g) <- NULL
    g <- as.data.frame(g, stringsAsFactors = FALSE)

    fg <- t(as.matrix(unique(within(g, {
      values <- ave(value, type, FUN = function(x) paste(x, collapse = sep))
    })[,c("type", "values")])))
    
    colnames(fg) <- fg[1,]

    return(data.frame(t(fg[2,])))
  }))

  names(game.attributes) <- paste("attributes", names(game.attributes), sep=".")
  
  return(data.frame(attributes.num=rowSums(!is.na(game.attributes)), game.attributes))
  
}