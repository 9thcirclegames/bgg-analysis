require(plyr)

source("./include/bgg.cache.R")

bgg.get <- function(ids,
                    parameters = list(
                      stats=TRUE,
                      historical=FALSE,
                      marketplace=FALSE,
                      comments=FALSE,
                      ratingcomments=FALSE,
                      videos=FALSE,
                      version=TRUE),
                    parsers = list(),
                    .progress = create_progress_bar()){
  
  id.pagination.define <- 40
  
  id.v <- unique(unlist(strsplit(ids, split = ",")))
  id.f <- rep(seq_len(ceiling(length(id.v) / id.pagination.define)),each = id.pagination.define,length.out = length(id.v))
  id.chunks <- split(id.v, f = id.f)
  
  if(length(id.chunks) > 1){
    
    # Init the progress bar
    .progress$init(length(id.chunks)+1)
    .progress$step()
    
    # Recursive calls for each chunk
    all.games <- do.call(rbind.fill,
            lapply(id.chunks, function(single.chunk) {
              bgg.get(ids = single.chunk, parameters = parameters, parsers = parsers, .progress = .progress)         
            })
    ) 
    
    # Reorder columns
    cols <- paste0("^(", names(parsers), ")")
    s <- names(bgg.complete)
    n <- seq_along(cols)
    for(i in n) s <- sub(cols[i], paste0(n[i], "\\1"), s)
    new_vec <- substr(s[order(s)], 2, nchar(s[order(s)]))
    return(all.games[,new_vec])
    
  }
  
  else {
    
    game.collection <- bgg.cache(ids = id.v, parameters = parameters)
    
    game.data <- do.call(data.frame, list(lapply(seq_along(parsers), function(y, n, i) { 
      games.snap <- do.call(y[[i]], list(game.collection))
      names(games.snap) <- paste(n[[i]], names(games.snap), sep=".")
      return(games.snap)
    }, y=parsers, n=names(parsers)), stringsAsFactors=FALSE)
    )
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
    
    return(game.data)
    
  }
  
}