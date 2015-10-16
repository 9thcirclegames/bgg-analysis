require(XML)
require(RCurl)
require(plyr)

source("./include/bgg.cache.R")

source("./include/parse.main.details.R")
source("./include/parse.attributes.R")
source("./include/parse.statistics.R")
source("./include/parse.polls.R")


bgg.get <- function(ids,
                    parameters = list(
                      stats=TRUE,
                      historical=FALSE,
                      marketplace=FALSE,
                      comments=FALSE,
                      ratingcomments=FALSE,
                      videos=FALSE,
                      version=TRUE),
                    parsers = list(parse.main.details, parse.attributes, parse.statistics, parse.polls),
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
    do.call(rbind.fill,
            lapply(id.chunks, function(single.chunk) {
              bgg.get(ids = single.chunk, parameters = parameters, parsers = parsers, .progress = .progress)         
            })
    )   
  }
  
  else {
    
    game.collection <- bgg.cache(ids = id.v, parameters = parameters)
    
    game.data <- do.call(data.frame, list(lapply(parsers, function(p) {
      do.call(p, list(game.collection))
    }), stringsAsFactors=FALSE)
    )
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
    
    return(game.data)
    
  }
  
}