require(XML)
require(RCurl)
require(plyr)

source("./include/detailsDataToDF.R")
source("./include/unserialize.polls.R")

bgg.get <- function(ids,
                    types="boardgame",
                    endpoint = "https://www.boardgamegeek.com/xmlapi2",
                    sep =",",
                    local.data = "./data",
                    .progress = create_progress_bar()){
  
  id.pagination.define <- 20
  sleep.time <- 2
  
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
              bgg.get(ids = single.chunk,
                      types = types,
                      endpoint = endpoint,
                      sep = sep,
                      local.data = local.data,
                      .progress = .progress)         
            })
    )   
  }
  
  else {
    
    Sys.sleep(sleep.time)
    
    url <- URLencode(paste0(endpoint,
                            "/thing?",
                            "id=", paste(ids, collapse=","),
                            "&stats=1",
                            "&comments=0",
                            "&ratingcomments=0",
                            "&historical=0",
                            "&videos=0",
                            "&version=1",
                            "&type=", paste0(types, collapse=","),
                            "&marketplace=0"
    ))
    
    file.hd <- NULL
    
    local.filename <- paste(local.data, "raw", id.pagination.define, paste(sub(",", "-", paste0(types, collapse=",")), paste(ids, collapse="-"), "xml", sep="."), sep="/")
    
    if(file.exists(local.filename)){
      file.hd <- read_file(local.filename)
    } else {
      file.hd <- getURL(url, ssl.verifypeer=FALSE)
      
      # Do not save incomplete chunks
      if(length(ids) == id.pagination.define){
        fileConn<-file(local.filename)
        writeLines(file.hd, fileConn)
        close(fileConn)
      }
    }
    
    items <- xmlToList(xmlParse(file.hd))
    
    #items <- xmlToList(xmlParse(getURL(url, ssl.verifypeer=FALSE)))
    items[[length(items)]] <- NULL
    game.details <- detailsDataToDF(items,
                                    fields=c("image",
                                             "thumbnail",
                                             "description",
                                             "yearpublished",
                                             "minplayers",
                                             "maxplayers",
                                             "playingtime",
                                             "minplaytime",
                                             "maxplaytime",
                                             "minage"))
    
    primary.name <- unlist(lapply(items, function(x) {
      n <- names(x)
      all.names <- as.data.frame(t(as.matrix(sapply(lapply(x[which(n=="name")], function(y) {y}), c))), stringsAsFactors = FALSE)
      return(all.names[which(all.names$type == "primary"),]$value)
    })
    )
    
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
    
    game.attr <- do.call(rbind.fill, lapply(items, function(x) { data.frame(id=as.integer(x$.attrs[2]), type=x$.attrs[1], stringsAsFactors = TRUE)}))
    
    game.stats <- do.call(rbind.fill, lapply(items, function(item){
      item.stats <- as.matrix(unlist(item$statistics$ratings))
      return(as.data.frame(t(item.stats[which(!grepl("^ranks", row.names(item.stats))),]), stringsAsFactors=FALSE)) 
    }))
    
    indx <- sapply(game.stats, is.character)
    game.stats[indx] <- lapply(game.stats[indx], function(x) as.numeric(x))
    
    # Unserialize Polls
    game.polls <- do.call(rbind.fill, lapply(items, unserialize.polls))
    
    # URI
    game.uri <-  
      data.frame(uri.endpoint=paste0(
        endpoint,
        "/thing?",
        "id=", game.attr$id,
        "&stats=1",
        "&comments=0",
        "&ratingcomments=0",
        "&historical=0",
        "&videos=0",
        "&version=1",
        "&type=", paste0(types, collapse=","),
        "&marketplace=0"
      ), stringsAsFactors = FALSE
      )
    
    # Adding column prefixes
    names(game.details) <- paste("details", names(game.details), sep=".")
    names(game.attributes) <- paste("attributes", names(game.attributes), sep=".")
    names(game.stats) <- paste("stats", names(game.stats), sep=".")
    
    game.raw <- data.frame(game.attr, name=primary.name, game.details, attributes.num=rowSums(!is.na(game.attributes)), game.attributes, game.stats, game.polls, game.uri, stringsAsFactors = FALSE)
    
    ix = grep("^poll.results.suggested_numplayers.(\\d+).*", names(game.raw))
    numbers = as.numeric(gsub("^poll.results.suggested_numplayers.(\\d+).*", "\\1", names(game.raw)[ix]))
    
    m = matrix(numbers, ncol=length(ix), nrow=nrow(game.raw), byrow=T)
    game.raw[ix][!(m>=game.raw$details.minplayers)] <-NA
    
    # Advance the progress bar
    if(inherits(try(.progress$step(), silent=T), "try-error")){
      .progress$init(length(ids)+1)
      .progress$step()
    }
    
    return(game.raw)
    
  }
  
}