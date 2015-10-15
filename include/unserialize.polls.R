unserialize.polls <- function(item){

  
  polls <- item[which(names(item)=="poll")]

  # Suggested Players
  suggested_players <- do.call(data.frame,list(lapply(polls[1], function(poll) {
    
    meta <- poll[length(poll)]
    
    meta.df <- data.frame(as.numeric(meta$.attrs["totalvotes"]))
    
    names(meta.df)[ncol(meta.df)] <- paste(meta$.attrs["name"], "totalvotes", sep=".")
    
    poll[length(poll)] <- NULL
    
    if(is.null(poll$results)){ 
      
      return(data.frame(meta.df))
      }
    
    results.df <- do.call(data.frame,
                          
                          lapply(poll, function(results) {
                            
                            meta.results <- results[length(results)]
                            
                            results[length(results)] <- NULL
                            
                            results <- do.call(data.frame, list(results, stringsAsFactors=FALSE))
                            
                            num = ifelse(is.na(meta.results), NULL, meta.results)
                            up  = as.numeric(sub("\\+", "", num))+1
                            
                            names(results) <- sub(" ", "", paste(meta$.attrs["name"], sub("[0-9]*\\+", up, num), gsub("(.*?)(-.*)", "\\1", as.character(results[1,])), rownames(results)[2], sep="."))

                            poll_summary.names <- sub(" ", "", paste(meta$.attrs["name"], sub("[0-9]*\\+", up, num), sep="."))
                            
                            if(!is.null(poll$results$result)){
                            
                            poll_summary <- data.frame(max_index=
                                                         unlist(strsplit(names(results[nrow(results),])[which.max(apply(results[nrow(results),],MARGIN=2,min))], "\\."))[[3]]
                            )

                            names(poll_summary)[1] <- poll_summary.names

                            if(as.numeric(meta$.attrs["totalvotes"])==0) poll_summary[,1] <- NA
                            
                            results.converted <- results[2,]
                            indx <- sapply(results.converted, is.character)
                            results.converted[indx] <- lapply(results.converted[indx], function(x) as.numeric(x))
                            
                            return(data.frame(poll_summary, results.converted))
                            
                            } else {
                              poll_summary <- data.frame(max_index=NA)
                              names(poll_summary)[1] <- poll_summary.names
                              return(poll_summary)
                              
                            }
                            
                          })
                          
    )
    
    return(data.frame(meta.df, results.df))
    
  }), stringsAsFactors=F)
  )
  
  # Age and Language
  other_polls <- do.call(data.frame,list(lapply(polls[c(2,3)], function(poll) {

    if(!is.null(poll$results)){

    meta <- poll[length(poll)]
    
    meta.df <- data.frame(as.numeric(meta$.attrs["totalvotes"]))
    
    names(meta.df)[ncol(meta.df)] <- paste(meta$.attrs["name"], "totalvotes", sep=".")
    
    poll[length(poll)] <- NULL
    do.call(data.frame,
            
            lapply(poll, function(results) {
              
              meta.results <- results[length(results)]
              
              results <- do.call(data.frame, list(results, stringsAsFactors=FALSE))
              
              names(results) <- sub(" ", "", paste(meta$.attrs["name"], gsub("(.*?)(\\s.*)", "\\1", as.character(results[nrow(results)-1,])), gsub(" ", "", rownames(results)[nrow(results)]), sep="."))
              
              poll_summary <- data.frame(max_index=
                                           unlist(strsplit(names(results[nrow(results),])[which.max(apply(results[nrow(results),],MARGIN=2,min))], "\\."))[[2]]
                                          )
              
              names(poll_summary)[1] <- unlist(strsplit(names(results[nrow(results),])[which.max(apply(results[nrow(results),],MARGIN=2,min))], "\\."))[[1]]

              if(as.numeric(meta$.attrs["totalvotes"])==0) poll_summary[,1] <- NA
    
              results.converted <- results[nrow(results),]
              indx <- sapply(results.converted, is.character)
              results.converted[indx] <- lapply(results.converted[indx], function(x) as.numeric(x))
              
              return(data.frame(meta.df, poll_summary, results.converted ))
              
            })
            
    )
    
    } else {
      meta <- poll[length(poll)]
      meta.df <- data.frame(as.numeric(meta$.attrs["totalvotes"]))
      names(meta.df)[ncol(meta.df)] <- paste("poll.results", meta$.attrs["name"], "totalvotes", sep=".")
      return(data.frame(meta.df))
    }
    
  }), stringsAsFactors=F)
  )

  return(data.frame(suggested_players, other_polls))
  

  
}