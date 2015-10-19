require(XML)
require(plyr)

parse.polls <- function(xml, cut.players = 10){

  collection <- xmlParse(xml, asText=TRUE, useInternalNodes = TRUE)
  games.polls <- do.call(rbind.fill, list(xpathApply(collection, "//items/item", function(item){

      #meta <- as.data.frame(t(xmlAttrs(item)))
      #meta$type <- as.factor(meta$type)
      
      polls <- do.call(data.frame, list(xpathApply(item, "poll", function(poll){
        
        poll.name <- xmlAttrs(poll)['name'] 
        poll.totalvotes <- data.frame(as.numeric(xmlAttrs(poll)['totalvotes'])) 
        names(poll.totalvotes) <- paste(poll.name, "totalvotes", sep=".")
        
        if(as.numeric(xmlAttrs(poll)['totalvotes'])==0) return(poll.totalvotes)
        
        poll.details <- switch(poll.name,
                               suggested_numplayers = (function(){
                                 
                                 poll.results <- do.call(data.frame, list( 
                                   xpathApply(poll, "results", function(results){
                                     
                                     results.name <- paste(poll.name, sub("[0-9]*\\+", "Over", xmlAttrs(results)['numplayers']), sep=".")
                                     
                                     results.set <- do.call(data.frame, list(
                                       lapply(xmlChildren(results, addFinalizer=FALSE), function(result) {
                                         result.name <- gsub("\\s", "", xmlAttrs(result)['value'])
                                         result.numvotes <- data.frame(as.numeric(xmlAttrs(result)['numvotes']))
                                         names(result.numvotes) <- paste(results.name, result.name, "numvotes", sep=".") 
                                         
                                         return(result.numvotes)
                                         
                                       }), stringsAsFactors=FALSE)
                                     )
                                     
                                     results.total <- rowSums(results.set)

                                     poll_summary <- data.frame(as.factor(ifelse(poll.totalvotes>0 & results.total>0, 
                                                                                 unlist(strsplit(names(results.set)[which.max(apply(results.set,MARGIN=2,min))], "\\."))[[3]],
                                                                                 NA))
                                     )
                                     names(poll_summary) <- results.name
                                     results.totalvotes <- as.data.frame(results.total)
                                     
                                     names(results.totalvotes) <- paste(results.name, "totalvotes", sep=".")

                                     if(nrow(results.set)==0) return(data.frame(poll_summary, results.totalvotes))
                                     
                                     return(data.frame(poll_summary, results.totalvotes, results.set))
                                     
                                   }, addFinalizer=FALSE), stringsAsFactors=FALSE)
                                 )
                                 
                                 return(poll.results[,unlist(lapply(strsplit(names(poll.results), "\\."), function(x){
                                   ifelse(
                                     x[1]=="suggested_numplayers" & 
                                       suppressWarnings(!is.na(as.numeric(x[2])) & as.numeric(x[2]) > cut.players
                                       ), FALSE,  TRUE)
                                 }))])
                                 return(poll.results)
                                 
                               })(),
                               # The 'default' case should cover the remaining polls
                               (function(){
                                 
                                 results.set <- do.call(data.frame, list( 
                                   xpathApply(poll, "results/result", function(result){
                                     
                                     result.name <- gsub("(.*?)(\\s.*)", "\\1", xmlAttrs(result)['value'])
                                     result.numvotes <- data.frame(as.numeric(xmlAttrs(result)['numvotes']))
                                     names(result.numvotes) <- paste(poll.name, result.name, "numvotes", sep=".") 
                                     return(result.numvotes) 
                                   }, addFinalizer=FALSE), stringsAsFactors=FALSE)
                                 )
                                 
                                 poll_summary <- data.frame(as.factor(ifelse(poll.totalvotes>0, 
                                                                             unlist(strsplit(names(results.set)[which.max(apply(results.set,MARGIN=2,min))], "\\."))[[2]],
                                                                             NA))
                                 )

                                 names(poll_summary) <- poll.name
                                 
                                 if(nrow(results.set)==0) return(data.frame(poll_summary))
                                 
                                 return(data.frame(poll_summary, results.set))
                                 
                                 
                               })()
        ) # end-switch
        
        return(data.frame(poll.totalvotes, poll.details))
        
      }, addFinalizer=FALSE), stringsAsFactors=FALSE)
      )
      
      #return(data.frame(meta, polls))
      return(polls)
      
    }), stringsAsFactors=FALSE)
  )
  
  free(collection)
  
  return(games.polls)
  
}