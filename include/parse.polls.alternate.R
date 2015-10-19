require(xml2)
require(plyr)

parse.polls.alternate <- function(xml, cut.players = 10){

  collection <- read_xml(xml)
  
  games.polls <- do.call(rbind.fill, list(lapply(xml_find_all(collection, "//items/item"), function(item){

      polls <- do.call(data.frame, list(lapply(xml_find_all(item, "poll"), function(poll){
        
        poll.name <- xml_attr(poll, 'name') 
        poll.totalvotes <- data.frame(as.numeric(xml_attr(poll, 'totalvotes'))) 
        names(poll.totalvotes) <- paste(poll.name, "totalvotes", sep=".")
        
        if(as.numeric(xml_attr(poll, 'totalvotes'))==0) return(poll.totalvotes)
        
        poll.details <- switch(poll.name,
                               suggested_numplayers = (function(){
                                 
                                 poll.results <- do.call(data.frame, list( 
                                   lapply(xml_find_all(poll, "results"), function(results){
                                     
                                     results.name <- paste(poll.name, sub("[0-9]*\\+", "Over", xml_attr(results, 'numplayers')), sep=".")
                                     
                                     results.set <- do.call(data.frame, list(
                                       lapply(xml_children(results), function(result) {
                                         result.name <- gsub("\\s", "", xml_attr(result, 'value'))
                                         result.numvotes <- data.frame(as.numeric(xml_attr(result, 'numvotes')))
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
                                     
                                   }), stringsAsFactors=FALSE)
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
                                   lapply(xml_find_all(poll, "results/result"), function(result){
                                     
                                     result.name <- gsub("(.*?)(\\s.*)", "\\1", xml_attr(result, 'value'))
                                     result.numvotes <- data.frame(as.numeric(xml_attr(result, 'numvotes')))
                                     names(result.numvotes) <- paste(poll.name, result.name, "numvotes", sep=".") 
                                     return(result.numvotes) 
                                   }), stringsAsFactors=FALSE)
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
        
      }))
      )
      
      return(polls)
      
    }))
  )

  return(games.polls)
  
}