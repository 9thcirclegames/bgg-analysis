require(XML)
require(plyr)

parse.statistics <- function(xml, sep = ","){
  
  collection <- xmlParse(xml, asText=TRUE, useInternalNodes = TRUE)
  
  games.statistics <- do.call(rbind.fill, list(
    
    xpathApply(collection, "//items/item", function(item){

      item.statistics <- do.call(data.frame,
                                 xpathApply(item, "statistics/ratings/child::*", function(statistic){
                                   statistic.name <- xmlName(statistic) 
                                   
                                   switch(statistic.name,
                                          ranks = (function(){
                                            
                                            statistic.ranks <- do.call(data.frame, list(lapply(xmlChildren(statistic, addFinalizer=FALSE), function(rank){
                                              
                                              rank.value <- data.frame(
                                                pos=suppressWarnings(as.numeric(xmlAttrs(rank)['value'])),
                                                bayesaverage=suppressWarnings(as.numeric(xmlAttrs(rank)['bayesaverage']))
                                              )
                                              
                                              names(rank.value) <- paste(xmlAttrs(rank)['type'], xmlAttrs(rank)['name'], names(rank.value), sep=".")
                                              
                                              return(rank.value)
                                              
                                            }), stringsAsFactors=FALSE)
                                            )
                                            
                                            return(statistic.ranks)
                                          })(),
                                          # The 'default' case should cover the remaining polls
                                          (function(){
                                            
                                            statistic.value <- data.frame(stat=as.numeric(xmlAttrs(statistic)['value']))
                                            names(statistic.value) <- xmlName(statistic)
                                            
                                            return(statistic.value)
                                            
                                          })()
                                   ) # end-switch
                                   
                                 }, addFinalizer=FALSE)
      )
      
    }), stringsAsFactors=FALSE)
  )
  
  free(collection)
  
  return(games.statistics)
}

