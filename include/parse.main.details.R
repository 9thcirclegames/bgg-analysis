require(XML)
require(plyr)
require(dplyr)

parse.main.details <- function(xml){
  
  fields <- c("image",
              "thumbnail",
              "description",
              "yearpublished",
              "releasedate",
              "minplayers",
              "maxplayers",
              "playingtime",
              "minplaytime",
              "maxplaytime",
              "minage")
  
  collection <- xmlParse(xml, asText=TRUE, useInternalNodes = TRUE)
  game.details <- do.call(rbind.fill, list(xpathApply(xmlRoot(collection), "//items/item", function(item){

    content <- xmlChildren(item, addFinalizer=FALSE)
    content.subset <- content[which(names(content) %in% fields)]
    
    details <- do.call(data.frame, list(lapply(content.subset, function(detail){
      
      detail.value <- data.frame(detail=ifelse(length(xmlAttrs(detail)['value']) >0, xmlAttrs(detail)['value'], xmlValue(detail, recursive=FALSE, trim=TRUE)), stringsAsFactors=FALSE)
      names(detail.value) <- xmlName(detail)
      
      return(detail.value)
      
    }), stringsAsFactors=FALSE)
    )
    
    primary.names <- xpathSApply(item, "name[@type='primary']", function(name){
      return(xmlAttrs(name)['value'])
    }, addFinalizer=FALSE)
    
    return(data.frame(name=primary.names, details, stringsAsFactors=FALSE))
    
  }, addFinalizer=FALSE), stringsAsFactors=FALSE)
  )
  
  game.details <- game.details %>% mutate_each(funs(type.convert(as.character(.))), -one_of("name", "image", "thumbnail", "description"))

  free(collection)
  
  return(game.details)
  
}