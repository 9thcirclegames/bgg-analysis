require(XML)
require(plyr)

parse.attributes <- function(xml, sep = ","){

  collection <- xmlParse(xml, asText=TRUE, useInternalNodes = TRUE)
  games.attributes <- do.call(rbind.fill, list(xpathApply(xmlRoot(collection), "//items/item", function(item){

      item.links <- do.call(rbind,
                            xpathApply(item, "link", function(link){
                              link.value <- data.frame(type=xmlAttrs(link)['type'], 
                                                       value=xmlAttrs(link)['value'], 
                                                       stringsAsFactors=FALSE)
                              
                              return(link.value)
                              
                            }, addFinalizer=FALSE)
      )
      
      links.concat <- t(as.matrix(unique(within(item.links, {
        values <- ave(value, type, FUN = function(x) paste(x, collapse = sep))
      })[,c("type", "values")])))
      
      colnames(links.concat) <- links.concat[1,]
      
      return(data.frame(t(links.concat[2,]), stringsAsFactors=FALSE))
    }, addFinalizer=FALSE), stringsAsFactors=FALSE)
  )
  
  free(collection)
  rm(collection)
  gc()
  
  return(data.frame(total=rowSums(!is.na(games.attributes)), games.attributes))
}