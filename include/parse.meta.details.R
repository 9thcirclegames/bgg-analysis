require(XML)
require(plyr)

parse.meta.details <- function(xml){

  collection <- xmlParse(xml, asText=TRUE, useInternalNodes = TRUE)
  header <- do.call(rbind.fill, list(xpathApply(xmlRoot(collection), "//items/item", function(item){
  
      meta <- as.data.frame(t(xmlAttrs(item)), stringsAsFactors=FALSE)
      meta$type <- as.factor(meta$type)
      
      return(meta)
      
    }, addFinalizer=FALSE), stringsAsFactors=FALSE
  ))
  
  free(collection)
  
  return(header[, rev(names(header))])
  
}