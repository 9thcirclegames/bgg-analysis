require(XML)
require(plyr)

parse.meta.details <- function(xml){

  header <- do.call(rbind.fill, list(xpathApply(xmlRoot(xmlParse(xml, asText=TRUE)), "//items/item", function(item){
  
      meta <- as.data.frame(t(xmlAttrs(item)), stringsAsFactors=FALSE)
      meta$type <- as.factor(meta$type)
      
      return(meta)
      
    }), stringsAsFactors=FALSE
  ))
  
  return(header[, rev(names(header))])
  
}