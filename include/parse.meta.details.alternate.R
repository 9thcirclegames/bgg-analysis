require(xml2)
require(plyr)

parse.meta.details.alternate <- function(xml){

  collection <- read_xml(xml)
  
  header <- do.call(rbind.fill, list(lapply(xml_find_all(collection, "//items/item"), function(item){
    
    meta <- as.data.frame(t(xml_attrs(item)), stringsAsFactors=FALSE)
    meta$type <- as.factor(meta$type)
    
    return(meta)
    
  }))
  )
  
  return(header[, rev(names(header))])

}