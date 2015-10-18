require(XML)
require(plyr)

parse.attributes <- function(xml, sep = ","){

  games.attributes <- do.call(rbind.fill, list(xpathApply(xmlRoot(xmlParse(xml, asText=TRUE)), "//items/item", function(item){
  
      #meta <- as.data.frame(t(xmlAttrs(item)), stringsAsFactors=FALSE)
      #meta$type <- as.factor(meta$type)

      item.links <- do.call(rbind,
                            xpathApply(item, "link", function(link){
                              link.value <- data.frame(type=xmlAttrs(link)['type'], 
                                                       value=xmlAttrs(link)['value'], 
                                                       stringsAsFactors=FALSE)
                              
                              return(link.value)
                              
                            })
      )
      
      links.concat <- t(as.matrix(unique(within(item.links, {
        values <- ave(value, type, FUN = function(x) paste(x, collapse = sep))
      })[,c("type", "values")])))
      
      colnames(links.concat) <- links.concat[1,]
      
      return(data.frame(t(links.concat[2,]), stringsAsFactors=FALSE))
    }), stringsAsFactors=FALSE)
  )
  
  return(data.frame(total=rowSums(!is.na(games.attributes)), games.attributes))
}