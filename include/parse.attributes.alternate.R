require(xml2)
require(plyr)

parse.attributes.alternate <- function(xml, sep = ","){

  collection <- read_xml(xml)
  games.attributes <- do.call(rbind.fill, list(lapply(xml_find_all(collection, "//items/item"), function(item){

      item.links <- do.call(rbind,
                            lapply(xml_find_all(item, "link"), function(link){
                              link.value <- data.frame(type=xml_attr(link, 'type'), 
                                                       value=xml_attr(link,'value'), 
                                                       stringsAsFactors=FALSE)
                              
                              return(link.value)
                              
                            })
      )
      
      links.concat <- t(as.matrix(unique(within(item.links, {
        values <- ave(value, type, FUN = function(x) paste(x, collapse = sep))
      })[,c("type", "values")])))
      
      colnames(links.concat) <- links.concat[1,]
      
      return(data.frame(t(links.concat[2,]), stringsAsFactors=FALSE))
    }))
  )
  
  return(data.frame(total=rowSums(!is.na(games.attributes)), games.attributes))
}