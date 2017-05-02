require(xml2)

bgg.cache <- function(ids,
                      parameters,
                      endpoint = "http://www.boardgamegeek.com/xmlapi2",
                      cache.dir = "./cache",
                      verbose = TRUE){
  
  if (!file.exists(cache.dir)){
    dir.create(cache.dir)
  }
  
  collection.attributes <- sub("_$", "",
                               sub('([[:punct:]])\\1+', '\\1',
                                   do.call(paste, list(
                                     lapply(seq_along(parameters), function(y, n, i) {
                                       v <- ifelse(is.null((y[[i]])), FALSE , y[[i]])
                                       paste0(substr(n[[i]], 1, 2), as.numeric(y[[i]]))
                                     }
                                     , y=parameters, n=names(parameters)),
                                     collapse = "_"))
                               )
  )
  
  query.parameters <- sub("&$", "",
                          sub('([[:punct:]])\\1+', '\\1',
                              do.call(paste, list(
                                lapply(seq_along(parameters), function(y, n, i) {
                                  if(is.null((y[[i]]))) return("")
                                  paste(n[[i]], as.numeric(y[[i]]), sep="=")
                                }
                                , y=parameters, n=names(parameters)),
                                collapse = "&"))
                          )
  )
  
  files <- paste(ids, "xml", sep=".")
  
  dir.create(file.path(cache.dir, collection.attributes), showWarnings = FALSE)
  cache <- list.files(file.path(cache.dir, collection.attributes))
  
  '%nin%' <- Negate('%in%')
  
  ids.misses <- ids[files %nin% cache]
  
  collection.txt <- '<?xml version="1.0" encoding="UTF-8"?>\n<items termsofuse="http://boardgamegeek.com/xmlapi/termsofuse">'
  
  if(length(ids.misses) > 0){
    
    require(RCurl)
    require(XML)
    
    if(verbose){
      message(paste("Cache miss! Going to download the games with the following ids:", paste(ids.misses, collapse=","), "..."))
    }
    
    url <- URLencode(paste0(endpoint, "/thing?", paste0("id=", paste(ids.misses, collapse=","), "&", query.parameters)))
    
    misses.xml <- xmlParse(getURL(url, ssl.verifypeer=FALSE, .opts=curlOptions(followlocation = TRUE)), encoding = "UTF-8", asText=TRUE, asTree=TRUE, useInternalNodes = TRUE)
    
    # Save each item to the cache
    sapply(xmlChildren(xmlRoot(misses.xml)), function(x){
      xml.cache <- suppressWarnings(xmlTree(xmlName(xmlRoot(misses.xml)), attrs = xmlAttrs(xmlRoot(misses.xml))))
      addChildren(xmlRoot(xml.cache), list(x))
      xml.cache$closeTag()
      
      cat(
        saveXML(xml.cache,
                prefix = '<?xml version="1.0" encoding=\"UTF-8\"?>',
                indent = TRUE
        ), file = file.path(
          cache.dir,
          collection.attributes,
          paste(xmlAttrs(x)['id'], "xml", sep=".")
        )
      )
      
      return(xmlAttrs(x)['id'])
    })
    
    free(misses.xml)
  }
  
  hits.txt <- sapply(ids, function(id){
    
    hits.xml <- read_xml(file.path(cache.dir, collection.attributes, paste(id, "xml", sep=".")))
    item <- (xml_children(hits.xml))
    
    item.txt <- paste(item[[1]], collapse="")
    
    rm(hits.xml)
    gc()
    
    return(item.txt)
  })
  
  collection.txt <- paste(collection.txt, paste(hits.txt, collapse="\n"), '</items>', sep="\n")
  
  return(collection.txt)
}
