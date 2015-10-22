bgg.cache.XML <- function(ids,
                      parameters,
                      endpoint = "http://www.boardgamegeek.com/xmlapi2",
                      cache.dir = "./cache"){

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
  ids.hits   <- ids[files %in% cache]

  collection.xml          <- suppressWarnings(xmlTree("items"))
  collection.xml.root <- xmlRoot(collection.xml)

  if(length(ids.misses) > 0){

    message(paste("Cache miss! Going to download the games with the following ids:", paste(ids.misses, collapse=","), "..."))

    url <- URLencode(paste0(endpoint, "/thing?", paste0("id=", paste(ids.misses, collapse=","), "&", query.parameters)))

    misses.xml            <- xmlParse(getURL(url, ssl.verifypeer=FALSE), encoding = "UTF-8", asText=TRUE, asTree=TRUE, useInternalNodes = TRUE)

    # Update the collection
    xmlAttrs(collection.xml.root) <- xmlAttrs(xmlRoot(misses.xml))
    addChildren(collection.xml.root, xmlChildren(xmlRoot(misses.xml), addFinalizer=FALSE))

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

      free(xml.cache)
      rm(xml.cache)
      gc()

      return(xmlAttrs(x)['id'])
    })

    free(misses.xml)

  }

  if(length(ids.hits) > 0){
    # Save each item to the cache
    sapply(ids.hits, function(id){

      hits.xml <- xmlParse(file.path(cache.dir, collection.attributes, paste(id, "xml", sep=".")), asText=FALSE, useInternalNodes = TRUE)

      item <- xmlChildren(xmlRoot(hits.xml), addFinalizer=FALSE)

      # Update the collection
      xmlAttrs(collection.xml.root) <- xmlAttrs(xmlRoot(hits.xml))
      addChildren(collection.xml.root, item)

      free(hits.xml)
      rm(hits.xml)
      gc()

      return(id)
    })
  }

  collection.xml$closeTag()

  xml <- saveXML(collection.xml,
                 prefix = '<?xml version="1.0" encoding=\"UTF-8\"?>',
                 indent = TRUE)

  rm(collection.xml)
  gc()

  return(xml)
}
