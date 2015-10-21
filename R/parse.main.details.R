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

  collection <- read_xml(xml)

  game.details <- do.call(rbind.fill, list(lapply(xml_find_all(collection, "//items/item"), function(item){

    content <- xml_children(item)
    content.subset <- content[which(xml_name(content) %in% fields)]

    details <- do.call(data.frame, list(lapply(content.subset, function(detail){

      detail.value <- data.frame(detail=ifelse(xml_has_attr(detail, "value"), xml_attr(detail, "value"), xml_text(detail)), stringsAsFactors=FALSE)
      names(detail.value) <- xml_name(detail)

      return(detail.value)

    }), stringsAsFactors=FALSE)
    )

    primary.names <- sapply(xml_find_all(item, "name[@type='primary']"), function(name){
      return(xml_attr(name, "value"))
    })

    return(data.frame(name=primary.names, details, stringsAsFactors=FALSE))

  }))
  )

  game.details <- game.details %>% mutate_each(funs(type.convert(as.character(.))), -one_of("name", "image", "thumbnail", "description"))

  return(game.details)

}
