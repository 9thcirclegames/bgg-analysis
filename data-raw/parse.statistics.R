parse.statistics <- function(xml, sep = ","){

  collection <- read_xml(xml)

  games.statistics <- do.call(rbind.fill, list(lapply(xml_find_all(collection, "//items/item"), function(item){

      item.statistics <- do.call(data.frame,
                                 lapply(xml_find_all(item, "statistics/ratings/child::*"), function(statistic){
                                   statistic.name <- xml_name(statistic)
                                   switch(statistic.name,
                                          ranks = (function(){

                                            statistic.ranks <- do.call(data.frame, list(lapply(xml_children(statistic), function(rank){

                                              rank.value <- data.frame(
                                                pos=suppressWarnings(as.numeric(xml_attr(rank, 'value'))),
                                                bayesaverage=suppressWarnings(as.numeric(xml_attr(rank, 'bayesaverage')))
                                              )

                                              names(rank.value) <- paste(xml_attr(rank, 'type'), xml_attr(rank, 'name'), names(rank.value), sep=".")

                                              return(rank.value)

                                            }), stringsAsFactors=FALSE)
                                            )

                                            return(statistic.ranks)
                                          })(),
                                          # The 'default' case should cover the remaining polls
                                          (function(){

                                            statistic.value <- data.frame(stat=as.numeric(xml_attr(statistic, 'value')))
                                            names(statistic.value) <- xml_name(statistic)

                                            return(statistic.value)

                                          })()
                                   ) # end-switch

                                 })
      )

    }))
  )

  return(games.statistics)
}

