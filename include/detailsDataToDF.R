detailsDataToDF <- function(json, fields = NULL){
  
  if(length(json) == 0 | is.null(fields)) return(NULL)
  
  # remove duplicates, then collate
  collate.fields <- paste0(unique(
    unlist(strsplit(fields, split = ","))),
    collapse = ","
  )

  do.call(rbind.fill,
          lapply(lapply(json, function(item) {
            do.call(data.frame,
                    list(lapply(item[which(names(item) %in%  unlist(strsplit(collate.fields, split = ",")))], function(x) { ifelse(is.null(x), NA, x) }),
                         stringsAsFactors = FALSE),
            )
          }), function(l) {
            l
          })
  )
}