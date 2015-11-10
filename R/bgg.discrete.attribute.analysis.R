#' @importFrom arules itemFrequency dissimilarity discretize
#' @importFrom ggplot2 ggplot geom_bar geom_text ylab xlab theme ylim xlim
#' @importFrom dplyr select "%>%"
#' @export
bgg.discrete.attribute.analysis <- function(bgg.useful.dummy, 
                                            dummy.to.analyze, 
                                            minimum.freq, 
                                            plot.color){
  
  
  bgg.categories.matrix <- as.matrix(select(bgg.useful.dummy, starts_with(dummy.to.analyze)))
  
  colnames(bgg.categories.matrix) <- gsub(paste0(dummy.to.analyze, "."), "", 
                                          colnames(select(bgg.useful.dummy, starts_with(dummy.to.analyze)))
  )
  
  row.names(bgg.categories.matrix) <- paste("Game", c(1:nrow(bgg.useful.dummy)), sep=".")
  
  bgg.category.transactions <- as(bgg.categories.matrix, "transactions")
  bgg.category.freq <- itemFrequency(bgg.category.transactions)
  
  bgg.categories.plot <- ggplot(data.frame(category=names(bgg.category.freq), count=bgg.category.freq) %>% 
                                  arrange(desc(count)),
                                aes(reorder(category, -count), count)) +
    geom_bar(stat="identity", fill=plot.color, alpha=.2, col=plot.color) +
    geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
    ylab("Frequency in Games") + xlab("Category") +
    theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
    ylim(0,.22)
  
  categories.dis <- dissimilarity(bgg.category.transactions[,bgg.category.freq > minimum.freq], method = "phi", which="items")
  categories.dis[is.na(categories.dis)] <- 1
  
  categories.count.discrete <- discretize(left_join(data.frame(category=labels(bgg.categories.dend), stringsAsFactors = FALSE), bgg.categories.plot$data)$count, categories=10, ordered = TRUE)
  
  bgg.categories.dend <- categories.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram() %>%
    #set("labels_colors", value=brewer.pal(length(levels(categories.count.discrete)), brewer.palette)[as.numeric(categories.count.discrete)]) %>%
    set("labels_cex", value=as.numeric(categories.count.discrete)/2)

  return(list(attribute.plot=bgg.categories.plot, attribute.dend=bgg.categories.dend)) 
  
  
  
}