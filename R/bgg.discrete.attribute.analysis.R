#' @importFrom arules itemFrequency dissimilarity discretize apriori
#' @importFrom ggplot2 ggplot geom_bar geom_text ylab xlab theme ylim xlim
#' @importFrom dplyr select "%>%"
#' @importFrom dendextend hang.dendrogram set
#' @export
bgg.discrete.attribute.analysis <- function(bgg.useful.dummy,
                                            apriori.parameters = list(),
                                            apriori.appearance = list(),
                                            minimum.freq,
                                            brewer.palette){


  bgg.categories.matrix <- as.matrix(bgg.useful.dummy)

  colnames(bgg.categories.matrix) <- gsub("attributes.boardgame(category|mechanic).", "", colnames(bgg.useful.dummy))

  bgg.category.transactions <- as(bgg.categories.matrix, "transactions")
  bgg.category.freq <- itemFrequency(bgg.category.transactions)

  bgg.category.data <- data.frame(category=names(bgg.category.freq), count=bgg.category.freq) %>% arrange(desc(count))

  bgg.categories.plot <- ggplot(bgg.category.data, aes(reorder(category, -count), count)) +
    geom_bar(stat="identity", fill=brewer.pal(3, brewer.palette)[1], alpha=.2, col=brewer.pal(3, brewer.palette)[2]) +
    geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
    ylab("Frequency in Games") + xlab("Attribute") +
    theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
    ylim(0,.22)

  categories.dis <- dissimilarity(bgg.category.transactions[,bgg.category.freq > minimum.freq], method = "phi", which="items")
  categories.dis[is.na(categories.dis)] <- 1

  bgg.categories.dend <- categories.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram()

  categories.count.discrete <- discretize(left_join(data.frame(category=labels(bgg.categories.dend), stringsAsFactors = FALSE), bgg.categories.plot$data)$count, categories=10, ordered = TRUE)

  bgg.categories.dend <- bgg.categories.dend %>%
      #set("labels_colors", value=brewer.pal(length(levels(categories.count.discrete)), brewer.palette)[as.numeric(categories.count.discrete)]) %>%
    set("labels_cex", value=as.numeric(categories.count.discrete)/2)

  # -----------------

  category.rules <- apriori(bgg.category.transactions,
                            parameter = apriori.parameters,
                            appearance = apriori.appearance,
                            control = list(verbose=TRUE))

  #category.rules <- sort(category.rules, by="lift")

  # Find and remove redundant rules
  category.subset.matrix <- is.subset(category.rules, category.rules)
  category.subset.matrix[lower.tri(category.subset.matrix, diag=T)] <- NA
  category.rules.pruned <- category.rules[!(colSums(category.subset.matrix, na.rm=T) >= 1)]


  return(list(data=bgg.category.data, attribute.plot=bgg.categories.plot, attribute.dend=bgg.categories.dend, apriori.rules=category.rules.pruned))



}
