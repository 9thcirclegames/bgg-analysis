################
# REQS         #
################
if(! "dplyr" %in% installed.packages()) install.packages("dplyr", depend = TRUE)
if(! "ggplot2" %in% installed.packages()) install.packages("ggplot2", depend = TRUE)
if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "cluster" %in% installed.packages()) install.packages("cluster", depend = TRUE)
if(! "dendextend" %in% installed.packages()) {
  devtools::install_github('talgalili/dendextend')
  devtools::install_github('talgalili/dendextendRcpp')
}
if(! "dummies" %in% installed.packages()) install.packages("dummies", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)
if(! "RColorBrewer" %in% installed.packages()) install.packages("RColorBrewer", depend = TRUE)

require(arules)
require(plyr)
require(dplyr)
require(ggplot2)
require(arulesViz)
require(cluster)
require(dummies)
require(RColorBrewer)
require(dendextend)

require(bggAnalysis)

data("BoardGames")

brewer.palette <- "OrRd"

brewer.color.categories <- brewer.pal(3, brewer.palette)[3]
brewer.color.mechanics  <- brewer.pal(3, brewer.palette)[2]

minimum.support <- .001
minimum.freq <- .01

#########################################
# Prepare Data Sets                     #
#########################################
bgg.useful.arules <- bgg.prepare.data(BoardGames) %>% 
  filter(is.na(details.yearpublished) | details.yearpublished <= 2015) %>% 
  filter(game.type == "boardgame")

bgg.useful.arules$stats.weight.factor <- round_any(bgg.useful.arules$stats.averageweight, .5)
bgg.useful.arules$details.playingtime <- round_any(bgg.useful.arules$details.playingtime, 15)

bgg.useful.dummy <- bgg.prepare.dummy(bgg.useful.arules)

bgg.useful.dummy <- cbind(
  bgg.useful.dummy
  ,dummy("details.minplayers", bgg.useful.dummy, sep="=")
  ,dummy("details.maxplayers", bgg.useful.dummy, sep="=")
  ,dummy("details.playingtime", bgg.useful.dummy, sep="=")
  ,dummy("details.minage", bgg.useful.dummy, sep="=")
  ,dummy("stats.weight.factor", bgg.useful.dummy, sep="=")
)

# TopN
top.rated.num <- 200

bgg.topn.arules <-  (bgg.useful.arules %>% filter(stats.usersrated > 700))
bgg.topn.arules <-  bgg.topn.arules[order(-bgg.topn.arules$stats.usersrated),]
bgg.topn.dummy <- bgg.prepare.dummy(bgg.topn.arules)

##############
# CATEGORIES #
##############

bgg.categories.matrix <- as.matrix(select(bgg.useful.dummy, starts_with("attributes.boardgamecategory")))

colnames(bgg.categories.matrix) <- gsub("attributes.boardgamecategory.", "", 
                                        colnames(select(bgg.useful.dummy, starts_with("attributes.boardgamecategory")))
)

row.names(bgg.categories.matrix) <- bgg.useful.dummy$details.name

bgg.category.transactions <- as(bgg.categories.matrix, "transactions")
bgg.category.freq <- itemFrequency(bgg.category.transactions)

bgg.categories.plot <- ggplot(data.frame(category=names(bgg.category.freq), count=bgg.category.freq) %>% 
                                arrange(desc(count)),
                              aes(reorder(category, -count), count)) +
  geom_bar(stat="identity", fill=brewer.color.categories, alpha=.2, col=brewer.color.categories) +
  geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") + xlab("Category") +
  theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
  ylim(0,.22)

categories.dis <- dissimilarity(bgg.category.transactions[,bgg.category.freq > minimum.freq], method = "phi", which="items")
categories.dis[is.na(categories.dis)] <- 1

bgg.categories.dend <- categories.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram()
categories.count.discrete <- discretize(left_join(data.frame(category=labels(bgg.categories.dend), stringsAsFactors = FALSE), bgg.categories.plot$data)$count, categories=10, ordered = TRUE)

#set("labels_colors", value=brewer.pal(length(levels(categories.count.discrete)), brewer.palette)[as.numeric(categories.count.discrete)]) %>%
bgg.categories.dend %>%
  set("labels_cex", value=as.numeric(categories.count.discrete)/2.7) %>%
  plot()  

# -----------------

category.rules <- apriori(bgg.category.transactions,
                          parameter = list(minlen=2, supp= minimum.support),
                          control = list(verbose=TRUE))

category.rules <- sort(category.rules, by="lift")

# Find and remove redundant rules
category.subset.matrix <- is.subset(category.rules, category.rules)
category.subset.matrix[lower.tri(category.subset.matrix, diag=T)] <- NA
category.rules.pruned <- category.rules[!(colSums(category.subset.matrix, na.rm=T) >= 1)]

# Inspect the mined rules
inspect(category.rules.pruned)
plot(category.rules.pruned, method="graph", control=list(type="items"))

#############
# MECHANICS #
#############

bgg.mechanics.matrix <- as.matrix(select(bgg.useful.dummy, starts_with("attributes.boardgamemechanic")))

colnames(bgg.mechanics.matrix) <- gsub("attributes.boardgamemechanic.", "", 
                                       colnames(select(bgg.useful.dummy, starts_with("attributes.boardgamemechanic")))
)

row.names(bgg.mechanics.matrix) <- bgg.useful.dummy$details.name

bgg.mechanic.transactions <- as(bgg.mechanics.matrix, "transactions")
bgg.mechanic.freq <- itemFrequency(bgg.mechanic.transactions)

bgg.mechanics.plot <- ggplot(data.frame(mechanic=names(bgg.mechanic.freq), count=bgg.mechanic.freq) %>% 
                               arrange(desc(count)),
                             aes(reorder(mechanic, -count), count)) +
  geom_bar(stat="identity", fill=brewer.color.mechanics, alpha=.2, col=brewer.color.mechanics) +
  geom_text(aes(reorder(mechanic, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") + xlab("mechanic") +
  theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
  ylim(0,.22)

mechanics.dis <- dissimilarity(bgg.mechanic.transactions[,bgg.mechanic.freq > minimum.freq], method = "phi", which="items")
mechanics.dis[is.na(mechanics.dis)] <- 1

bgg.mechanics.dend <- mechanics.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram()
mechanics.count.discrete <- discretize(left_join(data.frame(mechanic=labels(bgg.mechanics.dend), stringsAsFactors = FALSE), bgg.mechanics.plot$data)$count, categories=10, ordered = TRUE)

bgg.mechanics.dend %>%  
  set("labels_cex", value=as.numeric(mechanics.count.discrete)/3) %>%
  plot() 

# -----------------

mechanic.rules <- apriori(bgg.mechanic.transactions,
                          parameter = list(minlen=2, supp=0.001),
                          #appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
                          control = list(verbose=TRUE))

mechanic.rules <- sort(mechanic.rules, by="lift")

# Find and remove redundant rules
subset.matrix <- is.subset(mechanic.rules, mechanic.rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
mechanic.rules.pruned <- mechanic.rules[!(colSums(subset.matrix, na.rm=T) >= 1)]

inspect(mechanic.rules.pruned)
plot(mechanic.rules.pruned, method="graph", control=list(type="items"))

#############
# CROSS #
#############

bgg.cross.matrix <- as.matrix(select(
  bgg.useful.dummy, 
  starts_with("details.minplayers="),
  #starts_with("details.maxplayers="),
  #starts_with("details.playingtime="),
  #starts_with("details.minage="),
  starts_with("attributes.boardgamecategory"),
  #starts_with("stats.weight.factor="),
  starts_with("attributes.boardgamemechanic"),
  -one_of("attributes.boardgamecategory.Memory")
)
)

colnames(bgg.cross.matrix) <- gsub(
  "attributes.(boardgamemechanic|boardgamecategory).", "", 
  colnames(bgg.cross.matrix)
)

row.names(bgg.cross.matrix) <- bgg.useful.dummy$details.name

bgg.cross.transactions <- as(bgg.cross.matrix, "transactions")
bgg.cross.freq <- itemFrequency(bgg.cross.transactions)

cross.dis <- dissimilarity(bgg.cross.transactions[,bgg.cross.freq > minimum.freq], method = "phi", which="items")
cross.dis[is.na(cross.dis)] <- 1

cross.dend <- cross.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram()

cross.colors <- ifelse(labels(cross.dend) %in% names(bgg.category.freq), 2, ifelse(labels(cross.dend) %in% names(bgg.mechanic.freq), 3, 4))
cross.dend %>% set("labels_colors", value=brewer.pal(length(unique(cross.colors)), brewer.palette)[cross.colors]) %>% plot()

# -----------------

cross.rules <- apriori(bgg.cross.transactions,
                       parameter = list(minlen=2, supp=0.005),
                       #appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
                       control = list(verbose=TRUE))

cross.rules <- sort(cross.rules, by="lift")

# Find and remove redundant rules
subset.matrix <- is.subset(cross.rules, cross.rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
cross.rules.pruned <- cross.rules[!(colSums(subset.matrix, na.rm=T) >= 1)]

inspect(cross.rules.pruned)
plot(cross.rules.pruned, method="graph", control=list(type="items"))

############################
# Similarity between games #
# ------------------------ #
# Only for top-rated games #
############################

# This analysis shows the similarities between games, for TopN games
# We can observer the strong clustering effects of weight and categories
# against mechanics and other attributes

bgg.topn.matrix <- as.matrix(select(bgg.topn.dummy 
                                    ,starts_with("details.minplayers.")
                                    ,starts_with("details.maxplayers.")
                                    ,starts_with("details.playingtime.")
                                    ,starts_with("details.minage.")
                                    ,starts_with("attributes.boardgamecategory")
                                    ,starts_with("stats.weight.factor.")
                                    ,starts_with("attributes.boardgamemechanic")
                                    ,-one_of("attributes.boardgamecategory.Memory")
)
)

colnames(bgg.topn.matrix) <- gsub("attributes.(boardgamemechanic|boardgamecategory).", "", colnames(bgg.topn.matrix))
row.names(bgg.topn.matrix) <- gsub(" ", " ", bgg.topn.dummy$details.name)

bgg.topn.transactions <- as(bgg.topn.matrix, "transactions")

games.dis <- dissimilarity(bgg.topn.transactions[1:top.rated.num,], method = "phi")
games.dis[is.na(games.dis)] <- 1


top.games.dend <- games.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram()
top.games.weight <- round_any((left_join(data.frame(details.name=labels(top.games.dend), stringsAsFactors = FALSE), bgg.topn.dummy))$stats.averageweight, 1)

top.games.dend <- top.games.dend %>% 
  set("labels_colors", value=brewer.pal(length(unique(top.games.weight)), brewer.palette)[top.games.weight]) %>%
  set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==1)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette)[1],Inf), type="all") %>%
  set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==2)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette)[2],Inf), type="all") %>%
  set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==3)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette)[3],Inf), type="all") %>%
  set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==4)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette)[4],Inf), type="all") %>%
  set("nodes_pch", 19) %>% 
  set("nodes_cex", .9) %>% 
  set("nodes_col", "orange")
plot(top.games.dend)


pdf(file="./tmp/games.similarity.huge.pdf", width=45, height=15)
plot(top.games.dend)
dev.off()

