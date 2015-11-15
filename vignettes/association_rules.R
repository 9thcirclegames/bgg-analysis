################
# REQS         #
################
lapply(c("dplyr", "ggplot2", "arules", "cluster", "dummies", "arulesViz", "RColorBrewer"), function(pkg){
  if(! require(pkg, character.only = TRUE)) install.packages(pkg, depend = TRUE)
  library(pkg, character.only = TRUE)
})

if(! require(dendextend)) {
  devtools::install_github('talgalili/dendextend')
  devtools::install_github('talgalili/dendextendRcpp')
}
require(dendextend)

require(bggAnalysis)

data("BoardGames")

brewer.palette.categories <- "PiYG"
brewer.palette.mechanics <- "RdBu"
brewer.palette.top <- "Spectral"

minimum.support <- .001
minimum.freq <- .01

#########################################
# Prepare Data Sets                     #
#########################################
bgg.useful.arules <- bgg.prepare.data(BoardGames) %>%
  filter(is.na(details.yearpublished) | details.yearpublished <= 2015) %>%
  filter(game.type == "boardgame")

rownames(bgg.useful.arules) <- make.names(bgg.useful.arules$details.name, unique=TRUE)

bgg.useful.dummy <- bgg.prepare.dummy(bgg.useful.arules)

bgg.useful.dummy$stats.average.factor <- discretize(bgg.useful.dummy$stats.average, method="frequency", categories = 5, ordered = TRUE)

bgg.useful.dummy <- cbind(
  bgg.useful.dummy
  ,dummy("details.minplayers", bgg.useful.dummy, sep="=")
  ,dummy("details.maxplayers", bgg.useful.dummy, sep="=")
  ,dummy("details.playingtime", bgg.useful.dummy, sep="=")
  ,dummy("details.minage", bgg.useful.dummy, sep="=")
  ,dummy("stats.weight.factor", bgg.useful.dummy, sep="=")
  ,dummy("stats.average.factor", bgg.useful.dummy, sep="=")
  ,dummy("polls.language.dependence", bgg.useful.dummy, sep="=")
)

# TopN
top.rated.num <- 200

bgg.topn.arules <-  (bgg.useful.arules %>% filter(stats.usersrated > 700))
bgg.topn.arules <-  bgg.topn.arules[order(-bgg.topn.arules$stats.usersrated),]
rownames(bgg.topn.arules) <- make.names(bgg.topn.arules$details.name, unique=TRUE)

bgg.topn.dummy <- bgg.prepare.dummy(bgg.topn.arules)

##############
# CATEGORIES #
##############
bgg.categories.analysis <- bgg.discrete.attribute.analysis(select(bgg.useful.dummy,
                                                                  starts_with("attributes.boardgamecategory")),
                                                           apriori.parameters = list(minlen = 2, supp = minimum.support),
                                                           minimum.freq = minimum.freq,
                                                           brewer.palette = brewer.palette.categories)


# Inspect the mined rules
inspect(sort(bgg.categories.analysis$apriori.rules, by="lift"))
plot(bgg.categories.analysis$apriori.rules, method="graph", control=list(type="items"))

plot(bgg.categories.analysis$attribute.dend)

bgg.categories.analysis$attribute.plot

#############
# MECHANICS #
#############

bgg.mechanics.analysis <- bgg.discrete.attribute.analysis(select(bgg.useful.dummy,
                                                                 starts_with("attributes.boardgamemechanic")),
                                                          apriori.parameters = list(minlen = 2, supp = minimum.support),
                                                          minimum.freq = minimum.freq,
                                                          brewer.palette = brewer.palette.mechanics)


# Inspect the mined rules
inspect(sort(bgg.mechanics.analysis$apriori.rules, by="lift"))
plot(bgg.mechanics.analysis$apriori.rules, method="graph", control=list(type="items"))

plot(bgg.mechanics.analysis$attribute.dend)

bgg.mechanics.analysis$attribute.plot

#############
# MIXED     #
#############

bgg.cross.data <-  bgg.useful.dummy %>% filter(stats.average > 0) %>% select(
  #starts_with("details.minplayers="),
  #starts_with("details.maxplayers="),
  #starts_with("details.playingtime="),
  #starts_with("details.minage="),
  starts_with("attributes.boardgamecategory"),
  starts_with("stats.weight.factor="),
  #starts_with("polls.language_dependency="),
  #starts_with("attributes.boardgamemechanic"),
  starts_with("stats.average.factor="),
  -one_of("attributes.boardgamecategory.Memory", "attributes.boardgamecategory.Dice")
)

bgg.cross.analysis <- bgg.discrete.attribute.analysis(
 bgg.cross.data,
  apriori.parameters = list(minlen = 2, supp = minimum.support),
  apriori.appearance = list(
    #rhs=gsub("attributes.boardgamecategory.", "", colnames(bgg.cross.data)[which(grepl("^attributes.boardgamecategory", colnames(bgg.cross.data)))]),
    rhs=colnames(bgg.cross.data)[which(grepl("^stats.average.factor", colnames(bgg.cross.data)))],
    default="lhs"
    ),
  minimum.freq = minimum.freq,
  brewer.palette = brewer.palette.top)

# Inspect the mined rules
inspect(sort(bgg.cross.analysis$apriori.rules, by="lift"))
[plot(bgg.cross.analysis$apriori.rules, method="graph", control=list(type="items"))

plot(bgg.cross.analysis$attribute.dend)

bgg.cross.analysis$attribute.plot

# ---------------- Frequent Itemsets --------------------

bgg.frequent.data <- select(
  bgg.useful.dummy,
  starts_with("details.minplayers="),
  #starts_with("details.maxplayers="),
  starts_with("details.playingtime="),
  starts_with("details.minage="),
  starts_with("attributes.boardgamecategory"),
  starts_with("stats.weight.factor="),
  starts_with("polls.language_dependency="),
  starts_with("attributes.boardgamemechanic"),
  -one_of("attributes.boardgamecategory.Memory"),
  -one_of("attributes.boardgamecategory.Dice")
)

bgg.frequent.analysis <- bgg.discrete.attribute.analysis(
  bgg.frequent.data,
  apriori.parameters = list(minlen = 3, supp = 0.01,  target="frequent itemsets"),
  apriori.appearance = list(none = c("details.minplayers=2",
                                     "details.minplayers=4",
                                     "stats.weight.factor=0",
                                     "details.playingtime=0",
                                     "details.minage=NA"), default="both"),
  minimum.freq,
  brewer.palette = brewer.palette.top)

inspect(sort(bgg.frequent.analysis$apriori.rules, by="support"))
plot(bgg.frequent.analysis$apriori.rules, method="graph", control=list(type="items"))

plot(bgg.frequent.analysis$attribute.dend)

bgg.cross.analysis$attribute.plot

############################
# Similarity between games #
# ------------------------ #
# Only for top-rated games #
############################

# This analysis shows the similarities between games, for TopN games
# We can observer the strong clustering effects of weight and categories
# against mechanics and other attributes

top.games.dend <- (function(){

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

  colnames(bgg.topn.matrix) <- gsub("attributes.boardgame(memechanic|category).", "", colnames(bgg.topn.matrix))

  bgg.topn.transactions <- as(bgg.topn.matrix, "transactions")

  games.dis <- dissimilarity(bgg.topn.transactions[1:top.rated.num,], method = "phi")
  games.dis[is.na(games.dis)] <- 1


  top.games.dend <- games.dis %>% hclust %>% as.dendrogram %>% hang.dendrogram()
  top.games.weight <- round_any((left_join(data.frame(details.name=labels(top.games.dend), stringsAsFactors = FALSE), bgg.topn.dummy))$stats.averageweight, 1)

  top.games.dend <- top.games.dend %>%
    set("labels_colors", value=brewer.pal(length(unique(top.games.weight)), brewer.palette.top)[top.games.weight]) %>%
    set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==1)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette.top)[1],Inf), type="all") %>%
    set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==2)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette.top)[2],Inf), type="all") %>%
    set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==3)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette.top)[3],Inf), type="all") %>%
    set("by_labels_branches_col", value=labels(top.games.dend)[which(top.games.weight==4)], TF_values=c(brewer.pal(length(unique(top.games.weight)), brewer.palette.top)[4],Inf), type="all") %>%
    set("nodes_pch", 19) %>%
    set("nodes_cex", .9) %>%
    set("nodes_col", "orange")

  return(top.games.dend)

})()

pdf(file="./tmp/games.similarity.huge.pdf", width=45, height=15)
plot(top.games.dend)
dev.off()

