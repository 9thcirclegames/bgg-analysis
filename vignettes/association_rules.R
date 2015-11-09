################
# REQS         #
################
if(! "dplyr" %in% installed.packages()) install.packages("dplyr", depend = TRUE)
if(! "ggplot2" %in% installed.packages()) install.packages("ggplot2", depend = TRUE)
if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)
if(! "cluster" %in% installed.packages()) install.packages("cluster", depend = TRUE)
if(! "ggdendro" %in% installed.packages()) install.packages("ggdendtro", depend = TRUE)
if(! "dummies" %in% installed.packages()) install.packages("dummies", depend = TRUE)
if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)

require(arules)
require(dplyr)
require(ggplot2)
require(arulesViz)
require(cluster)
require(ggdendro)
require(dummies)

require(bggAnalysis)

data("BoardGames")

BoardGames <- bgg.prepare.data(BoardGames)

#########################################
# PREFILTERING                          #
#########################################
bgg.useful.arules <- BoardGames %>% filter(is.na(details.yearpublished) | details.yearpublished <= 2015) %>% filter(game.type == "boardgame")

bgg.useful.dummy <- bgg.prepare.dummy(bgg.useful.arules)

#########################
# CATEGORIES SPARSENESS #
#########################
bgg.categories.matrix <- as.matrix(select(bgg.useful.dummy, starts_with("attributes.boardgamecategory")))

colnames(bgg.categories.matrix) <- gsub("attributes.boardgamecategory.", "", colnames(select(bgg.useful.dummy, starts_with("attributes.boardgamecategory"))))
row.names(bgg.categories.matrix) <- paste("Game", c(1:nrow(bgg.useful.dummy)), sep=".")

bgg.category.transactions <- as(bgg.categories.matrix, "transactions")

bgg.category.freq <- itemFrequency(bgg.category.transactions)

ggplot(data.frame(category=names(bgg.category.freq), count=bgg.category.freq) %>% arrange(desc(count)),
       aes(reorder(category, -count), count)) +
  geom_bar(stat="identity", fill="deeppink", alpha=.2, col="red") +
  geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") + xlab("Category") +
  theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
  ylim(0,.22)


########################
# MECHANICS SPARSENESS #
########################
bgg.mechanics.matrix <- as.matrix(select(bgg.useful.dummy, starts_with("attributes.boardgamemechanic")))

colnames(bgg.mechanics.matrix) <- gsub("attributes.boardgamemechanic.", "", colnames(select(bgg.useful.dummy, starts_with("attributes.boardgamemechanic"))))
row.names(bgg.mechanics.matrix) <- paste("Game", c(1:nrow(bgg.useful.dummy)), sep=".")

bgg.mechanic.transactions <- as(bgg.mechanics.matrix, "transactions")

bgg.mechanic.freq <- itemFrequency(bgg.mechanic.transactions)

ggplot(data.frame(category=names(bgg.mechanic.freq), count=bgg.mechanic.freq) %>% arrange(desc(count)),
       aes(reorder(category, -count), count)) +
  geom_bar(stat="identity", fill="darkblue", alpha=.2, col="blue") +
  geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") +
  xlab("Game Mechanic") +
  theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
  ylim(0,.22)

#####################
# CROSS             #
#####################
bgg.cross.matrix <- cbind(bgg.categories.matrix, bgg.mechanics.matrix[,-which(colnames(bgg.mechanics.matrix)=="Memory")])
bgg.cross.transactions <- as(bgg.cross.matrix, "transactions")

bgg.cross.freq <- itemFrequency(bgg.cross.transactions)

bgg.cross.plot <- data.frame(item=names(c(bgg.mechanic.freq, bgg.category.freq)), count=c(bgg.mechanic.freq, bgg.category.freq)) %>%
  group_by(item) %>% summarize(count=sum(count))
bgg.cross.plot$type <- as.factor(ifelse(bgg.cross.plot$item %in% names(bgg.category.freq==TRUE), ifelse(bgg.cross.plot$item == "Memory", "Both", "Category"), "Mechanic"))

ggplot(bgg.cross.plot %>% arrange(desc(count)) %>% filter(count>.01),
       aes(reorder(item, -count), count, fill=type))+
  geom_bar(stat="identity", alpha=.4) +
  geom_text(aes(reorder(item, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") +
  xlab("Mechanics & Categories") +
  theme(axis.text.x = element_text(angle = 90, size=11), axis.text.y = element_text(size=11)) +
  ylim(0,.22)


###########################
#         APRIORI         #
# ----------------------- #
#        Mechanics        #
###########################
mechanic.rules <- apriori(bgg.mechanic.transactions,
                          parameter = list(minlen=2, supp=0.001),
                          #appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
                          control = list(verbose=TRUE))

mechanic.rules <- sort(mechanic.rules, by="lift")

# find redundant rules
subset.matrix <- is.subset(mechanic.rules, mechanic.rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
# remove redundant rules
mechanic.rules.pruned <- mechanic.rules[!(colSums(subset.matrix, na.rm=T) >= 1)]

inspect(mechanic.rules.pruned)
plot(mechanic.rules.pruned, method="graph", control=list(type="items"))


mechanics.dis <- dissimilarity(bgg.mechanic.transactions[,itemFrequency(bgg.mechanic.transactions)>0.01], method = "phi", which="items")
mechanics.dis[is.na(mechanics.dis)] <- 1
plot(hclust(mechanics.dis), cex=1)

mechanics.clustering <- pam(mechanics.dis, k = 8)
plot(mechanics.clustering)

###########################
#         APRIORI         #
# ----------------------- #
#       Categories        #
###########################
category.rules <- apriori(bgg.category.transactions,
                          parameter = list(minlen=2, supp=0.001),
                          #appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
                          control = list(verbose=TRUE))

category.rules <- sort(category.rules, by="lift")

# find redundant rules
category.subset.matrix <- is.subset(category.rules, category.rules)
category.subset.matrix[lower.tri(category.subset.matrix, diag=T)] <- NA
# remove redundant rules
category.rules.pruned <- category.rules[!(colSums(category.subset.matrix, na.rm=T) >= 1)]

inspect(category.rules.pruned)
plot(category.rules.pruned, method="graph", control=list(type="items"))


categories.dis <- dissimilarity(bgg.category.transactions[,itemFrequency(bgg.category.transactions)>0.01], method = "phi", which="items")
categories.dis[is.na(categories.dis)] <- 1
plot(hclust(categories.dis), cex=1)

categories.clustering <- pam(categories.dis, k = 8)
plot(categories.clustering)

###########################
#         APRIORI         #
# ----------------------- #
#          Cross          #
###########################
cross.unbalanced.rules <- apriori(bgg.cross.transactions,
                                  parameter = list(minlen=2, supp=0.001),
                                  #appearance = list(rhs=c("Survived=No", "Survived=Yes"), default="lhs"),
                                  control = list(verbose=TRUE))

cross.unbalanced.rules <- sort(cross.unbalanced.rules, by="lift")

# find redundant rules
cross.unbalanced.subset.matrix <- is.subset(cross.unbalanced.rules, cross.unbalanced.rules)
cross.unbalanced.subset.matrix[lower.tri(cross.unbalanced.subset.matrix, diag=T)] <- NA
# remove redundant rules
cross.unbalanced.rules.pruned <- cross.unbalanced.rules[!(colSums(cross.unbalanced.subset.matrix, na.rm=T) >= 1)]

cross.unbalanced.dis <- dissimilarity(bgg.cross.transactions[,itemFrequency(bgg.cross.transactions)>0.01], method = "phi", which="items")
cross.unbalanced.dis[is.na(cross.unbalanced.dis)] <- 1

plot(hclust(cross.unbalanced.dis), cex=1)

cross.unbalanced.dend <- dendro_data(hclust(cross.unbalanced.dis), type="rectangle")

png(file="./tmp/cross.similarity.color.png", height = 1200, width=760)
ggplot() + 
  geom_segment(data=segment(cross.unbalanced.dend), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=(label(cross.unbalanced.dend) %>% mutate(color=(ifelse(label %in% names(bgg.category.freq), "Category", "Mechanic")))), aes(x=x, y=y, label=label, hjust=0, color=color), size=4) +
  coord_flip() + scale_y_reverse(expand=c(0.2, 0)) +
  ggtitle(expression(atop("Games Similarities", atop(italic("Using Categories and Mechanics"))))) +
  theme_dendro() +
  theme(legend.position="top")
dev.off()

# Balanced
cross.balanced.rules <- apriori(bgg.cross.transactions,
                                parameter = list(minlen=2, supp=0.001),
                                appearance = list(rhs=names(bgg.mechanic.freq), default="lhs"),
                                control = list(verbose=TRUE))

cross.balanced.rules <- sort(cross.balanced.rules, by="lift")

# find redundant rules
cross.balanced.subset.matrix <- is.subset(cross.balanced.rules, cross.balanced.rules)
cross.balanced.subset.matrix[lower.tri(cross.balanced.subset.matrix, diag=T)] <- NA
# remove redundant rules
cross.balanced.rules.pruned <- cross.balanced.rules[!(colSums(cross.balanced.subset.matrix, na.rm=T) >= 1)]

inspect(cross.balanced.rules.pruned)

# We must remove "Dice" from game categories if we're going to cross analyzis as Pr(DiceRolling|Dice) =~1


# Backwarded analysis is much more interesting...
cross.balanced.rules2 <- apriori(bgg.cross.transactions,
                                 parameter = list(minlen=2, supp=0.001),
                                 appearance = list(lhs=names(bgg.mechanic.freq), default="rhs"),
                                 control = list(verbose=TRUE))

cross.balanced.rules2 <- sort(cross.balanced.rules2, by="lift")

# find redundant rules
cross.balanced.subset.matrix2 <- is.subset(cross.balanced.rules2, cross.balanced.rules2)
cross.balanced.subset.matrix2[lower.tri(cross.balanced.subset.matrix2, diag=T)] <- NA
# remove redundant rules
cross.balanced.rules.pruned2 <- cross.balanced.rules2[!(colSums(cross.balanced.subset.matrix2, na.rm=T) >= 1)]

inspect(cross.balanced.rules.pruned2)

bgg.games.arules <- BoardGames %>% filter(is.na(details.yearpublished) | details.yearpublished <= 2015) %>% filter(game.type == "boardgame") %>% filter(stats.usersrated > 700)

bgg.games.arules <- bgg.games.arules[order(-bgg.games.arules$stats.usersrated),]

bgg.games.dummy <- bgg.prepare.dummy(bgg.games.arules)

############################
# Similarity between games #
# ------------------------ #
# Only for top-rated games #
############################
top.rated.num <- 200

bgg.games.arules <- sort(BoardGames, sort="details.usersrating") %>% filter(is.na(details.yearpublished) | details.yearpublished <= 2015) %>% filter(game.type == "boardgame") %>% filter(stats.usersrated > 700)

bgg.games.dummy <- bgg.prepare.dummy(bgg.games.arules)
bgg.games.dummy$stats.weight.factor <- round_any(bgg.games.dummy$stats.averageweight, .5)
bgg.games.dummy$details.playingtime <- round_any(bgg.games.dummy$details.playingtime, 15)

bgg.games.dummy <- cbind(
  bgg.games.dummy
  ,dummy("details.minplayers", bgg.games.dummy, sep=".")
  ,dummy("details.maxplayers", bgg.games.dummy, sep=".")
  ,dummy("details.playingtime", bgg.games.dummy, sep=".")
  ,dummy("details.minage", bgg.games.dummy, sep=".")
  ,dummy("stats.weight.factor", bgg.games.dummy, sep=".")
)

bgg.games.matrix <- as.matrix(select(bgg.games.dummy, 
                                     starts_with("details.minplayers."),
                                     starts_with("details.maxplayers."),
                                     starts_with("details.playingtime."),
                                     starts_with("details.minage."),
                                     starts_with("attributes.boardgamecategory"),
                                     starts_with("stats.weight.factor"),
                                     starts_with("attributes.boardgamemechanic"),
                                     -one_of("attributes.boardgamecategory.Memory")
                                     ,-one_of("attributes.boardgamemechanic.DiceRolling")
                                     )
                              )

colnames(bgg.games.matrix) <- gsub("attributes.(boardgamemechanic|boardgamecategory).", "", colnames(bgg.games.matrix))
row.names(bgg.games.matrix) <- gsub(" ", " ", bgg.games.dummy$details.name)

bgg.games.transactions <- as(bgg.games.matrix, "transactions")

pdf(file="./tmp/games.similarity.huge.pdf", width=35)
games.dis <- dissimilarity(bgg.games.transactions[1:top.rated.num,], method = "phi")
games.dis[is.na(games.dis)] <- 1
plot(hclust(games.dis), cex=.6)
dev.off()

