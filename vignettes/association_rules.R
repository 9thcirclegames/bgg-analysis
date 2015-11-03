################
# REQS         #
################
if(! "dplyr" %in% installed.packages()) install.packages("dplyr", depend = TRUE)
if(! "ggplot2" %in% installed.packages()) install.packages("ggplot2", depend = TRUE)
if(! "arules" %in% installed.packages()) install.packages("arules", depend = TRUE)

if(! "arulesViz" %in% installed.packages()) install.packages("arulesViz", depend = TRUE)

require(arules)
require(dplyr)
require(ggplot2)

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

bgg.category.transactions <- as(bgg.useful.matrix, "transactions")

bgg.category.freq <- itemFrequency(bgg.category.transactions)

ggplot(data.frame(category=names(bgg.category.freq), count=bgg.category.freq) %>% arrange(desc(count)),
       aes(reorder(category, -count), count)) +
  geom_bar(stat="identity", fill="deeppink", alpha=.2, col="red") +
  geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") + xlab("Category") +
  theme(axis.text.x = element_text(angle = 90)) +
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
  theme(axis.text.x = element_text(angle = 90)) +
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
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,.22)


