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


data("BoardGames")

BoardGames <- bgg.prepare.data(BoardGames)

#########################################
# PREFILTERING                          #
#########################################
bgg.useful.arules <- BoardGames %>% filter(is.na(details.yearpublished) | details.yearpublished <= 2015) %>% filter(game.type == "boardgame")

bgg.useful.dummy <- bgg.prepare.dummy(bgg.useful.arules)

bgg.useful.matrix <- as.matrix(select(bgg.useful.dummy, starts_with("attributes.boardgamecategory")))

colnames(bgg.useful.matrix) <- gsub("attributes.boardgamecategory.", "", colnames(select(bgg.useful.dummy, starts_with("attributes.boardgamecategory"))))
row.names(bgg.useful.matrix) <- paste("Game", c(1:nrow(bgg.useful.dummy)), sep=".")

bgg.trans <- as(bgg.useful.matrix, "transactions")
bgg.trans

bgg.category.freq <- itemFrequency(bgg.trans)

ggplot(data.frame(category=names(bgg.category.freq), count=bgg.category.freq) %>% arrange(desc(count)),
       aes(reorder(category, -count), count)) +
  geom_bar(stat="identity", fill="deeppink", alpha=.2, col="red") +
  geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") +
  xlab("Category") +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,.22)


#############################
bgg.useful.matrix2 <- as.matrix(select(bgg.useful.dummy, starts_with("attributes.boardgamemechanic")))

colnames(bgg.useful.matrix2) <- gsub("attributes.boardgamemechanic.", "", colnames(select(bgg.useful.dummy, starts_with("attributes.boardgamemechanic"))))
row.names(bgg.useful.matrix2) <- paste("Game", c(1:nrow(bgg.useful.dummy)), sep=".")

bgg.trans2 <- as(bgg.useful.matrix2, "transactions")
bgg.trans2

bgg.mechanic.freq <- itemFrequency(bgg.trans2)

ggplot(data.frame(category=names(bgg.mechanic.freq), count=bgg.mechanic.freq) %>% arrange(desc(count)),
       aes(reorder(category, -count), count)) +
  geom_bar(stat="identity", fill="deeppink", alpha=.2, col="red") +
  geom_text(aes(reorder(category, -count), count, label=paste((round(count,3)*100), "%", sep="")), angle=90, hjust=-.1)+
  ylab("Frequency in Games") +
  xlab("Game Mechanic") +
  theme(axis.text.x = element_text(angle = 90)) +
  ylim(0,.22)
