################
# REQS         #
################
lapply(c("dplyr", "ggplot2", "GGally", "plyr", "wesanderson"), function(pkg){
  if(! require(pkg, character.only = TRUE)) install.packages(pkg, depend = TRUE)
  library(pkg, character.only = TRUE)
})

require(bggAnalysis)

data("BoardGames")

BoardGames <- bgg.prepare.data(BoardGames)
#########################################
# PREFILTERING                          #
#########################################
bgg.useful <- BoardGames %>% filter(is.na(details.yearpublished) | details.yearpublished <= 2016) %>% filter(stats.usersrated >= 25, game.type == "boardgame")
BoardGames.test <- BoardGames %>% filter(details.yearpublished > 2016) %>% filter(game.type == "boardgame")

# summarize by year
boardgames.by.years <- BoardGames %>%
  mutate(details.yearpublished=as.numeric(details.yearpublished)) %>%
  filter(!is.na(details.yearpublished)) %>%
  filter(details.yearpublished <= 2016) %>%
  group_by(details.yearpublished) %>%
  dplyr::summarise(count = n())

# we got a 7700 missing and 1q=1994, Median=2006
ggplot(boardgames.by.years,aes(details.yearpublished,count)) +
  geom_line(col="red", lwd=1) +
  ggtitle(paste('Board Games released by Year, ', min(boardgames.by.years$details.yearpublished),'-', max(boardgames.by.years$details.yearpublished))) +
  xlab('Year') +
  ylab("Number of Games") +
  ylim(c(0,max(boardgames.by.years$count)))

# Stimiamo la % di missing degli attributi
col.nulls <- colMeans(is.na(BoardGames))
col.nulls.percent <- scales::percent(colMeans(is.na(BoardGames)))
names(col.nulls.percent) <- names(col.nulls)

# Cumulative Freq-plot of year (median in grey -> 2006)
ggplot(BoardGames, aes(as.numeric(details.yearpublished))) +
  stat_ecdf(geom="step", lwd=.5, col="red") +
  ggtitle("Cumulative freq-plot of Release Year") +
  geom_vline(xintercept=median(as.numeric(BoardGames$details.yearpublished), na.rm=TRUE), color = 'grey')

# Cumulative Freq-plot of user ratings. (80% has less than 35 ratings)
# We really don't wont the outliers to pollute the plot
quantile(BoardGames$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)

ggplot(BoardGames, aes(stats.usersrated)) +
  stat_ecdf(geom="step", lwd=.5, col="blue") +
  ggtitle("Cumulative freq-plot of User Ratings") +
  xlim(0, quantile(BoardGames$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)['95%'])

# Cumulative Freq-plot of average rating
quantile(bgg.useful$stats.average, seq(0, 1, 0.05), na.rm = TRUE)

ggplot(bgg.useful, aes(stats.usersrated)) +
  stat_ecdf(geom="step", lwd=.5, col="red") +
  ggtitle("Cumulative freq-plot of User Ratings") +
  xlim(c(0, quantile(bgg.useful$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

#########################################
# PREPARING SOME TOP RANK ATTRIBUTES    #
#########################################
how.many.tops <- 5

bgg.categories.freq <- (bgg.useful %>% group_by(attributes.boardgamecategory) %>% dplyr::summarize(count=n()))
bgg.categories.freq <- bgg.categories.freq[order(-bgg.categories.freq[,2]),]
bgg.top.categories <- subset(bgg.useful, attributes.boardgamecategory %in% head(bgg.categories.freq$attributes.boardgamecategory, how.many.tops))
bgg.worst.categories <- subset(bgg.useful, !(attributes.boardgamecategory %in% head(bgg.categories.freq$attributes.boardgamecategory, how.many.tops)))


bgg.mechanics.freq <- (bgg.useful %>% group_by(attributes.boardgamemechanic) %>% dplyr::summarize(count=n()))
bgg.mechanics.freq <- bgg.mechanics.freq[order(-bgg.mechanics.freq[,2]),]
bgg.top.mechanics <- subset(bgg.useful, attributes.boardgamemechanic %in% head(bgg.mechanics.freq$attributes.boardgamemechanic, how.many.tops))
bgg.worst.mechanics <- subset(bgg.useful, !(attributes.boardgamemechanic %in% head(bgg.mechanics.freq$attributes.boardgamemechanic, how.many.tops)))

#########################################
# EXAMINING THE AVERAGE RATINGS         #
#########################################

# Ratings are gaussian
ggplot(bgg.useful, aes(x = stats.average)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  xlim(0,10) +
  geom_vline(xintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black")

# Splitting by year class
# There is a strong positive shift in average ratings among the last decade
ggplot(bgg.useful %>%
         mutate(year.discrete=as.factor(round_any(as.numeric(details.yearpublished), 5))) %>%
         filter(as.numeric(details.yearpublished) >=1980 & as.numeric(details.yearpublished) < 2015)
       , aes(year.discrete, stats.average, fill=year.discrete)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Rating") + xlab("Decade") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black")

# Still gaussian while splitting by language_dependence
# Unplayable games seems to suffer a considerable rating loss
ggplot(bgg.useful, aes(x = stats.average, colour=polls.language_dependence)) +
  geom_density() +
  geom_density(aes(x = stats.average), col="red", lwd=1, fill="deeppink", alpha=.2) +
  xlim(0,10) +
  theme_bw() +
  geom_vline(xintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  scale_fill_manual(values=wes_palette(name="Darjeeling"))

ggplot(bgg.useful %>%
         mutate(year.discrete=as.factor(signif(as.numeric(details.yearpublished), 3))) %>%
         filter(as.numeric(details.yearpublished) >=1980 & as.numeric(details.yearpublished) < 2015)
       , aes(year.discrete, stats.average, fill=polls.language_dependence)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Density of Average Ratings") + xlab("Average Rating") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  scale_fill_manual(values=wes_palette(name="Darjeeling"))

# Some gamemechanics express a gaussian distribution with a different median than the cumulative distribution
# game mechanics from the top could be a good classification attribute: Roll, Tile Placement, Hand Managements
ggplot(bgg.useful %>%
         mutate(attributes.top.mechanics=ifelse(game.id %in% bgg.top.mechanics$game.id, attributes.boardgamemechanic, "Other Mechanic")),
       aes(x = stats.average, colour=attributes.top.mechanics)) +
  geom_density() +
  geom_density(data=bgg.useful, aes(x = stats.average), col="red", lwd=1, fill="deeppink", alpha=.2) +
  xlim(0,10) +
  geom_vline(xintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black")

# Mutual effects between mechanics and categories
ggplot(bgg.useful %>%
         mutate(attributes.top.categories=ifelse(game.id %in% bgg.top.categories$game.id, attributes.boardgamecategory, "Other Category")) %>%
         mutate(attributes.top.mechanics=ifelse(game.id %in% bgg.top.mechanics$game.id, attributes.boardgamemechanic, "Other Mechanic"))
       ,
       aes(x = stats.average, colour=attributes.top.categories)) +
  geom_density() +
  geom_density(data=bgg.useful, aes(x = stats.average), col="red", lwd=1, fill="deeppink", alpha=.2) +
  xlim(0,10) +
  geom_vline(xintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black") +
  facet_wrap(~ attributes.top.mechanics, nrow=3)

# Historical wargames have higher average rating, yet still gaussian distribution
ggplot(bgg.useful %>% mutate(attributes.top.categories=ifelse(game.id %in% bgg.top.categories$game.id, attributes.boardgamecategory, "Other Category")),
       aes(x = stats.average, colour=attributes.top.categories)) +
  geom_density() +
  geom_density(data=bgg.useful, aes(x = stats.average), col="red", lwd=1, fill="deeppink", alpha=.2) +
  xlim(0,10) +
  geom_vline(xintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black")

ggplot(bgg.useful, aes(x = stats.stddev)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  geom_vline(xintercept=mean(bgg.useful$stats.stddev, na.rm=TRUE), color="black")

# Cumulative Freq-plot of minimum age
quantile(bgg.useful$details.minage, seq(0, 1, 0.05), na.rm = TRUE)

# minage is not gaussian
ggplot(bgg.useful, aes(details.minage)) +
  geom_histogram(binwidth = 1, fill="deeppink", col="deeppink", alpha=.2)

# playingtime looks lognormal maybe...
quantile(bgg.useful$details.playingtime, seq(0, 1, 0.05), na.rm = TRUE)
ggplot(bgg.useful, aes(details.playingtime)) +
  geom_histogram(binwidth = 30, fill="deeppink", col="deeppink", alpha=.2) +
  xlim(c(0, quantile(bgg.useful$details.playingtime, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

# ...ok maybe not :)
ggplot(bgg.useful, aes(details.playingtime)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill="deeppink", alpha=.2, col="deeppink") + geom_density(col="deeppink", lwd=1) +
  xlim(c(0, quantile(bgg.useful$details.playingtime, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

# ok let's try the average weight. O is missing so let me leave it out. this distro is a multimodal I could love, sometimes...
ggplot(bgg.useful, aes(stats.averageweight)) +
  geom_histogram(aes(y = ..density..), binwidth = .05, fill="deeppink", alpha=.2, col="deeppink") + geom_density(col="deeppink", lwd=1) +
  xlim(1,5)

ggplot(bgg.useful, aes(details.maxplayers)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill="deeppink", alpha=.2, col="deeppink") + geom_density(col="deeppink", lwd=1) +
  xlim(1,30)

#### tutte le stats seguono la powerlaw...

# segue la power law
ggplot(bgg.useful, aes(x = stats.owned)) +
  geom_histogram(aes(y = ..density..), binwidth = 30, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  geom_vline(xintercept=median(bgg.useful$stats.owned, na.rm=TRUE), color="black") +
  xlim(c(0, quantile(bgg.useful$stats.owned, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

ggplot(bgg.useful, aes(x = stats.usersrated)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  geom_vline(xintercept=median(bgg.useful$stats.owned, na.rm=TRUE), color="black") +
  xlim(c(0, quantile(bgg.useful$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

ggplot(bgg.useful, aes(x = stats.wishing)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  geom_vline(xintercept=median(bgg.useful$stats.owned, na.rm=TRUE), color="black") +
  xlim(c(0, quantile(bgg.useful$stats.wishing, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

ggplot(bgg.useful, aes(x = stats.averageweight)) +
  geom_histogram(aes(y = ..density..), binwidth = .05, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1)

# la variabile dipendente è normale, tutte le statistiche di collection seguono la powerlaw

##########################################
# REGRESSION BETWEEN WEIGHT AND AVERAGE  #
##########################################

# There is an evident positive regression between weight and rating
# This is due the fact that BGG users are almost hardcore gamers
ggplot(bgg.useful %>% filter(stats.averageweight > 0), aes(x=stats.averageweight, y=stats.average)) +
  geom_point(alpha=.2, col="deeppink") +
  geom_smooth(method="lm", lwd=1, fullrange=TRUE) +
  ylim(2.5, 9)

# For simpler mechanics and games, the positive regression between weight and rating is stronger than for complex games like hex-and-counter
ggplot(bgg.useful %>% filter(stats.averageweight > 0) %>% mutate(attributes.top.mechanics=ifelse(game.id %in% bgg.top.mechanics$game.id, attributes.boardgamemechanic, "Other Mechanic")),
       aes(x=stats.averageweight, y=stats.average, colour=factor(attributes.top.mechanics))) +
  geom_point(alpha=.2) +
  geom_smooth(method="lm", lwd=1, fullrange=TRUE) +
  geom_smooth(data=bgg.useful %>% filter(stats.averageweight > 0),
              aes(x=stats.averageweight, y=stats.average), method="lm", se=FALSE, col="grey") +
  facet_wrap(~ attributes.top.mechanics, nrow=3) +
  ylim(2.5, 9)

# It doens't seems to be an effect of language dependence between weight and rating
ggplot(bgg.useful %>% filter(stats.averageweight > 0),
       aes(x=stats.averageweight, y=stats.average, colour=polls.language_dependence)) +
  geom_point(alpha=.2) +
  geom_smooth(method="lm", lwd=1, fullrange=TRUE) +
  geom_smooth(data=bgg.useful %>% filter(stats.averageweight > 0),
              aes(x=stats.averageweight, y=stats.average), method="lm", se=FALSE, col="grey") +
  facet_grid(. ~ polls.language_dependence) +
  ylim(2.5, 9)

# The positive regression between weight and rating is less strong in wargames and complex categories
ggplot(bgg.useful %>% filter(stats.averageweight > 0) %>% mutate(attributes.top.categories=ifelse(game.id %in% bgg.top.categories$game.id, attributes.boardgamecategory, "Other Category")),
       aes(x=stats.averageweight, y=stats.average, colour=factor(attributes.top.categories))) +
  geom_point(alpha=.2) +
  geom_smooth(method="lm", lwd=1, fullrange=TRUE) +
  geom_smooth(data=bgg.useful %>% filter(stats.averageweight > 0),
              aes(x=stats.averageweight, y=stats.average), method="lm", se=FALSE, col="grey") +
  facet_grid(. ~ attributes.top.categories) +
  ylim(2.5, 9)

#######################################
# LANGUAGE DEPENDENCE                 #
#######################################
# As seen before, only unplayable games suffer a minor rating loss
ggplot(bgg.useful %>% mutate(attributes.top.mechanics=ifelse(game.id %in% bgg.top.mechanics$game.id, attributes.boardgamemechanic, "Other Mechanic")),
       aes(x=polls.language_dependence, y=stats.average, colour=polls.language_dependence)) +
  geom_boxplot(alpha=.2) +
  #facet_grid(. ~ attributes.top.mechanics)
  ylim(2.5, 9)

# Weird enough, unplayable games are rated as the most simpler.
# I think that the "Unplayable status could have some severe issues (does the rater understand the meaning?)
ggplot(bgg.useful %>% filter(stats.averageweight > 0) %>% mutate(attributes.top.mechanics=ifelse(game.id %in% bgg.top.mechanics$game.id, attributes.boardgamemechanic, "Other Mechanic")),
       aes(x=polls.language_dependence, y=stats.averageweight, colour=polls.language_dependence)) +
  geom_boxplot(alpha=.2) +
  facet_grid(. ~ attributes.top.mechanics)
ylim(0, 5)

#
ggplot(bgg.useful, aes(x=stats.trading, y=stats.average, color=attributes.boardgamecategory)) +
  geom_point(alpha=.2, col="deeppink") +
  geom_smooth(data=subset(bgg.useful,
                          attributes.boardgamecategory %in% head(bgg.categories.freq[ order(-bgg.categories.freq[,2]), ]$attributes.boardgamecategory,10)), aes(x=stats.trading, y=stats.average, color=attributes.boardgamecategory), method="lm", se=FALSE) +
  xlim(c(0, quantile(bgg.useful$stats.trading, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

# regressione rating e playingtime
ggplot(bgg.useful, aes(x=details.playingtime, y=stats.average)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=1,height=.5)) +
  geom_smooth(method="lm", lwd=1) +
  geom_smooth(data=subset(bgg.useful,
                          attributes.boardgamemechanic %in% head(bgg.mechanics.freq[ order(-bgg.mechanics.freq[,2]), ]$attributes.boardgamemechanic,5)), aes(x=details.playingtime, y=stats.average, color=attributes.boardgamemechanic),
              method="lm",
              se=FALSE,
              fullrange=TRUE) +
  xlim(c(0, quantile(bgg.useful$details.playingtime, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

# players

ggplot(bgg.useful, aes(x=details.minplayers, y=stats.average)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=1,height=.5)) +
  geom_smooth(method="lm", lwd=1) +
  geom_smooth(data=subset(bgg.useful,
                          attributes.boardgamemechanic %in% head(bgg.mechanics.freq[ order(-bgg.mechanics.freq[,2]), ]$attributes.boardgamemechanic,5)), aes(x=details.minplayers, y=stats.average, color=attributes.boardgamemechanic),
              method="lm",
              se=FALSE,
              fullrange=TRUE)

ggplot(bgg.useful, aes(x=details.maxplayers, y=stats.average)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=.2,height=.2)) +
  geom_smooth(method="lm", lwd=1) +
  geom_smooth(data=subset(bgg.useful,
                          attributes.boardgamemechanic %in% head(bgg.mechanics.freq[ order(-bgg.mechanics.freq[,2]), ]$attributes.boardgamemechanic,5)), aes(x=details.maxplayers, y=stats.average, color=attributes.boardgamemechanic),
              method="lm",
              se=FALSE,
              fullrange=TRUE)+
  xlim(c(0, quantile(bgg.useful$details.maxplayers, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

ggplot(bgg.useful, aes(x=(details.maxplayers - details.minplayers), y=stats.average)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=.2,height=.2)) +
  geom_smooth(lwd=1) +
  xlim(c(0, quantile(bgg.useful$details.maxplayers, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

ggplot(bgg.useful, aes(y=(details.maxplayers - details.minplayers), x=details.minplayers)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=.2,height=.2)) +
  geom_smooth(lwd=1, method="lm") +
  xlim(c(0, quantile(bgg.useful$details.maxplayers, seq(0, 1, 0.05), na.rm = TRUE)['95%']))+
  ylim(0,12)

ggplot(bgg.useful, aes(y=details.maxplayers, x=details.minplayers)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=.2,height=.2)) +
  geom_smooth(lwd=1, method="lm") +
  xlim(c(0, quantile(bgg.useful$details.maxplayers, seq(0, 1, 0.05), na.rm = TRUE)['95%']))+
  ylim(0,12)

ggplot(bgg.useful, aes(y=details.maxplaytime, x=details.maxplayers)) +
  geom_point(alpha=.2, col="deeppink", position=position_jitter(width=.2,height=2)) +
  geom_smooth(lwd=1) +
  ylim(c(0, quantile(bgg.useful$details.maxplaytime, seq(0, 1, 0.05), na.rm = TRUE)['95%']))+
  xlim(0,10)

############################
# CORRELATION PLOTS        #
############################
bgg.pairs <- bgg.useful %>%
  filter(as.numeric(details.yearpublished) >=1980 & as.numeric(details.yearpublished) < 2015) %>%
  filter(details.playingtime <= quantile(bgg.useful$details.playingtime, seq(0, 1, 0.05), na.rm = TRUE)['95%']) %>%
  filter(details.minplayers < 5 & details.minplayers > 0) %>%
  filter(!is.na(stats.average) & stats.average > 0) %>%
  filter(!is.na(stats.averageweight) & stats.averageweight > 0) %>%
  mutate(
    details.minplayers=as.factor(details.minplayers),
    details.maxplayers=as.factor(details.maxplayers),
    year.discrete=as.factor(signif(as.numeric(details.yearpublished), 3))) %>%
  #select(one_of("stats.average", "details.minplayers", "details.playingtime", "stats.averageweight", "year.discrete"))
  select(one_of("stats.average", "details.minplayers", "details.playingtime", "stats.averageweight", "polls.language_dependence"))


# Gained insights:
# 1, Positive regressions between weight and playingtime
# 2. Quality is increasing over time. This is expecially true for the last decade
# 3. Avg. Weight is going decreasing in the last 3 decades
# 4. There is an increasing positive regression between weight and average that is going to be significant in the last decade for all games
# except those that are strongly language dependant.
# 5. Playing time assumes mostly discrete values and it could be discretized among these value. It's decreasing over time in the last two
#    decades
# 6. Years can be discretized in interval of 5 or 10yrs (TBD)
# 7. The playing time is still greater for 1-2 players game. It's decreasing in the last decade, but still very high for those two segments
ggpairs(bgg.pairs, color="polls.language_dependence", alpha=.4, lower=list(continuous="smooth"))


#############################
# DISCRETIZING PLAYING TIME #
#############################

# It could safely be discretized in itnerval of 15mins
ggplot(bgg.useful, aes(x = round_any(details.playingtime, 15))) +
  geom_histogram(aes(y = ..count..), binwidth = 5, fill="red", alpha=.2, col="deeppink") +
  geom_vline(xintercept=mean(bgg.useful$details.playingtime, na.rm=TRUE), color="black") +
  xlim(c(0, quantile(bgg.useful$details.playingtime, seq(0, 1, 0.05), na.rm = TRUE)['95%']))
