data("BoardGames")

BoardGames <- bgg.prepare.data(BoardGames)
BoardGames.test <- BoardGames %>% filter(is.na(details.yearpublished) | details.yearpublished <= 2015)


# summarize by year
boardgames.by.years <- BoardGames %>%
  mutate(details.yearpublished=as.numeric(details.yearpublished)) %>%
  filter(!is.na(details.yearpublished)) %>%
  filter(details.yearpublished <= 2015) %>%
  group_by(details.yearpublished) %>%
  summarise(count = n())

# we got a 7700 missing and 1q=1994, Median=2006
ggplot(boardgames.by.years,aes(details.yearpublished,count)) +
  geom_line(col="red", lwd=1) +
  ggtitle(paste('Board Games released by Year, ', min(boardgames.by.years$details.yearpublished),'-', max(boardgames.by.years$details.yearpublished))) +
  xlab('Year') +
  ylab("Number of Games") +
  ylim(c(0,max(boardgames.by.years$count)))

# Stimiamo la % di missing degli attributi
col.nulls <- colMeans(is.na(BoardGames))
col.nulls.percent <- percent(colMeans(is.na(BoardGames)))
names(col.nulls.percent) <- names(col.nulls)

which(col.nulls)


# Cumulative Freq-plot of year (median in grey -> 2006)
ggplot(BoardGames, aes(as.numeric(details.yearpublished))) +
  stat_ecdf(geom="smooth", lwd=1, col="red") +
  ggtitle("Cumulative freq-plot of Release Year") +
  geom_vline(xintercept=median(as.numeric(BoardGames$details.yearpublished), na.rm=TRUE), color = 'grey')

# Cumulative Freq-plot of user ratings. (80% has less than 25 ratings)
# We really don't wont the outliers to pollute the plot
quantile(BoardGames$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)

ggplot(BoardGames, aes(stats.usersrated)) +
  stat_ecdf(geom="step", lwd=1, col="red") +
  ggtitle("Cumulative freq-plot of User Ratings") +
  xlim(0, quantile(BoardGames$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)['95%'])

# ---------> filter out games where ratings < 25

bgg.useful <- BoardGames %>% filter(stats.usersrated >= 25)

# Cumulative Freq-plot of average rating
quantile(bgg.useful$stats.average, seq(0, 1, 0.05), na.rm = TRUE)

ggplot(bgg.useful, aes(stats.usersrated)) +
  stat_ecdf(geom="step", lwd=1, col="red") +
ggtitle("Cumulative freq-plot of User Ratings") +
  xlim(c(0, quantile(bgg.useful$stats.usersrated, seq(0, 1, 0.05), na.rm = TRUE)['95%']))

# Ratings are gaussian! Holy crap, this is cool (and unexpected)
ggplot(bgg.useful, aes(x = stats.average)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
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
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="deeppink", alpha=.2, col="deeppink") + geom_density(col="deeppink", lwd=1) +
  xlim(1,5)

ggplot(bgg.useful, aes(details.maxplayers)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill="deeppink", alpha=.2, col="deeppink") + geom_density(col="deeppink", lwd=1) +
  xlim(1,30)

ggplot(bgg.useful, aes(x = stats.owned)) +
  geom_histogram(aes(y = ..density..), binwidth = 30, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  geom_vline(xintercept=median(bgg.useful$stats.owned, na.rm=TRUE), color="black") +
  xlim(c(0, quantile(bgg.useful$stats.owned, seq(0, 1, 0.05), na.rm = TRUE)['95%']))
