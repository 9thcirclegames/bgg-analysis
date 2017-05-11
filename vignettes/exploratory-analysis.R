## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)

## ---- echo=FALSE, results='hide'-----------------------------------------
######################
# REQUIREMENTS       #
######################
if(FALSE %in% 
  do.call(c,lapply(c("plyr", "dplyr", "ggplot2", "GGally", "scales", "wesanderson"), function(pkg){
    if(! require(pkg, character.only = TRUE)) install.packages(pkg, depend = TRUE)
    library(pkg, character.only = TRUE, logical.return = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }))
) stop("Cannot resolve some dependencies; exiting now...")

if(!require("bggAnalysis")) {
  devtools::install_github("pkg, depend = TRUE")
  if(!library("bggAnalysis", logical.return = TRUE, quietly = TRUE, warn.conflicts = FALSE)) 
    stop("Cannot install bggAnalysis from GitHub; exiting now...")
}

source("https://gist.githubusercontent.com/theclue/a4741899431b06941c1f529d6aac4387/raw/f69d9b5a420e2c4707acad69f31c6e6a3c15e559/ggplot-multiplot.R")

#######################
# END OF REQS SECTION #
#######################

data("BoardGames")
BoardGames <- bgg.prepare.data(BoardGames)

## ------------------------------------------------------------------------
bgg.useful <- BoardGames %>% 
filter(!is.na(details.yearpublished)) %>% 
filter(details.yearpublished <= 2016) %>%
filter(details.yearpublished >= 1960) %>%
filter(stats.usersrated >= 5, game.type == "boardgame")

## ------------------------------------------------------------------------
# summarize by year
boardgames.by.years <- bgg.useful %>%
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

## ------------------------------------------------------------------------
# Cumulative Freq-plot of year (median in grey)
ggplot(bgg.useful, aes(as.numeric(details.yearpublished))) +
  stat_ecdf(geom="step", lwd=.5, col="red") +
  ggtitle("Cumulative freq-plot of Release Year") +
  xlab('Year') +
  ylab("Cumulative Frequency of Games") +
  geom_vline(xintercept=median(as.numeric(BoardGames$details.yearpublished), na.rm=TRUE), color = 'grey')

## ---- message=FALSE, warning=FALSE---------------------------------------
# Rating Distribution
ggplot(bgg.useful, aes(x = stats.average)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, fill="red", alpha=.2, col="deeppink") + geom_density(col="red", lwd=1) +
  xlim(0,10) +
  xlab('Avg. Rating') +
  geom_vline(xintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black")

## ------------------------------------------------------------------------
# Splitting by year class
ggplot(bgg.useful %>%
         mutate(year.discrete=as.factor(round_any(as.numeric(details.yearpublished), 5))) %>%
         filter(as.numeric(details.yearpublished) >=1960 & as.numeric(details.yearpublished) <= 2016)
       , aes(year.discrete, stats.average, fill=year.discrete)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("Avg. Rating") + xlab("Year Groups") +
  geom_hline(yintercept=mean(bgg.useful$stats.average, na.rm=TRUE), color="black")

## ------------------------------------------------------------------------
multiplot(
  ggplot(bgg.useful
         %>% filter(stats.usersrated >= 10)
         , aes(stats.average, stats.usersrated)) +
    geom_point(alpha=.2, lwd=.2, col="deeppink") +
    geom_smooth(col="blue", lwd=.7) +
    ylab("Number of Ratings") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                              labels = trans_format("log10", math_format(10^.x)),
                                              limits = c(10,10^5)) +
    xlab("Avg. Rating"),
  ggplot(bgg.useful
         %>% filter(stats.usersrated >= 10)
         , aes(stats.average, stats.usersrated)) +
    geom_point(alpha=.2, lwd=.2, col="orange") +
    geom_smooth(col="blue", lwd=.7) +
    ylab("Number of Owners") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                             labels = trans_format("log10", math_format(10^.x)),
                                             limits = c(10,10^5)) +
    xlab("Avg. Rating"), 
  cols=2)

## ------------------------------------------------------------------------
corr.palette <- wes_palette(name="Darjeeling", 3, type = "continuous")

bgg.marketplace.data <- bgg.useful[,c("stats.average", 
                            "stats.usersrated",
                            "stats.owned", 
                            "stats.wishing", 
                            "stats.wanting", 
                            "stats.trading",
                            "details.yearpublished")] %>% filter(stats.usersrated >= 100)

ggcorr(data = bgg.marketplace.data[,-which(names(bgg.marketplace.data) == "details.yearpublished")],
       label = TRUE,
       geom = "tile",
       low = corr.palette[1],
       mid = corr.palette[2],
       high = corr.palette[3],
       midpoint=0) + 
  ggtitle("Correlations between Marketplace Metrics")

## ------------------------------------------------------------------------
ggcorr(data = bgg.marketplace.data[,-which(names(bgg.marketplace.data) == "details.yearpublished")]
                %>% mutate(stats.trading.ratio=stats.trading/stats.owned)
                %>% filter(stats.trading.ratio <= .2),
       label = TRUE,
       geom = "tile",
       low = corr.palette[1],
       mid = corr.palette[2],
       high = corr.palette[3],
       midpoint=0) + 
  ggtitle("Correlations between Marketplace Metrics")

## ------------------------------------------------------------------------
multiplot(
ggplot(bgg.marketplace.data
         %>% mutate(stats.trading.ratio=stats.trading/stats.owned)
         %>% filter(stats.trading.ratio <= .2)
       , aes(stats.average, stats.trading.ratio)) +
  geom_point(alpha=.2, lwd=.2, col="deeppink") +
  geom_smooth(col="blue", lwd=.7) +
  ylab("% of Traders among Owners") + scale_y_continuous(labels=percent) +
  xlab("Avg. Rating") +
  ggtitle("Rating Influence")
  ,
ggplot(bgg.marketplace.data
         %>% mutate(stats.trading.ratio=stats.trading/stats.owned)
         %>% filter(stats.trading.ratio <= .2)
       , aes((stats.owned), stats.trading.ratio)) +
  geom_point(alpha=.2, lwd=.2, col="orange") +
  geom_smooth(col="blue", lwd=.7) +
  ylab("% of Traders among Owners") + scale_y_continuous(labels=percent) +
  xlab("Owners") + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Popularity Influence")
  , cols=2)

## ---- message=FALSE, warnings=FALSE--------------------------------------
ggplot(bgg.marketplace.data
       %>% mutate(stats.trading.ratio=stats.trading/stats.owned)
       %>% mutate(attribute.popularity=ifelse(stats.owned<10^3, "Low Popularity (<1000 Owners)", "High Popularity (>= 1000 Owners)"))
       %>% filter(stats.trading.ratio <= .2)
       , aes(stats.average, stats.trading.ratio, colour=attribute.popularity)) +
  geom_point(alpha=.2, lwd=.2) +
  geom_smooth(col="blue", lwd=.7, fullrange=TRUE) +
  ylab("% of Traders among Owners") + scale_y_continuous(labels=percent) +
  xlab("Avg. Rating") +
  facet_grid(. ~ attribute.popularity) +
  ggtitle("Rating Influence on Long-term Interest among Popularity Classes") +
  theme(legend.position="none")

## ------------------------------------------------------------------------
boardgames.traders.by.years <- (((bgg.useful[,c("details.yearpublished", "stats.average", "stats.usersrated","stats.owned", "stats.wishing", "stats.wanting", "stats.trading")]) %>%
  mutate(details.yearpublished=as.numeric(details.yearpublished)) %>%
  mutate(stats.trading.ratio=stats.trading/stats.owned) %>%
  filter(!is.na(details.yearpublished)) %>%
  filter(details.yearpublished <= 2016) %>%
  #filter(stats.trading.ratio <= .2) %>%
  filter(stats.owned >= 100) %>%
  group_by(details.yearpublished) %>%
  dplyr::summarise(stats.trading = sum(stats.trading), stats.owned = sum(stats.owned)))  %>% mutate(stats.trading.ratio=stats.trading/stats.owned))

# Splitting by year class
ggplot(boardgames.traders.by.years
         %>% mutate(year.discrete=as.factor(round_any(as.numeric(details.yearpublished), 10)))
         %>% filter(as.numeric(details.yearpublished) >=1960 & as.numeric(details.yearpublished) < 2015)
       , aes(year.discrete, stats.trading.ratio, fill=year.discrete)) +
  geom_boxplot(alpha=.4) +
  theme_bw() +
  ylab("% of Traders among Owners") + scale_y_continuous(labels=percent) +
  xlab("Decades") +
  geom_hline(yintercept=mean(boardgames.traders.by.years$stats.trading.ratio, na.rm=TRUE), color="black")

## ------------------------------------------------------------------------
multiplot(
ggplot(bgg.marketplace.data
       %>% mutate(stats.trading.ratio=stats.trading/stats.owned)
       %>% mutate(details.age=(2017 - details.yearpublished))
       %>% filter(stats.trading.ratio <= .2)
       , aes(details.age, stats.trading.ratio, col="orange")) +
  geom_point(alpha=.2, lwd=.2) +
  geom_smooth(col="blue", lwd=.5) +
  ylab("% of Traders among Owners") + scale_y_continuous(labels=percent) +
  xlab(NULL) + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Interest Drop-off with time, dropdown to various Rating Classes") +
  theme(legend.position="none")
,
ggplot(bgg.marketplace.data
       %>% mutate(stats.trading.ratio=stats.trading/stats.owned)
       %>% mutate(details.age=(2017 - details.yearpublished))
       %>% mutate(stats.rating.class=factor(ifelse(stats.average <= 4, "Low Rated", ifelse(stats.average <= 6, "Mid Rated", "Hi Rated")), levels=c("Low Rated", "Mid Rated", "Hi Rated")))
       %>% filter(stats.trading.ratio <= .2)
       , aes(details.age, stats.trading.ratio, col=stats.rating.class)) +
  geom_point(alpha=.2, lwd=.2) +
  geom_smooth(col="blue", lwd=.5) +
  ylab("% of Traders among Owners") + scale_y_continuous(labels=percent) +
  xlab("Years from First Publication") + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(. ~ stats.rating.class) +
  theme(legend.position="none")
)

## ------------------------------------------------------------------------
multiplot(
ggplot(bgg.marketplace.data
       %>% mutate(details.age=(2017 - details.yearpublished))
       , aes(details.age, stats.wishing, col="orange")) +
  geom_point(alpha=.2, lwd=.2) +
  geom_smooth(col="blue", lwd=.5) +
  ylab("Users Whishing") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  xlab(NULL) + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Wishing a Game, dropdown to various Rating Classes") +
  theme(legend.position="none")
,
ggplot(bgg.marketplace.data
       %>% mutate(details.age=(2017 - details.yearpublished))
       %>% mutate(stats.rating.class=factor(ifelse(stats.average <= 4, "Low Rated", ifelse(stats.average <= 6, "Mid Rated", "Hi Rated")), levels=c("Low Rated", "Mid Rated", "Hi Rated")))
       , aes(details.age, stats.wishing, col=stats.rating.class)) +
  geom_point(alpha=.2, lwd=.2) +
  geom_smooth(col="blue", lwd=.5) +
  ylab("Users Whishing") + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  xlab("Years from First Publication") + scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                       labels = trans_format("log10", math_format(10^.x))) +
  facet_grid(. ~ stats.rating.class) +
  theme(legend.position="none")
)

