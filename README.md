# What makes a game a _good_ game?
This package provides a skeleton and a set of data related to Board Games. This should be intended as a playing sandbox to you to analyze data about tabletop gaming and to undestand what makes a game a successful game.

Endless questions could arise while designing/promoting a board games. For ex:

* How many players should it have to be designed for?
* Which attributes does it has to have?
* Does it have to implements standard or custom mechanics?
* Should it have to be designed/endorser by a very influencer game designer?
* Should it be promoted trough crowdfunding or traditional marketing campaign?
* and so on...

## What will you get from this package?
A bounce of datasets to play with:

* **BoardGames**: including all the games from [BoardGameGeek](https://boardgamegeek.com) database, one game/expansion per row.
* **KickstarterCampaigns**: including all the crowdfunding campaigns made on [Kickstarter](https://www.kickstarter.com) so far in the _TableTop Games_ category, one campaign per row.
* **IndiegogoCampaigns**: same of the above, but on [IndieGogo](https://www.indiegogo.com)

Additionally, you'll find all the functions used to crawl the data (folder _data-raw_) and a bounce of useful transormation functions.

All the data and the functions are fully documented.

## Why a package?
Refer to this tutorial to understand how to perform an analysis task in R using a package and why you should do this way: [Analysis as Package](http://rmflight.github.io/posts/2014/07/analyses_as_packages.html).

This way, each analysis excercise would stay in its how vignette.

## How to install it
Since you won't find this package on CRAN, you must install it using _devtools_ and getting the source directly from the repo on GitHub:

```R
# install.packages("devtools")
devtools::install_github("theclue/bgg-analysis")
```

## How to load the datasets
Datasets are bundled with the package. Use the following commands to import in the workspace:
```R
data(BoardGames)
data(KickstarterCampaigns)
data(IndieGogoCampaigns)
```
