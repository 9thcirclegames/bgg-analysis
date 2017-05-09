require("rjson")
require("RCurl")

kickstarter.url <- "https://s3.amazonaws.com/weruns/forfun/Kickstarter/Kickstarter_2017-04-15T22_21_18_122Z.zip"

tmpdir <- tempdir()
kickstarter.file <- basename(kickstarter.url)

download.file(kickstarter.url, kickstarter.file)

unzip(kickstarter.file, exdir = tmpdir)

list.files(tmpdir)

temp = paste(tmpdir, list.files(tmpdir, pattern="*.csv"), sep="/")
kickstarter.data = lapply(paste(tmpdir, list.files(tmpdir, pattern="*.csv"), sep="/"), read.delim)
