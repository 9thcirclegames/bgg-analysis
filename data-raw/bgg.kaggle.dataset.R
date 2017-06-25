# This function creates a sqllite file suitable for being uploaded on Kaggle Datasets area

if (!require("pacman")) install.packages("pacman")
pacman::p_load("sqldf")

db <- dbConnect(SQLite(), dbname="data/database.sqlite")

on.exit(dbDisconnect(db))

dbWriteTable(conn = db, name = "BoardGames", BoardGames, overwrite=T, row.names=TRUE)

dbWriteTable(conn = db, name = "bgg.ldaOut.topics", as.data.frame(topics(bgg.ldaOut)), overwrite=T, row.names=TRUE)
dbWriteTable(conn = db, name = "bgg.ldaOut.top.terms", bgg.ldaOut.top.terms, overwrite=T, row.names=TRUE)
dbWriteTable(conn = db, name = "bgg.ldaOut.top.documents", bgg.ldaOut.top.documents, overwrite=T, row.names=TRUE)
dbWriteTable(conn = db, name = "bgg.topics", bgg.topics, overwrite=T, row.names=TRUE)


