# Set up the dataset
library(RSQLite)
con <- dbConnect(SQLite(), ":memory:") # setup databest (con: connection) in memory on local
dbWriteTable(con, "regional", chs_regional) # write table to database 
dbWriteTable(con, "individual", chs_individual)

# SQL INNER join 
ans <- dbGetQuery(con,
                  "SELECT *
  FROM regional AS a INNER JOIN individual AS b 
  ON a.townname = b.townname
  ") # write as a character string in R, and what to match on 
nrow(ans)

# SQL LEFT join
ans <- dbGetQuery(con, # keep everything from regional and anything that matches in individual dataset
                  "SELECT *
  FROM regional AS a LEFT JOIN individual AS b 
  ON a.townname = b.townname")
nrow(ans)

# SQL RIGHT join 
ans <- dbGetQuery(con,
                  "SELECT * /* RIGHT joins are not supported by SQLite*/
  FROM individual AS a LEFT JOIN regional AS b 
  ON a.townname = b.townname")
nrow(ans)

# SQL FULL join 
ans <- dbGetQuery(con,
                  "SELECT *
  FROM individual AS a FULL JOIN regional AS b 
  ON a.townname = b.townname")
nrow(ans)

# Discconet Database
dbDisconnect(con) # when done shut down database

# Distinct entries 
ans <- dbGetQuery(con,
                  "SELECT DISTINCT * FROM regional_dpl AS a INNER JOIN regional_dpl AS b 
    ON a.townname = b.townname
  ") # removes the duplicates
nrow(ans)