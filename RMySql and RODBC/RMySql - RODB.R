library(RMySQL)

con<-dbConnect(MySQL(),dbname="hrms",username="root",password="")

dbListTables(con)

import.sql1<-dbGetQuery(con,"SELECT * FROM main_cities")

class(import.sql1)

dbDisconnect(con)

library(RODBC)
#dsn has to be specified, this will be linked to only one database in sql server. dsn can be changed by accessing control panel, admnistrative tools, odbc data sources
connection<-odbcConnect("mysql",uid="root",pwd="") 
#Here mysql is the name of the DSN Connection.

sqlTables(connection)

import.sql2<-sqlQuery(connection,paste("SELECT * from main_cities"))

close(connection)

