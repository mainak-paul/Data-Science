# Reading a CSV file from local
good <- read.table("Sample.csv",sep=",",header=T,stringsAsFactor=F)
# need to specify path, also separator

good2 <- read.csv("Sample.csv",stringsAsFactor=F)
# need to specify path

library(XML)
web1<-readHTMLTable("http://www.inflationdata.com/Inflation/Consumer_Price_Index/HistoricalCPI.aspx?reloaded=true")
class(web1)
str(web1)

library(data.table)
web2<-fread("https://robjhyndman.com//tsdldata//roberts//skirts.dat",skip=5,showProgress=TRUE)
class(web2)
str(web2)

library(jsonlite)
web3<-fromJSON("http://api.glassdoor.com/api/api.htm?v=1&format=json&t.p=47699&t.k=g9GdVHlQ1eM&action=employers&q=pharmaceuticals&userip=192.168.43.42&useragent=Mozilla/%2F4.0")
class(web3)
str(web3)
