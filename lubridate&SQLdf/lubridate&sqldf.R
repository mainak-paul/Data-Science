
##Date
fd<-read.csv("C:\\Users\\mainak\\Desktop\\DADAI\\Data-Science\\Data Science With R\\lubridate&SQLdf\\Fd.csv")
str(fd)

fd$FlightDate<-as.Date(fd$FlightDate,"%d-%b-%y")
str(fd)

#Months of date object
unique(months(fd$FlightDate))

#Days of date object
unique(weekdays(fd$FlightDate))

#Finding time interval 

fd$FlightDate[60]-fd$FlightDate[900]

#Finding time interval using difftime
difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "weeks")

difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "days")

difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "hours")
difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "year")

as.numeric(difftime(Sys.Date(),'1989-03-14',units = "weeks"))/52.25

#finding difference between today and flightDate
fd$currentdiffYear <- as.numeric(difftime(Sys.Date(),fd$FlightDate,units = "weeks"))/52.25


#Subsetting data based on time information
library(dplyr)

#Subset the data for day=Sunday
dim(fd)
fd_s<-fd%>%filter(weekdays(FlightDate)=="Sunday")
dim(fd_s)
#Find the number of flights on Sundays for destination Atlanta
fd%>%filter(weekdays(FlightDate)=="Sunday",DestCityName=="Atlanta, GA")%>%nrow()
#Find the number of flights on Sundays for all cities
fd%>%filter(weekdays(FlightDate)=="Sunday")%>%group_by(DestCityName)%>%summarize(n())
#PISIXct and POSIXlt
date1<-Sys.time()
date1
class(date1)
weekdays(date1)
months(date1)
date2<-as.POSIXlt(date1)
date2
str(date2)
unclass(date2)
date2$wday
date2$zone
date2$hour

library(lubridate)
ref = as.Date('14/03/89', format = '%d/%m/%y')
today = as.Date(Sys.Date(), format = '%d/%m/%y')
yr = duration(num=1, units = 'years')
data = as.data.frame(ref)
data$Age = interval(data$ref,today)/yr
data

#sqldf
library(sqldf)
#Using SELECT statement
fd_s<-sqldf("SELECT (julianday('now') - julianday(FlightDate))/(86400) FROM fd")
fd_y<-sqldf("SELECT CAST(strftime('%Y',(strftime('%s','now') - strftime('%s',FlightDate))) AS real) FROM fd")


